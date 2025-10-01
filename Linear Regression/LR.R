###############################################
# ANALIZA VIDEO IGARA – LINEARNA REGRESIJA U R
# ---------------------------------------------
# Cilj: Predvideti User_Score na osnovu različitih
# faktora (platforma, godine, rejting, prodaja,
# kritički skorovi i sl.)
#
# Teorijski okvir:
# - Prvo čistimo podatke (data cleaning)
# - Pravimo dummy promenljive za kategorije
# - Imputiramo nedostajuće vrednosti
# - Proveravamo normalnost distribucija
# - Gradimo više linearnih modela
# - Proveravamo multikolinearnost (VIF)
# - Koristimo stepwise selekciju (AIC kriterijum)
###############################################

# Učitavanje CSV fajla sa podacima
data <- read.csv("video_games.csv")

# Pregled strukture dataset-a (koliko ima redova, kolona, tipovi podataka)
str(data)

# Filtriramo samo igre sa platformi PS2, PS3 i PS4 
# (jer nas zanimaju moderne konzole i da izbegnemo heterogenost podataka)
data <- subset(data, data$Platform == "PS2" | data$Platform == "PS3" | data$Platform == "PS4")

# Kreiranje dummy promenljivih za Platform (jer regresija ne može direktno da koristi string kategorije)
# Teorija: One-hot encoding pretvara kategoriju u binarne promenljive 0/1
dummies <- model.matrix(~Platform - 1, data=data) 
dummies <- as.data.frame(dummies)

# Dodajemo dummy promenljive nazad u dataset
data <- cbind(data, dummies)

# Biramo relevantne kolone za dalju analizu (dummy-jevi + prodaja + rating)
# Praktično: fokus na kvantitativnim i kategoričkim varijablama koje ulaze u regresiju
data_cleaned <- data[,c(17:19,6:14,16)]

# Pregled strukture dataset-a da proverimo da li je sve na mestu
str(data)

# Deskriptivna statistika – koliko ima igara po žanru, izdavaču i rejtingu
# Teorija: ovo je korisno za razumevanje distribucije podataka
table(data$Genre)
table(data$Publisher)
table(data$Rating)

# Provera nedostajućih vrednosti (NA) i praznih stringova u koloni Rating
sum(is.na(data$Rating))
sum(data$Rating == "")

# Prazne vrednosti u Rating-u zamenjujemo sa "T" (Teen)
# Teorija: imputacija kategorija – umesto da bacimo redove, dodeljujemo najčešću/moguću vrednost
data$Rating[data$Rating == ""] <- "T"

# Ponovo proveravamo da li su sve praznine popunjene
sum(data$Rating == "")

# Kreiranje dummy promenljivih za Rating (isto kao i za Platform)
dummies_rating <- model.matrix(~Rating - 1, data=data)
dummies_rating <- as.data.frame(dummies_rating)

# Dodajemo rating dummies u dataset
data_cleaned <- cbind(data_cleaned, dummies_rating)

# Redosled kolona prilagođavamo da bude pregledan
data_cleaned <- data_cleaned[,c(1:3,14:17,4:10,12,11)]

# Koliko unikatnih godina izdavanja igara ima
length(unique(data$Year_of_Release))
table(data$Year_of_Release)

# Neke godine imaju vrednost "N/A" → zamenjujemo ih sa 2008 (imputacija po domenu)
data$Year_of_Release[data$Year_of_Release == "N/A"] <- "2008"

# Godinu pretvaramo u numerički tip (regresija ne radi sa stringovima)
data$Year_of_Release <- as.numeric(as.factor(data$Year_of_Release))

# Ubacujemo u data_cleaned
data_cleaned$Year_of_Release <- data$Year_of_Release

# Premeštamo kolone tako da Year_of_Release bude prva
data_cleaned <- data_cleaned[,c(17,1:16)]

# Pregled dataset-a posle svih transformacija
str(data_cleaned)

# Konvertujemo User_Score iz char u numerički tip
# Teorija: regresija zahteva numerički target
data_cleaned$User_Score <- as.numeric(as.character(data$User_Score))

# Proveravamo razne oblike nedostajućih vrednosti u kolonama
colSums(data_cleaned[,9:17] == "", na.rm=T)
colSums(data_cleaned[,9:17] == " ", na.rm=T)
colSums(data_cleaned[,9:17] == "-", na.rm=T)
colSums(data_cleaned[,9:17] == "N/A", na.rm=T)
colSums(is.na(data_cleaned[,9:17]))

# User_Score = 1 tretiramo kao outlier → pretvaramo u NA
data_cleaned$User_Score[data_cleaned$User_Score == 1] <- NA

# Ponovna provera broja NA vrednosti
colSums(is.na(data_cleaned[,9:17]))

# Normality test za User_Score (Shapiro-Wilk test)
# Teorija: Ako p < 0.05 → distribucija odstupa od normalne
shapiro.test(data_cleaned$User_Score)

# Pošto nije normalno → imputacija medianom
userScoreMedian <- median(data_cleaned$User_Score, na.rm=T)
data_cleaned$User_Score[is.na(data_cleaned$User_Score)] <- userScoreMedian

# Isto radimo za User_Count
shapiro.test(data_cleaned$User_Count)
userCountMedian <- median(data_cleaned$User_Count, na.rm=T)
data_cleaned$User_Count[is.na(data_cleaned$User_Count)] <- userCountMedian

# Isto za Critic_Count
shapiro.test(data_cleaned$Critic_Count)
criticCountMedian  <- median(data_cleaned$Critic_Count, na.rm=T)
data_cleaned$Critic_Count[is.na(data_cleaned$Critic_Count )] <- criticCountMedian

# Isto za Critic_Score
shapiro.test(data_cleaned$Critic_Score)
criticScoreMedian  <- median(data_cleaned$Critic_Score, na.rm=T)
data_cleaned$Critic_Score[is.na(data_cleaned$Critic_Score )] <- criticScoreMedian

# Dataset je sada čist i spreman
str(data_cleaned)

# Korelaciona matrica (koristimo Pearsonovu korelaciju)
# Teorija: Korisno za proveru linearne veze između prediktora
library(corrplot)
matrix <- cor(data_cleaned)
corrplot(matrix, method = "number", diag = FALSE, type = "upper") 

# Delimo podatke na train i test (80/20)
library(caret)
set.seed(1010)
index <- createDataPartition(data_cleaned$User_Score, p = 0.8, list = F)
train.data <- data_cleaned[index,]
test.data <- data_cleaned[-index,]

###############################################
# 1) Jednostavna regresija (lm1)
# - Proveravamo samo efekat Critic_Score
###############################################
lm1 <- lm(User_Score ~ Critic_Score, data = train.data)
summary(lm1) 
# Teorija: R² ~ 0.24 znači da Critic_Score objašnjava oko 24% varijanse User_Score

###############################################
# 2) Višestruka regresija (lm2)
# - Uključujemo sve prediktore osim PlatformPS4 i RatingT 
###############################################
lm2 <- lm(User_Score ~ . - PlatformPS4 - RatingT, data = train.data)
summary(lm2) 
# Praktično: dodali smo sve dostupne prediktore → R² raste na ~0.35
# Ali proveravamo multikolinearnost

# Provera multikolinearnosti
library(car)
vif(lm2)
sort(sqrt(vif(lm2)))
# Teorija: VIF > 10 → jaka multikolinearnost
# Ovde su NA_Sales, EU_Sales i Global_Sales ekstremno visoki → redundantni prediktori

###############################################
# 3) Višestruka regresija (lm3)
# - Izbacujemo Global_Sales jer najviše duplira informacije
###############################################
lm3 <- lm(User_Score ~ . - PlatformPS4 - RatingT - Global_Sales, data = train.data)
summary(lm3)  
vif(lm3)
sort(sqrt(vif(lm3)))
# Teorija: sada su VIF vrednosti smanjene → model je stabilniji

###############################################
# 4) Stepwise selekcija (bazirano na AIC)
# - Automatski bira najbolji subset prediktora
###############################################
step(lm2)
# Teorija: AIC balansira fit modela i broj parametara
# Manji AIC = bolji model

###############################################
# 5) Finalni model (lm4)
# - Na osnovu stepwise procedure
###############################################
lm4 <- lm(User_Score ~ Year_of_Release + PlatformPS2 + RatingE + `RatingE10+` + NA_Sales + EU_Sales + Critic_Score, data = train.data)
summary(lm4)
sort(sqrt(vif(lm4)))
# Teorija: Ovaj model je jednostavan, stabilan i interpretabilan
# Najvažniji prediktor je Critic_Score, a zatim rejting i platforma


# Zatvaramo sve postojeće grafike da bismo počeli "čisto"
graphics.off()

# Podešavamo layout za dijagnostičke grafike regresije:
# 2 reda x 2 kolone → prikazaće 4 standardna dijagnostička grafa (residuals vs fitted, QQ-plot, scale-location, Cook's distance)
par(mfrow = c(2,2))

# Plotujemo dijagnostičke grafike za lm4 model
plot(lm4)

# --- PREDIKCIJA NA TEST SKUPU ---
# Pravimo predikcije User_Score na osnovu modela lm4
lm4.pred <- predict(lm4, test.data)

# Pogledamo prvih nekoliko predikcija
head(lm4.pred)

# Pogledamo stvarne vrednosti User_Score u test skupu radi poređenja
head(test.data$User_Score) 


# --- IZRAČUNAVANJE METRIKA ---
# Formula: R² = 1 - RSS/TSS
# RSS = Residual Sum of Squares (zbir kvadrata razlika između predikcija i stvarnih vrednosti)
RSS <- sum((lm4.pred - test.data$User_Score)^2)

# TSS = Total Sum of Squares (zbir kvadrata razlika između stvarnih vrednosti i srednje vrednosti u TRAIN skupu)
# Koristimo mean(train.data$User_Score) jer model uči na train skupu
TSS <- sum((mean(train.data$User_Score) - test.data$User_Score)^2)

# R² pokazuje koliko procenata varijanse zavisne promenljive model objašnjava
r_squared <- 1 - RSS / TSS
r_squared

# Pogledamo osnovni rezime lm4 modela
summary(lm4)

# RMSE = Root Mean Squared Error
# Standardna metrika koja pokazuje prosečno odstupanje predikcije od stvarnih vrednosti
RMSE <- sqrt(RSS / nrow(test.data))
RMSE

# Računamo prosečnu vrednost User_Score da bismo dobili relativnu grešku
mean(test.data$User_Score)

# Relativna greška = RMSE / prosek skora → pokazuje koliki je prosečan procenat greške u odnosu na tipičan skor
mean_error <- RMSE/mean(test.data$User_Score)
mean_error

# Dodajemo kolonu sa predikcijama u test skup radi vizualizacije
test.data$User_Score_pred <- lm4.pred


# --- VIZUALIZACIJA DISTRIBUCIJE ---
# Pravimo density plot (glađenje distribucije) da uporedimo distribuciju stvarnih i predikovanih vrednosti
# Ako se krive preklapaju → model dobro predviđa raspodelu
library(ggplot2)
ggplot(data = test.data) +
  geom_density(aes(x=User_Score, color = 'actual')) +     # Stvarna distribucija
  geom_density(aes(x=User_Score_pred, color = 'predicted'))  # Predikovana distribucija



