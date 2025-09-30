#Ucitavanje podataka
data <- read.csv("travel-times.csv", stringsAsFactors = F)
#informacije po kolonama
str(data)
#statistike informacije po kolonama (mean,median, std,...)
summary(data)

#provera nedostajucih vrednosti
# apply(data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
# apply(data, MARGIN = 2, function(x) sum(x==" "))
# apply(data, 2, function(x) sum(x == "-"))
# apply(data, 2, function(a) sum(a == ""))
# ?apply

#drugi lepsi i brzi nacin za proveru nedostajucih vrednosti
colSums(data == "" | data == "-" | data == " " | is.na(data))

#izbacivanje kolona
data$Date <- NULL
data$StartTime <- NULL
data$DayOfWeek <- NULL

#provera tipa podataka u koloni
class(data$GoingTo)
#provera vrednosti u koloni
table(data$GoingTo)

#eksplicitno pretavranje kolone u character variablu (mozeda i ne mora)
data$GoingTo <- as.character(data$GoingTo)
#zamena svih "" vrednosti sa vredinosti koja se najvise pojavljuje u koloni
data$GoingTo[data$GoingTo == ""] <- "Work"
#pretvaranje character varijable u faktorsku kako bi mogla da radi u modelu
data$GoingTo <- factor(data$GoingTo, levels = c("Home", "Work"))

#informacije po kolonama
str(data)

#pretavranje nedostajucih vrednosti u NA kako bi mogli da izracunamo medijanu
data$FuelEconomy[data$FuelEconomy == "" | data$FuelEconomy == " " | data$FuelEconomy == "-" | is.na(data$FuelEconomy)] <- NA
#pretvaranje kolone u numericku eksplicitno
data$FuelEconomy <- as.numeric(data$FuelEconomy)

#provera da li kolona ima normalnu raspodelu
shapiro.test(na.omit(data$FuelEconomy))
#shapiro.test(data$FuelEconomy[!is.na(data$FuelEconomy)])
#saznanjem da kolona nema normalnu raspodelu jer je sig < 0.05 NA vrednosti menjamo medijanom kolone

#kreiranje nove promenjive koja je medijana kolone
medianFuelEconomy <- median(data$FuelEconomy, na.rm = T)

#brisanje promenjive (samo primer)
rm(meanFuelEconomy)

#zameni svih NA vrednosti sa medijanom
data$FuelEconomy[is.na(data$FuelEconomy)] <- medianFuelEconomy

#kreiranje promenjive koja oznacava 60-ti percentil kolone
percentil60 <- quantile(data$Congestion407, 0.6)
percentil60

#kreiranje target promenjive po principu ako je Congestion407 manji od 60-tog percentila 
#i observacija nije unela komentar onda ce biti "Yes" u svim ostalim  slucajevima je "No"
data$Take407All <- ifelse(data$Congestion407 < percentil60 & data$Comments == "",
                          yes = "Yes", no = "No")

#pretvaranje taget kolone u faktor
data$Take407All <- as.factor(data$Take407All)

#izbacivanje kolona koje su ucestvovale u kreiranju target kolone radi sprecavanje curenja podataka 
#i nerealnih performansi modela
data$Congestion407 <- NULL
data$Comments <- NULL

str(data)

#iscratavnje box plot-a za kolonu MaxSpeed kako bi se utvrdilo postojanje outlajera
boxplot(data$MaxSpeed)

#statisticki zapis box plot-a
boxplot.stats(data$MaxSpeed)

#broj aoutlajera za datu kolonu
length(boxplot.stats(data$MaxSpeed)$out)

#provera za svaku kolonu koristeci apply funkciju
apply(data, 2, function(x) boxplot.stats(data$MaxSpeed))
apply(data[,2:8], 2, function(x) length(boxplot.stats(data$MaxSpeed)$out))

?scale

data.std <- apply(data[,2:8], 2, function(x) scale(x, median(x), IQR(x)))

data.std <- as.data.frame(data.std)


goingTo <- as.integer(data$GoingTo)
goingToMinus1 <- as.integer(data$GoingTo) - 1
rm(goingTo)
rm(goingToMinus1)

data.std$GoingTo <- as.integer(data$GoingTo) - 1
data.std$Take407All <- data$Take407All

data.std <- data.std[,c(8,1:7,9)]

str(data.std)

summary(data.std)





















