data <- read.csv("video_games.csv")

str(data)

data <- subset(data, data$Platform == "PS2" | data$Platform == "PS3" | data$Platform == "PS4")

dummies <- model.matrix(~Platform - 1, data=data)
dummies

dummies <- as.data.frame(dummies)
dummies

data <- cbind(data, dummies)

data_cleaned <- data[,c(17:19,6:14,16)]

str(data)

table(data$Genre)
table(data$Publisher)
table(data$Rating)

sum(is.na(data$Rating))
sum(data$Rating == "")

data$Rating[data$Rating == ""] <- "T"

sum(data$Rating == "")

dummies_genre <- model.matrix(~Genre - 1, data=data)
dummies_genre <- as.data.frame(dummies_genre)

dummies_rating <- model.matrix(~Rating - 1, data=data)
dummies_rating <- as.data.frame(dummies_rating)


data_dummies <- cbind(dummies_rating, dummies_genre)

data_cleaned <- cbind(data_cleaned, data_dummies)

data_cleaned <- data_cleaned[,c(1:3,14:29,4:12)]

length(unique(data$Year_of_Release))

table(data$Year_of_Release)

data$Year_of_Release[data$Year_of_Release == "N/A"] <- "2008"

data$Year_of_Release <- as.numeric(as.factor(data$Year_of_Release))

data_cleaned$Year_of_Release <- data$Year_of_Release

data_cleaned <- data_cleaned[,c(29,1:28)]

str(data_cleaned)

data_cleaned$User_Score <- as.numeric(as.factor(data_cleaned$User_Score))

colSums(data_cleaned[,21:29] == "", na.rm=T)
colSums(data_cleaned[,21:29] == " ", na.rm=T)
colSums(data_cleaned[,21:29] == "-", na.rm=T)
colSums(data_cleaned[,21:29] == "N/A", na.rm=T)
colSums(is.na(data_cleaned[,21:29]))

data_cleaned$User_Score[data_cleaned$User_Score == 1] <- NA

colSums(is.na(data_cleaned[,21:29]))

shapiro.test(data_cleaned$User_Score)
userScoreMedian <- median(data_cleaned$User_Score, na.rm=T)
data_cleaned$User_Score[is.na(data_cleaned$User_Score)] <- userScoreMedian

shapiro.test(data_cleaned$User_Count)
userCountMedian <- median(data_cleaned$User_Count, na.rm=T)
data_cleaned$User_Count[is.na(data_cleaned$User_Count)] <- userCountMedian

shapiro.test(data_cleaned$Critic_Count)
criticCountMedian  <- median(data_cleaned$Critic_Count, na.rm=T)
data_cleaned$Critic_Count[is.na(data_cleaned$Critic_Count )] <- criticCountMedian

shapiro.test(data_cleaned$Critic_Score)
criticScoreMedian  <- median(data_cleaned$Critic_Score, na.rm=T)
data_cleaned$Critic_Score[is.na(data_cleaned$Critic_Score )] <- criticScoreMedian

str(data_cleaned)

library(corrplot)

matrix <- cor(data_cleaned[,c(1:8,21:29)])

data_cleaned <- data_cleaned[,c(1:27,29,28)]

corrplot(matrix, method = "number", diag = FALSE, type = "upper") 

library(caret)

set.seed(1010)

index <- createDataPartition(data_cleaned$User_Score, p = 0.8, list = F)
train.data <- data_cleaned[index,]
test.data <- data_cleaned[-index,]





















 
