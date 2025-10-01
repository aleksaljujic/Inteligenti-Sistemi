data <- read.csv("wines.csv", stringsAsFactors = F)

data$X <- NULL

str(data)

data <- subset(data, (data$country == "Argentina" | data$country == "France" | data$country == "Italy"))

str(data)

#apply(data, 2, function(x) sum(x == ""))

colSums(data == "")
colSums(data == " ")
colSums(data == "-")
colSums(is.na(data))

data$designation <- NULL

table(data$description)

data$description <- NULL

str(data)

length(unique(data$province))
length(unique(data$region))
length(unique(data$title))
length(unique(data$variety))
length(unique(data$winery))

data$region <- NULL
data$title <- NULL
data$winery <- NULL
data$variety <- NULL


data$country <- as.factor(data$country)

colSums(is.na(data))

shapiro.test(data$price)

medianPrice <- median(data$price, na.rm = T)

data$price[is.na(data$price)] <= medianPrice

percentil25 <- quantile(data$price, 0.25, na.rm = T)

data$price_category <- ifelse(data$price <= percentil25, yes="cheap", no="not_cheap")

data$price <- NULL

data$price_category <- as.factor(data$price_category)

shapiro.test(data$points)


library(bnlearn)

library(ggplot2)

ggplot(data, aes(x = points)) + geom_histogram()

data$points <- as.numeric(data$points)
to.discretize <- as.data.frame(data$points)

discretize <-
            discretize(to.discretize,
            method = "quantile",
            breaks = c(5)) 


data <- as.data.frame(cbind(discretize, data[,c(1,3,4)]))

data$points_bin <- data$`data$points`

data <- data[,c(5,2:4)]

library(caret) 
set.seed(1010)  
indexes <- createDataPartition(data$price_category, p = 0.8, list = FALSE) 
train.data <- data[indexes, ] 
test.data <- data[-indexes, ] 

 library(e1071)

nb <- naiveBayes(price_category ~ ., data = train.data)

nb1.pred <- predict(object = nb, 
                    newdata = test.data, 
                    type = "class")

nb1.pred

getEvaluationMetrics <- function(cm){
  TP <- cm[1,1]
  TN <- cm[2,2]
  FP <- cm[2,1]
  FN <- cm[1,2]
  
  accuracy <- (TP+TN)/(TP+TN+FP+FN)
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  f1 <- (2*precision*recall)/(precision+recall)
  
  c(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1 = f1
  )
}

nb1.eval <- getEvaluationMetrics(nb1.cm)
nb1.eval

nb2.pred.prob <- predict(object = nb,
                  newdata = test.data,
                  type = "raw") 


library(pROC)
nb2.roc <- 
           roc(response = test.data$price_category>,
           predictor = nb2[, <1 | 2>]
           class!)
# <ROC curve parameters>$auc # extract and show AUC
# plot.roc(<ROC curve parameters>, # computed in the previous step
# print.thres = TRUE, # show the probability threshold (cut-off point) on the plot
# print.thres.best.method =
# "youden" | # maximize the sum of sensitivity and specificity (the distance to the
diag. line)
# "closest.topleft") # minimize the distance to the top-left point of the plot
# <ROC coords> <- coords(<ROC curve parameters>, # computed in the previous step
# ret = c("accuracy", "spec", "sens", "thr", ...), # ROC curve parameters to return
# x = # the coordinates to look for:
# "local maximas" | # local maximas of the ROC curve
# "best" | ...) # the point with the best sum of sensitivity and
specificity, i.e.
# # the same as the one shown on the ROC curve

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

