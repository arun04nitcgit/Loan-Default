
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(e1071)
library(xgboost)
library(stringr)
library(lubridate)
library(tm)
library(rms)
library(glmnet)
library(pROC)
library(doMC)
library(kernlab)



## 75% of the sample size
smp_size <- floor(0.75 * nrow(loans.train))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(loans.train)), size = smp_size)

train <- loans.train[train_ind, ]
test <- loans.train[-train_ind, ]

plotdata = traindata
p = ggplot(plotdata,aes(x=ApplicantIncome,  y=Loan_Status, color=Loan_Status))
p + geom_jitter(alpha=0.3) +  scale_color_manual(breaks = c('Y','N'),  values=c('red','darkgreen'))

p = ggplot(plotdata,aes(x=Property_Area,  y=Loan_Status, color = Loan_Status))
p + geom_jitter(alpha=0.3) +  scale_color_manual(breaks = c('Y','N'), values=c('red','darkgreen'))


p = ggplot(plotdata,aes(x=Loan_Status,  y=Credit_History, color=Loan_Status))
p + geom_jitter(alpha=0.3) +  scale_color_manual(breaks = c('Y','N'),   values=c('red','darkgreen'))


levels(traindata$Loan_Status)
train$Loan_Amount_Term <- train$Loan_Amount_Term/10

table(train$Loan_Status, train$Loan_Amount_Term)
train <- subset( train, select = -c( Loan_ID ))

loans.rpart.1 <- rpart(Loan_Status ~ . , data = train, 
                       control=rpart.control(minsplit=10, minbucket = 3, cp=0.0006))

summary(loans.rpart.1)
predicted= predict(loans.rpart.1,test)

results_prob <- predict(loans.rpart.1,test, type='class')
View(results_prob)
table(test$Loan_Status, results_prob)
summary(results_prob)
summary(test$Loan_Status)


logistic <- glm(Loan_Status ~ ., data = train,family='binomial')

summary(logistic)
predicted_L <- predict(logistic, test)
summary(predicted_L)


rpart.plot(loans.rpart.1)
fancyRpartPlot(loans.rpart.1)

loans.test <- subset( Loan.Prediction.test, select = -c( Loan_ID ))
predictions.1 <- predict(loans.rpart.1, loans.test, type = "class")
View(predictions.1)

library(e1071)

# Fitting model
fit_svm <-svm(Loan_Status ~ ., data = loans.train)
summary(fit_svm)
#Predict Output 
predicted_svm <- predict(fit_svm,loans.test)



library(randomForest)

# Fitting model
fit_rf <- randomForest(Loan_Status ~ ., data = loans.train,ntree=500, na.action = na.omit)
summary(fit_rf)
#Predict Output 

prediction_rf <- predict(fit_rf, loans.test)

fancyRpartPlot(fit_rf)


# XG Boost
require(caret)
# Fitting model
TrainControl <- trainControl( method = "repeatedcv", number = 10, repeats = 4)
model<- train(Loan_Status ~ ., data = loans.train, method = "xgbLinear", trControl = TrainControl,na.action = na.omit, verbose = FALSE)

