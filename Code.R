library(readr)
data <- read_delim("D:/Files/Sem 5/Bayesian/cardio_train.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

head(data)
str(data)
summary(data)

dim(data)

#mencari missing value
sum(is.na(data))

#model logistik
model <- glm(as.factor(cardio) ~ ap_hi + ap_lo + as.factor(data$cholesterol) + 
               as.factor(data$smoke) + as.factor(data$alco), data = data, family = "binomial")
summary(model)


#uji signifikansi serentak
#H0: beta1 = beta2 = ... = betai
#H1: terdapat satu atau lebih nilai beta yang tidak sama dengan 0
library(lmtest)
lrtest(model)

#Karena nilai p-value uji < 0.05, maka tolak H0


#uji signifikansi parsial
#H0: betai = betai
#H1: betai != betai
summary(model)

#Karena semua nilai p-value uji < 0.05, maka semua tolak H0


#goodness fit test
#H0: model sudah sesuai
#H1: model belum sesuai
library(ResourceSelection)
hoslem.test(data$cardio, fitted(model))

yhat = fitted(model)

prediction = as.factor(ifelse(round(yhat)==1, "cardio disease", "No cardio disease"))
actual = as.factor(ifelse(data$cardio==1, "cardio disease", "No cardio disease"))

library(caret)
confusionMatrix(prediction, actual)

#model bayesian
set.seed(45)
library(rstanarm)
bayesModel = stan_glm(as.factor(cardio) ~ ap_hi + ap_lo + as.factor(data$cholesterol) + 
                        as.factor(data$smoke) + as.factor(data$alco), data = data, family = "binomial")
summary(bayesModel)

#nilai intercept
bayesModel$coefficients

#cek signifikansi parameter
posterior_interval(bayesModel)

library(ResourceSelection)
hoslem.test(data$cardio, fitted(bayesModel))

#confusion matrix
yhatBayes = fitted(bayesModel)

predictionBayes = as.factor(ifelse(round(yhatBayes)==1, "cardio disease", "No cardio disease"))
actualBayes = as.factor(ifelse(data$cardio==1, "cardio disease", "No cardio disease"))

library(caret)
confusionMatrix(predictionBayes, actualBayes)

