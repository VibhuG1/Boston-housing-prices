rm(list=ls())
library(rpart)
library(ggplot2)
library(dplyr)
library(DMwR)
library(lmtest)

setwd("~/Desktop/Project")
boston  <- read.csv("Boston_train.csv", stringsAsFactors = F)
test_H  <- read.csv("Boston_test.csv", stringsAsFactors = F)

str(boston)
dim(boston)
summary(boston)
colSums(is.na(boston))

library(corrgram)
library(corrplot)
B <- cor(boston)
corrplot(B, method = "number")

boston <-  subset(boston, select= -c(ID))

#visualisation
boston %>%
  ggplot(aes(x = medv)) +
  stat_density() +
  labs(x = "Median Value ($1000s)", y = "Density", title = "Density Plot of Median Value House Price in Boston") +
  theme_minimal()

a <- ggplot(boston, aes(x =medv))
a + geom_histogram(color = "black", fill = "gray",bins = 40)+ 
  geom_vline(aes(xintercept=mean(medv)),color="#FC4E07", linetype="dashed", size=1) +
  theme_minimal()


#high vif
boston<-  subset(boston, select= -c(tax))
#statistically insignificant
boston<-  subset(boston, select= -c(indus,age,zn))
#outliers
boston <- boston[-c(249,264,333),]

set.seed(420)

#splitting
library(caTools)
ind <- sample.split( Y = boston$medv, SplitRatio  = 0.7)
train <- boston[ind,]
test <-  boston[!ind,]

dim(train)
dim(test)

model = lm(log(medv)~. , data=train)

library(car)

#to check multicollinearity
vif(model)
summary(model)
summary(model)$r.squared
outlierTest(model)

predicted_traindf <- as.data.frame(predict(model, newdata = train, interval = "prediction"))

predicted_train <- cbind(train,predicted_traindf)


predicted_testdf <- as.data.frame(predict(model, newdata = test, interval = "prediction"))
colnames(predicted_testdf)

predicted_test <- cbind(test,predicted_testdf)

predicted_train <- predicted_train %>% mutate(squared_err = (log(medv)- fit)^2 , err = log(medv) - fit)

predicted_test <-  predicted_test %>% mutate(squared_err = (log(medv) - fit)^2 , err = log(medv) - fit)

Mean_Squared_Error_train <- (colSums(predicted_train, na.rm = TRUE)[14])/nrow(predicted_train)
Mean_Squared_Error_test <-  (colSums(predicted_test, na.rm = TRUE)[14])/nrow(predicted_test)
Mean_Squared_Error_train
Mean_Squared_Error_test


#Residuls vs fitted values - homoskedasticity 

plot(y=predicted_train$fit,x=predicted_train$err)

plot(y=predicted_test$fit,x=predicted_test$err)

# Breusch-Pagan test P value should be greater than 0.05
lmtest::bptest(model)

#are errors are normally distributed? yes
hist(predicted_train$err)
mean(predicted_train$err)
median(predicted_train$err)


hist(predicted_test$err)


#errors auto-correlation assumption - no auto correlation
plot(x=predicted_train$err)
plot(x=predicted_test$err)
dwtest(model)

par(mfrow=c(2,2))
plot(model)

pred_train <- predict(model,boston)
regr.eval(boston$medv, pred_train)
#actual vs predict
predicted_test %>%
  ggplot(aes(x = fit, y = medv)) +
  geom_point() +
  stat_smooth(method = 'loess') +
  labs(x = "Predicted Values", y = "Original Values", title = "Predicted vs. Original Values") +
  theme_minimal()

########################################

predicted_Hdf <- as.data.frame(predict(model, newdata = test_H, interval = "prediction"))

predicted_H <- cbind(test_H,predicted_Hdf)


#write.table(predicted_Hdf, file = "predict_h.csv", row.names=F, sep = ",")

