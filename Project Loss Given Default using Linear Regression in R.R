rm(list = ls())
getwd()
setwd("C:/Users/Admin/Desktop/R Directory")
set.seed(2)

# read the data set loss given default
lgd <- read.csv("R_Module_Day_5.2_Data_Case_Study_Loss_Given_Default.csv", header = T)
str(lgd)
summary(lgd)
head(lgd)

# change the categorical variables gender & marital status
lgd$Gender<-ifelse(lgd$Gender=='M',1,0)
lgd$Married <- ifelse(lgd$Married=="Married",1,0)

# plot the relationship graph of all the variables
pairs(lgd)


cor(lgd)

# creating a training set and testing set from the dataset lgd
nrow(lgd)
sampl<- sample(1:nrow(lgd),.7*(nrow(lgd)))

#segregating the training dataset and the testing dataset
lgd_train <- lgd[sampl,]
nrow(lgd_train)

lgd_test <- lgd[-sampl,]
nrow(lgd_test)

cor_check<-cor(lgd_train)
cor_check

# install corrplot package
install.packages("corrplot")
library(corrplot)

# create corrplot
corrplot(cor_check,type = "lower", method = "circle")
corrplot(cor_check,type = "lower", method = "number")

# creating model with all the variables one by one
lin_mod <- lm(Losses.in.Thousands ~ ., data=lgd_train)
summary(lin_mod)

# creating model with all the variables except AC_No
lin_mod1 <- lm(Losses.in.Thousands ~ .-Ac_No, data=lgd_train)
summary(lin_mod1)


lin_mod2 <- lm(Losses.in.Thousands ~ Age+Number.of.Vehicles+Gender+Married, data=lgd_train)
summary(lin_mod2)

lin_mod3 <- lm(Losses.in.Thousands ~ Age+Gender+Married, data=lgd_train)
summary(lin_mod3)

install.packages("car")
library(car)

vif(lin_mod1)

#residual analysis

plot(lin_mod1,1) # check for linearity assumption
plot(lin_mod1,2) # Normality check
plot(lin_mod1,3) # check Homoscedasticity....slope
plot(lin_mod1,4) #
plot(lin_mod1,5)

#sum of all residuals is zero

sum(lin_mod1$residuals)

# Examining univariate distributions

hist(lin_mod1$residuals)

plot(density(lin_mod1$residuals))

boxplot(lin_mod1$residuals)

# using the models we created lets do the predictions

pred_test <- predict(lin_mod1, newdata = lgd_test)

errors_pred_lin_model1 <- (lgd_test$Losses.in.Thousands) - (pred_test)


#Akaike information criterion
AIC(lin_mod)
AIC(lin_mod1)
