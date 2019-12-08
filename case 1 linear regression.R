#linear regression

library(DAAG)
data(ais)
ais

levels(ais$sex)

#extracting male data from ais using dplyr package

library(dplyr)
data = filter(ais,sex=="m")
data      
str(data)
summary(data)

#plot to check the trend of the data and the result = upward trend

library(ggplot2)
ggplot(data,aes(hg,hc))+geom_point()+geom_smooth()

#box plot to find the outlier of the data

par(mfrow=c(1,2))

boxplot(data$hc)
boxplot(data$hg)

#density plot

plot(density(data$hc),main = "density plot = hc")
plot(density(data$hg),main = "density plot = hg")

#histogram

library(ggplot2)
ggplot(data,aes(hc))+geom_histogram(bind=15)
ggplot(data,aes(hg))+geom_histogram(bins = 15)

#checking the relationship of the two variables(hc & hp) using cor function

cor(data$hg,data$hc)

# lm model
lm1 = lm(hc~hg,data)
lm1
summary(lm1)

#split data

set.seed(1234)
rowind = sample(1:nrow(data),0.8*nrow(data))
train = data[rowind,]
test = data[-rowind,]

#training the model using linear regression method

train_lm = lm(hc~hg,train)
predit = predict(train_lm,test)
cbind(test$hc,predit,resid(train_lm))


#accuracy of the data
library(Metrics)
mape(test$hc,predit)#lesser the mape value lesser the error as it given only 1% error shows the model is excellent

#predicting the hc with hg = 19,20,15

val = data.frame(hg = c(19,20,15))
predict(train_lm,val)

#assumption

par(mfrow= c(2,2))

plot(train_lm)


