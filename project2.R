library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(MASS)
library(kableExtra)
library(olsrr)
library(qcc)
data<-read.csv("dataset2.csv")
data[, 4] <- as.factor(data[, 4])
data[, 6] <- as.factor(data[, 6])
data[, 11] <- as.factor(data[, 11])
data = data[, -2]
model<-glm(Total.Number.of.Family.members~Total.Household.Income+Total.Food.Expenditure +Household.Head.Sex+Household.Head.Age+
             Type.of.Household+House.Floor.Area+House.Age+Number.of.bedrooms+Electricity, data=data, family = "poisson")
summary(model)
step(model)
model1<-glm(Total.Number.of.Family.members~Total.Household.Income+Total.Food.Expenditure +Household.Head.Sex+Household.Head.Age+
             Type.of.Household+House.Age+Number.of.bedrooms, data=data, family = "poisson")
outlierTest(model1)
data<-data[-944,]
model1<-glm(Total.Number.of.Family.members~Total.Household.Income+Total.Food.Expenditure +Household.Head.Sex+Household.Head.Age+
              Type.of.Household+House.Age+Number.of.bedrooms, data=data, family = "poisson")
poisgof(model1)#Goodness of fit test for modeling of the data
summary(model1)
idr.display(model1)
exp(coef(model1))

fit.data <- data %>% 
  mutate(Total.Number.Predict = round(fitted(model1)))
g<-ggplot(data=fit.data,aes(Total.Number.of.Family.members,Total.Number.Predict))+
  geom_smooth(method = "lm")+
  xlim(c(0, 15)) +
  ylim(c(0, 15))+
  labs(subtitle="Predict Vs True",
       y="Total.Number.Predict",
       x="Total.Number.True",
       title="Scatter diagram of family members")+
  geom_jitter(width=0.5,height=0.5)
g

