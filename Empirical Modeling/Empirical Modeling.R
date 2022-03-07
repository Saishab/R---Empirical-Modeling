#Loading important library and data.Note: Our data is time dependent 
library(readxl)
my_data = read_excel(file.choose())
my_data
names(my_data)

#Assigning columns as per the model values
Y= my_data$`Adjusted savings: net forest depletion (% of GNI)`
X = my_data$`Industry (including construction), value added (% of GDP)`
Z = my_data$`GDP per capita (current US$)`
P = my_data$`Population, total`
T = my_data$Year

#Exploratory Data Analysis
plot.ts(Y)

library(ggplot2)
library(dplyr)
# Normal Plot as per provided data
plt1 = ggplot(my_data,aes(x = my_data$Year,y= my_data$`Adjusted savings: net forest depletion (% of GNI)`))+
  geom_line() +labs(title = "Time Series Plot", subtitle = "Net Forest Depletion") + labs(x = 'Year') + labs(y = 'Forest Depletion') +
  labs(caption = "(based on data from world development index)")

plt2 = ggplot(my_data,aes(x=my_data$Year,y=my_data$`Industry (including construction), value added (% of GDP)`))+
  geom_line() +labs(title = "Time Series Plot", subtitle = "Industry") + labs(x = 'Year') + labs(y = 'Industry Construction') +
  labs(caption = "(based on data from world development index)")

plt3 = ggplot(my_data,aes(x=my_data$Year,y=my_data$`GDP per capita (current US$)`))+
  geom_line() +labs(title = "Time Series Plot", subtitle = "GDP") + labs(x = 'Year') + labs(y = 'GDP Per Capita') +
  labs(caption = "(based on data from world development index)")


plt4 = ggplot(my_data,aes(x=my_data$Year,y= my_data$`Population, total`))+
  geom_line() +labs(title = "Time Series Plot", subtitle = "Population") + labs(x = 'Year') + labs(y = 'Population') +
  labs(caption = "(based on data from world development index)")

#Without Log Simulation 

cor(Y,P)
model<-lm(Y~X+Z+P)
summary(model) 
plot.ts(model$residuals)


#With Log Simulation
plot.ts(log(Y))#Irregularity in data
plot.ts(log(X)) #trend in data  #hetrogenousdata
plot.ts(log(Z)) #linear trend in data 
plot.ts(log(P)) #Linear trend in data

plt5 = ggplot(my_data,aes(x = my_data$Year,y= log(my_data$`Adjusted savings: net forest depletion (% of GNI)`)))+
  geom_line() +labs(title = "Time Series Plot", subtitle = "Log Net Forest Depletion") + labs(x = 'Year') + labs(y = 'Log Forest Depletion') +
  labs(caption = "(based on modified data from world development index)")

plt6 = ggplot(my_data,aes(x=my_data$Year,y=log(my_data$`Industry (including construction), value added (% of GDP)`)))+
  geom_line() +labs(title = "Time Series Plot", subtitle = "Log Industry") + labs(x = 'Year') + labs(y = 'LOg Industry Construction') +
  labs(caption = "(based on modified data from world development index)")

plt7 = ggplot(my_data,aes(x=my_data$Year,y=log(my_data$`GDP per capita (current US$)`)))+
  geom_line() +labs(title = "Time Series Plot", subtitle = "Log GDP") + labs(x = 'Year') + labs(y = 'Log GDP Per Capita') +
  labs(caption = "(based on modified data from world development index)")


plt8 = ggplot(my_data,aes(x=my_data$Year,y= log(my_data$`Population, total`)))+
  geom_line() +labs(title = "Time Series Plot", subtitle = "Log - Population") + labs(x = 'Year') + labs(y = 'Log -Population')+
  labs(caption = "(based on data from world development index)")



hist(log(P),breaks = 50)

#Taking log
Yt<-as.numeric(log(Y))
Xt<-as.numeric(log(X))
Zt<-as.numeric(log(Z))
Pt<-as.numeric(log(P))

cor(Yt,Xt) 
mod<-lm(Yt~Xt+Zt+Pt)  #first model ,log model 
summary(mod)
u.hat<-mod$residuals
plot.ts(u.hat) # irregularityin the residuals 
hist(u.hat) #seemslike normal dist ,but check normality via skweness and kurtosis or JB  test 


#M-S test 
#Independence test  
T<-length(u.hat)
AU1<-lm(u.hat[2:T]~u.hat[1:(T-1)])
summary(AU1) #siginificance estimate value 0.55 ,positive dependence.

#t-invariance
#t-invariance in mean
t<-c(1:T)
length(t)
AU3 <- lm(u.hat~poly(t,2)+Xt+Zt+Pt)
summary(AU3)
plot.ts(u.hat)


#t-invariance in variance
AU4 <- lm(u.hat^2~poly(t,2))
summary(AU4)
plot.ts(u.hat^2)

length(Yt[3:T])
t<-1:(T-2)
length(t)

#Respecified 
model1 <-lm (Yt[3:T] ~poly(t)+Zt[3:T]+Xt[3:T]+Pt[3:T]+Yt[1:(T-2)]+Yt[2:(T-1)]) #Hetrogenous AR(2)
summary(model1)
u.hat3<-model1$residuals
plot.ts(u.hat3)
abline(h=0,lwd=5)
hist(u.hat3)
plot(rnorm(length(u.hat3)),type="l") #example 


#Noramilty test 

skew<- mean((u.hat3/sd(u.hat))^3)
kurt<-mean((u.hat3/sd(u.hat))^4)  
JB<- (T*skew^2)/6+ (T*kurt^2)/24 #less then 2 

