#Load Libraries
library(lattice)
library(psych)
library(gmodels)
library(ggplot2)
library(readxl)
library(MASS)


#Set calculations to 3 digits
options(digits=3)
#Mean of stock_return_scaled
mean(Stock_MLRAnalysis$stock_return_scaled)
#Standard Deviation of stock_retun_scaled
sd(Stock_MLRAnalysis$stock_return_scaled)
#Variance of stock_return_scaled
var(Stock_MLRAnalysis$stock_return_scaled)
#Median of stock_return_scaled
median(Stock_MLRAnalysis$stock_return_scaled)
#Basic Statistics of stock_return_scaled
describe(Stock_MLRAnalysis$stock_return_scaled)

#Histogram of stock_return_scaled
x=Stock_MLRAnalysis$stock_return_scaled
h<-hist(x, breaks=10, col="blue", xlab="stock_return_scaled", 
        main="Stock Return Analysis")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="black", lwd=2)

#T-test
t.test(Stock_MLRAnalysis$stock_return_scaled, mu = 300)

#Building Simple Linear Model
simple.fit<-lm(stock_return_scaled ~ dividend , data=Stock_MLRAnalysis) 
LinearModel<-simple.fit
#Summary of Key Statistics of the Model
summary(LinearModel)

#Fit the Full Model and performing multiple linear regression
full.model<- lm(stock_return_scaled ~., data=Stock_MLRAnalysis)
summary(full.model)
#Backword Regression Model
back.model<-stepAIC(full.model,direction="backward",
                    trace=FALSE)
summary(back.model)

# Forward regression model
forward.model <- stepAIC(full.model, direction = "forward",
                         trace = FALSE)
summary(forward.model)