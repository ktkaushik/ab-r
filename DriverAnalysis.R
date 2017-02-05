##Sales Analysis##
#Load required libraries
library(dplyr)
library(lubridate)
library(car)
library(glmnet)
library(forecast)
#Load dataset
mydata <- read.csv("C:/Users/Nildip Mukherjee/Desktop/ABINBEV/Focus Area - Driver Analysis, Visualisation/GAC Hackathon_Sales_Data.csv",stringsAsFactors = F)
#Removing data points with zero/negative sales.What's negative sales???they returnd a bottle??
mydata <- subset(mydata,Price.per.Volume>0)
#Imputing Display & Feature count by 0 if Display & Feature share is 0 respectively
mydata$Display.Count <- ifelse(mydata$Display.Share==0,0,mydata$Display.Count)
mydata$Feature.Count <- ifelse(mydata$Feature.Share==0,0,mydata$Feature.Count)
#Deriving Week and Year
mydata$Date <- as.Date(mydata$Date,format = "%m/%d/%Y")
mydata$Year <- year(mydata$Date)
mydata$Week <- week(mydata$Date)
#We'll only work with observations with promo data/Push SKUs. Assume Push SKUs and sister SKUs to behave similarly
mydata <- subset(mydata,!is.na(mydata$Display.Count))
#Identifying our biggest rivals
companies <- group_by(mydata,Brewer.Value)
who.eats.the.pie <- summarise(companies,Total.Sales=sum(Volume.Sales))
#Grouping rivals by threat level.Sharks=High Threat, Pirhanas=Mid Threat, Other Fishes=Low Threat
for (i in c('COMPETITOR 1','COMPETITOR 2','COMPETITOR 3')){
mydata$Brewer.Value <- ifelse(mydata$Brewer.Value==i,'Sharks',mydata$Brewer.Value)
}
for (j in c('COMPETITOR 4','COMPETITOR 5','COMPETITOR 8','COMPETITOR 9')){
mydata$Brewer.Value <- ifelse(mydata$Brewer.Value==j,'Pirhanas',mydata$Brewer.Value)
}
for (k in c('COMPETITOR 10','COMPETITOR 589','COMPETITOR 1593','COMPETITOR 721','COMPETITOR 435','COMPETITOR 1509')){
mydata$Brewer.Value <- ifelse(mydata$Brewer.Value==k,'Other Fishes',mydata$Brewer.Value)
}
#Creating data at Brand Level for overall market. We'll use this data to understand firstly what drives the industry. We'll then proceed to understand what drives our sales.
brandgrouping <- group_by(mydata,Brand.Value,Year,Week)#We'll be working at weekly rollups
brand.data <- summarise(brandgrouping,DisplayCount=sum(Display.Count),FeatureCount=sum(Feature.Count),
                        Temp=median(Average.of.Mean.TemperatureC),Humidity =median(Average.of..Mean.Humidity),
                        GDP=median(State.Personal.income..thousands.of.dollars..seasonally.adjusted.quarterly.),
                        Retail.Employee=median(X.Alcohol.Retail.Trade.employees..in.1000s..State..),
                        Employement.rate=1-median(Unemp..Rate.in.city),
                        CPI.Beverage=median(Consumer.Price.Index..Malt.Beverages),
                        CPI.Wine=median(Consumer.Price.Index..Wine),Sale.Volume=sum(Volume.Sales),
                        Price=median(Price.per.Volume))
#Linear Regression
brand.lm <- lm(log(Sale.Volume) ~ DisplayCount+FeatureCount+DisplayCount:FeatureCount
               +Temp+Humidity+Temp:Humidity+CPI.Beverage+CPI.Wine+log(Price),brand.data)
summary(brand.lm)
vif(brand.lm)#Presence of Multicollinearity.Switching to Lasso.
#Lasso Regression
Y.overall <- as.matrix(log(brand.data[,13]))
X.overall <- data.frame(brand.data[,c(4:7,10:12,14)])
X.overall$Price <- log(X.overall$Price)
X.overall$DisplayCount.FeatureCount <- X.overall$DisplayCount*X.overall$FeatureCount
X.overall$Temp.Humidity <- X.overall$Temp*X.overall$Humidity
X.overall <- as.matrix(X.overall)
#Crossvalidating to find lambda
crossval.overall <-  cv.glmnet(X.overall,Y.overall)
plot(crossval.overall)
penalty.overall <- crossval.overall$lambda.min
brand.lasso.overall <-glmnet(X.overall,Y.overall,alpha = 1,lambda = penalty.overall) 
brandlevelcoeffs.overall <- data.frame(as.matrix(coef(brand.lasso.overall)))
names(brandlevelcoeffs.overall) <- c('Coefficient')#what drives the whole market
#now lets know what drives us
mydata_abi <- subset(mydata,Brewer.Value=="ABI")
brandgrouping_abi <- group_by(mydata_abi,Brand.Value,Year,Week)
brand.data_abi <- summarise(brandgrouping_abi,DisplayCount=sum(Display.Count),FeatureCount=sum(Feature.Count),
                        Temp=median(Average.of.Mean.TemperatureC),Humidity =median(Average.of..Mean.Humidity),
                        GDP=median(State.Personal.income..thousands.of.dollars..seasonally.adjusted.quarterly.),
                        Retail.Employee=median(X.Alcohol.Retail.Trade.employees..in.1000s..State..),
                        Employement.rate=1-median(Unemp..Rate.in.city),
                        CPI.Beverage=median(Consumer.Price.Index..Malt.Beverages),
                        CPI.Wine=median(Consumer.Price.Index..Wine),Sale.Volume=sum(Volume.Sales),
                        Price=median(Price.per.Volume))
#We'll be using the same factors as before and understand what works differently
#Directly jumping to Lasso Regression.Promotion & weather conditions will still be correlated, but its logically incorrect to drop any by our wish
Y.abi <- as.matrix(log(brand.data_abi[,13]))
X.abi <- data.frame(brand.data_abi[,c(4:7,10:12,14)])
X.abi$Price <- log(X.abi$Price)
X.abi$DisplayCount.FeatureCount <- X.abi$DisplayCount*X.abi$FeatureCount
X.abi$Temp.Humidity <- X.abi$Temp*X.abi$Humidity
X.abi <- as.matrix(X.abi)
#Crossvalidating to find lambda
crossval.abi <-  cv.glmnet(X.abi,Y.abi)
plot(crossval.abi)
penalty.abi <- crossval.abi$lambda.min
brand.lasso.abi <-glmnet(X.abi,Y.abi,alpha = 1,lambda = penalty.abi) 
brandlevelcoeffs.abi <- data.frame(as.matrix(coef(brand.lasso.abi)))
names(brandlevelcoeffs.abi) <- c('Coefficient')
#Digging deeper. Lets see what happens at our Segments.
diggingdeep <- group_by(mydata_abi,Segment.Value,Year,Week)
segment.data <- summarise(diggingdeep,DisplayCount=sum(Display.Count),FeatureCount=sum(Feature.Count),
                            Temp=median(Average.of.Mean.TemperatureC),Humidity =median(Average.of..Mean.Humidity),
                            GDP=median(State.Personal.income..thousands.of.dollars..seasonally.adjusted.quarterly.),
                            Retail.Employee=median(X.Alcohol.Retail.Trade.employees..in.1000s..State..),
                            Employement.rate=1-median(Unemp..Rate.in.city),
                            CPI.Beverage=median(Consumer.Price.Index..Malt.Beverages),
                            CPI.Wine=median(Consumer.Price.Index..Wine),Sale.Volume=sum(Volume.Sales),
                            Price=median(Price.per.Volume))
#Subsetting by segments and building models for each segment.
#Craft
data.craft <- subset(segment.data,Segment.Value=="CRAFT/IMPORT")
craft.lm <- lm(log(Sale.Volume) ~ DisplayCount+FeatureCount+DisplayCount:FeatureCount
               +Temp+Humidity+Temp:Humidity+CPI.Beverage+CPI.Wine+log(Price),data.craft)
summary(craft.lm)
vif(craft.lm)#Multicollinearity -->> Lasso
Y <- as.matrix(log(data.craft[,13]))
X <- data.frame(data.craft[,c(4:7,10:12,14)])
X$Price <- log(X$Price)
X$DisplayCount.FeatureCount <- X$DisplayCount*X$FeatureCount
X$Temp.Humidity <- X$Temp*X$Humidity
X <- as.matrix(X)
#Crossvalidating to find lambda
crossval.craft <-  cv.glmnet(X,Y)
plot(crossval.craft)
penalty.craft <- crossval.craft$lambda.min
brand.lasso.craft <-glmnet(X,Y,alpha = 1,lambda = penalty.craft) 
brandlevelcoeffs.craft <- data.frame(as.matrix(coef(brand.lasso.craft)))
names(brandlevelcoeffs.craft) <- c('Coefficient')
#Premium
data.premium <- subset(segment.data,Segment.Value=="PREMIUM")
#jumping to Lasso
Y <- as.matrix(log(data.premium[,13]))
X <- data.frame(data.premium[,c(4:7,10:12,14)])
X$Price <- log(X$Price)
X$DisplayCount.FeatureCount <- X$DisplayCount*X$FeatureCount
X$Temp.Humidity <- X$Temp*X$Humidity
X <- as.matrix(X)
#Crossvalidating to find lambda
crossval.premium <-  cv.glmnet(X,Y)
plot(crossval.premium)
penalty.premium <- crossval.premium$lambda.min
brand.lasso.premium <-glmnet(X,Y,alpha = 1,lambda = penalty.premium) 
brandlevelcoeffs.premium <- data.frame(as.matrix(coef(brand.lasso.premium)))
names(brandlevelcoeffs.premium) <- c('Coefficient')
#PremiumPlus
data.premiumplus <- subset(segment.data,Segment.Value=="PREMIUM PLUS")
#jumping to Lasso
Y <- as.matrix(log(data.premiumplus[,13]))
X <- data.frame(data.premiumplus[,c(4:7,10:12,14)])
X$Price <- log(X$Price)
X$DisplayCount.FeatureCount <- X$DisplayCount*X$FeatureCount
X$Temp.Humidity <- X$Temp*X$Humidity
X <- as.matrix(X)
#Crossvalidating to find lambda
crossval.premiumplus <-  cv.glmnet(X,Y)
plot(crossval.premiumplus)
penalty.premiumplus <- crossval.premiumplus$lambda.min
brand.lasso.premiumplus <-glmnet(X,Y,alpha = 1,lambda = penalty.premiumplus) 
brandlevelcoeffs.premiumplus <- data.frame(as.matrix(coef(brand.lasso.premiumplus)))
names(brandlevelcoeffs.premiumplus) <- c('Coefficient')
#value
data.value <- subset(segment.data,Segment.Value=="VALUE")
#jumping to Lasso
Y <- as.matrix(log(data.value[,13]))
X <- data.frame(data.value[,c(4:7,10:12,14)])
X$Price <- log(X$Price)
X$DisplayCount.FeatureCount <- X$DisplayCount*X$FeatureCount
X$Temp.Humidity <- X$Temp*X$Humidity
X <- as.matrix(X)
#Crossvalidating to find lambda
crossval.value <-  cv.glmnet(X,Y)
plot(crossval.value)
penalty.value <- crossval.value$lambda.min
brand.lasso.value <-glmnet(X,Y,alpha = 1,lambda = penalty.value) 
brandlevelcoeffs.value <- data.frame(as.matrix(coef(brand.lasso.value)))
names(brandlevelcoeffs.value) <- c('Coefficient')

#Lets now build a forward looking prediction model. We'll build a time series model at the Aggregate ABI level and use a mixed effect model to derive segment level sale predictions
tsdata <- subset(mydata,Brewer.Value=="ABI")
tsdata$uniquedate <- paste(tsdata$Week,"_",tsdata$Year)
tsdata_craft <- subset(tsdata,Segment.Value=="CRAFT/IMPORT")
tsdata_premium <- subset(tsdata,Segment.Value=="PREMIUM")
tsdata_premiumplus <- subset(tsdata,Segment.Value=="PREMIUM PLUS")
tsdata_value <- subset(tsdata,Segment.Value=="VALUE")
tsgroupall <- group_by(tsdata,Week,Year)#for all
allpredictiontrain <- summarise(tsgroupall,Sale.Volume=sum(Volume.Sales),Price=median(Price.per.Volume),
                                Display.Count=sum(Display.Count),Feature.Count=sum(Feature.Count))
attach(allpredictiontrain)
allpredictiontrain <- allpredictiontrain[order(Year,Week),]
detach(allpredictiontrain)
tsgroupcraft <- group_by(tsdata_craft,Week,Year)#for Craft
craftpredictiontrain <- summarise(tsgroupcraft,Sale.Volume=sum(Volume.Sales),Price=median(Price.per.Volume),
                                Display.Count=sum(Display.Count),Feature.Count=sum(Feature.Count))
attach(craftpredictiontrain)
craftpredictiontrain <- craftpredictiontrain[order(Year,Week),]
detach(craftpredictiontrain)
tsgrouppremium <- group_by(tsdata_premium,Week,Year)#for Premium
premiumpredictiontrain <- summarise(tsgrouppremium,Sale.Volume=sum(Volume.Sales),Price=median(Price.per.Volume),
                                  Display.Count=sum(Display.Count),Feature.Count=sum(Feature.Count))
attach(premiumpredictiontrain)
premiumpredictiontrain <- premiumpredictiontrain[order(Year,Week),]
detach(premiumpredictiontrain)
tsgrouppremiumplus <- group_by(tsdata_premiumplus,Week,Year)#for PremiumPlus
premiumpluspredictiontrain <- summarise(tsgrouppremiumplus,Sale.Volume=sum(Volume.Sales),Price=median(Price.per.Volume),
                                    Display.Count=sum(Display.Count),Feature.Count=sum(Feature.Count))
tsgroupvalue <- group_by(tsdata_value,Week,Year)#for Value
attach(premiumpluspredictiontrain)
premiumpluspredictiontrain <- premiumpluspredictiontrain[order(Year,Week),]
detach(premiumpluspredictiontrain)
valuepredictiontrain <- summarise(tsgroupvalue,Sale.Volume=sum(Volume.Sales),Price=median(Price.per.Volume),
                                        Display.Count=sum(Display.Count),Feature.Count=sum(Feature.Count))
attach(valuepredictiontrain)
valuepredictiontrain <- valuepredictiontrain[order(Year,Week),]
detach(valuepredictiontrain)
trainfinal <- cbind(allpredictiontrain,craftpredictiontrain,premiumpredictiontrain,
                    premiumpluspredictiontrain,valuepredictiontrain)
trainfinal <- trainfinal[c(1:6,9:12,15:18,21:24,27:30)]
names(trainfinal) <- c("Week","Year","Total.Sale.Volume","Total.Price","Total.Display.Count","Total.Feature.Count",
                       "Craft.Sale.Volume","Craft.Price","Craft.Display.Count","Craft.Feature.Count",
                       "Premium.Sale.Volume","Premium.Price","Premium.Display.Count","Premium.Feature.Count",
                       "PremiumPlus.Sale.Volume","PremiumPlus.Price","PremiumPlus.Display.Count","PremiumPlus.Feature.Count",
                       "Value.Sale.Volume","Value.Price","Value.Display.Count","Value.Feature.Count")
#Fitting ARIMA
arima.model <- auto.arima(ts(trainfinal$Total.Sale.Volume,frequency = 52),xreg = as.matrix(trainfinal[4:6]))
arima.past <- data.frame(forecast(arima.model,xreg = as.matrix(trainfinal[4:6])))
predict.past <- arima.past$Point.Forecast
predict.past <- as.data.frame(predict.past)
arima_1st52 <- data.frame(predict.past[c(1:52),])#HoltWinters doesnt give the 1st period
resid <-residuals(arima.model)#We'll use Holt Winters on residuals
#Fittting Holt Winters
holt <- HoltWinters(ts(resid,frequency = 52))
#WithinSample Predictions
arimaafter52 <- data.frame(predict.past[c(53:208),])
Holtpast <- data.frame(data.frame(fitted(holt))$xhat)
Allpast <- arimaafter52+0.5*Holtpast
names(arima_1st52) <- c("Predicted")
names(Allpast) <- c("Predicted")
Allpast <- rbind(arima_1st52,Allpast)
#Out of Sample 12 week predictions
Holtfuture <- Holtpast[c(104:128),]
arimafuture <- forecast.Arima(arima.model,h=12,xreg=as.matrix(trainfinal[c(157:168),c(4:6)]))
arimafuture <- data.frame(data.frame(arimafuture)[,1])
finalfuture <- data.frame(arimafuture+0.5*Holtfuture)
names(finalfuture) <- c("Predicted")
Prediction <- rbind(Allpast,finalfuture)
Actual <- data.frame(trainfinal$Total.Sale.Volume)
write.csv(Prediction,"C:/Users/Nildip Mukherjee/Desktop/ABINBEV/Focus Area - Driver Analysis, Visualisation/Predicted.csv")
