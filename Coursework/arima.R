
wd <- "C:\\Users\\46650\\Desktop\\Coursework\\"
setwd(wd)

library(forecast)
library(data.table)
library(ggplot2)
library(corrplot)
#install.packages("astsa")
library(astsa)
#install.packages("maps")
library(maps)
library(plyr)
#install.packages("fpp")
library(fpp)
library(lubridate)

database <- fread("C:\\Users\\46650\\Desktop\\Coursework\\database.csv",stringsAsFactors = T) #Load the data
database$Date <- as.Date(database$Date, format="%d/%m/%Y") #Set data format
database <- database[Type=="Earthquake"] 
database <- database[,c("ID","Date","Time","Latitude","Longitude","Magnitude")] #Clean some useless values
database <- database[complete.cases(database[,2]),] #Delete data with missing values
database$Year <- format(as.Date(database$Date, format="%d/%m/%Y"),"%Y") #We'll need the years more ahead
summary(database)

#Number and magnitude of earthquakes over the world
map <- ggplot(database) + borders("world", colour="black", fill="gray50")  
print(map + geom_point(aes(x=database$Longitude, y=database$Latitude,color=Magnitude),shape=18) +
        scale_color_gradient(low="blue", high="red") +
        theme(legend.position = "top")+
        ggtitle("Earthquakes by Magnitude")+labs(caption="jhervas"))

#Distribution of earthquake magnitude in tectonic activity
ggplot(database,aes(Magnitude))+
  geom_area(aes(y = ..count..,fill="blue"), stat = "bin")+
  labs(title="Earthquakes",caption="jhervas") + 
  guides(fill=FALSE)

#Whether this distribution remained unchanged over the past 50 years
magnitudes_over_years <- ddply(database, .(Year), summarize,  Mean_Magnitude=mean(Magnitude))

Magnitudes <- ts(magnitudes_over_years[2],
                 start=1965, #min(database$Date, na.rm=TRUE)
                 end=2016, #max(database$Date, na.rm=TRUE),
                 frequency =1)
plot(Magnitudes)

#Number of earthquakes over 50 years
Earthquakes <- ts(unname(table(database$Year)),
                  start=1965, #min(database$Date, na.rm=TRUE)
                  end=2016, #max(database$Date, na.rm=TRUE),
                  frequency =1)
plot(Earthquakes)

#Number and magnitude map of earthquakes after first-order difference
diff_Earthquakes <- diff(Earthquakes)
diff_Magnitudes <- diff(Magnitudes)
par(mfrow=c(2,1))
plot(diff_Earthquakes)
plot(diff_Magnitudes)

#Stationary test
Box.test(diff_Earthquakes, lag=20, type="Ljung-Box")
Box.test(diff_Magnitudes, lag=20, type="Ljung-Box")
adf.test(diff_Earthquakes, alternative ="stationary")
adf.test(diff_Magnitudes, alternative ="stationary")

kpss.test(diff_Earthquakes)
kpss.test(diff_Magnitudes)
plot(acf(diff_Earthquakes))
plot(pacf(diff_Earthquakes))
plot(acf(diff_Magnitudes))
plot(pacf(diff_Magnitudes))

fit<-auto.arima(Earthquakes)
accuracy(fit)
summary(fit)
tsdiag(fit)
forecast(fit,5)
plot(forecast(fit,5),xlab="Year",ylab = "Earthquakes")



fit<-auto.arima(Magnitudes)
accuracy(fit)
summary(fit)
tsdiag(fit)
forecast(fit,5)
plot(forecast(fit,5),xlab="Year",ylab = "Magnitudes")

