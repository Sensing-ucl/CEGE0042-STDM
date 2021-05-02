# Load the required libraries
#install.packages(c('maptools','lattice','spdep','sp','rgdal','tmap','ggplot2','gridExtra','gstat','OpenStreetMap','spacetime'))
library(maptools)
library(lattice)
library(spdep)
library(sp)
library(rgdal)
library(tmap)
library(ggplot2)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)
library(knitr)

# da<-read.csv('covid-case.csv',header = T)
# da_sum<-as.numeric(da[33,])
# da_sum
# plot(ts(da_sum[-1],start = c()))
#set working directory accordingly with setwd()
readOGR(dsn="ESRI/London_Borough_Excluding_MHW.shp")
ld_districts <- readOGR(dsn="ESRI/London_Borough_Excluding_MHW.shp", layer="London_Borough_Excluding_MHW", 
                        p4s = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")@projargs)


#2.2

temp<-read.csv('covid-case.csv',header = T)

uk_temp_matrix<-data.matrix(ld_districts@data)
# ld_temp_matrix<-data.matrix(ld_districts@data[,-c(1:3)])
ld_temp_matrix<-temp
# rownames(ld_temp_matrix) <- ld_districts@data[,"NAME"]

# tm_shape(ld_districts)+ 
#   tm_fill("Jul_1910", style="jenks", palette="Purples")+
#   tm_borders("white")+
#   tm_compass(position=c("left","top"))+
#   tm_scale_bar()
# 
# brks=quantile(as.numeric(unlist(uk_districts@data[,-c(1:2)])), seq(0,1,1/10)) 
# tm_shape(uk_districts)+ 
#   tm_fill("Jul_1910", style="fixed", palette="-Spectral", breaks=brks)+
#   tm_borders("white")+
#   tm_compass(position=c("left","top"))+
#   tm_scale_bar()
# acf(uk_temp_matrix["Midlands",], lag.max=50, main="ACF, Midlands")
# acf(colMeans(matrix(uk_temp_matrix["Midlands",],12)), main="ACF, Midlands Annual Average")
# pacf(uk_temp_matrix["Midlands",], lag.max=50, main="ACF, Midlands")


W <- nb2listw(poly2nb(ld_districts))
W
kable(listw2mat(W))

ld_temp_avg <- rowMeans(ld_temp_matrix[,-1])


source("starima_package.R")
Wmat <- listw2mat(W)
# ld_temp_matrix<-as.numeric(ld_temp_matrix[,-1])
stacf(t(ld_temp_matrix[,-1]), Wmat,48)

stpacf(t(ld_temp_matrix[,-1]), Wmat, 4)


library(forecast)
library(ggplot2)
library(fpp2)
library(gridExtra)
# p1 <- autoplot(goog)
# p2 <- autoplot(sunspotarea)
# p3 <- autoplot(marathon)
# p4 <- autoplot(oil)
# 
# grid.arrange(p1, p2, p3, p4)


# decom <- stl(arrivals[,"Japan"], t.window=5, s.window="periodic")
# autoplot(decom)

# p1 <- autoplot(goog)
# p2 <- autoplot(diff(goog))
# grid.arrange(p1,p2)
# 
# p1 <- autoplot(acf(goog,plot=FALSE))
# p2 <- autoplot(pacf(goog,plot=FALSE))
# p3 <- autoplot(acf(diff(goog),plot=FALSE)) 
# p4 <- autoplot(pacf(diff(goog),plot=FALSE))
# grid.arrange(p1,p2, p3, p4)

# p1 <- autoplot(arrivals[,"UK"])
# p2 <- autoplot(diff(arrivals[,"UK"]))
# p3 <- autoplot(diff(arrivals[,"UK"], lag=4))
# 
# grid.arrange(p1,p2,p3)

W1<-listw2mat(W)
write.csv(W1,"w1.csv", row.names = FALSE)
W <- as.matrix(read.table(file="w.csv", header=TRUE, sep=","))
colnames(W) <- ld_districts@data[,"NAME"]

ld_temp <- read.csv(file="covid-case.csv")
uk_temp <- read.csv(file="uk_avg_temp.csv")
ld_temp <-t(ld_temp[-1,])
W_fit<-list(w1=W)
uk_temp.mat <- as.matrix(uk_temp)
uk_temp.mat.diff <- diff(uk_temp.mat,lag=12,differences=1)
fit.star <- starima_fit(uk_temp.mat,W_fit,p=2,d=1,q=2)
stacf(fit.star$RES,W,48)
hist(fit.star$RES[,6])
pre.star <- starima_pre(uk_temp.mat[(1104-12-4+1):1224,],model=fit.star)
matplot(1:120,cbind(uk_temp[1105:1224,1],pre.star$PRE[,1]),type="l")
pre.star$NRMSE


# library(rgdal)
# uk_districts <- readOGR(dsn="Data/uk_districts.shp", layer="uk_districts",
#                         p4s = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")@projargs)
# 
# # uk_temp_matrix<-data.matrix(uk_districts@data[,-c(1:3)])
# rownames(uk_temp_matrix) <- uk_districts@data[,"NAME"]
# 
# # plot(uk_temp_matrix["East Anglia",], ylab="Monthly average temperature", xlab="Time (in months)", type="l")
# 
# # lag.plot(uk_temp_matrix["East Anglia",], lags=3, do.lines=FALSE)
# acf(ld_temp_matrix[1,], lag.max=36, xlab="Lag", ylab="ACF", main="Autocorrelation plot of monthly average temperatures")
# 
# EA.s.diff <- diff(uk_temp_matrix["East Anglia",], lag=12, differences=1)
# acf(EA.s.diff, lag.max=36, xlab="Lag", ylab="ACF", main="Differenced autocorrelation plot")
# 
# pacf(uk_temp_matrix["East Anglia",], lag.max=36,xlab="Lag",ylab="PACF",main="Partial Autocorrelation plot of monthly average temperatures")
# 
# pacf(EA.s.diff, lag.max=36, xlab="Lag", ylab="ACF",main="Partial Autocorrelation plot of monthly average temperatures")
# 
# fit.ar <- arima(uk_temp_matrix[,1:1104],order=c(1,0,2),seasonal=list(order=c(2,1,1),period=12))
# 
# fit.ar <- arima(uk_temp_matrix["East Anglia",1:1104],order=c(1,0,2),seasonal=list(order=c(2,1,1),period=12))
# fit.ar
# 
# source("starima_package.R")
# NRMSE_fit <- NRMSE(res=fit.ar$residuals, obs=uk_temp_matrix["East Anglia",1:1104])
# 
# tsdiag(fit.ar)
# 
# pre.ar<-predict(fit.ar, n.ahead=12)
# matplot(1:12,cbind(uk_temp_matrix["East Anglia", 1105:1116],pre.ar$pred),type="l",main="", xlab="Month", ylab="Average Temp. degrees Celsius")
# 
# fit.Ar <- Arima(uk_temp_matrix["East Anglia",1:1104],order=c(1,0,2),seasonal=list(order=c(2,1,1),period=12))
# pre.Ar <- Arima(uk_temp_matrix["East Anglia", 1105:(ncol(uk_temp_matrix))], model=fit.Ar)
# matplot(cbind(pre.Ar$fitted, pre.Ar$x), type="l")
# 
# fit.auto.ar <- auto.arima(uk_temp_matrix["East Anglia",1:1104])
# fit.auto.ar
