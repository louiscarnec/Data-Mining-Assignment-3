library(xlsx)
cycle_2015 <- read.xlsx("/Users/Carnec/Desktop/Business_Analytics/DataMining/Assignment3/cycle-counter-data-2012-to-2015-daily.xlsx", sheetName = "2015 Daily")
cycle_2014 <- read.xlsx("/Users/Carnec/Desktop/Business_Analytics/DataMining/Assignment3/cycle-counter-data-2012-to-2015-daily.xlsx", sheetName = "2014 Daily")
cycle_2013 <- read.xlsx("/Users/Carnec/Desktop/Business_Analytics/DataMining/Assignment3/cycle-counter-data-2012-to-2015-daily.xlsx", sheetName = "2013 Daily")
cycle_2012 <- read.xlsx("/Users/Carnec/Desktop/Business_Analytics/DataMining/Assignment3/cycle-counter-data-2012-to-2015-daily.xlsx", sheetName = "2012 Daily")


cycle_2012$Date <- as.Date(cycle_2012$Date)
cycle_2013$Date <- as.Date(cycle_2013$Date)
cycle_2014$Date <- as.Date(cycle_2014$Date)
cycle_2015$Date <- as.Date(cycle_2015$Date)

install.packages("lubridate")
library(lubridate)
cycle_2012$Month <- month(ymd(cycle_2012$Date)) 
cycle_2013$Month <- month(ymd(cycle_2013$Date)) 
cycle_2014$Month <- month(ymd(cycle_2014$Date)) 
cycle_2015$Month <- month(ymd(cycle_2015$Date))

cycle_2012$Year <- year(ymd(cycle_2012$Date)) 
cycle_2013$Year <- year(ymd(cycle_2013$Date)) 
cycle_2014$Year <- year(ymd(cycle_2014$Date)) 
cycle_2015$Year <- year(ymd(cycle_2015$Date)) 


for (i in (2:14)){
  cycle_2012[,i] <- as.character(cycle_2012[,i])
  cycle_2012[,i] <- as.numeric(cycle_2012[,i])
  cycle_2012[,i][is.na(cycle_2012[,i])]<-NA
}

for (i in (2:18)){
  cycle_2013[,i] <- as.character(cycle_2013[,i])
  cycle_2013[,i] <- as.numeric(cycle_2013[,i])
  cycle_2013[,i][is.na(cycle_2013[,i])]<-NA
}

for (i in (2:17)){
  cycle_2014[,i] <- as.character(cycle_2014[,i])
  cycle_2014[,i] <- as.numeric(cycle_2014[,i])
  cycle_2014[,i][is.na(cycle_2014[,i])]<-NA
}

for (i in (2:20)){
  cycle_2015[,i] <- as.character(cycle_2015[,i])
  cycle_2015[,i] <- as.numeric(cycle_2015[,i])
  cycle_2015[,i][is.na(cycle_2015[,i])]<-NA
}

cycle_2012[cycle_2012==0]<-NA
cycle_2013[cycle_2013==0]<-NA
cycle_2014[cycle_2014==0]<-NA
cycle_2015[cycle_2015==0]<-NA

#NA?
for (i in (1:20)){
  print(colnames(cycle_2015)[i])
  print(is.na(cycle_2015[,i]))
}

cycle_2015 <- as.data.frame(cycle_2015)
cycle_2014 <- as.data.frame(cycle_2014)
cycle_2013 <- as.data.frame(cycle_2013)
cycle_2012 <- as.data.frame(cycle_2012)

#Glenageary Bicycle in and out
cycle_2012$Glen.Bicycle <- cycle_2012$Bicycle_IN +  cycle_2012$Bicycle_OUT
cycle_2013$Glen.Bicycle <- cycle_2013$Bicycle_IN +  cycle_2013$Bicycle_OUT
cycle_2014$Glen.Bicycle <- cycle_2014$Bicycle_IN +  cycle_2014$Bicycle_OUT
cycle_2015$Glen.Bicycle <- cycle_2015$Bicycle_IN +  cycle_2015$Bicycle_OUT

#Creating a single data set
cycle_2012_tobind <- cycle_2012[-14]

cycle_2013_tobind <- cycle_2013
cycle_2013_tobind <- cycle_2013_tobind[-14] # *3
cycle_2013_tobind <- cycle_2013_tobind[-15]
cycle_2013_tobind <- cycle_2013_tobind[-15]
cycle_2013_tobind <- cycle_2013_tobind[-14]

dim(cycle_2012_tobind)
dim(cycle_2013_tobind)
dim(cycle_2014_tobind)


cycle_2014_tobind <- cycle_2014_tobind[-14] # *3
cycle_2014_tobind <- cycle_2014[-15]

library(dplyr)
cycle_training <- rbind(cycle_2012_tobind,cycle_2013_tobind)
cycle_training <- rbind(cycle_training,cycle_2014_tobind)


#Creating a single data set 2
cycle_2012$Totem.Clonskeagh.Road <- NA
cycle_2012$Totem.IN <- NA
cycle_2012$Totem.OUT <- NA
cycle_2012$Totem.Rock.Road <- NA

cycle_2014$Totem.N11.Stillorgan.Rd. <-NA

dim(cycle_2012)
dim(cycle_2013)
dim(cycle_2014)

cycle_training2 <- rbind(cycle_2012,cycle_2013)
cycle_training2 <- rbind(cycle_training2,cycle_2014)

#Mean Bicycle_IN 2012
tapply(cycle_2012$Bicycle_IN,cycle_2012$Month,mean)

#aggregate 2012
aggregate(data.frame(cycle_2012[,16],cycle_2012[,3:4],cycle_2012[,7:14]),by=list(cycle_2012$Month), FUN=sum ,na.rm=TRUE)
aggregate(data.frame(cycle_2012[,16],cycle_2012[,3:4],cycle_2012[,7:14]),by=list(cycle_2012$Month), mean,na.rm=TRUE)
aggregate(data.frame(cycle_2012[,16],cycle_2012[,3:4],cycle_2012[,7:14]),by=list(cycle_2012$Month), median,na.rm=TRUE)
aggregate(data.frame(cycle_2012[,16],cycle_2012[,3:4],cycle_2012[,7:14]),by=list(cycle_2012$Month), sd,na.rm=TRUE)
aggregate(data.frame(cycle_2012[,16],cycle_2012[,3:4],cycle_2012[,7:14]),by=list(cycle_2012$Month), min,na.rm=TRUE)
aggregate(data.frame(cycle_2012[,16],cycle_2012[,3:4],cycle_2012[,7:14]),by=list(cycle_2012$Month), max,na.rm=TRUE)

#aggregate 2013
aggregate(data.frame(cycle_2013[,20],cycle_2013[,3:4],cycle_2013[,7:18]),by=list(cycle_2013$Month), FUN = sum, na.rm=TRUE)
aggregate(data.frame(cycle_2013[,20],cycle_2013[,3:4],cycle_2013[,7:18]),by=list(cycle_2013$Month), mean,na.rm=TRUE)
aggregate(data.frame(cycle_2013[,20],cycle_2013[,3:4],cycle_2013[,7:18]),by=list(cycle_2013$Month), median,na.rm=TRUE)
aggregate(data.frame(cycle_2013[,20],cycle_2013[,3:4],cycle_2013[,7:18]),by=list(cycle_2013$Month), sd,na.rm=TRUE)
aggregate(data.frame(cycle_2013[,20],cycle_2013[,3:4],cycle_2013[,7:18]),by=list(cycle_2013$Month), min,na.rm=TRUE)
aggregate(data.frame(cycle_2013[,20],cycle_2013[,3:4],cycle_2013[,7:18]),by=list(cycle_2013$Month), max,na.rm=TRUE)


#aggregate 2014
aggregate(data.frame(cycle_2014[,19],cycle_2014[,3:4],cycle_2014[,7:17]),by=list(cycle_2014$Month), sum,na.rm=TRUE)
aggregate(data.frame(cycle_2014[,19],cycle_2014[,3:4],cycle_2014[,7:17]),by=list(cycle_2014$Month), mean,na.rm=TRUE)
aggregate(data.frame(cycle_2014[,19],cycle_2014[,3:4],cycle_2014[,7:17]),by=list(cycle_2014$Month), median,na.rm=TRUE)
aggregate(data.frame(cycle_2014[,19],cycle_2014[,3:4],cycle_2014[,7:17]),by=list(cycle_2014$Month), sd,na.rm=TRUE)
aggregate(data.frame(cycle_2014[,19],cycle_2014[,3:4],cycle_2014[,7:17]),by=list(cycle_2014$Month), min,na.rm=TRUE)
aggregate(data.frame(cycle_2014[,19],cycle_2014[,3:4],cycle_2014[,7:17]),by=list(cycle_2014$Month), max,na.rm=TRUE)

#aggregate all
aggregate(data.frame(cycle_training[,15],cycle_training[,3:4],cycle_training[,7:13]),by=list(cycle_training$Month), FUN=sum ,na.rm=TRUE)
aggregate(data.frame(cycle_training[,15],cycle_training[,3:4],cycle_training[,7:13]),by=list(cycle_training$Month), mean,na.rm=TRUE)
aggregate(data.frame(cycle_training[,15],cycle_training[,3:4],cycle_training[,7:13]),by=list(cycle_training$Month), median,na.rm=TRUE)
aggregate(data.frame(cycle_training[,15],cycle_training[,3:4],cycle_training[,7:13]),by=list(cycle_training$Month), sd,na.rm=TRUE)
aggregate(data.frame(cycle_training[,15],cycle_training[,3:4],cycle_training[,7:13]),by=list(cycle_training$Month), min,na.rm=TRUE)
aggregate(data.frame(cycle_training[,15],cycle_training[,3:4],cycle_training[,7:13]),by=list(cycle_training$Month), max,na.rm=TRUE)


#aggregate 2015
aggregate(cycle_2015[,2:20],by=list(cycle_2015$Month), sum)
aggregate(cycle_2015[,2:20],by=list(cycle_2015$Month), mean)
aggregate(cycle_2015[,2:20],by=list(cycle_2015$Month), median)
aggregate(cycle_2015[,2:20],by=list(cycle_2015$Month), sd)
aggregate(cycle_2015[,2:20],by=list(cycle_2015$Month), min)
aggregate(cycle_2015[,2:20],by=list(cycle_2015$Month), max)

#Histograms 2012
hist(cycle_2012[,2])
hist(tapply(cycle_2012$Bicycle_IN,cycle_2012$Month,mean))

sum2012 <- apply(cycle_2012[,2:14], 2, function(x) tapply(x, cycle_2012$Month, sum))
mean2012 <- apply(cycle_2012[,2:14], 2, function(x) tapply(x, cycle_2012$Month, mean))

hist(sum2012[,4],breaks=10)
hist(mean2012[,4],breaks=10)

##TWO WAY

apply(cycle_training[,2:16], 2, function(x) tapply(x, cycle_training$Month, mean))
trainingmean <-apply(cycle_training[,2:16], 2, function(x) tapply(x, cycle_training$Month, mean,na.rm=TRUE))
trainingmonthyear <-apply(cycle_training[,2:16], 2, function(x) tapply(x, list(cycle_training$Month,cycle_training$Year), mean,na.rm=TRUE))


mean2012 <-apply(cycle_2012[,2:17], 2, function(x) tapply(x, cycle_2012$Month, mean,na.rm=TRUE))
mean2013 <-apply(cycle_2013[,2:21], 2, function(x) tapply(x, cycle_2013$Month, mean,na.rm=TRUE))
mean2014 <-apply(cycle_2014[,2:20], 2, function(x) tapply(x, cycle_2014$Month, mean,na.rm=TRUE))

training2mean <-apply(cycle_training2[,2:21], 2, function(x) tapply(x, cycle_training$Month, mean,na.rm=TRUE))
training2monthyear <-apply(cycle_training2[,2:21], 2, function(x) tapply(x, list(cycle_training2$Month,cycle_training2$Year), mean,na.rm=TRUE))


for (i in (1:15)){
  pdf(paste("trainingmean",i,".pdf",sep=""))
  hist(trainingmean[,i],breaks=5,xlab="Mean Monthly Number Cyclist Count",main=names(cycle_training[2:16])[i])
  dev.off()}

for (i in (1:20)){
  pdf(paste("training2mean",i,".pdf",sep=""))
  hist(training2mean[,i],breaks=5,xlab="Mean Monthly Number Cyclist Count",main=names(cycle_training2[2:21])[i])
  dev.off()}



for (i in (1:15)){
  pdf(paste("trainingmeants", i,".pdf",sep=""))
  plot(trainingmean[,i],ylab='Number of cyclists',main=names(cycle_training[2:16])[i],xaxt='n', lty=3)
  axis(1, at=1:12, labels=c('January','February','March','April','May','June','July','August','Septembre','October','November','December'),las=2)
  lines(trainingmean[,i], type="o", pch=22, lty=2, col="blue")
  dev.off()}
  
for (i in (1:20)){
  pdf(paste("training2meants", i,".pdf",sep=""))
  plot(training2mean[,i],ylab='Number of cyclists',main=names(cycle_training2[2:21])[i],xaxt='n', lty=3)
  axis(1, at=1:12, labels=c('January','February','March','April','May','June','July','August','Septembre','October','November','December'),las=2)
  lines(training2mean[,i], type="o", pch=22, lty=2, col="blue")
  dev.off()}

for (i in (1:15)){
  pdf(paste("trainingmeants3", i,".pdf",sep=""))
  plot(trainingmonthyear[,i],ylab='Number of cyclists',main=names(cycle_training[2:16])[i],xaxt='n', lty=3)
  axis(1, at=1:36, labels=c('','',"March '12",'','',"June '12",'','',"Septembre '12",'','',"December '12",'','',"March '13",'','',"June '13",'','',"Septembre '13",'','',"December '13",'','',"March '14",'','',"June '14",'','',"Septembre '14",'','',"December '14"),las=2)
  lines(trainingmonthyear[,i], type="o", pch=22, lty=2, col="red")
  dev.off()}

for (i in (1:20)){
  pdf(paste("training2meants3", i,".pdf",sep=""))
  plot(training2monthyear[,i],ylab='Number of cyclists',main=names(cycle_training2[2:21])[i],xaxt='n', lty=3)
  axis(1, at=1:36, labels=c('','',"March '12",'','',"June '12",'','',"Septembre '12",'','',"December '12",'','',"March '13",'','',"June '13",'','',"Septembre '13",'','',"December '13",'','',"March '14",'','',"June '14",'','',"Septembre '14",'','',"December '14"),las=2)
  lines(training2monthyear[,i], type="o", pch=22, lty=2, col="red")
  dev.off()}

#Glenageary

hist(trainingmean[,14],breaks=5,xlab="Number Cyclists",main="Glenegeary Metals Two-way")

plot(trainingmean[,14],ylab='Number of cyclists',main='Glenageary Two-way',xaxt='n', lty=3)
axis(1, at=1:12, labels=c('January','February','March','April','May','June','July','August','Septembre','October','November','December'),las=2)

plot(trainingmonthyear[,14],ylab='Number of cyclists',main='Glenageary Two-way',xaxt='n', lty=3)
axis(1, at=1:36, labels=c('','',"March '12",'','',"June '12",'','',"Septembre '12",'','',"December '12",'','',"March '13",'','',"June '13",'','',"Septembre '13",'','',"December '13",'','',"March '14",'','',"June '14",'','',"Septembre '14",'','',"December '14"),las=2)

hist(mean2012[,15],breaks=5,xlab="Number Cyclists",main="Glenegeary Metals Two-way")
hist(mean2013[,19],breaks=6,xlab="Number Cyclists",main="Glenegeary Metals Two-way")
hist(mean2014[,18],breaks=6,xlab="Number Cyclists",main="Glenegeary Metals Two-way")



#Montrose
hist(trainingmean[,6],breaks=8,xlab="Number Cyclists",main="N11 Montrose Two-way")

plot(trainingmean[,6],ylab='Number of cyclists',main='Montrose N11 Two-way',xaxt='n',)
axis(1, at=1:12, labels=c('January','February','March','April','May','June','July','August','Septembre','October','November','December'),las=2)

plot(trainingmonthyear[,6],ylab='Number of cyclists',main='Glenageary Two-way',xaxt='n', lty=3)
axis(1, at=1:36, labels=c('','',"March '12",'','',"June '12",'','',"Septembre '12",'','',"December '12",'','',"March '13",'','',"June '13",'','',"Septembre '13",'','',"December '13",'','',"March '14",'','',"June '14",'','',"Septembre '14",'','',"December '14"),las=2)


#Rock Road Park
hist(trainingmean[,10],breaks=8,xlab="Number Cyclists",main="Rock Road Park Two-way")

plot(trainingmean[,10],ylab='Number of cyclists',main='Rock Road Park Two-way',xaxt='n',)
axis(1, at=1:12, labels=c('January','February','March','April','May','June','July','August','Septembre','October','November','December'),las=2)

plot(trainingmonthyear[,10],ylab='Number of cyclists',main='Rock Road Park Two-way',xaxt='n', lty=3)
axis(1, at=1:36, labels=c('','',"March '12",'','',"June '12",'','',"Septembre '12",'','',"December '12",'','',"March '13",'','',"June '13",'','',"Septembre '13",'','',"December '13",'','',"March '14",'','',"June '14",'','',"Septembre '14",'','',"December '14"),las=2)

#PREDICT

#need to have same number of columns
names(cycle_2015)[names(cycle_2015) == 'Glenageary.'] <- 'DLR.Co..Co..Glenageary.'
cycle_2015$N11.ECO.TOTEM <- NULL
names(cycle_2015)[names(cycle_2015) == 'Totem.N11.Stillorgan.Rd..Data.Only.'] <- 'Totem.N11.Stillorgan.Rd.'
cycle_2015$Totem.Rock.Road..Data.Only. <- NULL
names(cycle_2015)[names(cycle_2015) == 'Rock.Road.Inbound'] <- 'Totem.Rock.Road'
names(cycle_2015)[names(cycle_2015) == 'Totem.Clonskeagh.Road..Data.Only.'] <- 'Totem.Clonskeagh.Road'



test <-apply(cycle_2015[,2:21], 2, function(x) tapply(x, list(cycle_2015$Month,cycle_2015$Year), mean,na.rm=TRUE))


#creating new dataset for 2012, 2013, 2014, 2015
testdataset <- rbind(cycle_training2,cycle_2015)
testdataset <- as.data.frame(testdataset)
test <-apply(testdataset[,2:21], 2, function(x) tapply(x, list(testdataset$Month,testdataset$Year), mean,na.rm=TRUE))


install.packages("forecast")
library(forecast)

for (i in (1:15)){
  pdf(paste("trainingmeanpredict", i,".pdf",sep=""))
  plot.ts(s_train[,i],ylab='Number of cyclists',main=names(cycle_training[2:16])[i], lty=3)
  dev.off()}

#To be able to use loop need columns to be ordered in the same way so can test accuracy!
colnames(training2monthyear)
colnames(test)

test <- test[c(1,4,5,2,3,6,7,8,9,10,11,12,17,18,19,14,15,16,13,20)]
test<-as.data.frame(test)


trainGlen <- ts(test[,20], frequency=12,start=c(test$Year[1], test$Month[1]))
test_x <- window(trainGlen, start=c(2015, 1))
trainGlen<- window(trainGlen, end=c(2014, 12))

plot.ts(trainGlen)
plot.ts(test_x[,20])

trainGlen_D <- decompose(trainGlen, type="additive")
plot(trainGlen_D)

trainGlen_HW <- HoltWinters(trainGlen)
plot(trainGlen_HW)
plot(fitted(trainGlen_HW))
s_train_HW$SSE

trainGlen_F_HW <- forecast.HoltWinters(trainGlen_HW, h=12)
plot(trainGlen_F_HW,main="Holt-Winters Forecast for Glenageary")
lines(test_x, col='red')


accuracy(trainGlen_F_HW, test_x)



train_x <- ts(test, frequency=12,start=c(test$Year[1], test$Month[1]))
test_x <- window(train_x, start=c(2015, 1))
train_x<- window(train_x, end=c(2014, 12))

for (i in 2:20){
  pdf(paste("traints",i,".pdf",sep=""))
  plot.ts(train_x[,i],ylab="Count",main=names(testdataset[2:21])[i])
  dev.off()}

for (i in 2:20){
  pdf(paste("decomp",i,".pdf",sep=""))
  plot(decompose(train_x[,i], type="additive"))
  dev.off()}

for (i in 2:9){
  pdf(paste("HW",i,".pdf",sep=""))
  plot(HoltWinters(train_x[,i]),main=names(testdataset[2:21])[i])
  dev.off()}

##GLENAGEARY
pdf("HWGlen.pdf")
plot(HoltWinters(train_x[,20]),main="Holt-Winters Glenageary")
dev.off()

Glen_HW<-HoltWinters(train_x[,20])

Glen_F_HW <- forecast.HoltWinters(Glen_HW, h=12)

pdf("HWGlenF.pdf")
plot(trainGlen_F_HW,main="Holt-Winters Forecast for Glenageary")
lines(test_x[,20], col='red')
dev.off()

GlenIN_HW<-HoltWinters(train_x[,2])
GlenIN_F_HW <- forecast.HoltWinters(GlenIN_HW, h=12)

pdf("HWGlenINF.pdf")
plot(GlenIN_F_HW,main="Holt-Winters Forecast for Glenageary IN")
lines(test_x[,2], col='red')
dev.off()

GlenOUT_HW<-HoltWinters(train_x[,3])
GlenOUT_F_HW <- forecast.HoltWinters(GlenOUT_HW, h=12)

pdf("HWGlenOUTF.pdf")
plot(GlenOUT_F_HW,main="Holt-Winters Forecast for Glenageary OUT")
lines(test_x[,3], col='red')
dev.off()

xtable(accuracy(Glen_F_HW, test_x[,20]))
xtable(accuracy(GlenIN_F_HW, test_x[,2]))
xtable(accuracy(GlenOUT_F_HW, test_x[,3]))

#N11 Montrose
pdf("HWN11.pdf")
plot(HoltWinters(train_x[,6]),main="Holt-Winters N11 Montrose")
dev.off()
pdf("HWN11OUT.pdf")
plot(HoltWinters(train_x[,7]),main="Holt-Winters N11 Montrose OUT")
dev.off()
pdf("HWN11IN.pdf")
plot(HoltWinters(train_x[,8]),main="Holt-Winters N11 Montrose IN")
dev.off()

N11_HW<-HoltWinters(train_x[,6])
N11OUT_HW<-HoltWinters(train_x[,7])
N11IN_HW<-HoltWinters(train_x[,8])


N11_F_HW <- forecast.HoltWinters(N11_HW, h=12)
N11IN_F_HW <- forecast.HoltWinters(N11IN_HW, h=12)
N11OUT_F_HW <- forecast.HoltWinters(N11OUT_HW, h=12)

pdf("HWN11F.pdf")
plot(N11_F_HW,main="Holt-Winters Forecast for N11 Montrose")
lines(test_x[,6], col='red')
dev.off()

pdf("HWN11INF.pdf")
plot(N11IN_F_HW,main="Holt-Winters Forecast for N11 Montrose IN")
lines(test_x[,8], col='red')
dev.off()

pdf("HWN11OUTF.pdf")
plot(N11OUT_F_HW,main="Holt-Winters Forecast for N11 Montrose OUT")
lines(test_x[,7], col='red')
dev.off()

xtable(accuracy(N11_F_HW, test_x[,6]))
xtable(accuracy(N11IN_F_HW, test_x[,8]))
xtable(accuracy(N11OUT_F_HW, test_x[,7]))


#Rock Road Bus Lane Beside Park

pdf("RRBus.pdf")
plot(HoltWinters(train_x[,9]),main="Holt-Winters Rock Road Bus Lane Beside Park")
dev.off()

RRBus_HW<-HoltWinters(train_x[,9])
RRBus_F_HW <- forecast.HoltWinters(RRBus_HW, h=12)

pdf("HWRRBusF.pdf")
plot(RRBus_F_HW,main="Holt-Winters Forecast for Rock Road Bus Lane Beside Park")
lines(test_x[,9], col='red')
dev.off()

xtable(accuracy(RRBus_F_HW, test_x[,9]))

#Rock Road Park

train_x_park <- ts(test, frequency=12,start=c(test$Year[1], test$Month[1]))
test_x_park <- window(train_x_park, start=c(2014, 11))
train_x_park<- window(train_x_park, end=c(2014, 10))

pdf("RRP.pdf")
plot(HoltWinters(train_x_park[,10]),main="Holt-Winters Rock Road Park")
dev.off()
pdf("RRPIN.pdf")
plot(HoltWinters(train_x_park[,11]),main="Holt-Winters Rock Road Park")
dev.off()
pdf("RRPOUT.pdf")
plot(HoltWinters(train_x_park[,12]),main="Holt-Winters Rock Road Park")
dev.off()

RRP_HW<-HoltWinters(train_x_park[,10])
RRPIN_HW<-HoltWinters(train_x_park[,11])
RRPOUT_HW<-HoltWinters(train_x_park[,12])

RRP_F_HW <- forecast.HoltWinters(RRP_HW, h=14)
RRPIN_F_HW <- forecast.HoltWinters(RRPIN_HW, h=14)
RRPOUT_F_HW <- forecast.HoltWinters(RRPOUT_HW, h=14)

pdf("RRPF.pdf")
plot(RRP_F_HW,main="Holt-Winters Forecast for Rock Road Park two-way")
lines(test_x_park[,10], col='red')
dev.off()

pdf("RRPINF.pdf")
plot(RRPIN_F_HW,main="Holt-Winters Forecast for Rock Road Park IN")
lines(test_x_park[,11], col='red')
dev.off()

pdf("RRPOUTF.pdf")
plot(RRPOUT_F_HW,main="Holt-Winters Forecast for Rock Road Park OUT")
lines(test_x_park[,12], col='red')
dev.off()

xtable(accuracy(RRP_F_HW, test_x_park[,10]))
xtable(accuracy(RRPIN_F_HW, test_x_park[,11]))
xtable(accuracy(RRPOUT_F_HW, test_x_park[,12]))

RRP_HWmul<-HoltWinters(train_x_park[,10],seasonal = "mult")
RRPIN_HWmul<-HoltWinters(train_x_park[,11],seasonal = "mult")
RRPOUT_HWmul<-HoltWinters(train_x_park[,12])

RRP_F_HWmul <- forecast.HoltWinters(RRP_HW, h=14)
RRPIN_F_HWmul <- forecast.HoltWinters(RRPIN_HW, h=14)
RRPOUT_F_HWmul <- forecast.HoltWinters(RRPOUT_HW, h=14)

plot(RRP_F_HWmul)
plot(RRPIN_F_HWmul)

#Totem Clonskeagh Road

train_x_clon <- ts(test, frequency=12,start=c(test$Year[12], test$Month[1]))


train_x_clon[1,16] <-train_x_clon[13,16]
train_x_clon[2,16] <-train_x_clon[14,16]
train_x_clon[3,16] <-train_x_clon[15,16]
train_x_clon[4,16] <-train_x_clon[16,16]
train_x_clon[5,16] <-train_x_clon[17,16]
train_x_clon[6,16] <-train_x_clon[18,16]
train_x_clon[7,16] <-train_x_clon[19,16]
train_x_clon[8,16] <-train_x_clon[20,16]
train_x_clon[9,16] <-train_x_clon[21,16]
train_x_clon[10,16] <-train_x_clon[22,16]
train_x_clon[11,16] <-train_x_clon[23,16]
train_x_clon[12,16] <-train_x_clon[24,16]

train_x_clon[1,17] <-train_x_clon[13,17]
train_x_clon[2,17] <-train_x_clon[14,17]
train_x_clon[3,17] <-train_x_clon[15,17]
train_x_clon[4,17] <-train_x_clon[16,17]
train_x_clon[5,17] <-train_x_clon[17,17]
train_x_clon[6,17] <-train_x_clon[18,17]
train_x_clon[7,17] <-train_x_clon[19,17]
train_x_clon[8,17] <-train_x_clon[20,17]
train_x_clon[9,17] <-train_x_clon[21,17]
train_x_clon[10,17] <-train_x_clon[22,17]
train_x_clon[11,17] <-train_x_clon[23,17]
train_x_clon[12,17] <-train_x_clon[24,17]

train_x_clon[1,18] <-train_x_clon[13,18]
train_x_clon[2,18] <-train_x_clon[14,18]
train_x_clon[3,18] <-train_x_clon[15,18]
train_x_clon[4,18] <-train_x_clon[16,18]
train_x_clon[5,18] <-train_x_clon[17,18]
train_x_clon[6,18] <-train_x_clon[18,18]
train_x_clon[7,18]<-train_x_clon[19,18]
train_x_clon[8,18] <-train_x_clon[20,18]
train_x_clon[9,18] <-train_x_clon[21,18]
train_x_clon[10,18] <-train_x_clon[22,18]
train_x_clon[11,18] <-train_x_clon[23,18]
train_x_clon[12,18] <-train_x_clon[24,18]

test_x_clon <- window(train_x_clon, start=c(2014, 4))
train_x_clon<- window(train_x_clon, end=c(2014, 3))

pdf("clon.pdf")
plot(HoltWinters(train_x_clon[,16]),main="Holt-Winters Clonskeagh Road")
dev.off()
pdf("clonIN.pdf")
plot(HoltWinters(train_x_clon[,17]),main="Holt-Winters Clonskeagh Road IN")
dev.off()
pdf("clonOUT.pdf")
plot(HoltWinters(train_x_clon[,18]),main="Holt-Winters Clonskeagh Road OUT")
dev.off()

clon_HW<-HoltWinters(train_x_clon[,16])
clonIN_HW<-HoltWinters(train_x_clon[,17])
clonOUT_HW<-HoltWinters(train_x_clon[,18])

clon_F_HW <- forecast.HoltWinters(clon_HW, h=21)
clonIN_F_HW <- forecast.HoltWinters(clonIN_HW, h=21)
clonOUT_F_HW <- forecast.HoltWinters(clonOUT_HW, h=21)

pdf("clonF.pdf")
plot(clon_F_HW,main="Holt-Winters Forecast for Clonskeagh two-way")
lines(test_x_clon[,16], col='red')
dev.off()

pdf("clonINF.pdf")
plot(clonIN_F_HW,main="Holt-Winters Forecast for Clonskeagh IN")
lines(test_x_clon[,17], col='red')
dev.off()

pdf("clonOUTF.pdf")
plot(clonOUT_F_HW,main="Holt-Winters Forecast for Clonskeagh OUT")
lines(test_x_clon[,18], col='red')
dev.off()

xtable(accuracy(clon_F_HW, test_x[,16]))
xtable(accuracy(clonIN_F_HW, test_x[,17]))
xtable(accuracy(clonOUT_F_HW, test_x[,18]))
