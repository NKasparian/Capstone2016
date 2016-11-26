library("e1071")
library("lubridate")
library("xts")
library("quantmod")
library("tseries")
install.packages("rJava", dependencies = TRUE, type = "source")
install.packages("RWeka", dependencies = TRUE, type = "source")
library("rJava","RWeka")
library("FSelector")
library("data.table")
library("TTR")
library("rgp")
library("caTools")
library("SDMTools")
library("hts")

InfRateUS <- read.csv("~/Documents/Big Data Analytics/Capstone/InflationRateUS.csv",header=TRUE)
InfRateUS$DATE <- as.Date(as.character(InfRateUS$Date))
InfRateUS <- subset(InfRateUS, select= c("DATE","Inflation"))
IntRateUS <- read.csv("~/Documents/Big Data Analytics/Capstone/FederalFundsRate.csv",header=TRUE)
OilP <- read.csv("~/Documents/Big Data Analytics/Capstone/OilPrices%Change.csv",header=TRUE)
IntRateUS$DATE <- as.Date(as.character(IntRateUS$DATE))
OilP$DATE <- as.Date(as.character(OilP$DATE))
colnames(OilP) <- c("DATE","ChangeOilPrice")
OilP$ChangeOilPrice <- as.numeric(as.character(OilP$ChangeOilPrice))

SP <- as.xts(get.hist.quote("^GSPC",start = "2007-01-01",quote = c("Open","Volume","AdjClose")))
IX <- as.xts(get.hist.quote("^IXIC",start = "2007-01-01",quote = c("Open","Volume","AdjClose")))
SPs <- subset(SP, select = c("Date","Open","AdjClose"))
SPs$DailyReturn <- periodReturn(SPs, period = "daily")
SPs <- setDT(as.data.frame(SPs),keep.rownames = TRUE)
colnames(SPs) <- c("DATE","Open","AdjClose","DailyReturn")
IXs <- subset(IX, select = c("Date","Open","AdjClose"))
IXs$DailyReturn <- periodReturn(IXs, period = "daily")
IXs <- setDT(as.data.frame(IXs),keep.rownames = TRUE)
colnames(IXs) <- c("DATE","Open","AdjClose","DailyReturn")
VXD <- read.csv("~/Documents/Big Data Analytics/Capstone/VXDCLS.csv")
colnames(VXD) <- c("DATE","AdjClose")
VXD$DATE <- as.Date(VXD$DATE)
VXD$AdjClose <- as.numeric(VXD$AdjClose)

DJI <- as.xts(get.hist.quote("^DJI",start = "2007-01-01",quote = c("Open","Volume","AdjClose")))
DJIs <- subset(DJI, select=c("DATE","Open","AdjClose"))
DJIs$DailyReturn <- dailyReturn(DJIs, type = "arithmetic")
DJIs$MACD <- MACD(DJIs$AdjClose,maType="EMA", percent = FALSE)
DJIs$ZLMED <- ZLEMA(DJIs$AdjClose, n = 26)
DJIs <- setDT(as.data.frame(DJIs), keep.rownames = TRUE)
DJIs$rn <- as.Date(DJIs$rn)
colnames(DJIs) <- c("DATE","Open","AdjClose","DailyReturn","MACD","Signal","ZLEMA")

## estimate density of daily log returns of DJIA
DJIden <- density(DJIs$DailyReturn)
yl = c(min(DJIden$y),max(DJIden$y))
hist(DJIs$DailyReturn,probability =TRUE,main= "Histogram of DJIA Daily Returns", xlab ="DJIADailyReturns",ylim = yl)
lines(DJIden)
## plot the normal density with mean and standard deviation of DJIA
a <- seq(min(DJIs$DailyReturn),max(DJIs$DailyReturn),by = 0.001)
lines(a,(dnorm(a,mean(DJIs$DailyReturn),sd(DJIs$DailyReturn))),col="red")
shapiro.test(as.numeric(DJIs$DailyReturn))

# Define data table of features where each column is a feature
features <- merge(subset(SPs, select= c("DATE","AdjClose","DailyReturn")), subset(IXs, select = c("DATE","AdjClose","DailyReturn")),by ="DATE")
features <- as.data.table(features)
colnames(features) <- c("DATE","AdjCloseSPs","DailyReturnSPs","AdjCloseIXs","DailyReturnIXs")
features$DATE <- as.Date(as.character(features$DATE))
features <- merge(features, OilP, by= "DATE",all.x =TRUE)
features <- merge(features, IntRateUS, by= "DATE",all.x =TRUE)
features <- merge(features, InfRateUS, by= "DATE",all.x =TRUE)
features <- merge(features, VXD, by= "DATE",all.x =TRUE)
d <- subset(DJIs, select = c("DATE","DailyReturn","MACD","ZLEMA"))
features <- merge(features, d, by ="DATE",all.x= TRUE)
colnames(features) <- c("DATE","AdjCloseSPs","DailyReturnSPs","AdjCloseIXs","DailyReturnIXs","%ChangeOilPrice","IntRateUS","InfRateUS","VXD","DailyReturnDJIs","MACD", "ZLEMA")
dset <- merge(features, subset(DJIs,select = c("DATE","AdjClose")), by = "DATE", all.x = TRUE)
colnames(dset) <- c("DATE","AdjCloseSPs","DailyReturnSPs","AdjCloseIXs","DailyReturnIXs","%ChangeOilPrice","IntRateUS","InfRateUS","VXD","DailyReturnDJIs","MACD", "ZLEMA",
                    "AdjCloseDJIA")

# remove all NAs in dset 
dset$IntRateUS <- sapply(dset$IntRateUS,as.character)
dset$IntRateUS[is.na(dset$IntRateUS)] <- ""
dset$`%ChangeOilPrice`<- sapply(dset$`%ChangeOilPrice`,as.character)
dset$`%ChangeOilPrice`[is.na(dset$`%ChangeOilPrice`)] <- ""
dset$InfRateUS<- sapply(dset$InfRateUS,as.character)
dset$InfRateUS[is.na(dset$InfRateUS)] <- ""
dset$MACD <- sapply(dset$MACD,as.character)
dset$MACD[is.na(dset$MACD)] <- ""
dset$ZLEMA<- sapply(dset$ZLEMA,as.character)
dset$ZLEMA[is.na(dset$ZLEMA)] <- ""
dset$IntRateUS <- as.numeric(dset$IntRateUS)
dset$InfRateUS <- as.numeric(dset$InfRateUS)
dset$`%ChangeOilPrice` <- as.numeric(dset$`%ChangeOilPrice`)
dset$MACD <- as.numeric(dset$MACD)
dset$ZLEMA <- as.numeric(dset$ZLEMA)

# Filter based methods / Feature selection
cfs(AdjCloseDJIA~.,subset(dset, select=-DATE))
information.gain(AdjCloseDJIA~.,subset(dset, select=-DATE))
gain.ratio(AdjCloseDJIA~.,subset(dset, select=-DATE))
symmetrical.uncertainty(AdjCloseDJIA~.,subset(dset, select=-DATE))
linear.correlation(AdjCloseDJIA~.,subset(dset, select=-DATE))
rank.correlation(AdjCloseDJIA~.,subset(dset, select=-DATE))
chi.squared(AdjCloseDJIA~.,subset(dset, select=-DATE))

# divide data into training(75%) and testing(25%)
bound <- floor((nrow(dset)*0.25))
dset.test <- dset[1:bound,]
dset.train <- dset[bound:2491,]

# train model using SVM for Model 1
x3 <- subset(dset.train, select = c("AdjCloseSPs","ZLEMA"))
y3 <- subset(dset.train, select = "AdjCloseDJIA")
svm_tune3 <- tune(svm,x3, y3, kernel="radial",ranges=list(cost=seq(100,1000, by=100),gamma=10^(-4:1)))
gam3 <- svm_tune3$best.parameters$gamma
cst3 <- svm_tune3$best.parameters$cost
svmFit3 <- svm(x3,y3,type = "eps-regression",kernel ="radial",gamma = gam3, cost = cst3, fitted = TRUE, probability = TRUE, na.action = na.omit)
print(svmFit3)

# build SVM predictor for Model 1
z3 <- subset(dset.test, select = c("AdjCloseSPs","ZLEMA"))
predsvm3 <- as.matrix(predict(svmFit3, z3))

# evaluation of SVM model for Model 1
u3 <- as.matrix(subset(dset.test, select="AdjCloseDJIA"))[26:623,]
rmse(u3,predsvm3)
mae(u3,predsvm3)
kappa(u3,predsvm3)

# train model using SVM for Model 2
x <- subset(dset.train, select = c("AdjCloseSPs","AdjCloseIXs","ZLEMA"))
y <- subset(dset.train, select = "AdjCloseDJIA")
svm_tune <- tune(svm,x, y, kernel="radial",ranges=list(cost=seq(100,1000, by=100),gamma=10^(-4:1)))
gam <- svm_tune$best.parameters$gamma
cst <- svm_tune$best.parameters$cost
svmFit <- svm(x,y,type = "eps-regression",kernel ="radial",gamma = gam, cost = cst, fitted = TRUE, probability = TRUE, na.action = na.omit)
summary(svmFit)
print(svmFit)

# build SVM predictor for Model 2
z <- subset(dset.test, select = c("AdjCloseSPs","AdjCloseIXs","ZLEMA"))
predsvm <- as.matrix(predict(svmFit, z))

# evaluation of SVM model for Model 2 
u <- as.matrix(subset(dset.test, select="AdjCloseDJIA"))
u <- as.numeric(u[26:623,])
rmse(u,predsvm)
mae(u,predsvm)
kappa(u,predsvm)

# train model using SVM for Model 3
x2 <- subset(dset.train, select = c("AdjCloseSPs","AdjCloseIXs","ZLEMA","InfRateUS","VXD"))
y2 <- subset(dset.train, select = "AdjCloseDJIA")
svm_tune2 <- tune(svm,x2, y2, kernel="radial",ranges=list(cost=seq(100,1000, by=100),gamma=10^(-4:1)))
gam2 <- svm_tune2$best.parameters$gamma
cst2 <- svm_tune2$best.parameters$cost
svmFit2 <- svm(x2,y2,type = "eps-regression",kernel ="radial",gamma = gam2, cost = cst2, fitted = TRUE, probability = TRUE, na.action = na.omit)
print(svmFit2)

# build SVM predictor for Model 3
z2 <- subset(dset.test, select = c("AdjCloseSPs","AdjCloseIXs","ZLEMA","InfRateUS","VXD"))
predsvm2 <- as.matrix(predict(svmFit2, z2))

# evaluation of SVM model for Model 3
u2 <-as.matrix(subset(dset.test, select="AdjCloseDJIA"))[211:623,]
rmse(u2,predsvm2)
mae(u2,predsvm2)
kappa(u2,predsvm2)

# train model using SVM for Model 4
x4 <- subset(dset.train, select = c("AdjCloseSPs","ZLEMA","InfRateUS","VXD"))
y4 <- subset(dset.train, select = "AdjCloseDJIA")
svm_tune4 <- tune(svm,x4, y4, kernel="radial",ranges=list(cost=seq(100,1000, by=100),gamma=10^(-4:1)))
gam4 <- svm_tune4$best.parameters$gamma
cst4 <- svm_tune4$best.parameters$cost
svmFit4 <- svm(x4,y4,type = "eps-regression",kernel ="radial",gamma = gam4, cost = cst4, fitted = TRUE, probability = TRUE, na.action = na.omit)
print(svmFit4)

# build SVM predictor for Model 4
z4 <- subset(dset.test, select = c("AdjCloseSPs","ZLEMA","InfRateUS","VXD"))
predsvm4 <- as.matrix(predict(svmFit4, z4))

# evaluation of SVM model for Model 4
u4 <-as.matrix(subset(dset.test, select="AdjCloseDJIA"))[211:623,]
rmse(u4,predsvm4)
mae(u4,predsvm4)
kappa(u4,predsvm4)







