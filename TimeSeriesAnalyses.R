# FROM R Cookbook by Teetor

# select data to use
condition <- c("GDP", "PopSize", "PUBS.TOTL")
foo<-dplyr::filter(ALLDATA, Indicator.Code %in% condition)


####BRAZIL
condition <- c("Brazil")
brazil<-dplyr::filter(foo, Country.Name %in% condition)
brazil<-filter(brazil, Year >= 1991 & Year<2014)
brazil$Country.Code<-NULL
brazil$Data.Source<-NULL
brazil$Indicator.Name<-NULL
brazil<-tidyr::spread(brazil, Indicator.Code, Value)

BR1<-brazil
BR1$Country.Name<-NULL
BR1$Year<-NULL

####ECUADOR
condition <- c("Ecuador")
ecuador<-dplyr::filter(foo, Country.Name %in% condition)
ecuador<-filter(ecuador, Year >= 1991 & Year<2014)
ecuador$Country.Code<-NULL
ecuador$Data.Source<-NULL
ecuador$Indicator.Name<-NULL
ecuador<-tidyr::spread(ecuador, Indicator.Code, Value)

EC1<-ecuador
EC1$Country.Name<-NULL
EC1$Year<-NULL

####Nicaragua
condition <- c("Nicaragua")
nicaragua<-dplyr::filter(foo, Country.Name %in% condition)
nicaragua<-filter(nicaragua, Year >= 1991 & Year<2014)
nicaragua$Country.Code<-NULL
nicaragua$Data.Source<-NULL
nicaragua$Indicator.Name<-NULL
nicaragua<-tidyr::spread(nicaragua, Indicator.Code, Value)

NIC1<-nicaragua
NIC1$Country.Name<-NULL
NIC1$Year<-NULL

#ANALYSES
tsBR1<-zoo(BR1)
plot(tsBR1)
plot(tsBR1, screens=1)
acf(tsBR1)
Box.test(tsBR1$PUBS.TOTL, type="Ljung-Box")
pacf(tsBR1$PUBS.TOTL)
ccf(tsBR1$PUBS.TOTL,tsBR1$GDP)

tsEC1<-zoo(EC1)
plot(tsEC1)
plot(tsEC1, screens=1)
acf(tsEC1)
Box.test(tsEC1$PUBS.TOTL, type="Ljung-Box")
pacf(tsEC1$PUBS.TOTL)
ccf(tsEC1$PUBS.TOTL,tsEC1$GDP)

tsNIC1<-zoo(NIC1)
plot(tsNIC1)
plot(tsNIC1, screens=1)
acf(tsNIC1)
Box.test(tsNIC1$PUBS.TOTL, , type="Ljung-Box")
pacf(tsNIC1$PUBS.TOTL)
ccf(tsNIC1$PUBS.TOTL,tsNIC1$GDP)




pubs<-as.matrix(Fig1$articles)
gdpish<-(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,10,25,29,37,69,70,99))
gdpish<-as.data.frame(gdpish)
pubs<-cbind(pubs,gdpish)
summary(pubs)
# names(pubs)[3] <- "gdpish" 
# pubs<-pubs[c(2,1,3)]
# summary(pubs)


# Crawly
pubs<-as.matrix(Fig1$articles)
pubs<-ts(pubs, start=c(1991), end=(c(2014)), frequency=1)
plot(pubs)
length(pubs)
par(mfrow=c(2,2))
sapply(1:4, function(x) plot(pubs[-c(23: (23-x+1))], pubs[-c(1:x)]))


# http://www.stat.pitt.edu/stoffer/tsa3/R_toot.htm
#
plot(pubs, ylab="Pubs", main="Pubs per Year (overall LatAm)")
plot(pubs, type="o", col="blue", lty="dashed")
plot(diff(log(pubs)), main="logged and diffed") 

x = -5:5                  # sequence of integers from -5 to 5
y = 5*cos(x)              # guess
par(mfrow=c(3,2))         # multifigure setup: 3 rows, 2 cols
#---  plot:
plot(x, main="plot(x)")
plot(x, y, main="plot(x,y)")
#---  plot.ts:
plot.ts(x, main="plot.ts(x)")
plot.ts(x, y, main="plot.ts(x,y)")
#---  ts.plot:
ts.plot(x, main="ts.plot(x)")
ts.plot(ts(x), ts(y), col=1:2, main="ts.plot(x,y)")  # note- x and y are ts objects 
#---  the help files [? and help() are the same]:
?plot.ts
help(ts.plot)
?par        # might as well skim the graphical parameters help file while you're here


k = c(.5,1,1,1,.5)            # k is the vector of weights
(k = k/sum(k))       
fpubs = filter(pubs, sides=2, k)  # ?filter for help [but you knew that already]
plot(pubs)
lines(fpubs, col="red")         # adds a line to the existing plot
lines(lowess(pubs), col="blue", lty="dashed")


dlpubs = diff(log(pubs))        # difference the logged data
plot(dlpubs)                  # plot it (not shown)
shapiro.test(dlpubs)          # test for normality 

par(mfrow=c(2,1))        # set up the graphics 
hist(dlpubs, prob=TRUE, 12)   # histogram    
lines(density(dlpubs))     # smooth it - ?density for details 
qqnorm(dlpubs)             # normal Q-Q plot  
qqline(dlpubs)             # add a line   


lag.plot(dlpubs, 9, do.lines=FALSE)  
library(astsa)
lag1.plot(dlpubs, 1)  # if you have astsa loaded (not shown) 
# why the do.lines=FALSE? Because you get a phase plane if it's TRUE 
#   a little phase plane aside - try this on your own
x = cos(2*pi*1:100/4) + .2*rnorm(100)
plot.ts(x)
dev.new()
lag.plot(x, 4)

par(mfrow=c(2,1)) # The power of accurate observation is commonly called cynicism 
#     by those who have not got it. - George Bernard Shaw
acf(dlpubs, 20)     # ACF to lag 20 - no graph shown... keep reading
pacf(dlpubs, 20)    # PACF to lag 20 - no graph shown... keep reading
# !!NOTE!! acf2 on the line below is ONLY available in astsa and tsa3
acf2(dlpubs)        # this is what you'll see below
# 
# https://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
library("TTR")
pubSMA3 <- SMA(pubs$articles,n=2)
plot.ts(pubSMA3)