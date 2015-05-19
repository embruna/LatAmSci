
# SIMPSON HELP
#compare if linear vs nonlinear by additive model with a by = "series" spline so i have 1 per time series, & add ARMA errors.
# its relatively trivial to then compare the trends (or even their difference) directly using the model info
#http://www.sciencedirect.com/science/article/pii/S0016703711001438
# "Least squares regression was used to investigate the relationship between 
# sediment TOC and pollutant concentrations. All statistical analyses were 
# performed with the R statistical software, version 2.11.1 (R Core Development Team, 2010) 
# and the mgcv package, version 1.6-2 (Wood, 2004 and Wood, 2006).



require(mgcv)

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
BR<-brazil
BR$Country.Name<-NULL

yBR<-BR$PUBS.TOTL
x1BR<-BR$Year
x2BR<-BR$GDP
x3BR<-BR$PopSize

#pairs(~yBR+x1BR+x2BR+x3BR, main="Scatterplot Matrix")


op <- par(mfcol = c(3, 1))
c11 <- plot(x1BR, yBR, xlab="Year", ylab="pubs", mfg=c(1, 1))
c21 <- plot(x1BR, x2BR, xlab="Year", ylab="GDP", mfg=c(2, 1))
c31 <- plot(x1BR, x3BR, xlab="Year", ylab="PopSize", mfg=c(3, 1))
par(op); #Restore graphics parameters


data_plotBR = qplot(x1BR, yBR)+theme_bw()
print(data_plotBR)

#LINEAR
lmBR=gam(yBR~x1BR,family=poisson)
lm_summaryBR=summary(lmBR)
print(lm_summaryBR)
data_plotBR = data_plotBR + geom_smooth(colour="blue", method='lm', se=FALSE)
print(data_plotBR)

#SMOOTHED
gam_modBR=gam(yBR~s(x1BR),family=poisson)
summary(gam_modBR)
data_plotBR = data_plotBR + geom_smooth(colour="red",aes(yBR=fitted(gam_modBR)), se=FALSE)
print(data_plotBR)
plot(gam_modBR)

#Is assumption of linearity justified? use gam() and anova(). Must have smoothed model nested in linear one
lmBR=gam(yBR~x1BR,family=poisson)
nested_gam_modelBR=gam(yBR~s(x1BR)+x1BR,family=poisson)
print(anova(lmBR, nested_gam_modelBR, test="Chisq"))
#


#following Wood pdf
ct1<-gam(yBR~s(x1BR)+s(x2BR),family=poisson)
ct1
par(mfrow=c(1,2))
plot(ct1,residuals=TRUE,pch=19) ## calls plot.gam
#Basic Model Checking
#Deviance residuals are used: often approximately normal.
#Plots are utterly useless for binary data!
gam.check(ct1) ## note QQ beefed up for next mgcv version
#Other residual plots should be examined
plot(fitted(ct1),residuals(ct1))
plot(x1BR,residuals(ct1))
#To check robustness of smoothness selection, fit with an
#alternative smoothness selection criterion.
ct2 <- gam(yBR~s(x1BR)+s(x2BR), family=poisson,method="ML")
ct2
#Once checking suggests that the model is acceptable, then we
#can proceed to more formal inference.
summary(ct1)
#If you must have p-values, then anova is better for any model
#containing factor variables
anova(ct1)

#VISUALIZATIONS
par(mfrow=c(1,2))
plot(ct1,shade=TRUE,seWithMean=TRUE,scale=0)
#Sometimes it is helpful to see how the linear predictor or
#expected response would vary with 2 predictors, if all the
#others were held fixed at some value. vis.gam allows this
vis.gam(ct1,theta=30,ticktype="detailed")
vis.gam(ct1,theta=-45,ticktype="detailed",se=2)
vis.gam(ct1,plot.type="contour")
AIC(ct1,ct2)
# 







##BOLIVIA
condition <- c("Bolivia")
bolivia<-dplyr::filter(foo, Country.Name %in% condition)
bolivia<-filter(bolivia, Year >= 1991 & Year<2014)
bolivia$Country.Code<-NULL
bolivia$Data.Source<-NULL
bolivia$Indicator.Name<-NULL
bolivia<-tidyr::spread(bolivia, Indicator.Code, Value)
BO<-bolivia
BO$Country.Name<-NULL
yBO<-BO$PUBS.TOTL
x1BO<-BO$Year
x2BO<-BO$GDP
x3BO<-BO$PopSize


data_plotBO = qplot(x1BO, yBO)+theme_bw()
print(data_plotBO)

#LINEAR
lmBO=gam(yBO~x1BO,family=poisson)
lm_summaryBO=summary(lmBO)
print(lm_summaryBO)
data_plotBO = data_plotBO + geom_smooth(colour="blue", method='lm', se=FALSE)
print(data_plotBO)

#SMOOTHED
gam_modBO=gam(yBO~s(x1BO),family=poisson)
summary(gam_modBO)
data_plotBO = data_plotBO + geom_smooth(colour="red",aes(yBO=fitted(gam_modBO)), se=FALSE)
print(data_plotBO)
plot(gam_modBO)

#Is assumption of linearity justified? use gam() and anova(). Must have smoothed model nested in linear one
lmBO=gam(yBO~x1BO,family=poisson)
nested_gam_modelBO=gam(yBO~s(x1BO)+x1BO,family=poisson)
print(anova(lmBO, nested_gam_modelBO, test="Chisq"))





#following Wood pdf
ct1<-gam(yBO~s(x1BO)+s(x2BO),family=poisson)
ct1
par(mfrow=c(1,2))
plot(ct1,residuals=TRUE,pch=19) ## calls plot.gam
#Basic Model Checking
#Deviance residuals are used: often approximately normal.
#Plots are utterly useless for binary data!
gam.check(ct1) ## note QQ beefed up for next mgcv version
#Other residual plots should be examined
plot(fitted(ct1),residuals(ct1))
plot(x1BO,residuals(ct1))
#To check robustness of smoothness selection, fit with an
#alternative smoothness selection criterion.
ct2 <- gam(yBO~s(x1BO)+s(x2BO), family=poisson,method="ML")
ct2
#Once checking suggests that the model is acceptable, then we
#can proceed to more formal inference.
summary(ct1)
#If you must have p-values, then anova is better for any model
#containing factor variables
anova(ct1)

#VISUALIZATIONS
par(mfrow=c(1,2))
plot(ct1,shade=TRUE,seWithMean=TRUE,scale=0)
#Sometimes it is helpful to see how the linear predictor or
#expected response would vary with 2 predictors, if all the
#others were held fixed at some value. vis.gam allows this
vis.gam(ct1,theta=30,ticktype="detailed")
vis.gam(ct1,theta=-45,ticktype="detailed",se=2)
vis.gam(ct1,plot.type="contour")
AIC(gam_modBO, ct1)
# 


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

#ANALYSES (p 355)
tsBR1<-zoo(BR1)  #make it a time series object for package zoo
plot(tsBR1) #plot the time series together
plot(tsBR1, screens=1) #plot each time series on its own panel
acf(tsBR1) #plot to see if there is any autocorrelation
Box.test(tsBR1$PUBS.TOTL, type="Ljung-Box") #test for autocorrelation, using the test for small sample sizes
pacf(tsBR1$PUBS.TOTL) #plot the partial autocorrelation function 
ccf(tsBR1$PUBS.,tsBR1$GDP) #look lof lagged autocorrelation 
#detrend the data - PUBS
CDBR<-coredata(BR1$PUBS.TOTL)
INDEXBR<-index(BR1$PUBS.TOTL)
m<-lm(CDBR~INDEXBR)
detr<-zoo(resid(m), INDEXBR)
plot(detr)
#detrend the data - GDP
CDBR.GDP<-coredata(BR1$GDP)
INDEXBR.GDP<-index(BR1$GDP)
mGDP<-lm(CDBR.GDP~INDEXBR.GDP)
detrGDP<-zoo(resid(mGDP), INDEXBR.GDP)
plot(detrGDP)
#detrend the data - POPSIZE
CDBR.POP<-coredata(BR1$PopSize)
INDEXBR.POP<-index(BR1$PopSize)
mPOP<-lm(CDBR.POP~INDEXBR.POP)
detrPOP<-zoo(resid(mPOP), INDEXBR.POP)
plot(detrPOP)

#ARIMA_MODEL- Brazil
library(forecast)
auto.arima(tsBR1$PUBS.TOTL)
arimaBR<-auto.arima(tsBR1$PUBS.TOTL)
arimaBR
confint(arimaBR) #confidence intervals onm the coeff. Note that they may not be significant if they include zero
tsdiag(arimaBR) #Diagnostic tests of the ARIMA model

library(KernSmooth)
gridsize<-length(BR1$PUBS.TOTL)
y=BR1$PUBS.TOTL
t=seq(from=1991,to=2013)
bw<-dpill(t,y,gridsize=gridsize)
lp<-locpoly(x=t, y=y, bandwidth=bw, gridsize=gridsize)
smooth<-lp$y
plot(smooth)
plot(BR1$PUBS.TOTL)

#ARIMA_MODEL- Nicaragua
auto.arima(tsNIC1$PUBS.TOTL)
arimaNIC<-auto.arima(tsNIC1$PUBS.TOTL)
arimaNIC
confint(arimaNIC) #confidence intervals onm the coeff. Note that they may not be significant if they include zero
tsdiag(arimaNIC) #Diagnostic tests of the ARIMA model
# 

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









xv<-seq(1991,2014,1)
yv<-predict(exponential.model, list(x=xv))
lines(xv,yv)























library(tseries)
bds.test(x, m = 3, eps = seq(0.5 * sd(x), 2 * sd(x), length = 4),
         trace = FALSE)







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