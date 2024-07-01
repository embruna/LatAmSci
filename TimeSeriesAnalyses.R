
# SIMPSON HELP
# compare if linear vs nonlinear by additive model with a by = "series" spline so i have 1 per time series, & add ARMA errors.
# it's relatively trivial to then compare the trends (or even their difference) directly using the model info
# http://www.sciencedirect.com/science/article/pii/S0016703711001438
# "Least squares regression was used to investigate the relationship between 
#  sediment TOC and pollutant concentrations. All statistical analyses were 
#  performed with the R statistical software, version 2.11.1 (R Core Development Team, 2010) 
#  and the mgcv package, version 1.6-2 (Wood, 2004 and Wood, 2006).


# Objective: test the hypothesis that scientific productivity is on the rise accross latin american (i.e., in all countries).
# This can bet tested by looking at time series in which we include time and country and if fit is improvied with a 
# time x country interaction

require(mgcv)
#1. pull the data you need from ALLDATA and organize what you need (GDP, PopSize,Productivity, COuntry ID)

#ALLDATA<-rbind(GDP, PopSize,PUBS)
# ALLDATA<-filter(ALLDATA, Year >= 1990)
ALLDATA<-DATA
ALLDATA<-filter(ALLDATA,Year >= 1990)
str(ALLDATA)
arrange(ALLDATA, PopSize)
# condition <- c("GDP", "PopSize", "PUBS.TOTL" )
# GAMdata<-dplyr::filter(ALLDATA, Indicator.Code %in% condition)
# GAMdata<-filter(GAMdata, Year >= 1991 & Year<2015)
# GAMdata$Data.Source<-NULL #these aren't needed here
# GAMdata$Indicator.Name<-NULL  #these aren't needed here
# GAMdata$Country.Code<-NULL  #these aren't needed here
# #GAMdata$Country.Name<-NULL  #these aren't needed here
# str(GAMdata)
# #COSTA RICA AND USA GDP BEING LOST IN TRANSITION FROM LONG TO WIDE IN NEXT STEP
# 
# GAMdata<-tidyr::spread(GAMdata, Indicator.Code, Value) #go from long to wide form
# GAMdata<-dplyr::mutate(GAMdata, pubs.per.capita = PUBS.TOTL/PopSize)
# GAMdata<-dplyr::mutate(GAMdata, pubs.per.GDP = PUBS.TOTL/GDP)

#Toggle these off if you want to INCLUDE Canda and USA in analyses
GAMdata<-dplyr::filter(ALLDATA, Country.Name != "United.States")
GAMdata<-dplyr::filter(ALLDATA, Country.Name != "Canada")



#2. Identify the dependent variable and the different independent variables
# yALL<-GAMdata$pubs.per.pop.size
# x0ALL<-GAMdata$Country.Name
# x1ALL<-GAMdata$Year
# x2ALL<-GAMdata$GDP
# x3ALL<-GAMdata$PopSize

yALL<-GAMdata$Pubs
x0ALL<-GAMdata$Country.Name
x1ALL<-GAMdata$Year
# x2ALL<-GAMdata$GDPpercapita
x2ALL<-GAMdata$GDP
# x2ALL<-GAMdata$PopSize


#3. A little visualization: Plot publications, GDP, and PopSize over time
op <- par(mfcol = c(2, 1))
c11 <- plot(x1ALL, yALL, xlab="Year", ylab="pubs", mfg=c(1, 1))
c21 <- plot(x1ALL, x2ALL, xlab="Year", ylab="GDP", mfg=c(2, 1))
c31 <- plot(x1ALL, x3ALL, xlab="Year", ylab="PopSize", mfg=c(3, 1))
par(op); #Restore graphics parameters

#This plots all all publications per country per year, with no linking lines
data_plotALL = qplot(x1ALL, yALL)+theme_bw()
print(data_plotALL)

# LINEAR MODELS WITH DIFFERENT X VARIABLES ALONE or IN COMBINATION (BUT NO INTERACTIONS)
#LINEAR MODEL 1: Effect of "Country"
lmALL1=gam(yALL~x0ALL,family=poisson)
lm_summaryALL1=summary(lmALL1)
print(lm_summaryALL1)

#LINEAR 2: Effect of "Year"
lmALL2=gam(yALL~x1ALL,family=poisson)
lm_summaryALL2=summary(lmALL2)
print(lm_summaryALL2)

#LINEAR 3: GDP
lmALL3=gam(yALL~x2ALL,family=poisson)
lm_summaryALL3=summary(lmALL3)
print(lm_summaryALL3)

#LINEAR 4: Country+year
lmALL4=gam(yALL~x1ALL+x0ALL,family=poisson)
lm_summaryALL4=summary(lmALL4)
print(lm_summaryALL4)

#LINEAR 5: year+GDP
lmALL5=gam(yALL~x1ALL+x2ALL,family=poisson)
lm_summaryALL5=summary(lmALL5)
print(lm_summaryALL5)

#LINEAR 6: Country+year+GDP
lmALL6=gam(yALL~x1ALL+x0ALL+x2ALL,family=poisson)
lm_summaryALL6=summary(lmALL6)
print(lm_summaryALL6)

#LINEAR 7: Country+GDP
lmALL7=gam(yALL~x0ALL+x2ALL,family=poisson)
lm_summaryALL7=summary(lmALL7)
print(lm_summaryALL7)





# SMOOTHED MODELS
#SMOOTHED 1: Country (categorical) + Year (smoothed)
sm1 = gam(yALL~x0ALL+s(x1ALL),family=poisson)
summary.sm1=summary(sm1)
print(summary.sm1$p.table)
print(summary.sm1$s.table)
plot(sm1)

#SMOOTHED 2: Country (categorical) + GDP (smoothed) 
sm2 = gam(yALL~x0ALL+s(x2ALL),family=poisson)
sm2_summary = summary(sm2)
print(sm2_summary$p.table)
print(sm2_summary$s.table)
plot(sm2, page=1)

#SMOOTHED 3: Country (categorical) + Year (smoothed) + GDP (smoothed)
sm3 = gam(yALL~x0ALL+s(x1ALL)+s(x2ALL),family=poisson)
sm3_summary = summary(sm3)
print(sm3_summary$p.table)
print(sm3_summary$s.table)
plot(sm3, page=1)

#Is assumption of linearity justified? use gam() and anova(). Must have smoothed model nested in linear one
#testing assumption of linearity (ie not smoothed for Year
# Details: Pedersen Workshop SLides
nested_gam_modelALL1=gam(yALL~s(x1ALL)+x1ALL,family=poisson)
print(anova(lmALL2, nested_gam_modelALL1, test="Chisq"))

#testing assumption of linearity (ie not smoothed for GDP
# Details: Pedersen Workshop SLides
nested_gam_modelALL2=gam(yALL~s(x2ALL)+x2ALL,family=poisson)
print(anova(lmALL3, nested_gam_modelALL2, test="Chisq"))

#can use an ANOVA to test if the additional smoothed term is necessary
anova(sm2,sm3, test="Chisq")

#INTERACTIONS.  Model of Time + Country, with Year x Country Interaction. 2x: This will tell you which productivity trends are time invariant and which are not, correct?
#THIS IS ALL WE CARE ABOUT - ARE COUNTRIES "ON THE RISE (significant interaction)" OR NOT (no interaction).  
#We don't need to know if a country is significantly more productive than another for this part
catagorical_interact1 = gam(yALL~x0ALL+s(x1ALL,by=x0ALL),family=poisson)
catagorical_interact_summary1 = summary(catagorical_interact1)
catagorical_interact_summary1
print(catagorical_interact_summary1$s.table)

#SAME BUT WITH GDP INCLUDED AS ADDITIONAL SMOOTHED TERM
catagorical_interact2 = gam(yALL~x0ALL+s(x1ALL,by=x0ALL)+s(x2ALL))
catagorical_interact_summary2 = summary(catagorical_interact2)
catagorical_interact_summary2
print(catagorical_interact_summary2$s.table)

#Interactions between smooth terms - thisnk just tells you GDP changes over time. 
smooth_interact = gam(yALL~x0ALL+s(x1ALL,x2ALL))
smooth_interact_summary = summary(smooth_interact)
summary(smooth_interact)
print(smooth_interact_summary$s.table)



#following Wood pdf
ct1<-catagorical_interact1 
ct1
par(mfrow=c(1,2))
plot(ct1,residuals=TRUE,pch=19) ## calls plot.gam
#Basic Model Checking
#Deviance residuals are used: often approximately normal.
#Plots are utterly useless for binary data!
gam.check(ct1) ## note QQ beefed up for next mgcv version
#Other residual plots should be examined
plot(fitted(ct1),residuals(ct1))
plot(x1ALL,residuals(ct1))
#To check robustness of smoothness selection, fit with an
#alternative smoothness selection criterion.
ct2 <- gam(yALL~s(x1ALL)+s(x2ALL), family=poisson,method="ML")
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









# SHOULD YOU BE INTERESTED IN DOING SINGLE COUNTRY ANALYSES, HERE IS HOW YOU DO THAT WITH BRAZIL AS EXAMPLE
require(mgcv)
#1. pull the data you need from ALLDATA and organize what you need (GDP, PopSize,Productivity, COuntry ID)
condition <- c("GDP", "PopSize", "PUBS.TOTL")
GAMdata<-dplyr::filter(ALLDATA, Indicator.Code %in% condition)

#2. Reduce it to just Brazil 
condition <- c("Honduras")
brazil<-dplyr::filter(GAMdata, Country.Name %in% condition)
brazil<-filter(brazil, Year >= 1991 & Year<2014)
brazil$Country.Code<-NULL
brazil$Data.Source<-NULL
brazil$Indicator.Name<-NULL
brazil<-tidyr::spread(brazil, Indicator.Code, Value)
brazil$Country.Name<-NULL
BR<-brazil

#Identify the dependent and independent variables
yBR<-BR$PUBS.TOTL
x1BR<-BR$Year
x2BR<-BR$GDP
x3BR<-BR$PopSize

#Some plotting of different variables over time
#pairs(~yBR+x1BR+x2BR+x3BR, main="Scatterplot Matrix")
op <- par(mfcol = c(3, 1))
c11 <- plot(x1BR, yBR, xlab="Year", ylab="pubs", mfg=c(1, 1))
c21 <- plot(x1BR, x2BR, xlab="Year", ylab="GDP", mfg=c(2, 1))
c31 <- plot(x1BR, x3BR, xlab="Year", ylab="PopSize", mfg=c(3, 1))
par(op); #Restore graphics parameters

#Plot of publications over time
data_plotBR = qplot(x1BR, yBR)+theme_bw()
print(data_plotBR)

#LINEAR: YEAR
lmBR=gam(yBR~x1BR,family=poisson)
lm_summaryBR=summary(lmBR)
print(lm_summaryBR)
data_plotBR = data_plotBR + geom_smooth(colour="blue", method='lm', se=FALSE)
print(data_plotBR)

#SMOOTHED: YEAR
gam_modBR=gam(yBR~s(x1BR),family=poisson)
summary(gam_modBR)
data_plotBR = data_plotBR + geom_smooth(colour="red",aes(yBR=fitted(gam_modBR)), se=FALSE)
print(data_plotBR)
plot(gam_modBR)

#Is assumption of linearity justified? use gam() and anova(). Must have smoothed model nested in linear one
lmBR=gam(yBR~x1BR,family=poisson)
nested_gam_modelBR=gam(yBR~s(x1BR)+x1BR,family=poisson)
print(anova(lmBR, nested_gam_modelBR, test="Chisq"))


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









#AS ARIMA (BUT NOTE NOT LONG ENOUGH TIME SERIES - OPTED FOR GAM INSTEAD)

# FROM R Cookbook by Teetor
# Conducted for both BRAZIL and NICARAGUA for comparison
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
#BRAZIL FIRST
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

#Plotting pubs over time
library(KernSmooth)
gridsize<-length(BR1$PUBS.TOTL)
y=BR1$PUBS.TOTL
t=seq(from=1991,to=2013)
bw<-dpill(t,y,gridsize=gridsize)
lp<-locpoly(x=t, y=y, bandwidth=bw, gridsize=gridsize)
smooth<-lp$y
plot(smooth)
plot(BR1$PUBS.TOTL)


#NOW NICARAGUA
tsNIC1<-zoo(NIC1)  #make it a time series object for package zoo
plot(tsNIC1) #plot the time series together
plot(tsNIC1, screens=1) #plot each time series on its own panel
acf(tsNIC1) #plot to see if there is any autocorrelation
Box.test(tsNIC1$PUBS.TOTL, type="Ljung-Box") #test for autocorrelation, using the test for small sample sizes
pacf(tsNIC1$PUBS.TOTL) #plot the partial autocorrelation function 
ccf(tsNIC1$PUBS.,tsNIC1$GDP) #look lof lagged autocorrelation 
#detrend the data - PUBS
CDNIC<-coredata(NIC1$PUBS.TOTL)
INDEXNIC<-index(NIC1$PUBS.TOTL)
m<-lm(CDNIC~INDEXNIC)
detr<-zoo(resid(m), INDEXNIC)
plot(detr)
#detrend the data - GDP
CDNIC.GDP<-coredata(NIC1$GDP)
INDEXNIC.GDP<-index(NIC1$GDP)
mGDP<-lm(CDNIC.GDP~INDEXNIC.GDP)
detrGDP<-zoo(resid(mGDP), INDEXNIC.GDP)
plot(detrGDP)
#detrend the data - POPSIZE
CDNIC.POP<-coredata(NIC1$PopSize)
INDEXNIC.POP<-index(NIC1$PopSize)
mPOP<-lm(CDNIC.POP~INDEXNIC.POP)
detrPOP<-zoo(resid(mPOP), INDEXNIC.POP)
plot(detrPOP)

#ARIMA_MODEL- Nicaragua
auto.arima(tsNIC1$PUBS.TOTL)
arimaNIC<-auto.arima(tsNIC1$PUBS.TOTL)
arimaNIC
confint(arimaNIC) #confidence intervals onm the coeff. Note that they may not be significant if they include zero
tsdiag(arimaNIC) #Diagnostic tests of the ARIMA model

#Plotting pubs over time
library(KernSmooth)
gridsize<-length(NIC1$PUBS.TOTL)
y=NIC1$PUBS.TOTL
t=seq(from=1991,to=2013)
bw<-dpill(t,y,gridsize=gridsize)
lp<-locpoly(x=t, y=y, bandwidth=bw, gridsize=gridsize)
smooth<-lp$y
plot(smooth)
plot(NIC1$PUBS.TOTL)
















#OTHER OPTIONS - This is just a sandbox where I ket stuff as I was learning.
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