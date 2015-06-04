#####TO DO LIST: DATA ENTRY AND MANIPULATION
  #Figure out if need HDI. If so, need to reinsert it somewhere
#####Analyses and Figures
#COMPLETEE Figure2: Map shaded by total pubs over period 1991-2014
#Figure3: Map shaded by % of productivity over period 1991-2014
#Figure4b: alternative: line chart of % of total LATAM producvitivy by year for each country  
#Figure5: Map shaded by total % change in publications over period 1991-2014. Because there is some interannual variability, 
    #used the sum of articles 1991-1995 and 2010-2014 and calclulated as Relative Growth Rate

#R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN: 
#install.packages("refnet_0.6.tar.gz", repos=NULL, type="source")
#setwd("/Users/emiliobruna/Desktop/LATAM Data Updates")
#setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/refnet")
##  Set this to wherever you unzipped the archive folders (not /src):

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(RGraphics)
library(rworldmap)

rm(list=ls())
#LOAD THE NECESSARY FUNCTIONS
setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/LatAmSci")
source("QSprep.R")
source("UniRankSummary.R")
source("GDPprep.R")
source("PopSizeprep.R")
source("RDprep.R")
source("WBEDprep.R")
source("UNEDprep.R")
source("PUBSprep.R")

#LOAD THE DATASETS
setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/Data") #These data are in a different Folder from the code
#Importing QS UNIVERSITY RANKINGS DATA on Latin America's top 100 Universities
UNIRANK<-read.csv("QS.csv", dec=".", header = TRUE, sep = ",", na.strings='NULL', check.names=FALSE)
#Importing World Bank Data on GDP
GDPdata<-read.csv("GDP.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE)
#Importing World Bank Data on total population size per country
PopSizeData<-read.csv("PopSize.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE)
#Importing the Investment in R&D Data. 
RDData<-read.csv("R&D.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE)
#Importing World Bank Education Data
WBEDdata<-read.csv("WorldBankEdData.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE)
#Importing data of publications per year per country
PUBSdata<-read.csv("PUBCOUNT_21may2015.csv", dec=".", header = TRUE, sep = ",", na.strings='NULL', check.names=FALSE)
#Importing UN Education Data
UNEDdata<-read.csv("UNDP_EdIndex.csv", dec=".", header = TRUE, sep = ",", na.strings='NULL', check.names=FALSE)


###################################################
###################################################
##  LOAD AND CLEAN RAW GLOBAL DATASETS 
###################################################
###################################################

#############################################################################################################################
# Uses function QSprep to read QS UNIVERSITY RANKINGS DATA, organize it to match the other datassets, and return a dataframe
#   of the top 100 Universities in Latin America in 20012 based on the QS University Rankings
UNIRANK<-QSprep(UNIRANK)
#############################################################################################################################

#############################################################################################################################
#Use function UniRankSummary to produce a summary table of how manu institutions are in each country
UNIRANK.table<-UniRankSummary(UNIRANK)
#############################################################################################################################

#############################################################################################################################
# Use function GDPprep to clean and standardize the WORLD BANK GDP DATA. 
# In World Bank data each country has a code. We import the entire dataset, then
# select just countries of in our study:ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN CAN USA
GDP<-GDPprep(GDPdata)  
# Note that while we have GDP data for the entire world, the function only selects the data for the countries of 
# interest.If you want to add or delete countries you need to do it from inside the function.  It would probably 
# be a good idea to change this main code later so that country slection is made here
#############################################################################################################################

#############################################################################################################################
#Use function PopSizeprep to clean and standardize the World Bank Population Total Size Data. As above it selects just
#countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN
PopSize<-PopSizeprep(PopSizeData)  
#############################################################################################################################

#############################################################################################################################
#Use function RDprep to clean and standardize the % GD Devoted to RD Data from UNESCO. As above it selects just
#countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN
RD<-RDprep(RDData)  
#############################################################################################################################

#############################################################################################################################
#Use function WBEDprep to clean and standardize the WorldBank Education Data. As above it selects just
#countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN
WBED<-WBEDprep(WBEDdata)  
#############################################################################################################################

#############################################################################################################################
#Use function UNEDprep to clean and standardize the UN Education Data. As above it selects just
#countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN
UNED<-UNEDprep(UNEDdata)  
#############################################################################################################################

#############################################################################################################################
#Use the function PUBSprep clean and standardize the Publication Data. These data were not from REFNET, 
#They were collected with searches of WOS Key words
PUBS<-PUBSprep(PUBSdata)  
#############################################################################################################################


#############################################################################################################################
# Structure of the different datasets 
#############################################################################################################################
# str(UNED)
# str(WBED)
# str(GDP)
# str(PopSize)
# str(RD)
# str(PUBS)



###################################################
###################################################
##  DATA MANIPULATION FOR ANALYSES AND FIGURES
###################################################
###################################################


#############################################################################################################################
##  PUBLICATIONS PER REGION/COUNTRY PER YEAR 
PUBS.COUNT.LONG<-PUBS #Publications per year in LONG FORMAT
PUBS.COUNT.LONG$Indicator.Name<-NULL	#Delete unecessary column
PUBS.COUNT.LONG$Indicator.Code<-NULL	#Delete unecessary column
PUBS.COUNT.LONG$Data.Source<-NULL     #Delete unecessary column
PUBS.COUNT.WIDE<-spread(PUBS.COUNT.LONG, Year, Value) #Convert to WIDE FORMAT
# #If you wanted to change the olumn names to avoid having them be numbers use the following two lines
# colnames(PUBS.COUNT.WIDE)[5:28] <- paste("YR", colnames(PUBS.COUNT.WIDE)[5:28], sep = ".") #Change the column names to make sure they don't start with a number
# colnames(PUBS.COUNT.WIDE)[4]<- "Avg1980s"

#############################################################################################################################
##  PERCENT CHANGE IN PUBLICATIONS PRODCUED By Region & Country
#############################################################################################################################
#FOR NATIONAL PATTERNS OF % CHANGE: Create a dataframe to store the data on proportional change from year to year. 
#It is created based on PUBS.COUNT.WIDE and includes Country.Name, Country.Code, Region
PUBS.PROP.NAT<-select(PUBS.COUNT.WIDE, contains("Country"))
PUBS.PROP.NAT$Region<-PUBS.COUNT.WIDE$Region

#This loop will calculate the proportional change in pub number compared to avg for 1981-1990 (the column "1990")
#Note that it will give NaN for all those for which there were no publicatons in that decade
for (n in 5:28)
{
  PUBS.PROP.NAT[n-1]<-(((PUBS.COUNT.WIDE[n])-(PUBS.COUNT.WIDE[4]))/(PUBS.COUNT.WIDE[4]))*100
}

# 
# #This loop will calculate the proportional change in pub number compared to avg for 1981-1990
# #addedd 0.001 to correct for zero values in some years, but note that this hyperinflates the numbers for some countries
# for (n in 5:28)
# {
#   PUBS.PROP.NAT[n-1]<-(((PUBS.COUNT.WIDE[n]+0.1)-(PUBS.COUNT.WIDE[4]+0.1))/(PUBS.COUNT.WIDE[4]+0.1))*100
# }


#Create a dataframe to store the data on proportional change from year to year. THIS IS FOR REGIONAL PATTERNS
#It is created based on PUBS.COUNT and includes Country.Name, Country.Code, Region
PUBS.PROP.REG<-PUBS
PUBS.PROP.REG<-aggregate(Value ~ Region+Year, data = PUBS.PROP.REG, sum)
PUBS.PROP.REG<-spread(PUBS.PROP.REG, Year, Value)
# colnames(PUBS.PROP.REG)[3:26] <- paste("YR", colnames(PUBS.PROP.REG)[3:26], sep = ".") #Change the column names to make sure they don't start with a number
# colnames(PUBS.PROP.REG)[2]<- "Avg1980s"
PUBS.PROP.REG2<-select(PUBS.PROP.REG, contains("Region"))

#This loop will calculate the proportional diff in publications between a year and the average number of ppaers per year 1981-1990.
#It does so using the PUBS.PROP.REG, then append it as a column in PUBS.PROP.REG2
#The 
for (n in 3:26)
{
  PUBS.PROP.REG2[n-1]<-(((PUBS.PROP.REG[n])-(PUBS.PROP.REG[2]))/(PUBS.PROP.REG[2]))*100
}


#Take the dataframes from wide to long
PUBS.COUNT<-gather(PUBS.COUNT.WIDE, "Year", "Count", 4:28)
PUBS.PROP.NAT<-gather(PUBS.PROP.NAT, "Year", "Count", 4:27)
PUBS.PROP.REG<-gather(PUBS.PROP.REG2, "Year", "Count", 2:25)

#Now Sort them by country and year
PUBS.COUNT <- arrange(PUBS.COUNT,Country.Name, Year)
PUBS.PROP.NAT <- arrange(PUBS.PROP.NAT,Country.Name, Year)
PUBS.PROP.REG<-arrange(PUBS.PROP.REG,Region, Year)

#Chnage name of column from count to percent change
colnames(PUBS.PROP.NAT)[5]<- "Percent.Change"
colnames(PUBS.PROP.REG)[3]<- "Percent.Change"

#Need to convert year from factor to numeric
PUBS.PROP.NAT$Year<-as.numeric(levels(PUBS.PROP.NAT$Year))[PUBS.PROP.NAT$Year] 
PUBS.COUNT$Year<-as.numeric(levels(PUBS.COUNT$Year))[PUBS.COUNT$Year] 
PUBS.PROP.REG$Year<-as.numeric(levels(PUBS.PROP.REG$Year))[PUBS.PROP.REG$Year] 

# str(PUBS.PROP.NAT)
# str(PUBS.PROP.REG)
# str(PUBS.COUNT)
#Some rounding to clean up
PUBS.PROP.REG[3] <-round(PUBS.PROP.REG[3],2)
PUBS.PROP.NAT[5] <-round(PUBS.PROP.NAT[5],2)


#############################################################################################################################
##  calclulate PUBS PER POP SIZE
#############################################################################################################################

PUBS<-filter(PUBS, Year >= 1990)
PopSize<-filter(PopSize, Year >= 1990)

PubsPerCapita<-select(PUBS, contains("Country"))
PubsPerCapita$Region<-PUBS$Region
PubsPerCapita$Year<-PUBS$Year
PubsPerCapita$PubsPerCapita<-PUBS$Value/PopSize$Value


#############################################################################################################################
##  Ncalclulate PUBS PER GDP Prior YEAR
#############################################################################################################################

PUBS1<-filter(PUBS, Year >= 1991)
PUBS1$Indicator.Name<-NULL  
PUBS1$Indicator.Code<-NULL  
PUBS1$Data.Source<-NULL
PUBS1$Indicator.Name<-NULL
names(PUBS1)[4]<-"Publications"
names(PUBS1)[3]<-"Year.Pubs"

GDP1<-filter(GDP, Year >= 1990)
GDP1<-filter(GDP1, Year < 2014)
GDP1$Indicator.Name<-NULL  
GDP1$Indicator.Code<-NULL  
GDP1$Data.Source<-NULL
names(GDP1)[4]<-"GDP"
GDP1$Country.Name<-NULL
GDP1$Country.Code<-NULL
GDP1$Region<-NULL
names(GDP1)[1]<-"Year.GDP"

PubsPerGDP<-cbind(PUBS1,GDP1)
PubsPerGDP$PubsPerGDP<-PubsPerGDP$Publications/PubsPerGDP$GDP


###################################################
###################################################
##  FIGURES
###################################################
###################################################



#############################################################################################################################
#TOTAL PRODUCTIVITY (ALL REGIONS TOGETHER)
#############################################################################################################################
# FigALL<-PUBS.COUNT
# FigALL<-as.data.frame(tapply(FigALL$Count, FigALL$Year, sum))
# names(FigALL)[1] <- "Publications" #need to rename the column after tapply
# FigALL$Year<-c(1990:2014)
# MyFigALL<-qplot(Year,Publications, data = FigALL, geom="line", main = "Total Articles, 1991-2014")
# MyFigALL + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
#                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig1 productivity per year per region
#############################################################################################################################
Fig1<-PUBS.COUNT  #create a dataframe to make this figure
Fig1[Fig1=="LatAm"]<-"Latin America"   #change LatAm to Latin America so figure legends look nicer
Fig1<-aggregate(Count ~ Region+Year, data = Fig1, sum)  #sum the productivity of countries in each region in each year
# Use ggplot to make the figures.  this one is a more basic qplot. The following line greates a line plot of publication 
# count of publications by year by region. Regions differentiatiated by color
MyFig1<-qplot(Year, Count, data = Fig1, color = Region, geom = "line", ylab="Number of Articles per Year")
MyFig1<-MyFig1 +ggtitle("A")       #Makes the Main title "A" because it will be figure A in a multi-panel plot
# The following lines are options for changing the colors of the lines
#MyFig1<-MyFig1 + scale_color_brewer(palette = "Paired")   #Changes the line color tothe RcolorBrewer palette "Paired"
MyFig1<-MyFig1 + scale_colour_manual(values=c("gray15", "darkgreen", "blue3"))  #I chose my own colors for the lines
MyFig1<-MyFig1 + geom_line(aes(group=factor(Region)),size=1)  #Changes the thickness of the lines
# The following two-part line 1) sets the Y axix and 2) sets the frequency of the tick marks
MyFig1<-MyFig1 + coord_cartesian(ylim = c(-20, 4000)) + scale_y_continuous(breaks=seq(0, 4000, 250)) 
# I wanted to change one of the labels on X axis. to do so need scale_x_discrete. Left in Scale_X_continuous to show how would be for numerical axes
#MyFig1<-MyFig1 + coord_cartesian(xlim = c(1989, 2016)) + scale_x_continuous(breaks=seq(1990, 2014, 4))
MyFig1<-MyFig1 + coord_cartesian(xlim = c(1989, 2015)) + scale_x_discrete(labels=c("'80s (avg.)"," ", " ", " ", "1994"," ", " ", " ", "1998"," ", " ", " ", "2002"," ", " ", " ", "2006"," ", " ", " ", "2010"," ", " ", " ", "2014"))
# I wanted labels om the individual lines, so I used the following to define the label and position. 
# For a standard legend comment out the following three lines and add the ones below labeled "LEGEND" as instructed
# Fig1[Fig1$Year=="2013" controls on which data point to put the label, hjust/vjust adjust location relative to data point labeled.
# Can also change font size and style
MyFig1<-MyFig1 + geom_text(data = Fig1[Fig1$Year=="2013" & Fig1$Region=="Latin America",], aes(label = Region), hjust = 1, vjust = -1, size=7, fontface="bold")
MyFig1<-MyFig1 + geom_text(data = Fig1[Fig1$Year=="2013" & Fig1$Region=="USA",], aes(label = Region), hjust = 1, vjust = -1, size=7,fontface="bold")
MyFig1<-MyFig1 + geom_text(data = Fig1[Fig1$Year=="2013" & Fig1$Region=="Canada",], aes(label = Region), hjust = 1, vjust = 2, size=7,fontface="bold")
# The following selects the theme and manipulates elements of the plot
# I wanted to remove the gray background, dots, and gridlines from the plot
MyFig1<-MyFig1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                             plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=22),        #Sets title size, style, location
                             axis.title.x=element_text(colour="black", size = 18, face = "bold",vjust=-2),            #sets x axis title size, style, distance from axis
                             axis.title.y=element_text(colour="black", size = 18, face = "bold", vjust=2),            #sets y axis title size, style, distance from axis
                             axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                             plot.margin = unit(c(1,3,3,1), "cm"),                                          #Changes the margins around the plot. This will help with spacing in multi plt panels
                             legend.position = "none")                                                       #Removes the Legend

MyFig1
# #LEGEND
# #If you want a legend in the figure instead of labeled lines, comment out three label lines above and 
# #add these lines inside theme(---)
# legend.title=element_blank(), #Deletes the title of the legend
# legend.text=element_text(colour="black", size = 16, face = "bold"), #Increases the size of the names on the legend
# legend.position=c(.8, .5),  #Position of the legend on the plot
# legend.background = element_rect(fill=NULL, size=0.5, colour="black", linetype="solid"), #Puts a box around the legend and removes grey background
# legend.key = element_blank() 

#############################################################################################################################
#############################################################################################################################


#############################################################################################################################
#Fig2 Productivity per year per country ***INCLUDES*** USA AND CANADA
#############################################################################################################################
Fig2<-PUBS.COUNT
MyFig2<-qplot(Year, Count, data = Fig2, color = Country.Name, geom = "line", ylab="Articles Produced Annually, 1991-2014")+ggtitle("B")
#This changes the color scheme of the lines
#MyFig2<-MyFig2 + scale_color_brewer(palette = "Paired") #Changes the line color to one of the RcolorBrewer palettes
#MyFig2<-MyFig2 + scale_colour_manual(values=c("gray35", "darkgreen", "blue2")) #Changes the line color
MyFig2<-MyFig2 + geom_line(aes(group=factor(Country.Name)),size=1)  #CHnages the Line width
MyFig2<-MyFig2 + coord_cartesian(ylim = c(-20, 3500)) + scale_y_continuous(breaks=seq(0, 3500, 250))
#these removes the gray background, dots, and gridlines from the plot
MyFig2<-MyFig2 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                      plot.title = element_text(hjust=0.02, face="bold", size=18), legend.title=element_blank(),
                                      legend.text=element_text(colour="black", size = 16, face = "bold"),
                                      legend.position=c(.8, .5), axis.text=element_text(colour="black", size = 16),
                                      axis.title.x=element_text(colour="black", size = 20, face = "bold"),
                                      axis.title.y=element_text(colour="black", size = 20, face = "bold"),
                                      legend.key = element_blank())
MyFig2
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig3 productivity per year per country ***WITHOUT*** USA or CANADA
#############################################################################################################################
Fig3<-filter(PUBS.COUNT, Region == "LatAm")
MyFig3<-qplot(Year, Count, data = Fig3, color = Country.Name, geom = "line",ylab="Articles Produced Annually, 1991-2014")+ggtitle("B")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 
#these removes the gray background, dots, and gridlines from the plot
MyFig3<-MyFig3 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                      plot.title = element_text(hjust=0.02, face="bold", size=18), legend.title=element_blank(),
                                      legend.text=element_text(colour="black", size = 16, face = "bold"),
                                      legend.position=c(.1, .6), axis.text=element_text(colour="black", size = 16),
                                      axis.title.x=element_text(colour="black", size = 20, face = "bold"),
                                      axis.title.y=element_text(colour="black", size = 20, face = "bold"),
                                      legend.key = element_blank())

MyFig3
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig. 4: PROP. CHANGE in PRODUCTIVITY RELATIVE TO AVG PER YEAR IN 80s (ALL REGIONS)
#############################################################################################################################
# Fig4<-PUBS.COUNT
# Fig4<-as.data.frame(tapply(Fig4$Count, Fig4$Year, sum))
# names(Fig4)[1] <- "Publications" #need to rename the column after tapply
# Fig4$Year<-c(1992:2014)
# MyFig4<-qplot(Year,Percent.Change, data = Fig4, geom="line", main = "Annual.Percent.Change, 1991-2014")
# MyFig4 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
#                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig. 5: PROP. CHANGE in PRODUCTIVITY RELATIVE TO AVG PER YEAR IN 80s (BY REGION)
#############################################################################################################################
# For Details on figure construction see Fig 1 Notatation
Fig5<-PUBS.PROP.REG
Fig5[Fig5=="LatAm"]<-"Latin America"
MyFig5<-qplot(Year, Percent.Change, data = Fig5, color = Region, geom = "line", ylab="Change in Productivity Compared to Avg.for 1980s (%)")+ggtitle("B")
MyFig5<-MyFig5 + scale_colour_manual(values=c("gray15", "darkgreen", "blue3")) #Changes the line color
MyFig5<-MyFig5 + geom_line(aes(group=factor(Region)),size=1)  #CHnages the Line width
MyFig5<-MyFig5 + coord_cartesian(ylim = c(-20, 1500)) + scale_y_continuous(breaks=seq(0, 1500, 250))
#MyFig5<-MyFig5 + coord_cartesian(xlim = c(1990, 2016)) + scale_x_continuous(breaks=seq(1991, 2014, 4))
MyFig5<-MyFig5 + coord_cartesian(xlim = c(1989, 2015)) + scale_x_discrete(labels=c("1991"," ", " ", "1994"," ", " ", " ", "1998"," ", " ", " ", "2002"," ", " ", " ", "2006"," ", " ", " ", "2010"," ", " ", " ", "2014"))

MyFig5<-MyFig5 + geom_text(data = Fig5[Fig5$Year=="2013" & Fig5$Region=="Latin America",], aes(label = Region), hjust = 1, vjust = -0.5, size=7,fontface="bold")
MyFig5<-MyFig5 + geom_text(data = Fig5[Fig5$Year=="2013" & Fig5$Region=="USA",], aes(label = Region), hjust = 1, vjust = 2, size=7,fontface="bold")
MyFig5<-MyFig5 + geom_text(data = Fig5[Fig5$Year=="2013" & Fig5$Region=="Canada",], aes(label = Region), hjust = 1, vjust = -1, size=7,fontface="bold")
MyFig5<-MyFig5 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                      plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=22),  
                                      axis.title.x=element_text(colour="black", size = 18, face = "bold",vjust=-2),            
                                      axis.title.y=element_text(colour="black", size = 18, face = "bold", vjust=2),          
                                      axis.text=element_text(colour="black", size = 16),
                                      plot.margin = unit(c(1,1,3,3), "cm"),                                          
                                      legend.position = "none")

MyFig5
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig. 6: PROP. CHANGE in PRODUCTIVITY RELATIVE TO AVG PER YEAR IN 80s PER COUNTRY ***INCLUDES*** USA AND CANADA
#############################################################################################################################
Fig6<-PUBS.PROP.NAT
MyFig6<-qplot(Year, Percent.Change, data = Fig6, color = Country.Name, geom = "line",main = "% Increase in articles over 1995 (1st year in WOS), 1991-2014")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 
#these removes the gray background, dots, and gridlines from the plot
MyFig6 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig. 7: PROP. CHANGE in PRODUCTIVITY RELATIVE TO AVG PER YEAR IN 80s PER COUNTRY ***WITHOUT*** USA AND CANADA
#############################################################################################################################
Fig7<-filter(PUBS.PROP.NAT, Region == "LatAm")
MyFig7<-qplot(Year, Percent.Change, data = Fig7, color = Country.Name, geom = "line",main = "Articles Produced Annually, 1991-2014")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 
#these removes the gray background, dots, and gridlines from the plot
MyFig7 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig. 8: PAPERS PER CAPITA BY REGION
#############################################################################################################################
Fig8<-PubsPerCapita
Fig8<-aggregate(PubsPerCapita ~ Region+Year, data = Fig8, sum)
MyFig8<-qplot(Year, PubsPerCapita, data = Fig8, color = Region, geom = "line", main = "Articles Produced Per Capita, 1991-2014")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 
#these removes the gray background, dots, and gridlines from the plot
MyFig8 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig. 9: PAPERS PER CAPITA BY COUNTRY **INCLUDES USA & CANADA**
#############################################################################################################################
Fig9<-PubsPerCapita
MyFig9<-qplot(Year, PubsPerCapita, data = Fig9, color = Country.Name, geom = "line",main = "Pubs Per Capita, 1991-2014")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 
#these removes the gray background, dots, and gridlines from the plot
MyFig9 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#############################################################################################################################
#############################################################################################################################


#############################################################################################################################
#Fig. 10: PAPERS PER CAPITA BY COUNTRY **WITHOUT USA & CANADA**
#############################################################################################################################
Fig10<-PubsPerCapita
Fig10<-filter(PubsPerCapita, Region == "LatAm")
MyFig10<-qplot(Year, PubsPerCapita, data = Fig10, color = Country.Name, geom = "line",main = "Pubs per capita, 1991-2014")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 
#these removes the gray background, dots, and gridlines from the plot
MyFig10 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#############################################################################################################################
#############################################################################################################################


#############################################################################################################################
#Fig. 11: PAPERS PER $ GDP BY REGION
#############################################################################################################################
Fig11<-PubsPerGDP
Fig11<-aggregate(PubsPerGDP ~ Region+Year.Pubs, data = Fig11, sum)
MyFig11<-qplot(Year.Pubs, PubsPerGDP, data = Fig11, color = Region, geom = "line", main = "Articles Produced Per S GDP, 1991-2014")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 
#these removes the gray background, dots, and gridlines from the plot
MyFig11 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig. 12: PAPERS PER $ GDP BY COUNTRY **INCLUDES USA & CANADA***
#############################################################################################################################
Fig12<-PubsPerGDP
MyFig12<-qplot(Year.Pubs, PubsPerGDP, data = Fig12, color = Country.Name, geom = "line",main = "Pubs Per S GDP, 1991-2014")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 
#these removes the gray background, dots, and gridlines from the plot
MyFig12 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig. 13: PAPERS PER $ GDP BY COUNTRY **NO USA & CANADA***
#############################################################################################################################
Fig13<-filter(PubsPerGDP, Region == "LatAm")
MyFig13<- ggplot(Fig13, aes(x = Year.Pubs, y = PubsPerGDP, group = Country.Name)) + geom_line(colour="red",size=0.7) + ylab("Publications per US$ of GDP ") +
  xlab("Year") +
  facet_wrap( ~ Country.Name, ncol=6)+ggtitle("Figure 4")
MyFig13<-MyFig13 + coord_cartesian(xlim = c(1989, 2016)) + 
  scale_x_discrete(labels=c("1991"," ", " "," "," ", " "," ", " ", " "," ", " ","2002", " ", " ", " "," ", " ", " ", " "," ", " ", " ", " ","2014"))

MyFig13<- MyFig13 + theme_bw()+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                     plot.title = element_text(hjust=0.5, vjust=-60, face="bold", size=20), 
                                     axis.title.x=element_text(colour="black", size = 20, face = "bold", vjust=-1.5),
                                     axis.title.y=element_text(colour="black", size = 20, face = "bold", vjust=2),
                                     axis.text=element_text(colour="black", size = 13),
                                     strip.text.x = element_text(size=12, face="bold", colour="midnightblue"),
                                     plot.margin = unit(c(1,3,3,1), "cm"),   
                                     panel.margin = unit(1, "lines"),    #Adds more space between the facets (=panels)
                                     strip.background = element_rect(colour="black", size=0.5, fill="white"))

MyFig13
#############################################################################################################################
#############################################################################################################################



#############################################################################################################################
#Fig. 14: DATA IN DIFFERENT PANELS
#############################################################################################################################
Fig14<-filter(PubsPerCapita, Region == "LatAm" & Year >1990)
MyFig14<- ggplot(Fig14, aes(x = Year, y = PubsPerCapita, group = Country.Name)) + geom_line(colour="red",size=0.7) + ylab("Publications per capita") +
  facet_wrap( ~ Country.Name, ncol=6)+ggtitle("Figure 3")
MyFig14<-MyFig14 + coord_cartesian(xlim = c(1989, 2016)) + 
  scale_x_discrete(labels=c("1991"," ", " "," "," ", " "," ", " ", " "," ", " "," ", "2002", " ", " "," ", " ", " ", " "," ", " ", " ", " ","2013"))

MyFig14<- MyFig14 + theme_bw()+theme(panel.grid.major = element_blank(), 
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                               plot.title = element_text(hjust=0.5, vjust=-60, face="bold", size=20), 
                               axis.title.x=element_text(colour="black", size = 20, face = "bold", vjust=-1.5),
                               axis.title.y=element_text(colour="black", size = 20, face = "bold", vjust=2),
                               axis.text=element_text(colour="black", size = 13),
                               strip.text.x = element_text(size=12, face="bold", colour="midnightblue"),
                               plot.margin = unit(c(1,3,3,1), "cm"),   
                               panel.margin = unit(1, "lines"),    #Adds more space between the facets (=panels)
                               strip.background = element_rect(colour="black", size=0.5, fill="white"))
                               
MyFig14
#############################################################################################################################
#############################################################################################################################

#see to reorder the subplots http://stackoverflow.com/questions/15116081/controlling-order-of-facet-grid-facet-wrap-in-ggplot2
                             


#############################################################################################################################
#############################################################################################################################
Fig15<-filter(PUBS.COUNT, Region == "LatAm" & Year >1990)
Fig15$Country.Code<-NULL  
Fig15$Region<-NULL
# Fig15[4]<-NA
# names(Fig15)[4] <- "LatAmMean"
PUBS.AVG<-aggregate(Count ~ Year, data = Fig15, median)  #LINE IS THE MEDIAN COULD CHANGE TO MEAN
Fig15 <- merge(Fig15, PUBS.AVG, by = 'Year', suffixes = c('.Country', '.Avg')) #JOIN THE FUN to the others (Mean,median...

MyFig15<- ggplot(Fig15, aes(x = Year, y = Count.Country, group = Country.Name)) + geom_line(colour="red",size=0.7) + ylab("Publications") +
  facet_wrap( ~ Country.Name, ncol=6)+ggtitle("Figure 2") +
  geom_line(aes(y = Count.Avg), color = 'black', lty=2)

MyFig15<-MyFig15 + coord_cartesian(xlim = c(1989, 2016)) + 
  scale_x_discrete(labels=c("1991"," ", " "," "," ", " "," ", " ", " "," ", " "," ", "2002", " ", " "," ", " ", " ", " "," ", " ", " ", " ","2013"))

MyFig15<-MyFig15 + theme_bw()+theme(panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                                     plot.title = element_text(hjust=0.5, vjust=-60, face="bold", size=20), 
                                     axis.title.x=element_text(colour="black", size = 20, face = "bold", vjust=-1.5),
                                     axis.title.y=element_text(colour="black", size = 20, face = "bold", vjust=2),
                                     axis.text=element_text(colour="black", size = 13),
                                     strip.text.x = element_text(size=12, face="bold", colour="midnightblue"),
                                     plot.margin = unit(c(1,3,3,1), "cm"),   
                                     panel.margin = unit(1, "lines"),
                                     strip.background = element_rect(colour="black", size=0.5, fill="white"))
MyFig15
#############################################################################################################################
#############################################################################################################################


#############################################################################################################################
#############################################################################################################################
# Mapping % of total productivity 1991-2014 by each country
# Prep the data 
MyFig16<-filter(PUBS.COUNT, Region == "LatAm" & Year >1990)  #Reduce dataset to Latin America & 1991 On
MyFig16<-aggregate(Count ~ Country.Name+Country.Code, data = MyFig16, sum) #Total for each country over entire time period
MyFig16$perc.total<-(MyFig16$Count/sum(MyFig16$Count))*100   #calculates each countrys percent of the total productivty 
MyFig16$perc.total<-round(MyFig16$perc.total, 2)   #Round to 2 decimal places
MyFig16<-arrange(MyFig16, Count)  #Arrange them from low to high - easier to see who is high and low

MyFig16$Country.Name<- as.character(MyFig16$Country.Name)
MyFig16$Country.Name[MyFig16$Country.Name == "El.Salvador"] <- "El Salvador"
MyFig16$Country.Name[MyFig16$Country.Name == "Costa.Rica"] <- "Costa Rica"
MyFig16$Country.Name<- as.factor(MyFig16$Country.Name)

# making the maps: First map to the whole globe, then make the map just Latin America
sPDF <- joinCountryData2Map( MyFig16, joinCode = "ISO3", nameJoinColumn = "Country.Code") 
mapCountryData(sPDF, nameColumnToPlot="perc.total") #Maps your variable of interest into the map of the world
# How to just plot to LATAM (with HT to http://stackoverflow.com/questions/28838866/mapping-all-of-latin-america-with-rworldmap/28863992#28863992)
sPDFmyCountries <- sPDF[sPDF$NAME %in% MyFig16$Country.Name,] #select out your countries
# use the bbox to define xlim & ylim
#mapCountryData(sPDF, nameColumnToPlot="articles", xlim=bbox(sPDFmyCountries)[1,], ylim=bbox(sPDFmyCountries)[2,])
# OR BETTER YET: If you wanted just to display the boundaries of the countries you have 
# (i.e. if you had all of the Latin American countries in your data) you could do :
mapCountryData(sPDFmyCountries, nameColumnToPlot="perc.total", catMethod="fixedWidth", 
               colourPalette="heat", borderCol="black",  mapTitle = "% of Articles Published, 1991-2014") #numCats=30

#catMethod: ”pretty”, ”fixedWidth”, ”diverging”,”logFixedWidth”,”quantiles”,”categorical”,
#or a numeric vector defining breaks.

#Adding labels for each country, requirespackage RASTER
# get the coordinates for each country
#country_coord<-data.frame(coordinates(sPDFmyCountries),stringsAsFactors=F)
# label the countries
#text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord))
#############################################################################################################################
#############################################################################################################################
# Mapping total productivity 1991-2014 by each country
MyFig17<-filter(PUBS.COUNT, Region == "LatAm" & Year >1990)  #Reduce dataset to Latin America & 1991 On
MyFig17<-aggregate(Count ~ Country.Name+Country.Code, data = MyFig16, sum)
MyFig17<-arrange(MyFig17, Count)  #Arrange them from low to high - easier to see who is high and low

MyFig17$Country.Name<- as.character(MyFig17$Country.Name)
MyFig17$Country.Name[MyFig17$Country.Name == "El.Salvador"] <- "El Salvador"
MyFig17$Country.Name[MyFig17$Country.Name == "Costa.Rica"] <- "Costa Rica"
MyFig17$Country.Name<- as.factor(MyFig17$Country.Name)

# making the maps: First map to the whole globe, then make the map just Latin America
sPDF <- joinCountryData2Map( MyFig17, joinCode = "ISO3", nameJoinColumn = "Country.Code") 
mapCountryData(sPDF, nameColumnToPlot="Count") #Maps your variable of interest into the map of the world
# How to just plot to LATAM (with HT to http://stackoverflow.com/questions/28838866/mapping-all-of-latin-america-with-rworldmap/28863992#28863992)
sPDFmyCountries <- sPDF[sPDF$NAME %in% MyFig17$Country.Name,] #select out your countries
# use the bbox to define xlim & ylim
#mapCountryData(sPDF, nameColumnToPlot="articles", xlim=bbox(sPDFmyCountries)[1,], ylim=bbox(sPDFmyCountries)[2,])
# OR BETTER YET: If you wanted just to display the boundaries of the countries you have 
# (i.e. if you had all of the Latin American countries in your data) you could do :
mapCountryData(sPDFmyCountries, nameColumnToPlot="Count", catMethod=breaks, 
               colourPalette="heat", borderCol="black",  mapTitle = "Articles Published, 1991-2014") #numCats=30

#catMethod: ”pretty”, ”fixedWidth”, ”diverging”,”logFixedWidth”,”quantiles”,”categorical”,
#or a numeric vector defining breaks.
breaks<-c(0,40,100,200,300,400,500,600,700,800)
breaks<-seq(0, 3500, by = 100)
breaks<-c(seq(0, 1500, by = 100),3500)


###################################################
###################################################
##  BUILDING MULTI-PANEL FIGURES
###################################################
###################################################

###################################################################################################################
#FIGURE 1: REGIONAL comparison on productivity and % increase relative to 1980s
main = textGrob("Figure 1", vjust = 0, gp = gpar(fontface = "bold", fontsize = 20))
Fig1<-grid.arrange(MyFig1, MyFig5, sub=main, ncol=2, nrow=1) 
#FIGURE 2: Publications per country with either LatAm mean or median as a scale
Fig2<-MyFig15
#FIGURE 3: Publications per capita per country (LatAm mean or median as a scale?)
Fig3<-MyFig14
#FIGURE 4: Publications per $ GDP per country (LatAm mean or median as a scale?)
Fig4<-MyFig13











##########################################################################################################################################
# SANDBOX
##########################################################################################################################################








#line chart of %change per year per country
MyFig8a<-qplot(Year, prop.change, data = PUBS.COUNT3, color = Country.Name, geom = "line",
               colour = Country.Name,
               main = "prop change, 1991-2014")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 

#these removes the gray background, dots, and gridlines from the plot
MyFig8a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




# library(maptools)
# library(reshape2)
# 
# library(RecordLinkage)
# library(igraph)
# library(network)
# library(sna)
# library(Hmisc)
# 
library(ggplot2)

# library(rgdal)
#require(refnet)
#library(raster)
#library(colorspace)
# library(RColorBrewer)
# library(xts)
# library(zoo)



####################
# visualizations of individual independent variables
####################
#R&D Box Plot

boxplot(Value~Country.Code,data=SESdata[SESdata$Indicator.Code=="R&D",], main="RD", xlab="Ncountry", ylab="median val")

#Box Plot Education Indices
condition <- c("WBED", "UN_ED")
ED.DATA<-dplyr::filter(SESdata, Data.Source %in% condition)
boxplot(Value~Country.Code,data=ED.DATA[ED.DATA$Data.Source=="UN_ED",], main="UNED", xlab="Ncountry", ylab="median val")
boxplot(Value~Country.Code,data=ED.DATA[ED.DATA$Data.Source=="WBED",], main="UNED", xlab="Ncountry", ylab="median val")



#scatter plot of GDP vs PopSize

POP2<-SESdata[(SESdata$Indicator.Code=="PopSize" | SESdata$Indicator.Code=="GDP") & SESdata$Year>=1991 ,]
POP2<-droplevels(POP2)
POP2<-select(POP2, one_of(c("Country.Name","Indicator.Code", "Value", "Year" )))
POP2<-spread(POP2, Indicator.Code, Value)
POPvGDP<- ggplot(POP2, aes(x=PopSize, y=GDP))
POPvGDP+ geom_point()

qplot(PopSize, GDP, data = POP2) + facet_wrap(~ Country.Name, scales = "free")


####NEED TO REDO THE FIGURES NOW THAT DATA ARE ALL INA  SINGLE FILE!!!!

#some figures using ggplot2


str(Fig2)
#Figure 2: 
Fig2<-filter(PUBS.COUNT, Region == "LatAm" & Year >1990)
Fig2<-aggregate(Count ~ Country.Name+Country.Code, data = Fig2, sum)
Fig2$perc.total<-(Fig2$Count/sum(Fig2$Count))*100
Fig2<-arrange(Fig2, Count)

#sPDF <- getMap()  
#mapCountryData(sPDF, mapRegion='latin america' )

sPDF <- joinCountryData2Map( Fig2, joinCode = "ISO3", nameJoinColumn = "Country.Code") 
#Lat <- c(-55,30) #-20 in first value cuts off antarctica perfectly
#Long<-c(-120,-40)
mapCountryData(sPDF, nameColumnToPlot="perc.total") #, mapRegion='latin america' xlim = Long, ylim = Lat

#How to just plot to LATAM (with HT to http://stackoverflow.com/questions/28838866/mapping-all-of-latin-america-with-rworldmap/28863992#28863992)
#select out your countries
sPDFmyCountries <- sPDF[sPDF$NAME %in% Fig2$Country.Name,]
#use the bbox to define xlim & ylim
#mapCountryData(sPDF, nameColumnToPlot="articles", xlim=bbox(sPDFmyCountries)[1,], ylim=bbox(sPDFmyCountries)[2,])
#OR BETTER YET: If you wanted just to display the boundaries of the countries you have 
#(i.e. if you had all of the Latin American countries in your data) you could do :
mapCountryData(sPDFmyCountries, nameColumnToPlot="perc.total", catMethod="categorical", colourPalette="heat", borderCol="black",  mapTitle = "% of Articles Published, 1991-2014") #numCats=30






#Adding labels for each country, requirespackage RASTER
# get the coordinates for each country
#country_coord<-data.frame(coordinates(sPDFmyCountries),stringsAsFactors=F)
# label the countries
#text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord))




#Fig4b is line chart of GDP per year per country
Fig4b<-ALLDATA[ALLDATA$Indicator.Name=="GDP",]
Fig4b[complete.cases(Fig4b),]
MyFig4b<-qplot(Year, Value, data = Fig4b, color = Country.Name, geom = "line",
               colour = Country.Name,
               main = "GDP, 1991-2014")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 

#these removes the gray background, dots, and gridlines from the plot
MyFig4b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


GDPdf<-ALLDATA[ALLDATA$Indicator.Name=="GDP",]
Pubsdf<-ALLDATA[ALLDATA$Indicator.Name=="Articles",]
GDPdf<-GDPdf[complete.cases(GDPdf),]
Pubsdf<-Pubsdf[complete.cases(Pubsdf),]
pubsgdp<-full_join(GDPdf, Pubsdf, by = "Year")
pubsgdp<-pubsgdp[complete.cases(pubsgdp[,"Country.Name.y"]),] 
plot(pubsgdp$Value.x,pubsgdp$Value.y, variablename=pubsgdp$Country.Code.x)

theme_set(theme_bw())
ggplot(pubsgdp, aes(x = Value.x, y = Value.y, color = Country.Code.x, shape = Country.Code.x)) + 
  geom_point() + 
  stat_smooth(method = 'lm')



cor.test(pubsgdp$Value.x,pubsgdp$Value.y, method="spearman")

hist(pubsgdp$Value.y)
hist(pubsgdp$Value.x)
Fig4c$Year<-as.factor(Fig4c$Year)
spread(Fig4c, Indicator.Name,Value)
Fig4c<-dcast(Fig4c, Fig4c$Indicator.Name~GDP+Articles)

#Figure 5: % change from 1991-2014
#select the data you need
Fig5.1991_1995<-filter(Fig4a, Year =="1991"| Year =="1992" | Year =="1993"| Year =="1994"| Year =="1995")
#Fig5.1991_1995<-select(Fig5.1991_1995, -Country.Code) #Remove column "Country.code"

Fig5.2010_2014<-filter(Fig4a, Year =="2010"| Year =="2011" | Year =="2012"| Year =="2013"| Year =="2014")
#Fig5.2010_2014<-select(Fig5.2010_2014, -Country.Code) #Remove column "Country.code"

#sum the total productivity in the two 5-Year windows of interest using aggregate
Fig5.1991_1995<-aggregate(Value ~ Country.Name+Country.Code, data = Fig5.1991_1995, sum)
Fig5.2010_2014<-aggregate(Value ~ Country.Name+Country.Code, data = Fig5.2010_2014, sum)

#bind the two time frames together and rename the columns
Fig5<-cbind(Fig5.1991_1995,Fig5.2010_2014[3])


#calclulate the % cchange
perc.change<-0
perc.change<-(((Fig5[4]+0)-(Fig5[3]+0))/(Fig5[3]+0))*100

Fig5<-cbind(Fig5,perc.change)
names(Fig5)[3] <- "interval1" #need to rename the column after aggregate/bind
names(Fig5)[4] <- "interval2" #need to rename the column after aggregate/bind
names(Fig5)[5] <- "percent.change" #need to rename the column after aggregate/bind

#Calclulate each countries productivity as % of total LatAm Productivity
perc.tot.int1<-(Fig5[3]/(sum(Fig5[3])))*100
perc.tot.int2<-(Fig5[4]/(sum(Fig5[4])))*100
#bind to dataframe
Fig5<-cbind(Fig5,perc.tot.int1, perc.tot.int2)
names(Fig5)[6] <- "percent.of.productivity.Int1" #need to rename the column after aggregate/bind
names(Fig5)[7] <- "percent.of.productivity.Int2" #need to rename the column after aggregate/bind

Fig5<-arrange(Fig5, percent.change)

##Need to do some reshaping to plot figures you want.

#Fig5.1: Number of Articles: Interval 1 to Interval 2
Fig5.1<-select(Fig5, -percent.change, -percent.of.productivity.Int1, 
               -percent.of.productivity.Int2, -Country.Code)
Fig5.1<-gather(Fig5.1,"interval", "Value", 2:3)
#summary(Fig5.1)
MyFig5.1<-ggplot(data=Fig5.1, aes(x=interval, y=Value, group=Country.Name, colour=Country.Name)) + geom_line() + geom_point()
MyFig5.1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#Fig5.2: Percent of Latam's productivity in Interval 1 vs Interval 2
Fig5.2<-select(Fig5, -percent.change, -interval1, 
               -interval2, -Country.Code)
names(Fig5.2)[2] <- "Interval.1" #need to rename the column 
names(Fig5.2)[3] <- "Interval.2" #need to rename the column 

Fig5.2<-gather(Fig5.2,"interval", "percent.total.productivity", 2:3)

MyFig5.2<-ggplot(data=Fig5.2, aes(x=interval, y=percent.total.productivity, group=Country.Name, colour=Country.Name)) + geom_line() + geom_point()
MyFig5.2 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))










##Play figure 4a with smoothed curves
ggplot(Fig1, aes(x=Year, y = Value, colour = Country.Name))+geom_point(color="firebrick")+stat_smooth()
#need to put data in long form
#Fig1<- dcast(Fig1, Fig1$country ~ Fig1$Year, value.var="pubs")
#names(Fig1)[names(Fig1)=="Fig1$country"] <- "country"
#str(Fig1)
#summary(Fig1)




library(rworldmap)
#get coarse resolution world from rworldmap
sPDF <- getMap()  
#mapCountries using the 'continent' attribute  
mapCountryData(sPDF, nameColumnToPlot='continent')
mapCountryData(sPDF, nameColumnToPlot='REGION')












###########
output <- read_authors(ecuador_references, filename_root="output/ecuador")
ecuador_authors <- output$authors
ecuador_authors__references <- output$authors__references



##	After reading the files in you can check the ecuador_authors.csv file
##		and by hand in Excel, using the AU_ID_Dupe and Similarity fields,
##		merge any author records that represent the same author.  After doing
##		so you can read these back into R using the following, or if you're
##		not starting from scratch above:

###	Can be read back in without importing from the following three commands:
#ecuador_references <- read.csv("output/ecuador_references.csv", as.is=TRUE)
#ecuador_authors <- read.csv("output/ecuador_authors.csv", as.is=TRUE)
#ecuador_authors__references <- read.csv("output/ecuador_authors__references.csv", as.is=TRUE)

##	Process Brazilian records:
brazil_references <- read_references("data/savedrecs (5).ciw", dir=FALSE, filename_root="output/brazil")
output <- read_authors(brazil_references, filename_root="output/brazil")
brazil_authors <- output$authors
brazil_authors__references <- output$authors__references

#brazil_references <- read.csv("output/brazil_references.csv", as.is=TRUE)
#brazil_authors <- read.csv("output/brazil_authors.csv", as.is=TRUE)
#brazil_authors__references <- read.csv("output/brazil_authors__references.csv", as.is=TRUE)

##	Calculate the percentage of author records without contact information:
sum(brazil_authors$C1 == "" | is.na(brazil_authors$C1))/length(brazil_authors$C1)*100


##	Let's remove duplicates from our presumably updated and corrected author lists:

output <- remove_duplicates(authors=ecuador_authors, authors__references=ecuador_authors__references, filename_root="output/ecuador_nodupe")
ecuador_authors <- output$authors
ecuador_authors__references <- output$authors__references

output <- remove_duplicates(authors=brazil_authors, authors__references=brazil_authors__references, filename_root="output/brazil_nodupe")
brazil_authors <- output$authors
brazil_authors__references <- output$authors__references


##	Now let's merge references, authors, and authors__references:
output <- merge_records(
  references=brazil_references, 
  authors=brazil_authors, 
  authors__references=brazil_authors__references, 
  references_merge=ecuador_references, 
  authors_merge=ecuador_authors, 
  authors__references_merge=ecuador_authors__references, 
  filename_root = "output/merged"
)

merged_references <- output$references
merged_authors <- output$authors
merged_authors__references <- output$authors__references

##	And finally after scrolling through and hand-correcting any authors
##		from the merged list that have a high similarity:
#merged_authors <- read.csv("merged_authors.csv", as.is=TRUE)

output <- remove_duplicates(authors=merged_authors, authors__references=merged_authors__references, filename_root="output/merged_nodupe")
merged_authors <- output$authors
merged_authors__references <- output$authors__references



######
##	Sample geographic plotting of author locations based on RP or C1:

##	How to process addresses:
authors_working <- merged_authors

##	Process a single address at a time:
refnet_geocode(data.frame("AU_ID"=authors_working$AU_ID[1], "type"="RP", "address"=authors_working$RP[1], stringsAsFactors=FALSE))
refnet_geocode(data.frame("AU_ID"=authors_working$AU_ID[2], "type"="RP", "address"=authors_working$RP[2], stringsAsFactors=FALSE), verbose=TRUE)

##	Process a group of addresses:
read_addresses(data.frame("AU_ID"=authors_working$AU_ID[1:10], "type"="RP", "address"=authors_working$RP[1:10], stringsAsFactors=FALSE), verbose=TRUE)


##	Sample using the first C1 address listed for the first 1000 authors, 
##		keyed by author so we can join it back:
##	NOTE:  The string "NA" does get translated to a point so we'll remove it
##		before passing it along:
address_list_working <- sapply(strsplit(authors_working$C1[1:1000], "\n"), FUN=function(x) { return(x[1]) })
address_list_working_au_id <- authors_working$AU_ID[1:1000][!is.na(address_list_working)]
address_list_working <- address_list_working[!is.na(address_list_working)]

##	Let's try to strip off any institutional references which may complicate geocoding:
address_list_working <- gsub("^.* (.*,.*,.*)$", "\\1", address_list_working)
address_list_working <- gsub("[. ]*$", "", address_list_working)

##	Use the full list to create addresses from the C1 records:
addresses_working <- read_addresses(data.frame("id"=address_list_working_au_id, "type"="C1", "address"=address_list_working, stringsAsFactors=FALSE), filename_root="output/merged_nodupe_addresses_C1_first1000")
#addresses_working <- read.csv("output/merged_nodupe_addresses_C1_first1000_addresses.csv")


##	Now we can use those addresses to plot things out:
plot_addresses_country(addresses_working)
plot_addresses_points(addresses_working)

##	Uncomment to save as a PDF, and display the semi-transparent edge color:
#pdf("output/merged_nodupe_first1000_linkages_countries.pdf")
net_plot_coauthor(addresses_working, merged_authors__references)
#dev.off()
net_plot_coauthor_country(addresses_working, merged_authors__references)

##	The default plot area doesn't show semitransparent colors, so we'll output to PDF:
output <- net_plot_coauthor_country(addresses_working, merged_authors__references)
ggsave("output/merged_nodupe_first1000_linkages_countries_world_ggplot.pdf", output, h = 9/2, w = 9)


##	We can subset records any way that makes sense.  For example, if we wanted to only use references from 2012 (note that the way records are read in they are strings and have a hard return character):
ref_index <- merged_references$PY == "2012\n"
summary(ref_index)

##	Pull reference IDs (UT field) for just those from 2012:
UT_index <- merged_references$UT[ref_index]
merged_authors__references_subset <- merged_authors__references[ merged_authors__references$UT %in% UT_index, ]

##	Plot the subset for 2012:
net_plot_coauthor_country(addresses_working, merged_authors__references_subset)


##	Compare to 2011:
ref_index <- merged_references$PY == "2011\n"
UT_index <- merged_references$UT[ref_index]
merged_authors__references_subset <- merged_authors__references[ merged_authors__references$UT %in% UT_index, ]

##	Plot the subset for 2011:
net_plot_coauthor_country(addresses_working, merged_authors__references_subset)
