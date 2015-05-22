#####TO DO LIST: DATA ENTRY AND MANIPULATION
  #Figure out if need HDI. If so, need to reinsert it somewhere
#####Analyses and Figures
#COMPLETE: Figure1: Total Productivity, all countries combined   
#COMPLETEE Figure2: Map shaded by total pubs over period 1991-2014
#Figure3: Map shaded by % of productivity over period 1991-2014
#COMPLETE: Figure4a: line chart of # papers by year for each country  
#Figure4b: alternative: line chart of % of total LATAM producvitivy by year for each country  
#Figure5: Map shaded by total % change in publications over period 1991-2014. Because there is some interannual variability, 
    #used the sum of articles 1991-1995 and 2010-2014 and calclulated as Relative Growth Rate
#Figure6 and Analyses: Line X = pop size, Y = Papers  QUESTION: do this by year? certain year? average of years?
#Figure7 and Analyses: pubs per capita QUESTION: do this by year? certain year? average of years?
#Figure8 and Analyses: pubs per $gdp previous year  QUESTION: do this by year? certain year? average of years?
#Figure9: pubs per education index  QUESTION: do this by year? certain year? average of years?

#R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN: 
#install.packages("refnet_0.6.tar.gz", repos=NULL, type="source")
#setwd("/Users/emiliobruna/Desktop/LATAM Data Updates")
#setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/refnet")
##  Set this to wherever you unzipped the archive folders (not /src):

#detach(package:refnet, unload=TRUE)
#remove.packages("refnet")
# library(maptools)
# library(reshape2)
# library(rworldmap)
# library(RecordLinkage)
# library(igraph)
# library(network)
# library(sna)
# library(Hmisc)
# library(refnet)
# library(ggplot2)
library(dplyr)
library(tidyr)
# library(rgdal)
#require(refnet)
#library(raster)
#library(colorspace)
# library(RColorBrewer)
# library(xts)
# library(zoo)

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


#Load the datasets
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

##########################################################################################################################################
# Uses function QSprep to read QS UNIVERSITY RANKINGS DATA, organize it to match the other datassets, and return a dataframe
#   of the top 100 Universities in Latin America in 20012 based on the QS University Rankings
UNIRANK<-QSprep(UNIRANK)
##########################################################################################################################################

##########################################################################################################################################
#Use function UniRankSummary to produce a summary table of how manu institutions are in each country
UNIRANK.table<-UniRankSummary(UNIRANK)
##########################################################################################################################################

##########################################################################################################################################
# Use function GDPprep to clean and standardize the WORLD BANK GDP DATA. 
# In World Bank data each country has a code. We import the entire dataset, then
# select just countries of in our study:ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN CAN USA
GDP<-GDPprep(GDPdata)  
# Note that while we have GDP data for the entire world, the function only selects the data for the countries of 
# interest.If you want to add or delete countries you need to do it from inside the function.  It would probably 
# be a good idea to change this main code later so that country slection is made here
##########################################################################################################################################

##########################################################################################################################################
#Use function PopSizeprep to clean and standardize the World Bank Population Total Size Data. As above it selects just
#countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN
PopSize<-PopSizeprep(PopSizeData)  
##########################################################################################################################################

##########################################################################################################################################
#Use function RDprep to clean and standardize the % GD Devoted to RD Data from UNESCO. As above it selects just
#countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN
RD<-RDprep(RDData)  
##########################################################################################################################################

##########################################################################################################################################
#Use function WBEDprep to clean and standardize the WorldBank Education Data. As above it selects just
#countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN
WBED<-WBEDprep(WBEDdata)  
##########################################################################################################################################

##########################################################################################################################################
#Use function UNEDprep to clean and standardize the UN Education Data. As above it selects just
#countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN
UNED<-UNEDprep(UNEDdata)  
##########################################################################################################################################

##########################################################################################################################################
#Use the function PUBSprep clean and standardize the Publication Data. These data were not from REFNET, 
#They were collected with searches of WOS Key words
PUBS<-PUBSprep(PUBSdata)  
##########################################################################################################################################

##########################################################################################################################################
# THis section is to see the structure of the different datasets and can be commented out.
##########################################################################################################################################

str(UNED)
str(WBED)
str(GDP)
str(PopSize)
str(RD)
str(PUBS)

####################
####################
### To Choose what data and years you want for the data to be analyzed
####################
####################
#For example
GDP<-filter(GDP, Year >= 1990)
GDP<-droplevels(GDP)
summary(GDP)
str(GDP)


######################################
######################################
##  Now some analyses and figures - THese will need to be revised since I know have seperate datasets instead of one giant
######################################
######################################
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


#line chart of popsize per year per country

POP<-SESdata[SESdata$Indicator.Code=="PopSize" & SESdata$Year>=1991 ,]
POP[complete.cases(POP),]
POPFig<-qplot(Year, Value, data = POP, color = Country.Name, geom = "line",
               colour = Country.Name,
               main = "Population Size")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 

#these removes the gray background, dots, and gridlines from the plot
POPFig + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

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

#FIGURE 1 TOTAL PRODUCTIVITY BY YEAR
Fig1<-ALLDATA[ALLDATA$Indicator.Name=="Publications",]
Fig1[complete.cases(Fig1),]
Fig1<-as.data.frame(tapply(Fig1$Value, Fig1$Year, sum))
names(Fig1)[1] <- "Publications" #need to rename the column after tapply
Fig1$year<-c(1991:2014)
MyFig1<-qplot(year,Publications, data = Fig1, geom="line", main = "Articles with Latin American Authors/Co-Authors, 1991-2014")
MyFig1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



#Figure 2: 
Fig2<-ALLDATA[ALLDATA$Indicator.Name=="Publications" ,]
Fig2[complete.cases(Fig2),]
Fig2<-aggregate(Value ~ Country.Name+Country.Code, data = Fig2, sum)
perc.total<-(Fig2$Value/sum(Fig2$Value))*100
Fig2<-cbind(Fig2,perc.total)
Fig2<-arrange(Fig2, Value)

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





#Fig4a is line chart of productivity per year per country
Fig4a<-ALLDATA[ALLDATA$Indicator.Name=="Publications",]
Fig4a[complete.cases(Fig4a),]
MyFig4a<-qplot(Year, Value, data = Fig4a, color = Country.Name, geom = "line",
      colour = Country.Name,
      main = "Articles Produced Annually, 1991-2014")
#This changes the color scheme of the lines
#MyFig4a<-MyFig4a + scale_color_brewer(palette = "Paired") 

#these removes the gray background, dots, and gridlines from the plot
MyFig4a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



















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
