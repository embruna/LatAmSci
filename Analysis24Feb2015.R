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
library(maptools)
library(reshape2)
library(rworldmap)
library(RecordLinkage)
library(igraph)
library(network)
library(sna)
library(Hmisc)
library(refnet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
#require(refnet)
#library(raster)
#library(colorspace)
library(RColorBrewer)

rm(list=ls())

##########################################################################################################################################
##########################################################################################################################################
#######   QS UNIVERSITY RANKINGS DATA. These data are for all countries x years
#######   Enters data, Summarizes it, and appends 3 leter Country Codes
##########################################################################################################################################
##########################################################################################################################################

#Read in the QS Rankings Data
setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/SocioEconomic Data/UniversityRankings") #These data are in a different Folder
UNIRANK<-read.csv("QS.csv", dec=".", header = TRUE, sep = ",", na.strings='NULL', check.names=FALSE)
titles<-UNIRANK$category[1:19]

#Convert it from a single column dataframe to a 19x100 dataframe and add column headings
mat<- matrix(UNIRANK$data, ncol = 19, byrow = T)
UNIRANK<- as.data.frame(mat, stringsAsFactors = T)
names(UNIRANK)<-titles

#Add a column with the 3 letter country codes to be consistent with the other datasets
UNIRANK$Country.Code[UNIRANK$Country.Territory == "AR"]  <- "ARG"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "Bolivia"]  <- "BOL"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "BR"]  <- "BRA"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "CL"]  <- "CHL"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "CO"]  <- "COL"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "CR"]  <-"CRI"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "CU"]  <- "CUB"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "EC"]  <-"ECU"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "El Salvador"]  <-"SLV"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "Guatemala"]  <-"GTM"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "Honduras"]  <-"HND"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "MX"]  <-"MEX"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "Nicaragua"]  <-"NIC"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "Panama"]  <-"PAN"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "Paraguay"]  <-"PRY"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "PR"]  <-"PER"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "UY"]  <-"URY"
UNIRANK$Country.Code[UNIRANK$Country.Territory == "VE"]  <-"VEN"
UNIRANK$Country.Code<-as.factor(UNIRANK$Country.Code)
#COnvert the ranking in 2012 in case you need to take avg. per country, etc.
UNIRANK[1]<-as.character(UNIRANK$rank.2012)
UNIRANK[1]<-as.numeric(UNIRANK$rank.2012)

#Summary of how many of the top 200 university are in each country
UNIRANK.table<-as.data.frame(table(UNIRANK$Country.Code))
#Add the countries with NO universities in top 100
countries.no.u<-as.data.frame(c("BOL", "SLV","GTM","HND", "NIC", "PAN", "PRY"))
count.no.u<-as.data.frame(c("0","0","0","0", "0", "0", "0"))
no.u<-cbind(countries.no.u,count.no.u)
names(no.u)<-c("Var1","Freq")
UNIRANK.table<-rbind(UNIRANK.table, no.u)

#add columns needed to match other SES data to bind below
UNIRANK.table$Year<-as.factor("2012")
UNIRANK.table$Indicator.Code<-as.factor("NumberOfUnivTop100")
names(UNIRANK.table)[1] <- "Country.Code"
names(UNIRANK.table)[2] <- "Value"
UNIRANK.table$Value<-as.numeric(UNIRANK.table$Value)

#Put in the format to bind all the datasets together.
UNIRANK.table$Country.Name<-NA
UNIRANK.table$Data.Source<-as.factor("QC")
UNIRANK.table$Indicator.Name<-as.factor("TopUnis")
#Change the values of the 
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "ARG"]  <-"Argentina"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "BOL"]  <- "Bolivia"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "BRA"]  <- "Brazil"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "CHL"]  <- "Chile"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "COL"]  <-"Colombia"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "CRI"]  <-"Costa.Rica"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "CUB"]  <- "Cuba"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "ECU"]  <-"Ecuador"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "SLV"]  <-"El.Salvador"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "GTM"]  <-"Guatemala"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "HND"]  <-"Honduras"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "MEX"]  <-"Mexico"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "NIC"]  <-"Nicaragua"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "PAN"]  <-"Panama"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "PRY"]  <-"Paraguay"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "PER"]  <-"Peru"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "URY"]  <-"Uruguay"
UNIRANK.table$Country.Name[UNIRANK.table$Country.Code == "VEN"]  <-"Venezuela"
UNIRANK.table$Country.Name<-as.factor(UNIRANK.table$Country.Name)

#reorder the columns to bind
UNIRANK.table<- UNIRANK.table[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","Value")] 
str(UNIRANK.table)
#summary(UNIRANK.table)
#levels(UNIRANK.table$Country.Name)



##########################################################################################################################################
##########################################################################################################################################
#######  WORLD BANK GDP DATA. In World Bank data each country has a code. We import the entire dataset, then
#######  select just countries of in our study:ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN
##########################################################################################################################################
##########################################################################################################################################


#Importing the GDP Data. These data are for all countries x years, so will need to select just the years of interest and the countries in the analyses
setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/SocioEconomic Data/GDP_WorldBank_Global") #These data are in a different Folder
GDP<-read.csv("GDP.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE)
GDP<-GDP[GDP$Country.Code=="ARG" | GDP$Country.Code=="BOL"| GDP$Country.Code=="BRA" | GDP$Country.Code=="CHL" | GDP$Country.Code=="COL"
         | GDP$Country.Code=="CRI" | GDP$Country.Code=="CUB" | GDP$Country.Code=="ECU" | GDP$Country.Code=="SLV" | GDP$Country.Code=="GTM" 
         | GDP$Country.Code=="HND" | GDP$Country.Code=="MEX" | GDP$Country.Code=="NIC" | GDP$Country.Code=="PAN" | GDP$Country.Code=="PRY" 
         | GDP$Country.Code=="PER" | GDP$Country.Code=="URY" | GDP$Country.Code=="VEN",]
#clean up name of variable of interest
GDP$Indicator.Name<-as.factor("GDP")
#Add columns with data source
GDP$Data.Source<-as.factor("WB")
GDP$Country.Name<-gsub("Venezuela,.RB", "Venezuela", GDP$Country.Name)
GDP$Country.Name<-gsub("El Salvador", "El.Salvador", GDP$Country.Name)
GDP$Country.Name<-gsub("Costa Rica", "Costa.Rica", GDP$Country.Name)
GDP$Country.Name<-as.factor(GDP$Country.Name)

#Convert to Long Form
GDP<-gather(GDP, "Year", "Value", 5:59)
GDP$Data.Source<-as.factor("WB")
names(GDP)[3] <- "Indicator.Code" 
names(GDP)[4] <- "Indicator.Name" 
GDP$Year<-gsub("YR", "", GDP$Year) #replacing YR with year
GDP$Year<-as.factor(GDP$Year) #setting back as factor
GDP<-droplevels(GDP)

#reorder the columns to bind
GDP<- GDP[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","Value")] #head(GDP)
#summary(GDP)
str(GDP)
#levels(GDP$Country.Name)



##########################################################################################################################################
##########################################################################################################################################
#######  COUNTRY TOTAL POPULATION SIZE DATA. Data are for all countries and years, so will need to select just the years of interest 
#######  select just countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN
##########################################################################################################################################
##########################################################################################################################################


#Importing the PopSize Data. #These data are in a different Folder
setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/SocioEconomic Data/worldBank_PopSize") 
PopSize<-read.csv("PopSize.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE)

PopSize<-PopSize[PopSize$Country.Code=="ARG" | PopSize$Country.Code=="BOL"| PopSize$Country.Code=="BRA" | PopSize$Country.Code=="CHL" | PopSize$Country.Code=="COL"
                 | PopSize$Country.Code=="CRI" | PopSize$Country.Code=="CUB" | PopSize$Country.Code=="ECU" | PopSize$Country.Code=="SLV" | PopSize$Country.Code=="GTM" 
                 | PopSize$Country.Code=="HND" | PopSize$Country.Code=="MEX" | PopSize$Country.Code=="NIC" | PopSize$Country.Code=="PAN" | PopSize$Country.Code=="PRY" 
                 | PopSize$Country.Code=="PER" | PopSize$Country.Code=="URY" | PopSize$Country.Code=="VEN",]
PopSize<-PopSize[, -(5:30)]
PopSize$Data.Source<-as.factor("WB")
PopSize$Indicator.Name<-as.factor("Pop. Size")
PopSize$Country.Name<-gsub("Venezuela,.RB", "Venezuela", PopSize$Country.Name)
PopSize$Country.Name<-gsub("El Salvador", "El.Salvador", PopSize$Country.Name)
PopSize$Country.Name<-gsub("Costa Rica", "Costa.Rica", PopSize$Country.Name)
PopSize$Country.Name<-as.factor(PopSize$Country.Name)
summary(PopSize)

#Convert to Long Form
PopSize<-gather(PopSize, "Year", "Value", 5:33)
PopSize$Data.Source<-as.factor("WB")
names(PopSize)[3] <- "Indicator.Code" 
names(PopSize)[4] <- "Indicator.Name" 
PopSize$Year<-gsub("YR", "", PopSize$Year) #replacing YR with year
PopSize$Year<-as.factor(PopSize$Year) #setting back as factor
PopSize<-droplevels(PopSize)
str(PopSize)
#reorder the columns to bind
PopSize<- PopSize[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","Value")] #head(PopSize)
#summary(PopSize)
str(PopSize)



##########################################################################################################################################
##########################################################################################################################################
#######  UNESCO R&D DATA. Data are for all countries and years, so will need to select just the years of interest 
#######  select just countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN
##########################################################################################################################################
##########################################################################################################################################

#Importing the Investment in R&D Data. 
setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/SocioEconomic Data/R&D (% of GDP)")
RD<-read.csv("R&D.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE)
RD<-RD[RD$Country.Code=="ARG" | RD$Country.Code=="BOL"| RD$Country.Code=="BRA" | RD$Country.Code=="CHL" | RD$Country.Code=="COL"
         | RD$Country.Code=="CRI" | RD$Country.Code=="CUB" | RD$Country.Code=="ECU" | RD$Country.Code=="SLV" | RD$Country.Code=="GTM" 
         | RD$Country.Code=="HND" | RD$Country.Code=="MEX" | RD$Country.Code=="NIC" | RD$Country.Code=="PAN" | RD$Country.Code=="PRY" 
         | RD$Country.Code=="PER" | RD$Country.Code=="URY" | RD$Country.Code=="VEN",]


RD$Data.Source<-as.factor("UNESCO")
RD$Indicator.Name<-as.factor("R&D")
RD$Country.Name<-gsub("Venezuela,.RB", "Venezuela", RD$Country.Name)
RD$Country.Name<-gsub("El Salvador", "El.Salvador", RD$Country.Name)
RD$Country.Name<-gsub("Costa Rica", "Costa.Rica", RD$Country.Name)
RD$Country.Name<-as.factor(RD$Country.Name)

#Convert to Long Form
RD<-gather(RD, "Year", "Value", 5:59)
RD$Data.Source<-as.factor("WB")
names(RD)[3] <- "Indicator.Code" 
names(RD)[4] <- "Indicator.Name" 
RD$Year<-gsub("YR", "", RD$Year) #replacing YR with year
RD$Year<-as.factor(RD$Year) #setting back as factor
RD<-droplevels(RD)
str(RD)
#reorder the columns to bind
RD<- RD[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","Value")] #head(RD)
#summary(RD)
summary(RD)
str(RD)

##########################################################################################################################################
##########################################################################################################################################
#######   WORLD BANK EDUCATION DATA. These data are for all countries x years
#######   Need to select just the years of interest and the countries in the analyses
##########################################################################################################################################
##########################################################################################################################################

setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/SocioEconomic Data/WorldBank_ED")
WBED<-read.csv("WorldBankEdData.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE)
WBED<-WBED[WBED$Country.Code=="ARG" | WBED$Country.Code=="BOL"| WBED$Country.Code=="BRA" | WBED$Country.Code=="CHL" | WBED$Country.Code=="COL"
       | WBED$Country.Code=="CRI" | WBED$Country.Code=="CUB" | WBED$Country.Code=="ECU" | WBED$Country.Code=="SLV" | WBED$Country.Code=="GTM" 
       | WBED$Country.Code=="HND" | WBED$Country.Code=="MEX" | WBED$Country.Code=="NIC" | WBED$Country.Code=="PAN" | WBED$Country.Code=="PRY" 
       | WBED$Country.Code=="PER" | WBED$Country.Code=="URY" | WBED$Country.Code=="VEN",]
#Reduces it down to juyst the countries for which you have a assigned a code.
WBED<-WBED[complete.cases(WBED[,"Country.Code"]),]
#Correct the names of 2 of the countries
WBED$Country.Name<-gsub("Venezuela,.RB", "Venezuela", WBED$Country.Name)
WBED$Country.Name<-gsub("El Salvador", "El.Salvador", WBED$Country.Name)
WBED$Country.Name<-gsub("Costa Rica", "Costa.Rica", WBED$Country.Name)
WBED$Country.Name<-as.factor(WBED$Country.Name)


########Convert to Long
WBED<-gather(WBED, "Year", "Value", 5:47)
WBED$Data.Source<-as.factor("WBED")
names(WBED)[3] <- "Indicator.Code" 
names(WBED)[4] <- "Indicator.Name" 
WBED$Year<-gsub("YR", "", WBED$Year) #replacing YR with year
WBED$Year<-as.factor(WBED$Year) #setting back as factor
WBED<-droplevels(WBED)

#reorder the columns to bind
WBED<- WBED[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","Value")] #head(WBED)
summary(WBED)
str(WBED)

##########################################################################################################################################
##########################################################################################################################################
#######   UN EDUCATION DATA. These data are for all countries x years
#######   Need to select just the years of interest and the countries in the analyses
##########################################################################################################################################
##########################################################################################################################################

#Importing the UNDP ED Data. 
setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/SocioEconomic Data/UNDP_ED")
UNED<-read.csv("UNDP_EdIndex.csv", dec=".", header = TRUE, sep = ",", na.strings='NULL', check.names=FALSE)

#Correct name of two of the countries
UNED$Country<-gsub(" ", ".", UNED$Country)
UNED$Country[UNED$Country == "Venezuela.(Bolivarian.Republic.of)"] <- "Venezuela"
UNED$Country[UNED$Country == "Bolivia.(Plurinational.State.of)"] <- "Bolivia"
UNED$Country<-as.factor(UNED$Country)

UNED$Country.Code<- NA
UNED$Country.Code[UNED$Country == "Argentina"]  <- "ARG"
UNED$Country.Code[UNED$Country == "Bolivia"]  <- "BOL"
UNED$Country.Code[UNED$Country == "Brazil"]  <- "BRA"
UNED$Country.Code[UNED$Country == "Chile"]  <- "CHL"
UNED$Country.Code[UNED$Country == "Costa.Rica"]  <-"CRI"
UNED$Country.Code[UNED$Country == "Colombia"]  <- "COL"
UNED$Country.Code[UNED$Country == "Cuba"]  <- "CUB"
UNED$Country.Code[UNED$Country == "Ecuador"]  <-"ECU"
UNED$Country.Code[UNED$Country == "El.Salvador"]  <-"SLV"
UNED$Country.Code[UNED$Country == "Guatemala"]  <-"GTM"
UNED$Country.Code[UNED$Country == "Honduras"]  <-"HND"
UNED$Country.Code[UNED$Country == "Mexico"]  <-"MEX"
UNED$Country.Code[UNED$Country == "Nicaragua"]  <-"NIC"
UNED$Country.Code[UNED$Country == "Panama"]  <-"PAN"
UNED$Country.Code[UNED$Country == "Paraguay"]  <-"PRY"
UNED$Country.Code[UNED$Country == "Peru"]  <-"PER"
UNED$Country.Code[UNED$Country == "Uruguay"]  <-"URY"
UNED$Country.Code[UNED$Country == "Venezuela"]  <-"VEN"
UNED<-UNED[complete.cases(UNED[,"Country.Code"]),] #Reduces it down to juyst the countries for which you have a assigned a code.
UNED$Country.Code<-as.factor(UNED$Country.Code) #they were being converted as characters, so convert them to factors


#CONVERT TO LONG
UNED$HDI.Rank<- NULL #delete the column with HDI rank - this is for only one year, so best left to getting it elsewhere
UNED<-gather(UNED, "Year", "Value", 2:15)
UNED$Year<-gsub("YR", "", UNED$Year) #deleting YR
UNED$Year<-as.factor(UNED$Year) #setting back as factor
UNED$Data.Source<-as.factor("UN_ED")
UNED$Indicator.Name<-as.factor("UNED")
UNED$Indicator.Code<-as.factor("UN.ED.Index")
names(UNED)[1] <- "Country.Name" 
UNED<-droplevels(UNED)
UNED<- UNED[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","value")] #reorder the columns to bind
#head(UNED)
#summary(UNED)
str(UNED)


##########################################################################################################################################
##########################################################################################################################################
####  Now subset and bind all the SES / ED/ PopSize Data Together
##########################################################################################################################################
##########################################################################################################################################

####HERE YOU CAN DECIDE WHAT WORLD BANK DATA TO USE. 
###I STARTED BY TOGGLING ON Public.expenditure.on.education.as.%.of.GDP

WBED<-filter(WBED, Indicator.Name == "SE.XPD.TOTL.GD.ZS")

#The $ invested in R&D, GDP and PopSize data are the same dimensions and all in wide form
#start by rowbinding them then converitng to long form
SESdata<-rbind(PopSize,GDP,RD, WBED, UNED, UNIRANK.table)
str(SESdata)
droplevels(SESdata)
summary(SESdata)

####################
####################
###THIS SECTION IMPORTS PUBLICATIONS DATA from 2001-2014. 
###THE DATA HAVE TO BE IN THE REFNET FOLDER, SO THE WORKING DIRECTORY IS CHANGED BELOW
####################
####################

setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/refnet")

##	Let's read in single files, though we can specify a directory and 
##		set the dir=TRUE flag and read in an entire directory of files.
##		If the filename_root argument is not "" then it is used to create
##		the root filenames for CSV output:
ecuador_references <- read_references("data/Ecuador_2001-2014.txt", dir=FALSE, filename_root="output/ecuador")
bolivia_references <- read_references("data/Bolivia_2001-2014.txt", dir=FALSE, filename_root="output/bolivia")
argentina_references <- read_references("data/Argentina_2001-2014.txt", dir=FALSE, filename_root="output/argentina")
brazil_references <- read_references("data/Brazil_2001-2014.txt", dir=FALSE, filename_root="output/brazil")
chile_references <- read_references("data/Chile_2001-2014.txt", dir=FALSE, filename_root="output/chile")
colombia_references <- read_references("data/Colombia_2001-2014.txt", dir=FALSE, filename_root="output/colombia")
costarica_references <- read_references("data/Costa Rica_2001-2014.txt", dir=FALSE, filename_root="output/costarica")
cuba_references <- read_references("data/Cuba_2001-2014.txt", dir=FALSE, filename_root="output/cuba")
elsalvador_references <- read_references("data/El Salvador_2001-2014.txt", dir=FALSE, filename_root="output/elsalvador")
guatemala_references <- read_references("data/Guatemala_2001-2014.txt", dir=FALSE, filename_root="output/guatemala")
honduras_references <- read_references("data/Honduras_2001-2014.txt", dir=FALSE, filename_root="output/honduras")
mexico_references <- read_references("data/Mexico_2001-2014.txt", dir=FALSE, filename_root="output/mexico")
nicaragua_references <- read_references("data/Nicaragua_2001-2014.txt", dir=FALSE, filename_root="output/nicaragua")
panama_references <- read_references("data/Panama_2001-2014.txt", dir=FALSE, filename_root="output/panama")
paraguay_references <- read_references("data/Paraguay_2001-2014.txt", dir=FALSE, filename_root="output/paraguay")
peru_references <- read_references("data/Peru_2001-2014.txt", dir=FALSE, filename_root="output/peru")
uruguay_references <- read_references("data/Uruguay_2001-2014.txt", dir=FALSE, filename_root="output/uruguay")
venezuela_references <- read_references("data/Venezuela_2001-2014.txt", dir=FALSE, filename_root="output/venezuela")

pubs<-rbind(ecuador_references, bolivia_references, argentina_references, brazil_references, chile_references, 
                  colombia_references, costarica_references,cuba_references, elsalvador_references,guatemala_references, 
                  honduras_references, mexico_references, nicaragua_references,panama_references, paraguay_references, 
                  peru_references, uruguay_references, venezuela_references)


#Extract only thro country and year of each publication to plot them over time
output_by_year<-pubs[,c("filename","PY")]
#Change the names of the columns
colnames(output_by_year) <- c("country", "year")

#need to clean up this dataframe
#remove the extraneous characters ("\n" in column 'country' and "data/", "_2001-2014.txt"in column filename 'year')
wordstoremove <- c("\n", "data/", "_2001-2014.txt")

output_by_year <- as.data.frame(sapply(output_by_year, function(x) 
  gsub(paste(wordstoremove, collapse = '|'), '', x)))


#Now start doing so me tabulating and joining with data from previous ytears
#table of the number of papers by each country in each year (2001-2014)
yearly_prod<-table(output_by_year$country, output_by_year$year)
yearly_prod<-as.data.frame(yearly_prod, stringsAsFactors=TRUE)
colnames(yearly_prod) <- c("country", "year", "articles")


####################
####################
###THIS SECTION IMPORTS PUBLICATIONS DATA from 1991-2000. 
###THE DATA ARE **NOT** in REFNET FOLDER, SO THE WORKING DIRECTORY IS CHANGED BELOW THEN CHANGED BACK
####################
####################

setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience") #These data are in a different Folder
early_data<-read.csv("productivity_data_1991-2000.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE)
setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/refnet") #Go back to original WD
as.data.frame(early_data)

####################
####################
###Now Bind the 1991-2000 and 2001-2014 DATA together
####################
####################

pubs<-rbind(early_data, yearly_prod)
pubs$year<-as.numeric(pubs$year)
pubs<-pubs[order(pubs$country, pubs$year),] 

#Add a column with the World Bank
pubs$Country.Code<- NA

pubs$Country.Code[pubs$country == "Argentina"]  <-"ARG"
pubs$Country.Code[pubs$country == "Bolivia"]  <- "BOL"
pubs$Country.Code[pubs$country == "Brazil"]  <- "BRA"
pubs$Country.Code[pubs$country == "Chile"]  <- "CHL"
pubs$Country.Code[pubs$country == "Colombia"]  <-"COL"
pubs$Country.Code[pubs$country == "Costa Rica"]  <-"CRI"
pubs$Country.Code[pubs$country == "Cuba"]  <- "CUB"
pubs$Country.Code[pubs$country == "Ecuador"]  <-"ECU"
pubs$Country.Code[pubs$country == "El Salvador"]  <-"SLV"
pubs$Country.Code[pubs$country == "Guatemala"]  <-"GTM"
pubs$Country.Code[pubs$country == "Honduras"]  <-"HND"
pubs$Country.Code[pubs$country == "Mexico"]  <-"MEX"
pubs$Country.Code[pubs$country == "Nicaragua"]  <-"NIC"
pubs$Country.Code[pubs$country == "Panama"]  <-"PAN"
pubs$Country.Code[pubs$country == "Paraguay"]  <-"PRY"
pubs$Country.Code[pubs$country == "Peru"]  <-"PER"
pubs$Country.Code[pubs$country == "Uruguay"]  <-"URY"
pubs$Country.Code[pubs$country == "Venezuela"]  <-"VEN"
pubs$Country.Code<-as.factor(pubs$Country.Code)
pubs$Data.Source<-as.factor("EB&WH")
summary(pubs)

#Cleanup and sorting to merge the publications and the 3 indicators dataframes
#head(pubs)
#summary(pubs)
#head(SESdata)
#summary(SESdata)
#str(SESdata)
#str(pubs)

#Add columns and rename to match SESData
names(pubs)[1:3] <- c("Country.Name", "Year", "Value")  #need to rename the columns
pubs$Indicator.Code<-as.factor("PUBS.TOTL")                        #add a few new columns
pubs$Indicator.Name<-as.factor("Articles")                         #add a few new columns

names(UNED)[1] <- "Country.Name"                             #need to rename the columns
UNED$Indicator.Code<-as.factor("UN.ED")                                 #add a few new columns
UNED$Indicator.Name<-as.factor("UN Education Index")                    #add a few new columns

names(WBED)[3] <- "Indicator.Name"                             #need to rename the columns
names(WBED)[4] <- "Indicator.Code"                             #need to rename the columns

#Coonvert year to a number (it was previously a factor)  #note can't just use as.numeric
#the reason is here http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
SESdata$Year<-as.numeric(levels(SESdata$Year))[SESdata$Year] 
UNED$Year<-as.numeric(levels(UNED$Year))[UNED$Year] 
WBED$Year<-as.numeric(levels(WBED$Year))[WBED$Year] 

#Sort it first within a frame by rows....
SESdata<-SESdata[order(SESdata$Country.Name, SESdata$Year),]
pubs<-pubs[order(pubs$Country.Name, pubs$Year),]
UNED<-UNED[order(UNED$Country.Name, UNED$Year),]
WBED<-WBED[order(WBED$Country.Name, WBED$Year),]
#....and then sort by coloumn names
SESdata<-SESdata[,order(names(SESdata))]
pubs<-pubs[,order(names(pubs))]
UNED<-UNED[,order(names(UNED))]
WBED<-WBED[,order(names(WBED))]

##The following were are here to help with 2x data cleanup cleanup
#head(pubs)
#head(SESdata)
#head(UNED)
#head(WBED)
#str(WBED)
#str(SESdata)
#str(pubs)
#str(UNED)


####################
####################
###Now Bind pubs data to SES data
####################
####################

ALLDATA<-rbind(pubs,SESdata)
str(ALLDATA)
summary(ALLDATA)

####################
####################
###Now Choose whatyears you want for the data
####################
####################
ALLDATA<-filter(ALLDATA, Year >= 1986)

ALLDATA<-droplevels(ALLDATA)
summary(ALLDATA)
str(ALLDATA)

#mytable <- xtabs(~Country.Code+Indicator.Code+Year, data=ALLDATA)
#ftable(mytable) # print table
#FOO<-filter(ALLDATA, Indicator.Name == "------")

###################
###################
##Now some analyses and figures
##
###################
###################

####NEED TO REDO THE FIGURES NOW THAT DATA ARE ALL INA  SINGLE FILE!!!!

#some figures using ggplot2

#FIGURE 1 TOTAL PRODUCTIVITY BY YEAR
Fig1<-ALLDATA[ALLDATA$Indicator.Name=="Articles",]
Fig1[complete.cases(Fig1),]
Fig1<-as.data.frame(tapply(Fig1$Value, Fig1$Year, sum))
names(Fig1)[1] <- "articles" #need to rename the column after tapply
Fig1$year<-c(1991:2014)
MyFig1<-qplot(year,articles, data = Fig1, geom="line", main = "Articles with Latin American Authors/Co-Authors, 1991-2014")
MyFig1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



#Figure 2: 
Fig2<-ALLDATA[ALLDATA$Indicator.Name=="Articles",]
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
Fig4a<-ALLDATA[ALLDATA$Indicator.Name=="Articles",]
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
