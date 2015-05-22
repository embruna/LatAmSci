# This function takes the UNIRANK dataframe - a list of the top 100 universitiesin Latin America- and summarizes
# how many of the top 100 university are in each country

UniRankSummary <- function(x) {

UNIRANK.table<-as.data.frame(table(UNIRANK$Country.Code))
#Several countries don't have any universities in top 100 - add them to the dataset using the standard three digit codes
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

}