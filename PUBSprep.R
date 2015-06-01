# This function imports and cleans up  Data Collected from the Web of Science on Publications per year for different countries. As with the others selects only countries of interest
# select just countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN

PUBSprep <- function(x) {

  
#Add a column with the country code
PUBSdata$Country.Code[PUBSdata$Country.Name == "Argentina"]  <-"ARG"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Bolivia"]  <- "BOL"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Brazil"]  <- "BRA"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Chile"]  <- "CHL"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Colombia"]  <-"COL"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Costa.Rica"]  <-"CRI"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Cuba"]  <- "CUB"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Ecuador"]  <-"ECU"
PUBSdata$Country.Code[PUBSdata$Country.Name == "El.Salvador"]  <-"SLV"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Guatemala"]  <-"GTM"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Honduras"]  <-"HND"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Mexico"]  <-"MEX"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Nicaragua"]  <-"NIC"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Panama"]  <-"PAN"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Paraguay"]  <-"PRY"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Peru"]  <-"PER"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Uruguay"]  <-"URY"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Venezuela"]  <-"VEN"
PUBSdata$Country.Code[PUBSdata$Country.Name == "United.States"]  <-"USA"
PUBSdata$Country.Code[PUBSdata$Country.Name == "Canada"]  <-"CAN"

#add columns with an indicator Name and source of the data
PUBSdata$Indicator.Name<-"Publications"
PUBSdata$Data.Source<-"WOS"
#convert some of the newly added columns to class 'factor'
PUBSdata$Data.Source<-as.factor(PUBSdata$Data.Source)
PUBSdata$Indicator.Name<-as.factor(PUBSdata$Indicator.Name)
PUBSdata$Country.Code<-as.factor(PUBSdata$Country.Code)
#Convert year and number of publications to numbers
#Coonvert year to a number (it was previously a factor)  #note can't just use as.numeric
#the reason is here http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
PUBSdata$Value<-as.numeric(PUBSdata$Value)
PUBSdata$Year<-as.numeric(PUBSdata$Year)
#reorder the columns (this will make them match up with SESData later)
PUBSdata<-PUBSdata[,c(1,5,6,2,7,4,3)]
# str(PUBSdata)
# summary(PUBSdata)
#Sort it first within a frame by rows to make it easier to read
PUBSdata<-PUBSdata[order(PUBSdata$Country.Name, PUBSdata$Year),]
}
