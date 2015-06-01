# This function imports and cleans up World Bank Data on POPULATION SIZE, selects TOTAL POPULATION SIZE, and cleans/reshapes data. 
# As with the others selects only countries of interesy
# select just countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN
# 

PopSizeprep <- function(x) {
  
PopSizeData<-PopSizeData[PopSizeData$Country.Code=="ARG" | PopSizeData$Country.Code=="BOL"| PopSizeData$Country.Code=="BRA" | PopSizeData$Country.Code=="CHL" | PopSizeData$Country.Code=="COL"
                 | PopSizeData$Country.Code=="CRI" | PopSizeData$Country.Code=="CUB" | PopSizeData$Country.Code=="ECU" | PopSizeData$Country.Code=="SLV" | PopSizeData$Country.Code=="GTM" 
                 | PopSizeData$Country.Code=="HND" | PopSizeData$Country.Code=="MEX" | PopSizeData$Country.Code=="NIC" | PopSizeData$Country.Code=="PAN" | PopSizeData$Country.Code=="PRY" 
                 | PopSizeData$Country.Code=="PER" | PopSizeData$Country.Code=="URY" | PopSizeData$Country.Code=="VEN" | PopSizeData$Country.Code=="USA"| PopSizeData$Country.Code=="CAN",]
PopSizeData<-PopSizeData[, -(5:30)]
PopSizeData$Data.Source<-as.factor("WB")
PopSizeData$Indicator.Name<-as.factor("PopSize")
PopSizeData$Country.Name<-gsub("Venezuela,.RB", "Venezuela", PopSizeData$Country.Name)
PopSizeData$Country.Name<-gsub("El Salvador", "El.Salvador", PopSizeData$Country.Name)
PopSizeData$Country.Name<-gsub("Costa Rica", "Costa.Rica", PopSizeData$Country.Name)
PopSizeData$Country.Name<-as.factor(PopSizeData$Country.Name)
#summary(PopSizeData)

#Convert to Long Form
PopSizeData<-gather(PopSizeData, "Year", "Value", 5:33)
PopSizeData$Data.Source<-as.factor("WB")
names(PopSizeData)[3] <- "Indicator.Code" 
names(PopSizeData)[4] <- "Indicator.Name" 
PopSizeData$Year<-gsub("YR", "", PopSizeData$Year) #replacing YR with year
PopSizeData$Year<-as.factor(PopSizeData$Year) #setting back as factor


PopSizeData<-droplevels(PopSizeData)
#str(PopSizeData)
#reorder the columns to bind
PopSizeData$Region<-"LatAm"
PopSizeData$Region[PopSizeData$Country.Code == "USA"]<-"USA"
PopSizeData$Region[PopSizeData$Country.Code == "CAN"]<-"Canada"

PopSizeData<- PopSizeData[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","Value", "Region")] #head(PopSizeData)
#summary(PopSizeData)
#str(PopSizeData)
#Coonvert year to a number (it was previously a factor)  #note can't just use as.numeric
#the reason is here http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
PopSizeData$Year<-as.numeric(levels(PopSizeData$Year))[PopSizeData$Year] 
#str(RDData)
#Sort it first within a frame by rows to make it easier to read
PopSizeData<-PopSizeData[order(PopSizeData$Country.Name, PopSizeData$Year),]
#PopSizeData
}