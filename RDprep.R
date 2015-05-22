# This function imports and cleans up UNESCO R&D DATA. As with the others selects only countries of interest
# select just countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN

RDprep <- function(x) {
  
  
  
  RDData<-RDData[RDData$Country.Code=="ARG" | RDData$Country.Code=="BOL"| RDData$Country.Code=="BRA" | RDData$Country.Code=="CHL" | RDData$Country.Code=="COL"
         | RDData$Country.Code=="CRI" | RDData$Country.Code=="CUB" | RDData$Country.Code=="ECU" | RDData$Country.Code=="SLV" | RDData$Country.Code=="GTM" 
         | RDData$Country.Code=="HND" | RDData$Country.Code=="MEX" | RDData$Country.Code=="NIC" | RDData$Country.Code=="PAN" | RDData$Country.Code=="PRY" 
         | RDData$Country.Code=="PER" | RDData$Country.Code=="URY" | RDData$Country.Code=="VEN"| RDData$Country.Code=="USA"| RDData$Country.Code=="CAN",]
  
  
  RDData$Data.Source<-as.factor("UNESCO")
  RDData$Indicator.Name<-as.factor("R&D")
  RDData$Country.Name<-gsub("Venezuela,.RB", "Venezuela", RDData$Country.Name)
  RDData$Country.Name<-gsub("El Salvador", "El.Salvador", RDData$Country.Name)
  RDData$Country.Name<-gsub("Costa Rica", "Costa.Rica", RDData$Country.Name)
  RDData$Country.Name<-as.factor(RDData$Country.Name)
  
  #Convert to Long Form
  RDData<-gather(RDData, "Year", "Value", 5:59)
  RDData$Data.Source<-as.factor("WB")
  names(RDData)[3] <- "Indicator.Code" 
  names(RDData)[4] <- "Indicator.Name" 
  RDData$Year<-gsub("YR", "", RDData$Year) #replacing YR with year
  RDData$Year<-as.factor(RDData$Year) #setting back as factor
  RDData<-droplevels(RDData)
  str(RDData)
  #reoRDDataer the columns to bind
  RDData<- RDData[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","Value")] #head(RDData)
  #summary(RDData)
  #str(RDData)
  
  
  #Coonvert year to a number (it was previously a factor)  #note can't just use as.numeric
  #the reason is here http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
  RDData$Year<-as.numeric(levels(RDData$Year))[RDData$Year] 
  #str(RDData)
  #Sort it first within a frame by rows to make it easier to read
  RDData<-RDData[order(RDData$Country.Name, RDData$Year),]
  #RDData

}