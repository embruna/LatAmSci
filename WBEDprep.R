# This function imports and cleans up  WORLD BANK EDUCATION DATA. As with the others selects only countries of interest
# select just countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN

WBEDprep <- function(x) {
  
  WBEDdata<-WBEDdata[WBEDdata$Country.Code=="ARG" | WBEDdata$Country.Code=="BOL"| WBEDdata$Country.Code=="BRA" | WBEDdata$Country.Code=="CHL" | WBEDdata$Country.Code=="COL"
             | WBEDdata$Country.Code=="CRI" | WBEDdata$Country.Code=="CUB" | WBEDdata$Country.Code=="ECU" | WBEDdata$Country.Code=="SLV" | WBEDdata$Country.Code=="GTM" 
             | WBEDdata$Country.Code=="HND" | WBEDdata$Country.Code=="MEX" | WBEDdata$Country.Code=="NIC" | WBEDdata$Country.Code=="PAN" | WBEDdata$Country.Code=="PRY" 
             | WBEDdata$Country.Code=="PER" | WBEDdata$Country.Code=="URY" | WBEDdata$Country.Code=="VEN"| WBEDdata$Country.Code=="CAN"| WBEDdata$Country.Code=="USA",]
  #Reduces it down to juyst the countries for which you have a assigned a code.
  WBEDdata<-WBEDdata[complete.cases(WBEDdata[,"Country.Code"]),]
  #Correct the names of 2 of the countries
  WBEDdata$Country.Name<-gsub("Venezuela,.RB", "Venezuela", WBEDdata$Country.Name)
  WBEDdata$Country.Name<-gsub("El Salvador", "El.Salvador", WBEDdata$Country.Name)
  WBEDdata$Country.Name<-gsub("Costa Rica", "Costa.Rica", WBEDdata$Country.Name)
  WBEDdata$Country.Name<-as.factor(WBEDdata$Country.Name)
  
  
  ########Convert to Long
  WBEDdata<-gather(WBEDdata, "Year", "Value", 5:47)
  WBEDdata$Data.Source<-as.factor("WBEDdata")
  names(WBEDdata)[3] <- "Indicator.Code" 
  names(WBEDdata)[4] <- "Indicator.Name" 
  WBEDdata$Year<-gsub("YR", "", WBEDdata$Year) #replacing YR with year
  WBEDdata$Year<-as.factor(WBEDdata$Year) #setting back as factor
  
  WBEDdata$Region<-"LatAm"
  WBEDdata$Region[WBEDdata$Country.Code == "USA"]<-"USA"
  WBEDdata$Region[WBEDdata$Country.Code == "CAN"]<-"Canada"
  
  WBEDdata<-droplevels(WBEDdata)
  
  #reorder the columns to bind
  WBEDdata<- WBEDdata[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","Value", "Region")] #head(WBEDdata)
  #summary(WBEDdata)
  #str(WBEDdata)
  #Coonvert year to a number (it was previously a factor)  #note can't just use as.numeric
  #the reason is here http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
  WBEDdata$Year<-as.numeric(levels(WBEDdata$Year))[WBEDdata$Year] 
  #str(RDData)
  
  ####HERE YOU CAN DECIDE WHAT WORLD BANK DATA TO USE. 
  ###I STARTED BY TOGGLING ON Public.expenditure.on.education.as.%.of.GDP
  
  WBEDdata<-filter(WBEDdata, Indicator.Name == "SE.XPD.TOTL.GD.ZS")
  #Sort it first within a frame by rows to make it easier to read
  WBEDdata<-WBEDdata[order(WBEDdata$Country.Name, WBEDdata$Year),]
  
}