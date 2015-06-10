# This function takes the WORLD BANK GDP DATA, selects only the countries of interest, and cleans up and reorganizes their data.
# The countries used here are:ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN CAN USA

GDPprep <- function(x) {

  GDPdata<-GDPdata[GDPdata$Country.Code=="ARG" | GDPdata$Country.Code=="BOL"| GDPdata$Country.Code=="BRA" | GDPdata$Country.Code=="CHL" | GDPdata$Country.Code=="COL"
           | GDPdata$Country.Code=="CRI" | GDPdata$Country.Code=="CUB" | GDPdata$Country.Code=="ECU" | GDPdata$Country.Code=="SLV" | GDPdata$Country.Code=="GTM" 
           | GDPdata$Country.Code=="HND" | GDPdata$Country.Code=="MEX" | GDPdata$Country.Code=="NIC" | GDPdata$Country.Code=="PAN" | GDPdata$Country.Code=="PRY" 
           | GDPdata$Country.Code=="PER" | GDPdata$Country.Code=="URY" | GDPdata$Country.Code=="VEN"| GDPdata$Country.Code=="USA"| GDPdata$Country.Code=="CAN",]
  
  #clean up name of variable of interest
  GDPdata$Indicator.Name<-as.factor("GDP")
  #Add columns with data source
  GDPdata$Data.Source<-as.factor("WB")
  GDPdata$Country.Name<-gsub("Venezuela RB", "Venezuela", GDPdata$Country.Name)
#   GDPdata$Country.Name<-gsub("El Salvador", "El.Salvador", GDPdata$Country.Name)
#   GDPdata$Country.Name<-gsub("Costa Rica", "Costa.Rica", GDPdata$Country.Name)
  GDPdata$Country.Name<-as.factor(GDPdata$Country.Name)
  
  #Convert to Long Form
  GDPdata<-gather(GDPdata, "Year", "Value", 5:59)
  GDPdata$Data.Source<-as.factor("WB")
  names(GDPdata)[3] <- "Indicator.Code" 
  names(GDPdata)[4] <- "Indicator.Name" 
  GDPdata$Year<-gsub("YR", "", GDPdata$Year) #replacing YR with year
  GDPdata$Year<-as.factor(GDPdata$Year) #setting back as factor
  GDPdata<-droplevels(GDPdata)
  GDPdata$Region<-"LatAm"
  GDPdata$Region[GDPdata$Country.Code == "USA"]<-"USA"
  GDPdata$Region[GDPdata$Country.Code == "CAN"]<-"Canada"
    #reorder the columns to bind
  GDPdata<- GDPdata[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","Value", "Region")] #head(GDPdata)
  #summary(GDPdata)
  #str(GDPdata)
  #levels(GDPdata$Country.Name)
  #Coonvert year to a number (it was previously a factor)  #note can't just use as.numeric
  #the reason is here http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
  GDPdata$Year<-as.numeric(levels(GDPdata$Year))[GDPdata$Year] 
  #Sort it first within a frame by rows to make it easier to read
  GDPdata<-GDPdata[order(GDPdata$Country.Name, GDPdata$Year),]
 
  #GDPdata
  
}