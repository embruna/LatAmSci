# This function imports and cleans up  WORLD BANK EDUCATION DATA. As with the others selects only countries of interest
# select just countries of in our study: ARG BOL BRA CHL COL CRI CUB ECU SLV GTM HND MEX NIC PAN PRY PER URY VEN USA CAN

UNEDprep <- function(x) {
     
  #Correct name of two of the countries
  UNEDdata$Country<-gsub(" ", ".", UNEDdata$Country)
  UNEDdata$Country[UNEDdata$Country == "Venezuela.(Bolivarian.Republic.of)"] <- "Venezuela"
  UNEDdata$Country[UNEDdata$Country == "Bolivia.(Plurinational.State.of)"] <- "Bolivia"
  UNEDdata$Country<-as.factor(UNEDdata$Country)
  
  UNEDdata$Country.Code<- NA
  UNEDdata$Country.Code[UNEDdata$Country == "Argentina"]  <- "ARG"
  UNEDdata$Country.Code[UNEDdata$Country == "Bolivia"]  <- "BOL"
  UNEDdata$Country.Code[UNEDdata$Country == "Brazil"]  <- "BRA"
  UNEDdata$Country.Code[UNEDdata$Country == "Chile"]  <- "CHL"
  UNEDdata$Country.Code[UNEDdata$Country == "Costa.Rica"]  <-"CRI"
  UNEDdata$Country.Code[UNEDdata$Country == "Colombia"]  <- "COL"
  UNEDdata$Country.Code[UNEDdata$Country == "Cuba"]  <- "CUB"
  UNEDdata$Country.Code[UNEDdata$Country == "Ecuador"]  <-"ECU"
  UNEDdata$Country.Code[UNEDdata$Country == "El.Salvador"]  <-"SLV"
  UNEDdata$Country.Code[UNEDdata$Country == "Guatemala"]  <-"GTM"
  UNEDdata$Country.Code[UNEDdata$Country == "Honduras"]  <-"HND"
  UNEDdata$Country.Code[UNEDdata$Country == "Mexico"]  <-"MEX"
  UNEDdata$Country.Code[UNEDdata$Country == "Nicaragua"]  <-"NIC"
  UNEDdata$Country.Code[UNEDdata$Country == "Panama"]  <-"PAN"
  UNEDdata$Country.Code[UNEDdata$Country == "Paraguay"]  <-"PRY"
  UNEDdata$Country.Code[UNEDdata$Country == "Peru"]  <-"PER"
  UNEDdata$Country.Code[UNEDdata$Country == "Uruguay"]  <-"URY"
  UNEDdata$Country.Code[UNEDdata$Country == "Venezuela"]  <-"VEN"
  UNEDdata$Country.Code[UNEDdata$Country == "Canada"]  <-"CAN"
  UNEDdata$Country.Code[UNEDdata$Country == "United.States"]  <-"USA"
  
  UNEDdata<-UNEDdata[complete.cases(UNEDdata[,"Country.Code"]),] #Reduces it down to juyst the countries for which you have a assigned a code.
  UNEDdata$Country.Code<-as.factor(UNEDdata$Country.Code) #they were being converted as characters, so convert them to factors
  
  
  #CONVERT TO LONG
  UNEDdata$HDI.Rank<- NULL #delete the column with HDI rank - this is for only one year, so best left to getting it elsewhere
  UNEDdata<-gather(UNEDdata, "Year", "Value", 2:15)
  UNEDdata$Year<-gsub("YR", "", UNEDdata$Year) #deleting YR
  UNEDdata$Year<-as.factor(UNEDdata$Year) #setting back as factor
  UNEDdata$Data.Source<-as.factor("UN_ED")
  UNEDdata$Indicator.Name<-as.factor("UNEDdata")
  UNEDdata$Indicator.Code<-as.factor("UN.ED.Index")
  names(UNEDdata)[1] <- "Country.Name" 
  UNEDdata<-droplevels(UNEDdata)
  UNEDdata<- UNEDdata[,c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","Data.Source","Year","Value")] #reorder the columns to bind
  #head(UNEDdata)
  #summary(UNEDdata)
  #str(UNEDdata)
  #Coonvert year to a number (it was previously a factor)  #note can't just use as.numeric
  #the reason is here http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
  UNEDdata$Year<-as.numeric(levels(UNEDdata$Year))[UNEDdata$Year] 
  #Sort it first within a frame by rows to make it easier to read
  UNEDdata<-UNEDdata[order(UNEDdata$Country.Name, UNEDdata$Year),]
  #UNEDdata
  
}