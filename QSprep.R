# This function will take the raw QS UNIVERSITY RANKINGS data andc clean it up.  The output is a dataframe of the 
# top 100 universities in Latin American in 2012, their rankings in 2011, and what country each is in, and the scores 
# on each of the ranking criteria. 

QSprep <- function(x) {
  setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/SocioEconomic Data/UniversityRankings") #These data are in a different Folder
  
  UNIRANK<-read.csv("QS.csv", dec=".", header = TRUE, sep = ",", na.strings='NULL', check.names=FALSE)
  titles<-UNIRANK$category[1:19]
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
  UNIRANK$Country.Territory<- NULL
  UNIRANK
}