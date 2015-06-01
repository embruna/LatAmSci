# If you wanted to input data on publications from Endnote files using REFNET, this is what you would use.

#detach(package:refnet, unload=TRUE)
#remove.packages("refnet")
#library(refnet)
# REFNET SECTION

####################
####################
###THIS SECTION IMPORTS PUBLICATIONS DATA from 2001-2014. 
###THE DATA HAVE TO BE IN THE REFNET FOLDER, SO THE WORKING DIRECTORY IS CHANGED BELOW
####################
####################

setwd("/Volumes/ifas/Emilio's Folder Current/RESEARCH/LatAmScience/refnet")

##  Let's read in single files, though we can specify a directory and 
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
head(pubs)
summary(pubs)
#head(SESdata)
#summary(SESdata)
#str(SESdata)
#str(pubs)

#Add columns and rename to match SESData
names(pubs)[1:3] <- c("Country.Name", "Year", "Value")  #need to rename the columns
pubs$Indicator.Code<-as.factor("PUBS.TOTL")                        #add a few new columns
pubs$Indicator.Name<-as.factor("Articles")                         #add a few new columns




