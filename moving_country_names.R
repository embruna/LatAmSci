library(rworldmap)  

rm(list=ls())
df <- NULL  
df$country <- c("El Salvador","Mexico","Panama", "Nicaragua", "Costa Rica",       "Cuba", "Honduras", "Guatemala", "Venezuela")  
df$code<-c("SLV", "MEX", "PAN", "NIC", "CRI", "CUB", "HON", "GTM", "VEN")  
df$number<-c(100, 500, 200, 150, 300, 390, 140, 330, 60)  
df<-as.data.frame(df)  
sPDF <- joinCountryData2Map( df, joinCode = "ISO3", nameJoinColumn = "code") 
mapCountryData(sPDF, nameColumnToPlot="number") 
sPDFmyCountries <- sPDF[sPDF$NAME %in% df$country,] 
mapCountryData(sPDFmyCountries, nameColumnToPlot="number", catMethod="fixedWidth", colourPalette="heat", borderCol="black",  mapTitle = ("Made Up Number"))
text(sPDFmyCountries, labels="NAME")


rm(list=ls())
df <- NULL  
df$country <- c("El Salvador","Mexico","Panama", "Nicaragua", "Costa Rica",       "Cuba", "Honduras", "Guatemala", "Venezuela")  
df$code<-c("SLV", "MEX", "PAN", "NIC", "CRI", "CUB", "HON", "GTM", "VEN")  
df$number<-c(100, 500, 200, 150, 300, 390, 140, 330, 60)  
df<-as.data.frame(df)  
sPDF <- joinCountryData2Map( df, joinCode = "ISO3", nameJoinColumn = "code") 
mapCountryData(sPDFmyCountries, nameColumnToPlot="number", catMethod="fixedWidth", colourPalette="heat", borderCol="black",  mapTitle = ("Made Up Number"))
df2=as.data.frame(sPDFmyCountries)
df2$latOffset=4 #4 degree offset
df2$lonOffset=4
text(df2$LON+df2$latOffset, df2$LAT+df2$lonOffset, labels=df2$country)