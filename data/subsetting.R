

#############################################################################################################################
#SUBSETTING BY COUNTRY SIZE
#############################################################################################################################
DATA<-PubsPerGDP  #create a dataframe to make this figure

small<-subset(DATA, Year.Pubs==2012 & PopSize < 10000000 & Region =="LatAm")$Country.Code
droplevels(small)
small<-filter(DATA, Country.Code%in%small)

medium<-subset(DATA, Year.Pubs==2012 & (PopSize >= 10000000 & PopSize< 50000000 & Region =="LatAm"))$Country.Code
droplevels(medium)
medium<-filter(DATA, Country.Code%in%medium)

large<-subset(DATA, Year.Pubs==2012 & PopSize > 50000000  & Region =="LatAm")$Country.Code
droplevels(large)
large<-filter(DATA, Country.Code%in%large)


DATA<-small


# Use ggplot to make the figures.  this one is a more basic qplot. The following line greates a line plot of publication 
# count of publications by year by region. Regions differentiatiated by color

MyFig1<-qplot(GDPpercapita, Publications, data = DATA, color = Country.Name, geom = "line", ylab="Articles")

MyFig1<-MyFig1 +ggtitle("A")       #Makes the Main title "A" because it will be figure A in a multi-panel plot
# The following lines are options for changing the colors of the lines
#MyFig1<-MyFig1 + scale_color_brewer(palette = "Paired")   #Changes the line color tothe RcolorBrewer palette "Paired"
# MyFig1<-MyFig1 + scale_colour_manual(values=c("gray15", "darkgreen", "blue3"))  #I chose my own colors for the lines
MyFig1<-MyFig1 + geom_line(aes(group=factor(Country.Name)),size=1)  #Changes the thickness of the lines
# The following two-part line 1) sets the Y axix and 2) sets the frequency of the tick marks
# MyFig1<-MyFig1 + coord_cartesian(ylim = c(-20, 3500)) + scale_y_continuous(breaks=seq(0, 3500, 250)) 
# I wanted to change one of the labels on X axis. to do so need scale_x_discrete. Left in Scale_X_continuous to show how would be for numerical axes
MyFig1<-MyFig1 + scale_y_continuous(breaks = seq(0, 250, 50), limits = c(-20, 250))
MyFig1<-MyFig1 + coord_cartesian(xlim = c(1988, 2015)) + scale_x_discrete(labels=c("Avg. per yr\n1981-1990" ," ", " ", " ", "1994"," ", " ", " ", "1998"," ", " ", " ", "2002"," ", " ", " ", "2006"," ", " ", " ", "2010"," ", " ", " ", "2014"))
# I wanted labels om the individual lines, so I used the following to define the label and position. 
# For a standard legend comment out the following three lines and add the ones below labeled "LEGEND" as instructed
# DATA[DATA$Year=="2013" controls on which data point to put the label, hjust/vjust adjust location relative to data point labeled.
# Can also change font size and style
# MyFig1<-MyFig1 + geom_text(data = DATA[DATA$Year=="2013" & DATA$Region=="Latin America",], aes(label = Region), hjust = 1, vjust = -1, size=7) #add fontface="bold" if you want bold labels
# MyFig1<-MyFig1 + geom_text(data = DATA[DATA$Year=="2013" & DATA$Region=="USA",], aes(label = Region), hjust = 1, vjust = -1, size=7) # ,fontface="bold"
# MyFig1<-MyFig1 + geom_text(data = DATA[DATA$Year=="2013" & DATA$Region=="Canada",], aes(label = Region), hjust = 1, vjust = 2, size=7) #,fontface="bold"
# The following selects the theme and manipulates elements of the plot
# I wanted to remove the gray background, dots, and gridlines from the plot
MyFig1<-MyFig1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                    plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=22),        #Sets title size, style, location
                                    axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                    axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                    axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                    plot.margin = unit(c(1,3,2,1), "cm"),                                          #Changes the margins around the plot. This will help with spacing in multi plt panels
                                    legend.position = "none")                                                       #Removes the Legend

MyFig1