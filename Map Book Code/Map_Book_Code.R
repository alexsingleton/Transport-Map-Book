
setwd("/Users/alex/Google Drive/Projects/Transport_MapBook/")

library(classInt)
library(RColorBrewer)
library(rgeos)
library(maptools)
library(Hmisc)
##########
#FUNCTIONS
##########

#Hacked version of the map.scale function
map.scale2 <- function (xc, yc, len, units, ndivs, subdiv = 1, tcol = "black", 
                        scol = "black", sfcol = "black") 
{
  frame = par("usr")
  l <- len
  tic = (frame[4] - frame[3])/100
  ul = l/ndivs
  for (i in seq(0, ndivs - 1, by = 2)) rect(xc - l/2 + i * ul, yc, xc - l/2 + (i + 1) * ul, yc + tic/2, border = NA, col = sfcol)
  lines(c(xc - l/2, xc - l/2, xc + l/2, xc + l/2), c(yc + tic, yc, yc, yc + tic), col = scol)
  lines(c(xc - l/2, xc + l/2), c(yc + tic/2, yc + tic/2), col = scol)
  for (i in c(0,ndivs)) text(xc - l/2 + ul * i, yc - strheight(i * subdiv) * 0.7, (i * subdiv)/100, col = tcol,cex = 0.7,font=1, family="sans")
  text(xc, yc - 2 * strheight(units), units, col = tcol,cex = 0.7,font=1, family="sans")
}

#Function to deal with rounding
mround <- function(x,base){ 
  base*round(x/base) 
}

###############
#Data Import
###############

#Read lookups
OA_Lookup <- read.csv("./lookups/OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv")
OA_Lookup <- OA_Lookup[,c("OA11CD","LAD11CD","MSOA11CD")]
MSOA_Lookup <- unique(OA_Lookup[,c('MSOA11CD','LAD11CD')])
OA_Lookup$MSOA11CD <- NULL
Choro_Vars <- read.csv("./lookups/Chro_lookup.csv")
WZ_lookup <- read.csv("./lookups/WZ11_MSOA11_LAD11_EW_LU.csv")
WZ_lookup <- WZ_lookup[,c("WZ11CD","LAD11CD")]
WARD_lookup <- read.csv("./lookups/WD11_CMWD11_LAD11_EW_LU.csv")
WARD_lookup <- WARD_lookup[,c("WD11CD","LAD11CD")]
LSOA_Lookup <- read.csv("./lookups/LSOA01_LSOA11_LAD11_EW_LU.csv")
LSOA_Lookup <- LSOA_Lookup[,c(1,6)]


#Read CO2 Data
Primary <- read.csv("./LSOA/CO2/Primary.csv")
colnames(Primary) <- c("LSOA","Primary")
Secondary <- read.csv("./LSOA/CO2/Secondary.csv")
colnames(Secondary) <- c("LSOA","Secondary")


#Read Census data
#Output Area
QS701EW <- read.csv("./output-area/QS701EWDATA.CSV")
QS416EW <- read.csv("./output-area/QS416EWDATA.CSV")
oa <- merge(QS416EW,QS701EW,by="GeographyCode",all.x=TRUE)

#Workplace Zones
WP702EW <- read.csv("./workplace-zones/WP702EWDATA.CSV")
WP703EW <- read.csv("./workplace-zones/WP703EWDATA.CSV")
workplace <- merge(WP702EW,WP703EW,by="GeographyCode",all.x=TRUE)

#Read OD Data
OD <- read.csv("./origin-destination/wu03ew_v2.csv")

#Merge MSOA onto workplace
OD <- merge(OD,MSOA_Lookup, by.x="Area.of.workplace", by.y="MSOA11CD",all.x=TRUE)
OD <- merge(OD,MSOA_Lookup, by.x="Area.of.residence", by.y="MSOA11CD",all.x=TRUE)

#Read Boundaries and append lookup
library(rgdal)
MSOA <- readOGR("./boundaries/", "England_msoa_2011_gen_clipped")
LAD <- readOGR("./boundaries/", "England_lad_2011_gen_clipped")
WZ <- readOGR("./boundaries/", "England_WZ_2011_gen_clipped")
OA_ZONE <- readOGR("./boundaries/", "England_oa_2011_gen_clipped")
WARD <- readOGR("./boundaries/", "england_cwa_2011_gen_clipped")
MSOA@data$x <- coordinates(MSOA)[,1]
MSOA@data$y <- coordinates(MSOA)[,2]
MSOA@data <- data.frame(MSOA@data, MSOA_Lookup[match(MSOA@data[, "CODE"], MSOA_Lookup[, "MSOA11CD"]), ])
WZ@data <- data.frame(WZ@data, WZ_lookup[match(WZ@data[, "CODE"], WZ_lookup[, "WZ11CD"]), ])
WARD@data <- data.frame(WARD@data, WARD_lookup[match(WARD@data[, "CODE"], WARD_lookup[, "WD11CD"]), ])
OA_ZONE@data <- data.frame(OA_ZONE@data, OA_Lookup[match(OA_ZONE@data[, "CODE"], OA_Lookup[, "OA11CD"]), ])

LSOA <- readOGR("./boundaries/", "england_low_soa_2001_gen")
LSOA@data <- as.data.frame(LSOA@data[,1])
colnames(LSOA@data) <- "LSOA01CD"
LSOA@data <- data.frame(LSOA@data, LSOA_Lookup[match(LSOA@data[, "LSOA01CD"], LSOA_Lookup[, "LSOA01CD"]), ])
LSOA$LSOA01CD.1 <- NULL

LSOA_CO2 <- readOGR("./boundaries/", "england_low_soa_2001_gen")
LSOA_CO2@data <- as.data.frame(LSOA_CO2@data[,1])
colnames(LSOA_CO2@data) <- "LSOA01CD"
LSOA_CO2@data <- data.frame(LSOA_CO2@data, Primary[match(LSOA_CO2@data[, "LSOA01CD"], Primary[, "LSOA"]), ])
LSOA_CO2@data <- data.frame(LSOA_CO2@data, Secondary[match(LSOA_CO2@data[, "LSOA01CD"], Secondary[, "LSOA"]), ])

LSOA_CO2@data <- data.frame(LSOA_CO2@data, LSOA_Lookup[match(LSOA_CO2@data[, "LSOA01CD"], LSOA_Lookup[, "LSOA01CD"]), ])
LSOA_CO2$LSOA01CD.1 <- NULL
LSOA_CO2$LSOA.1 <- NULL
LSOA_CO2$LSOA <- NULL

#Create an MSOA xy file
MSOA_xy <- data.frame(MSOA@data$CODE,coordinates(MSOA)[,1],coordinates(MSOA)[,2])
colnames(MSOA_xy) <-c("CODE","x","y")

OD <- merge(OD,MSOA_xy, by.x="Area.of.workplace", by.y="CODE",all.x=TRUE)
OD <- merge(OD,MSOA_xy, by.x="Area.of.residence", by.y="CODE",all.x=TRUE)
colnames(OD) <- c("Workplace","Residence","All","Work_Home","Rail_light_etc","Train","Bus","Taxi","Motorcycle","Driving","Passenger","Bicycle","Foot","Other","LAD11CD_workplace","LAD11CD_residence","x_workplace","y_workplace","x_residence","y_residence")


#Read DfT LSOA Data
DfT_Lookup <- read.csv("./lookups/DfT.csv")
DfT_Lookup$table <- c(paste(paste(DfT_Lookup$Table,"_",DfT_Lookup$Year,sep='')[1:49]))


#acs0501
acs0501_2010 <- read.csv("./LSOA/acs0501_2010.csv",skip=7,header=FALSE)
h_name <- apply(read.table(pipe("sed -n -e'6p' ./LSOA/acs0501_2010.csv"), sep=','),1, as.character)
colnames(acs0501_2010) <- h_name

acs0501_2011 <- read.csv("./LSOA/acs0501_2011.csv",skip=7,header=FALSE)
h_name <- apply(read.table(pipe("sed -n -e'6p' ./LSOA/acs0501_2011.csv"), sep=','),1, as.character)
colnames(acs0501_2011) <- h_name

acs0501_2012 <- read.csv("./LSOA/acs0501_2012.csv",skip=7,header=FALSE)
h_name <- apply(read.table(pipe("sed -n -e'6p' ./LSOA/acs0501_2012.csv"), sep=','),1, as.character)
colnames(acs0501_2012) <- h_name

#acs0501
acs0501_2010 <- read.csv("./LSOA/acs0501_2010.csv",skip=7,header=FALSE)
h_name <- apply(read.table(pipe("sed -n -e'6p' ./LSOA/acs0501_2010.csv"), sep=','),1, as.character)
colnames(acs0501_2010) <- h_name

acs0501_2011 <- read.csv("./LSOA/acs0501_2011.csv",skip=7,header=FALSE)
h_name <- apply(read.table(pipe("sed -n -e'6p' ./LSOA/acs0501_2011.csv"), sep=','),1, as.character)
colnames(acs0501_2011) <- h_name

acs0501_2012 <- read.csv("./LSOA/acs0501_2012.csv",skip=7,header=FALSE)
h_name <- apply(read.table(pipe("sed -n -e'6p' ./LSOA/acs0501_2012.csv"), sep=','),1, as.character)
colnames(acs0501_2012) <- h_name


#acs0502
acs0502_2010 <- read.csv("./LSOA/acs0502_2010.csv",skip=7,header=FALSE)
colnames(acs0502_2010) <- scan('./LSOA/acs0502_2010.csv', '', skip = 5, nlines = 1, sep = ',')

acs0502_2011 <- read.csv("./LSOA/acs0502_2011.csv",skip=7,header=FALSE)
colnames(acs0502_2011) <- scan('./LSOA/acs0502_2011.csv', '', skip = 5, nlines = 1, sep = ',')

acs0502_2012 <- read.csv("./LSOA/acs0502_2012.csv",skip=7,header=FALSE)
colnames(acs0502_2012) <- scan('./LSOA/acs0502_2012.csv', '', skip = 5, nlines = 1, sep = ',')

#acs0503
acs0503_2010 <- read.csv("./LSOA/acs0503_2010.csv",skip=7,header=FALSE)
colnames(acs0503_2010) <- scan('./LSOA/acs0503_2010.csv', '', skip = 5, nlines = 1, sep = ',')

acs0503_2011 <- read.csv("./LSOA/acs0503_2011.csv",skip=7,header=FALSE)
colnames(acs0503_2011) <- scan('./LSOA/acs0503_2011.csv', '', skip = 5, nlines = 1, sep = ',')

acs0503_2012 <- read.csv("./LSOA/acs0503_2012.csv",skip=7,header=FALSE)
colnames(acs0503_2012) <- scan('./LSOA/acs0503_2012.csv', '', skip = 5, nlines = 1, sep = ',')

#acs0505
acs0505_2010 <- read.csv("./LSOA/acs0505_2010.csv",skip=7,header=FALSE)
colnames(acs0505_2010) <- scan('./LSOA/acs0505_2010.csv', '', skip = 5, nlines = 1, sep = ',')

acs0505_2011 <- read.csv("./LSOA/acs0505_2011.csv",skip=7,header=FALSE)
colnames(acs0505_2011) <- scan('./LSOA/acs0505_2011.csv', '', skip = 5, nlines = 1, sep = ',')

acs0505_2012 <- read.csv("./LSOA/acs0505_2012.csv",skip=7,header=FALSE)
colnames(acs0505_2012) <- scan('./LSOA/acs0505_2012.csv', '', skip = 5, nlines = 1, sep = ',')

#acs0506
acs0506_2010 <- read.csv("./LSOA/acs0506_2010.csv",skip=7,header=FALSE)
colnames(acs0506_2010) <- scan('./LSOA/acs0506_2010.csv', '', skip = 5, nlines = 1, sep = ',')

acs0506_2011 <- read.csv("./LSOA/acs0506_2011.csv",skip=7,header=FALSE)
colnames(acs0506_2011) <- scan('./LSOA/acs0506_2011.csv', '', skip = 5, nlines = 1, sep = ',')

acs0506_2012 <- read.csv("./LSOA/acs0506_2012.csv",skip=7,header=FALSE)
colnames(acs0506_2012) <- scan('./LSOA/acs0506_2012.csv', '', skip = 5, nlines = 1, sep = ',')


#acs0507
acs0507_2010 <- read.csv("./LSOA/acs0507_2010.csv",skip=7,header=FALSE)
colnames(acs0507_2010) <- scan('./LSOA/acs0507_2010.csv', '', skip = 5, nlines = 1, sep = ',')

acs0507_2011 <- read.csv("./LSOA/acs0507_2011.csv",skip=7,header=FALSE)
colnames(acs0507_2011) <- scan('./LSOA/acs0507_2011.csv', '', skip = 5, nlines = 1, sep = ',')

acs0507_2012 <- read.csv("./LSOA/acs0507_2012.csv",skip=7,header=FALSE)
colnames(acs0507_2012) <- scan('./LSOA/acs0507_2012.csv', '', skip = 5, nlines = 1, sep = ',')

#acs0508
acs0508_2010 <- read.csv("./LSOA/acs0508_2010.csv",skip=7,header=FALSE)
colnames(acs0508_2010) <- scan('./LSOA/acs0508_2010.csv', '', skip = 5, nlines = 1, sep = ',')

acs0508_2011 <- read.csv("./LSOA/acs0508_2011.csv",skip=7,header=FALSE)
colnames(acs0508_2011) <- scan('./LSOA/acs0508_2011.csv', '', skip = 5, nlines = 1, sep = ',')

acs0508_2012 <- read.csv("./LSOA/acs0508_2012.csv",skip=7,header=FALSE)
colnames(acs0508_2012) <- scan('./LSOA/acs0508_2012.csv', '', skip = 5, nlines = 1, sep = ',')


#Limit DfT lookup to just minutes variables - others are not useful for mapping
DfT_Lookup <- DfT_Lookup[DfT_Lookup$Type == "Minutes",]

#Create a combined table for LSOA DfT data, plus clean up tables
Dft_tabs <- unique(DfT_Lookup$table)[1:length(unique(DfT_Lookup$table))-1]#Get a list of the tables

#Create an output file
DfT_Out <- as.data.frame(acs0506_2010[substr(acs0506_2010$HOSPO001,1,1) == "E",1]) 
colnames(DfT_Out) <- "LSOA"

for (i in 1:length(Dft_tabs)) {  
  get_tab <- Dft_tabs[i] #Get name of table  
  tmp_tab <- get(get_tab)[substr(get(get_tab)[,colnames(get(get_tab))[1]],1,1) == "E",] #Removes junk by limiting rows to only LSOA starting with E
  get_vars <- DfT_Lookup[DfT_Lookup$table == get_tab,"Variable"] #Get a list of varables to extract
  tmp_tab <- tmp_tab[,c(colnames(tmp_tab)[1],paste(get_vars))] #Cut down table to only thos variables of interest
  DfT_Out <- merge(DfT_Out,tmp_tab, by.x="LSOA",by.y=paste(c(colnames(get(get_tab))[1])),all.x=TRUE) #Merge onto output
  remove(get_tab,tmp_tab,get_vars)
}

#Clean up some factors
DfT_Out$EMPLO011 <- as.numeric(as.character(DfT_Out$EMPLO011))
DfT_Out$PSCHO011 <- as.numeric(as.character(DfT_Out$PSCHO011))
DfT_Out$SSCHO011 <- as.numeric(as.character(DfT_Out$SSCHO011))
DfT_Out$GPSO011 <- as.numeric(as.character(DfT_Out$GPSO011))
DfT_Out$HOSPO011 <- as.numeric(as.character(DfT_Out$HOSPO011))
DfT_Out$SUPO011 <- as.numeric(as.character(DfT_Out$SUPO011))


#Merge onto polygons
LSOA@data <- data.frame(LSOA@data, DfT_Out[match(LSOA@data[, "LSOA01CD"], DfT_Out[, "LSOA"]), ])#Append data to map for municipality



###############
#Data Prep
###############
#WZ Data Prep

choro_tab <- unique(Choro_Vars$Table)[1:2]
wz_maps <- Choro_Vars[Choro_Vars$Table %in% choro_tab,]
wz_out <- as.data.frame(workplace[,1])
colnames(wz_out) <- "WZ_CODE"

#Loop to calculate percentages
for (n in 1:nrow(wz_maps)){
  #Uses grep to get the column id matching the current numerator
  numerator <- workplace[,grep(paste("^",wz_maps[n,1],"$",sep=""), colnames(workplace))]
  
  #Uses grep to get the column id matching the current denominator
  denominator <- workplace[,grep(paste("^",wz_maps[n,2],"$",sep=""), colnames(workplace))]
  
  assign(paste(wz_maps[n,1],"_PCT",sep=""),as.data.frame(numerator/denominator*100))# Calculate %
  assign(paste(wz_maps[n,1],"_PCT",sep=""),setNames(get(paste(wz_maps[n,1],"_PCT",sep="")),paste(wz_maps[n,1],"_PCT",sep="")))#Change column name
  
  #Create output files
  wz_out <- cbind(wz_out,get(paste(wz_maps[n,1],"_PCT",sep="")))
  
  remove(list=paste(wz_maps[n,1],"_PCT",sep=""))
  remove(list=c("numerator","denominator"))
  
} 

#Merge onto polygons
WZ@data <- data.frame(WZ@data, wz_out[match(WZ@data[, "CODE"], wz_out[, "WZ_CODE"]), ])#Append data to map for municipality

#OA Data Prep

choro_tab <- unique(Choro_Vars$Table)[3:4]
OA_maps <- Choro_Vars[Choro_Vars$Table %in% choro_tab,]
OA_out <- as.data.frame(oa[,1])
colnames(OA_out) <- "OA_CODE"

#Loop to calculate percentages
for (n in 1:nrow(OA_maps)){
  #Uses grep to get the column id matching the current numerator
  numerator <- oa[,grep(paste("^",OA_maps[n,1],"$",sep=""), colnames(oa))]
  
  #Uses grep to get the column id matching the current denominator
  denominator <- oa[,grep(paste("^",OA_maps[n,2],"$",sep=""), colnames(oa))]
  
  assign(paste(OA_maps[n,1],"_PCT",sep=""),as.data.frame(numerator/denominator*100))# Calculate %
  assign(paste(OA_maps[n,1],"_PCT",sep=""),setNames(get(paste(OA_maps[n,1],"_PCT",sep="")),paste(OA_maps[n,1],"_PCT",sep="")))#Change column name
  
  #Create output files
  OA_out <- cbind(OA_out,get(paste(OA_maps[n,1],"_PCT",sep="")))
  
  remove(list=paste(OA_maps[n,1],"_PCT",sep=""))
  remove(list=c("numerator","denominator"))
  
} 

#Merge onto polygons
OA_ZONE@data <- data.frame(OA_ZONE@data, OA_out[match(OA_ZONE@data[, "CODE"], OA_out[, "OA_CODE"]), ])#Append data to map for municipality




###############
#Create Maps
###############

#Create list of LAD
LAD_LIST <- unique(OA_Lookup$LAD11CD)
LAD_LIST <- LAD_LIST[substr(LAD_LIST,1,1) == "E" & LAD_LIST != "E09000001"]
var_LIST <- c("All","Rail_light_etc","Train","Bus","Taxi","Motorcycle","Driving","Passenger","Bicycle","Foot")

LAD_LIST<- LAD_LIST[sample(1:length(LAD_LIST), 5)]

for (i in 1:length(LAD_LIST)){#Start LAD Map Loop
  
  LAD_plot <- as.character(LAD_LIST[i])#Get LAD
  
  ############################
  #Flow Maps
  ############################

  for (v in 1:length(var_LIST)){#Start Mode Loop

    
    var_plot <- as.character(var_LIST[v])#Get Mode

    #Limit flow data to manageable size
      LAD_flow <- subset(OD,LAD11CD_workplace == LAD_plot & LAD11CD_residence == LAD_plot)#Extract LAD
      LAD_flow <- LAD_flow[as.character(LAD_flow$Workplace) != as.character(LAD_flow$Residence),]#Remove internal flow
      LAD_flow <- LAD_flow[order(-LAD_flow[,paste(var_plot)]),]#Order
      LAD_flow <- LAD_flow[1:(min(c(60,nrow(LAD_flow)))),]#Limit top 60 flows
      LAD_flow <- LAD_flow[order(LAD_flow[,paste(var_plot)]),]#Order plot small to large
    
         if (length(unique(LAD_flow[,paste(var_plot)])) >5){    #check if worth mapping


        colint <- classIntervals(LAD_flow[,paste(var_plot)], 5, style = "jenks")$brks #Get breaks
        width <- c(0.3,0.5,1,2,4) #Assign line widths
        colours <- brewer.pal(5, "YlOrBr")#Flows


#Create Map
pdf(paste("./Map_out/","FL_",LAD_plot,"_",var_plot,".pdf",sep=''))
plot(LAD[LAD@data$CODE == LAD_plot,],col="#003333",lwd=0.05)#Background
plot(MSOA[MSOA@data$LAD11CD == LAD_plot,], border = "#2b5555", lwd=0.5,add=TRUE)#MSOA Borders
for (x in 1:nrow(LAD_flow)){
    segments(LAD_flow[x,"x_workplace"],  LAD_flow[x,"y_workplace"],	LAD_flow[x,"x_residence"],	LAD_flow[x,"y_residence"],lwd=width[findInterval(LAD_flow[x,var_plot], colint,all.inside=TRUE)], col=colours[findInterval(LAD_flow[x,var_plot], colint,all.inside=TRUE)])
}
#Plotting region dimensions
rng <- par("usr")
#Call legend
lg <- legend(rng[1],rng[3],legend=c(leglabs(colint),"MSOA"), col=c(colours,"#2b5555"),lty=c(1,1,1,1,1,NA),lwd=c(0.3,0.5,1,2,4,NA), fill=c(NA,NA,NA,NA,NA,"#003333"),border=c(NA,NA,NA,NA,NA,"#2b5555"),text.col = "#FFFFFF",bg="#003333",merge = TRUE,cex=.4,horiz=TRUE,plot=FALSE)

#Set appropriate legend values for selected breaks 
  lty <- c(1,1,1,1,1,NA)
  lwd <- c(0.3,0.5,1,2,4,NA)
  fill <- c(NA,NA,NA,NA,NA,"#003333")
  border <- c(NA,NA,NA,NA,NA,"#2b5555")
  legend <- c(leglabs(colint),"MSOA")

#Adjust legend plot position
legend(rng[1]+(((rng[2]-rng[1])/2)-(lg$rect$w/2)),rng[3]+ (lg$rect$h*0.6),legend=legend, col=c(colours,"#2b5555"),lty=lty,lwd=lwd, fill=fill,border=border,text.col = "#FFFFFF",bg="#003333",merge = TRUE,cex=.4,horiz=TRUE,plot=TRUE,xpd = NA)
dev.off()
system(paste("pdfcrop '",paste(getwd(),"/Map_out/","FL_",LAD_plot,"_",var_plot,".pdf",sep=''),"' '",paste("/Users/alex/Map_out/","FL_",LAD_plot,"_",var_plot,".pdf",sep=''),"'",sep=""),wait=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE)

rm(colint,width,colours,rng,lg)
}#End check if worth mapping
}#End Mode Loop
rm(LAD_flow,var_plot)

############################ End Flow Map ############################





############################
#WZ Maps
############################

plot_vars_WZ <- colnames(wz_out)[-1]
my_colours <- brewer.pal(5, "YlOrRd")

for (i in 1:length(plot_vars_WZ)) {
  
  pdf(paste("./Map_out/","WZ_",LAD_plot,"_",gsub("_PCT","",plot_vars_WZ[i]),".pdf",sep=''))
  
  tmp_plot_vars <- WZ@data[WZ@data$LAD11CD == LAD_plot,plot_vars_WZ[i]]
  
  if (length(unique(tmp_plot_vars)) >5){
  
  breaks <- classIntervals(tmp_plot_vars, 5, style = "jenks")$brks #Get breaks
  
  plot(WZ[WZ@data$LAD11CD == LAD_plot,], col = my_colours[findInterval(tmp_plot_vars, breaks, all.inside = TRUE)], axes = FALSE, border = NA)
  plot(WARD[WARD@data$LAD11CD == LAD_plot,],border="#707070",add=TRUE)
  
  
  #A loop to check that ward labels are appropriate
  ig_lab <- c("E06000052","E07000031","E07000165","E07000168","E09000001","E06000054","E06000048")
  
  #Add on text labels for the wards
  if (!LAD_plot %in% ig_lab){
    pointLabel(coordinates(WARD[WARD@data$LAD11CD == LAD_plot,])[,1],coordinates(WARD[WARD@data$LAD11CD == LAD_plot,])[,2],labels=WARD@data[WARD@data$LAD11CD == LAD_plot,"NAME"], cex=.5)
  }
  
  #Plotting region dimensions
  rng <- par("usr")
  #Call legend
  lg <- legend(rng[1],rng[3], legend = leglabs(round(breaks, digits = 1), between = " to "), fill = my_colours, bty = "o", ,bg="#FFFFFF", border = NA,horiz=TRUE,box.lty=0,cex=.4,plot=FALSE)

  #Adjust legend plot position
  legend(rng[1]+(((rng[2]-rng[1])/2)-(lg$rect$w/2)),rng[3]+ (lg$rect$h*0.1), legend = paste(leglabs(round(breaks, digits = 1), between = " to "),"%",sep=''), fill = my_colours, bty = "o", ,bg="#FFFFFF", border = NA ,horiz=TRUE,box.lty=0,xpd = NA,cex=.4)
dev.off()
system(paste("pdfcrop '",paste(getwd(),"/Map_out/","WZ_",LAD_plot,"_",gsub("_PCT","",plot_vars_WZ[i]),".pdf",sep=''),"' '",paste("/Users/alex/Map_out/","WZ_",LAD_plot,"_",gsub("_PCT","",plot_vars_WZ[i]),".pdf",sep=''),"'",sep=""),wait=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE)

rm(breaks,rng,lg)
}#Check if worth plotting
rm(tmp_plot_vars)
}


################################################################################################################

############################
#OA Maps
############################


plot_vars_OA <- colnames(OA_out)[-1]
my_colours <- brewer.pal(5, "Blues")

for (i in 1:length(plot_vars_OA)) {
  
  pdf(paste("./Map_out/","OA_",LAD_plot,"_",gsub("_PCT","",plot_vars_OA[i]),".pdf",sep=''))
  
  tmp_plot_vars <- OA_ZONE@data[OA_ZONE@data$LAD11CD == LAD_plot,plot_vars_OA[i]]
  
  if (length(unique(tmp_plot_vars)) >5){
  
  breaks <- classIntervals(tmp_plot_vars, 5, style = "jenks")$brks #Get breaks
  
  plot(OA_ZONE[OA_ZONE@data$LAD11CD == LAD_plot,], col = my_colours[findInterval(tmp_plot_vars, breaks, all.inside = TRUE)], axes = FALSE, border = NA)
  plot(WARD[WARD@data$LAD11CD == LAD_plot,],border="#707070",add=TRUE)
  
  
  #A loop to check that ward labels are appropriate
  ig_lab <- c("E06000052","E07000031","E07000165","E07000168","E09000001","E06000054","E06000048")
  
  #Add on text labels for the wards
  if (!LAD_plot %in% ig_lab){
    pointLabel(coordinates(WARD[WARD@data$LAD11CD == LAD_plot,])[,1],coordinates(WARD[WARD@data$LAD11CD == LAD_plot,])[,2],labels=WARD@data[WARD@data$LAD11CD == LAD_plot,"NAME"], cex=.5)
  }
  
  #Plotting region dimensions
  rng <- par("usr")
  #Call legend
  lg <- legend(rng[1],rng[3], legend = leglabs(round(breaks, digits = 1), between = " to "), fill = my_colours, bty = "o", ,bg="#FFFFFF", border = NA,horiz=TRUE,box.lty=0,cex=.4,plot=FALSE)
  
  #Adjust legend plot position
  legend(rng[1]+(((rng[2]-rng[1])/2)-(lg$rect$w/2)),rng[3]+ (lg$rect$h*0.1), legend = paste(leglabs(round(breaks, digits = 1), between = " to "),"%",sep=''), fill = my_colours, bty = "o", ,bg="#FFFFFF", border = NA ,horiz=TRUE,box.lty=0,xpd = NA,cex=.4)
  dev.off()
  system(paste("pdfcrop '",paste(getwd(),"/Map_out/","OA_",LAD_plot,"_",gsub("_PCT","",plot_vars_OA[i]),".pdf",sep=''),"' '",paste("/Users/alex/Map_out/","OA_",LAD_plot,"_",gsub("_PCT","",plot_vars_OA[i]),".pdf",sep=''),"'",sep=""),wait=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE)
  rm(breaks,rng,lg)
  }#Check if worth plotting
  rm(tmp_plot_vars)
}



################################################################################################################

############################
#LSOA Maps
############################


plot_vars_LSOA <- colnames(LSOA@data)[4:length(colnames(LSOA@data))]
my_colours <- brewer.pal(5, "Greens")

for (i in 1:length(plot_vars_LSOA)) {
  
  
  tmp_plot_vars <- LSOA@data[LSOA@data$LAD11CD == LAD_plot,plot_vars_LSOA[i]]
 
  if (length(unique(tmp_plot_vars)) >5){
  
  breaks <- classIntervals(tmp_plot_vars, 5, style = "jenks")$brks #Get breaks
    
  pdf(paste("./Map_out/","LSOA_",LAD_plot,"_",plot_vars_LSOA[i],".pdf",sep=''))
  
  plot(LSOA[LSOA@data$LAD11CD == LAD_plot,], col = "#b7c3d0", axes = FALSE, border = NA)
  plot(LSOA[LSOA@data$LAD11CD == LAD_plot,], col = my_colours[findInterval(tmp_plot_vars, breaks, all.inside = TRUE)], axes = FALSE, border = NA,add=TRUE)
  plot(WARD[WARD@data$LAD11CD == LAD_plot,],border="#707070",add=TRUE)
  
  
  #A loop to check that ward labels are appropriate
  ig_lab <- c("E06000052","E07000031","E07000165","E07000168","E09000001","E06000054","E06000048")
  
  #Add on text labels for the wards
  if (!LAD_plot %in% ig_lab){
    pointLabel(coordinates(WARD[WARD@data$LAD11CD == LAD_plot,])[,1],coordinates(WARD[WARD@data$LAD11CD == LAD_plot,])[,2],labels=WARD@data[WARD@data$LAD11CD == LAD_plot,"NAME"], cex=.5)
  }
  
  #Plotting region dimensions
  rng <- par("usr")
  #Call legend
  lg <- legend(rng[1],rng[3], legend = leglabs(round(breaks, digits = 1), between = " to "), fill = my_colours, bty = "o", ,bg="#FFFFFF", border = NA,horiz=TRUE,box.lty=0,cex=.4,plot=FALSE)
  
  #Adjust legend plot position
  legend(rng[1]+(((rng[2]-rng[1])/2)-(lg$rect$w/2)),rng[3]+ (lg$rect$h*0.1), legend = paste(leglabs(round(breaks, digits = 1), between = " to "),"min.",sep=''), fill = my_colours, bty = "o", ,bg="#FFFFFF", border = NA ,horiz=TRUE,box.lty=0,xpd = NA,cex=.4)
  dev.off()
  system(paste("pdfcrop '",paste(getwd(),"/Map_out/","LSOA_",LAD_plot,"_",plot_vars_LSOA[i],".pdf",sep=''),"' '",paste("/Users/alex/Map_out/","LSOA_",LAD_plot,"_",plot_vars_LSOA[i],".pdf",sep=''),"'",sep=""),wait=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE)
  rm(breaks,rng,lg)
  }#Check if worth plotting
  rm(tmp_plot_vars)
}




############################
#LSOA CO2 Maps
############################


plot_vars_LSOA <- colnames(LSOA_CO2@data)[c(2,3)]
my_colours <- brewer.pal(5, "RdPu")

for (i in 1:length(plot_vars_LSOA)) {
  
  tmp_plot_vars <- LSOA_CO2@data[LSOA_CO2@data$LAD11CD == LAD_plot,plot_vars_LSOA[i]]
  
  if (length(unique(tmp_plot_vars)) >5){
    
    breaks <- classIntervals(tmp_plot_vars, 5, style = "jenks")$brks #Get breaks
    
    pdf(paste("./Map_out/","LSOACO2_",LAD_plot,"_",plot_vars_LSOA[i],".pdf",sep=''))
    
    plot(LSOA_CO2[LSOA_CO2@data$LAD11CD == LAD_plot,], col = "#b7c3d0", axes = FALSE, border = NA)
    plot(LSOA_CO2[LSOA_CO2@data$LAD11CD == LAD_plot,], col = my_colours[findInterval(tmp_plot_vars, breaks, all.inside = TRUE)], axes = FALSE, border = NA,add=TRUE)
    plot(WARD[WARD@data$LAD11CD == LAD_plot,],border="#707070",add=TRUE)
    
    
    #A loop to check that ward labels are appropriate
    ig_lab <- c("E06000052","E07000031","E07000165","E07000168","E09000001","E06000054","E06000048")
    
    #Add on text labels for the wards
    if (!LAD_plot %in% ig_lab){
      pointLabel(coordinates(WARD[WARD@data$LAD11CD == LAD_plot,])[,1],coordinates(WARD[WARD@data$LAD11CD == LAD_plot,])[,2],labels=WARD@data[WARD@data$LAD11CD == LAD_plot,"NAME"], cex=.5)
    }
    
    #Plotting region dimensions
    rng <- par("usr")
    #Call legend
    lg <- legend(rng[1],rng[3], legend = leglabs(round(breaks, digits = 1), between = " to "), fill = my_colours, bty = "o", ,bg="#FFFFFF", border = NA,horiz=TRUE,box.lty=0,cex=.4,plot=FALSE)
    
    #Adjust legend plot position
    legend(rng[1]+(((rng[2]-rng[1])/2)-(lg$rect$w/2)),rng[3]+ (lg$rect$h*0.1), legend = paste(leglabs(round(breaks, digits = 1), between = " to ")," g",sep=''), fill = my_colours, bty = "o", ,bg="#FFFFFF", border = NA ,horiz=TRUE,box.lty=0,xpd = NA,cex=.4)
    dev.off()
    system(paste("pdfcrop '",paste(getwd(),"/Map_out/","LSOACO2_",LAD_plot,"_",plot_vars_LSOA[i],".pdf",sep=''),"' '",paste("/Users/alex/Map_out/","LSOACO2_",LAD_plot,"_",plot_vars_LSOA[i],".pdf",sep=''),"'",sep=""),wait=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE)
    rm(breaks,rng,lg)
  }#Check if worth plotting
  rm(tmp_plot_vars)
}






#########################
# Latex
#########################

#COntext
LAD_Names_Full <- unique(LAD@data)[,1:2]

#Create Map lookup table

map_lookup <- as.data.frame(cbind(as.character(Choro_Vars$Numerator),paste(Choro_Vars$Table_description,": ",Choro_Vars$Description," (",Choro_Vars$Numerator,")",sep='')))
colnames(map_lookup) <- c("Variable","Description")
f <- c("All","Work_Home","Rail_light_etc","Train","Bus","Taxi","Motorcycle","Driving","Passenger","Bicycle","Foot","Other")
ff <- as.data.frame(cbind(f,paste("Travel to Work:",gsub("_"," ",f),"Flows")))
colnames(ff) <- colnames(map_lookup) <- c("Variable","Description")
map_lookup <-  rbind(map_lookup,ff)
gg <- cbind(paste(DfT_Lookup$Variable),paste(DfT_Lookup$Description," in ",DfT_Lookup$Year," (",DfT_Lookup$Table,": ",DfT_Lookup$Variable,")",sep=''))
colnames(gg) <- colnames(map_lookup) <- c("Variable","Description")
map_lookup <-  rbind(map_lookup,gg)

hh<- as.data.frame(cbind(c("Primary","Secondary"),c("Estimate of Average CO^2 grams emitted during the journey to Primary school (LSOA)","Estimate of Average CO^2 grams emitted during the journey to Secondary school (LSOA)")))
colnames(hh) <- colnames(map_lookup) <- c("Variable","Description")
map_lookup <-  rbind(map_lookup,hh)

#Create Maps...

#Create a list of the maps, and a table - variable lookup
maps_list <- list.files(path="./Map_Out/")
maps_list <- gsub(".pdf","",maps_list)
maps_list <- gsub("PCT","",maps_list)
maps_list <- strsplit(maps_list,"_")
maps_list <- data.frame(matrix(unlist(maps_list), nrow=length(maps_list), byrow=T))
colnames(maps_list) <- c("Type","Table","Variable")
maps_list$Scale <- as.character(maps_list$Type)
maps_list$Scale[maps_list$Type == "FL"] <- "MSOA"



  
  LAD_Name_No_Non_Char <- LAD_Names_Full[LAD_Names_Full$CODE == LAD_plot,"NAME"]
  LAD_Name_No_Non_Char <- gsub("'","",LAD_Name_No_Non_Char, fixed = TRUE)
  LAD_Name_No_Non_Char <- gsub(",","",LAD_Name_No_Non_Char, fixed = TRUE)
  LAD_Name_No_Non_Char <- gsub(".","",LAD_Name_No_Non_Char, fixed = TRUE)
    
    #Create a LAD tex file
    file.create(paste(getwd(),"/",LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".tex",sep=''))
    #Open tex file for edits
    fileConn<-file((paste(getwd(),"/",LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".tex",sep='')))
    
    #Get the name of the LAD
    LAD_name <- LAD_Names_Full[LAD_Names_Full$CODE == LAD_plot,"NAME"]
    
    #Create the header
    header <- paste("\\documentclass[a4paper,10pt]{article}
                    \\pagenumbering{gobble}
                    \\makeatletter
                    \\renewcommand{\\@dotsep}{10000} 
                    \\makeatother
                    \\usepackage{helvet}
                    \\renewcommand{\\familydefault}{\\sfdefault}
                    \\setlength{\\textwidth}{13cm}
                    \\setlength{\\oddsidemargin}{2.5cm}
                    \\setlength{\\evensidemargin}{2.5cm}
                    \\setlength{\\topmargin}{1cm}
                    \\title{",paste("Transport Map Book (",gsub("'","\\'",LAD_name)," / ", LAD_plot,")",sep=''),"}
                    \\author{Alex D Singleton}
                    \\usepackage[hidelinks]{hyperref}
                    \\usepackage{needspace}
                    \\usepackage{float}
                    \\usepackage[pdftex]{graphicx}
                    \\usepackage[margin=2cm]{geometry}
                    \\usepackage{subfigure}
                    \\usepackage{caption}
                    \\usepackage{graphicx}
                    \\needspace{.25\\textheight}
                    \\begin{document}
                    \\maketitle
                    \\phantomsection
                    \\label{listfigs}
                    \\listoffigures
                    \\clearpage
                    \\graphicspath{{",paste(getwd(),"/Map_Out/",sep=''),"}}",sep="")
    
  
  #Setup Figure List
  fig_list <- NULL
  
    
  for (z in 1:nrow(maps_list)){#ATLAS loop
  
      map_id <- paste(maps_list[z,1],"_",maps_list[z,2],"_",maps_list[z,3],sep="")#create the map id
      caption <- paste(as.character(map_lookup[map_lookup$Variable == as.character(maps_list[z,3]),"Description"])," (",maps_list[z,4],")",sep='')
      variable_id <- as.character(maps_list[z,3])
      
      assign("content_temp",paste(" 
                                  \\begin{minipage}{\\textwidth}
                                  \\begin{figure}[H]
                                  \\centering
                                  \\hyperref[listfigs]{\\includegraphics[width=15cm,height=20cm,keepaspectratio]{",map_id,".pdf}}
                                  \\caption{", caption,"}
                                  \\end{figure}
                                                          
                                  \\noindent \\tiny Variable ID -- ",variable_id,".  \\\\  Contains National Statistics data \\copyright{}  Crown copyright and database right 2014. \\\\ 
                                  Contains Ordnance Survey data \\copyright{}  Crown copyright and database right 2014.  \\\\  Map created by Alex Singleton www.alex-singleton.com.
                                  \\end{minipage}",sep=""))
      
      
      fig_list <- c(fig_list,content_temp)
      
      
      rm(map_id,caption,variable_id)
      
      
    }#End atlas loop  
    
    #Adds content to the tex file
    
    footer <- "\\end{document}"
    
    content <- as.character(fig_list)
    
    writeLines(c(header,content,footer), fileConn)
    close(fileConn)


#Create LaTex document
system(paste("pdflatex '",getwd(),"/",paste(LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".tex'",sep=''),sep=''),wait=FALSE,ignore.stdout = FALSE, ignore.stderr = FALSE)
system(paste("pdflatex '",getwd(),"/",paste(LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".tex'",sep=''),sep=''),wait=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE)

cover_location <- "/Users/alex/Google Drive/Projects/Transport_MapBook/Cover.pdf"
atlas_location <- paste("/Users/alex/",LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".pdf'",sep='')
final_atlas_location <- paste("/Users/alex/Google Drive/Projects/Transport_MapBook/Mapbooks/",LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".pdf'",sep='')

  
  #Cleanup
  system(paste("pdftk ","'",cover_location,"' '",atlas_location," cat output '",final_atlas_location,sep=''),wait = TRUE)#Add Cover

file.remove(paste("",getwd(),"/",LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".tex",sep=''))#Remove Latex File
file.remove(paste("/Users/alex/",LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".aux",sep=''))#Remove tmp files
file.remove(paste("/Users/alex/",LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".lof",sep=''))#Remove tmp files
file.remove(paste("/Users/alex/",LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".log",sep=''))#Remove tmp files
file.remove(paste("/Users/alex/",LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".out",sep=''))#Remove tmp files
file.remove(paste("/Users/alex/",LAD_plot,"_",gsub(" ","_",LAD_Name_No_Non_Char),".pdf",sep=''))#Remove tmp files
system(paste("find ",paste("'",getwd(),"/Map_Out/'",sep='')," -name '*.pdf' -delete",sep=''),wait = TRUE)#Remove maps

rm(LAD_plot)
}#End LAD Map Loop

