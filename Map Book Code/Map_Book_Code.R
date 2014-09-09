
setwd("/Users/alex/Google Drive/Projects/Transport_MapBook/")

library(classInt)
library(RColorBrewer)
library(rgeos)
library(maptools)
library(TeachingDemos)


        
        
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
colnames(OD) <- c("Workplace","Residence","All","Work_Home","Rail-light-etc","Train","Bus","Taxi","Motorcycle","Driving","Passenger","Bicycle","Foot","Other","LAD11CD_workplace","LAD11CD_residence","x_workplace","y_workplace","x_residence","y_residence")


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


#### LATEX PREP ####

#COntext
LAD_Names_Full <- unique(LAD@data)[,1:2]

#Create Map lookup table

map_lookup <- as.data.frame(cbind(as.character(Choro_Vars$Numerator),paste(Choro_Vars$Table_description,": ",Choro_Vars$Description," (",Choro_Vars$Numerator,")",sep='')))
colnames(map_lookup) <- c("Variable","Description")
f <- c("All","Work_Home","Rail-light-etc","Train","Bus","Taxi","Motorcycle","Driving","Passenger","Bicycle","Foot","Other")
ff <- as.data.frame(cbind(f,paste("Travel to Work:",gsub("_"," ",f),"Flows")))
colnames(ff) <- colnames(map_lookup) <- c("Variable","Description")
map_lookup <-  rbind(map_lookup,ff)
gg <- cbind(paste(DfT_Lookup$Variable),paste(DfT_Lookup$Description," in ",DfT_Lookup$Year," (",DfT_Lookup$Table,": ",DfT_Lookup$Variable,")",sep=''))
colnames(gg) <- colnames(map_lookup) <- c("Variable","Description")
map_lookup <-  rbind(map_lookup,gg)

hh<- as.data.frame(cbind(c("Primary","Secondary"),c("Estimate of Average $CO^2$ grams emitted during the journey to Primary school (LSOA)","Estimate of Average $CO^2$ grams emitted during the journey to Secondary school (LSOA)")))
colnames(hh) <- colnames(map_lookup) <- c("Variable","Description")
map_lookup <-  rbind(map_lookup,hh)


####


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



