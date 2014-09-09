
#Get R Mysql packages
install.packages("RPostgreSQL", dependencies = TRUE)
library("RPostgreSQL")

install.packages("doBy", dependencies = TRUE)# Packages for summaryBy
library(doBy)


#Set working directory
setwd("/Volumes/Macintosh HD 2/Dropbox/Projects/School_Carbon/Final/")#work
setwd("/Users/alex/Dropbox/Projects/School_Carbon/Final")#laptop


#Get data from DB


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="", user="", password="") 


query <- "
SELECT 
  \"Spring_Census_2011_WD\".\"Mode\", 
  \"Spring_Census_2011_WD\".\"URN_SPR11\",
  \"Spring_Census_2011_WD\".\"LAEstab_SPR11\",
  \"Spring_Census_2011_WD\".\"LA..code.\",
  \"Spring_Census_2011_WD\".\"NCyearActual_SPR11\",
  \"Spring_Census_2011_WD\".\"PCD\", 
  \"Spring_Census_2011_WD\".\"EthnicGroupMajor_SPR11\", 
  \"Spring_Census_2011_WD\".acorntype, 
  \"Spring_Census_2011_WD\".\"TypeOfEstablishment..name.\", 
  \"Spring_Census_2011_WD\".\"IMD_2010_D\", 
  \"Spring_Census_2011_WD\".\"IMD_2010_R\", 
  \"Spring_Census_2011_WD\".\"PhaseOfEducation..name.\", 
  \"Spring_Census_2011_WD\".final_distance, 
  \"Spring_Census_2011_WD\".final_type,
  \"Spring_Census_2011_WD\".sl_distance,
  \"Spring_Census_2011_WD\".network_type
FROM 
  public.\"Spring_Census_2011_WD\"
WHERE 
  \"Spring_Census_2011_WD\".final_distance IS NOT NULL  AND 
  \"Spring_Census_2011_WD\".\"Mode\" IS NOT NULL ;
"


ed_data <- dbGetQuery(con, query)




##########################
#########Data Prep########
##########################

#Recode Mode of Travel into courser groupings

attach(ed_data)

   ed_data$Mode2[Mode == "PSB" | Mode == "DSB" | Mode == "BNK"] <- "BUS"
   ed_data$Mode2[Mode == "TRN" | Mode == "LUL" | Mode == "MTL"] <- "TRA"
   ed_data$Mode2[Mode == "WLK" | Mode == "CYC" ] <- "NON"
   ed_data$Mode2[Mode == "CRS" | Mode == "CAR" | Mode == "TXI"] <- "CAR"
   ed_data$Mode2[Mode == "BDR" ] <- "BDR"
   ed_data$Mode2[Mode == "OTH" ] <- "OTH"
  
detach(ed_data)


#Create a lookup for outliers

ed_data$Qlookup <- paste(ed_data$LA..code., ed_data$NCyearActual_SPR11, ed_data$Mode2, sep = "")

#Download ONS Postcode Directory (ONSPD) from MySociety
temp <- tempfile()
download.file("http://parlvid.mysociety.org:81/os/ONSPD_FEB_2012_UK_O.zip",temp) 
NSPD <- read.csv(unz(temp, "ONSPD_FEB_2012_UK_O.csv"),head = FALSE) 
unlink(temp)

#Cut NSPD down and apply to ed_data...


NSPD2 <- subset(NSPD, !is.na(V10) & !is.na(V11), select=c("V1","V36","V38"))
NSPD2$V1   <- gsub(" ","", NSPD2$V1, fixed=TRUE)

colnames(NSPD2) <- c("Postcode","CASWARD","LSOA")

ed_data <- merge(ed_data, NSPD2, by.x='PCD', by.y='Postcode', all.x=TRUE)





##########################
###Cleaning Procedures####
##########################

#Identify Outliers based on SL distances


#Calculate Quartiles and IQR
Quartiles_SL <- summaryBy(sl_distance ~ LA..code. + NCyearActual_SPR11  + Mode2, data = ed_data, 
                       FUN = function(x) { c(IQR = IQR(x,na.rm=TRUE, type= 7),m = quantile(x,na.rm=TRUE)) } )

colnames(Quartiles_SL) <- c("LA..code.","NCyearActual_SPR11","Mode2","IQR","","Q1","","Q3","")

Quartiles_SL <- subset(Quartiles_SL,  select=c(LA..code.  , NCyearActual_SPR11, Mode2,  IQR, Q1, Q3))
Quartiles_SL$Qlookup <- paste(Quartiles_SL$LA..code.  , Quartiles_SL$NCyearActual_SPR11, Quartiles_SL$Mode2, sep = "")
Quartiles_SL <- subset(Quartiles_SL,  select=c(Qlookup,  IQR, Q1, Q3))


#Merge Results back onto data frome 

ed_data <- merge(ed_data,Quartiles_SL, by= "Qlookup", all.x=TRUE)

attach(ed_data)

ed_data$outlier_SL <- ifelse((sl_distance < (Q1 - (5 * IQR)) | sl_distance > (Q3 + (5 * IQR))),NA,sl_distance)                    

detach(ed_data)




ed_data$IQR <- NULL
ed_data$Q1 <- NULL
ed_data$Q3 <- NULL


# Identify Outliers based on Travel Distance



#Calculate Quartiles and IQR
Quartiles <- summaryBy(final_distance ~ LA..code. + NCyearActual_SPR11  + Mode2, data = ed_data, 
                  FUN = function(x) { c(IQR = IQR(x,na.rm=TRUE, type= 7),m = quantile(x,na.rm=TRUE)) } )
colnames(Quartiles) <- c("LA..code.","NCyearActual_SPR11","Mode2","IQR","","Q1","","Q3","")
Quartiles <- subset(Quartiles,  select=c(LA..code., NCyearActual_SPR11, Mode2,  IQR, Q1, Q3))
Quartiles$Qlookup <- paste(Quartiles$LA..code., Quartiles$NCyearActual_SPR11, Quartiles$Mode2, sep = "")
Quartiles <- subset(Quartiles,  select=c(Qlookup,  IQR, Q1, Q3))


#Merge Results back onto data frome x

ed_data <- merge(ed_data,Quartiles, by= "Qlookup", all.x=TRUE)



attach(ed_data)

ed_data$final_distance2 <- ifelse((final_distance < (Q1 - (5 * IQR)) | final_distance > (Q3 + (5 * IQR))),NA,outlier_SL)                    

detach(ed_data)



################Trim out remaining junk##########

ed_data$outlier_SL <- NULL

ed_data$IQR <- NULL
ed_data$Q1 <- NULL
ed_data$Q3 <- NULL



###Create outlier flag
ed_data$outlier_binary=ifelse(is.na(ed_data$final_distance2),1,0)


#Remove Distance calculations for anyone who is a border, those without a mode choice
ed_data$final_distance2 <- ifelse(ed_data$Mode == "BDR",NA,ed_data$final_distance2)
ed_data$final_distance2 <- ifelse(ed_data$Mode == "",NA,ed_data$final_distance2)


############Create an ID

#Create ID
id<- as.vector(seq(1:nrow(ed_data)))
ed_data<-cbind(ed_data,id)


############




####Write the results of outlier detection back to DB#######

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

dbWriteTable(con, 'Spring_Census_2011_WD_NOutlier', ed_data)

dbDisconnect(con)


###################################
###Create CO2 Estimates for Cars###
###################################


#Get Cars Data


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

query <- "

SELECT 
\"Spring_Census_2011_WD_NOutlier\".\"id\",
\"Spring_Census_2011_WD_NOutlier\".\"URN_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LAEstab_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LA..code.\", 
  \"Spring_Census_2011_WD_NOutlier\".\"NCyearActual_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LSOA\", 
  \"Spring_Census_2011_WD_NOutlier\".\"CASWARD\",
  \"Spring_Census_2011_WD_NOutlier\".\"EthnicGroupMajor_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".acorntype, 
  \"Spring_Census_2011_WD_NOutlier\".\"TypeOfEstablishment..name.\", 
  \"Spring_Census_2011_WD_NOutlier\".\"IMD_2010_D\", 
  \"Spring_Census_2011_WD_NOutlier\".\"IMD_2010_R\", 
  \"Spring_Census_2011_WD_NOutlier\".\"PhaseOfEducation..name.\", 
  \"Spring_Census_2011_WD_NOutlier\".sl_distance, 
  \"Spring_Census_2011_WD_NOutlier\".final_distance2, 
  \"Spring_Census_2011_WD_NOutlier\".\"Mode\", 
  \"Spring_Census_2011_WD_NOutlier\".outlier_binary
FROM 
public.\"Spring_Census_2011_WD_NOutlier\"
WHERE 
\"Spring_Census_2011_WD_NOutlier\".outlier_binary = 0 AND 
  \"Spring_Census_2011_WD_NOutlier\".\"Mode2\" = 'CAR';
"

cars <- dbGetQuery(con, query)



#Append CO2 Data


LSOA_CO2 <-  read.csv("CO2 Data/120314-Cars by LSOA 2011Q2-Singleton-DL.csv") # grm / KM

cars <- merge(cars, LSOA_CO2, by.x='LSOA', by.y='LSOA', all.x=TRUE)

#calculate CO2 model for cars

cars$co2g_route <- ifelse(cars$Mode == "CAR", cars$final_distance2 * cars$AvgCO2,(ifelse(cars$Mode == "CRS",( cars$final_distance2 * cars$AvgCO2)/2,(ifelse(cars$Mode == "TXI",cars$final_distance2 * 150.3,NA)))))



# Car shared are assumed as two pupils ; Taxi Emissions detailed P24: http://www.defra.gov.uk/publications/files/pb13625-emission-factor-methodology-paper-110905.pdf


###################################
###Create CO2 Estimates for Bus###
###################################


#Get Bus Data
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

query <- "

SELECT 
\"Spring_Census_2011_WD_NOutlier\".\"id\",
\"Spring_Census_2011_WD_NOutlier\".\"URN_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LAEstab_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LA..code.\", 
  \"Spring_Census_2011_WD_NOutlier\".\"NCyearActual_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LSOA\", 
  \"Spring_Census_2011_WD_NOutlier\".\"CASWARD\",
  \"Spring_Census_2011_WD_NOutlier\".\"EthnicGroupMajor_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".acorntype, 
  \"Spring_Census_2011_WD_NOutlier\".\"TypeOfEstablishment..name.\", 
  \"Spring_Census_2011_WD_NOutlier\".\"IMD_2010_D\", 
  \"Spring_Census_2011_WD_NOutlier\".\"IMD_2010_R\", 
  \"Spring_Census_2011_WD_NOutlier\".\"PhaseOfEducation..name.\", 
  \"Spring_Census_2011_WD_NOutlier\".sl_distance, 
  \"Spring_Census_2011_WD_NOutlier\".final_distance2, 
  \"Spring_Census_2011_WD_NOutlier\".\"Mode\", 
  \"Spring_Census_2011_WD_NOutlier\".outlier_binary
FROM 
public.\"Spring_Census_2011_WD_NOutlier\"
WHERE 
\"Spring_Census_2011_WD_NOutlier\".outlier_binary = 0 AND 
  \"Spring_Census_2011_WD_NOutlier\".\"Mode2\" = 'BUS';
"

bus <- dbGetQuery(con, query)




#Identify those trips made within London

bus$London <- ifelse(substr(bus$LAEstab_SPR11,1,3) %in% c('201', '202', '203', '204', '205', '206', '207', '208', '209', '210', '211', '212', '213', '301', '302', '303', '304', '305', '306', '307', '308', '309', '310', '311', '312', '313', '314', '315', '316', '317', '318', '319', '320'),1 ,0)



# Average occupancy of a bus is 11
#http://assets.dft.gov.uk/statistics/releases/light-rail-tram-statistics-2011-12/lrt-2011-12.pdf
# however, differences for London: http://www.defra.gov.uk/publications/files/pb13625-emission-factor-methodology-paper-110905.pdf (P31)
# In London - 16.7 ; and non london 6.3 ; also - coach is 16.2. Thus, the gCO2 are:
#Non London Bus - 184.3 ; London Bus - 85.7 ; Coach (which we will assume is a dedicated school bus) 30;

#ANational Travel Survey: 2010 http://www.dft.gov.uk/statistics/releases/national-travel-survey-2010/ states that 85% of households in Great Britain lived within a 6 minute walk of a bus stop. Average walking speed in children is around 70 meters / minute (http://onlinelibrary.wiley.com/doi/10.1002/jor.1100060208/abstract) - thus, a average walk to the bus stop would be around 420m


#Make the assumption that school bus are coaches and all the rest are bus ; account for London on PSB ; Makes the assumption that the agregate of the pupils equates to the distance travelled by a bus

######
#This is really slow - quicker to use ifelse
######
# bus_out <- bus[1,]
# bus_out$co2g_route <- NA
# bus_out <- bus_out[-1,]
# 
# 
# for (i in 1:nrow(bus)) {
#   
#   temp_bus <- bus[i,]
#   
#   if ((temp_bus$Mode == "PSB" | temp_bus$Mode == "BNK") & (temp_bus$London == 1)) { 
#     
#         temp_bus$co2g_route <- temp_bus$distance * 85.7
#   
#   } else if ((temp_bus$Mode == "PSB" | temp_bus$Mode == "BNK") & (temp_bus$London == 0)) {
#     
#         temp_bus$co2g_route <- temp_bus$distance * 184.3
#     
#   } else if (temp_bus$Mode == "DSB"){
#     
#     temp_bus$co2g_route <- temp_bus$distance * 30
#     
#   } else {
#     
#     temp_bus$co2g_route <- NA
#     
#   }
#   
#   bus_out <- rbind (bus_out, temp_bus)
#   temp_bus <- NULL
#   print(i)
#   
# }


#calculate CO2 model for bus
bus$co2g_route <- ifelse(((bus$Mode == "PSB" | bus$Mode == "BNK") & (bus$London == 1)),bus$final_distance2 * 85.7,ifelse(((bus$Mode == "PSB" | bus$Mode == "BNK") & (bus$London == 0)),bus$final_distance2 * 184.3,ifelse(bus$Mode == "DSB",bus$final_distance2 * 30,NA)))

bus$London <- NULL






###################################
###Create CO2 Estimates for Rail### some infor - http://assets.dft.gov.uk/statistics/releases/light-rail-tram-statistics-2011-12/lrt-2011-12.pdf
###################################

#Estimates from http://www.defra.gov.uk/publications/files/pb13625-emission-factor-methodology-paper-110905.pdf P35
# Grm CO2 / person KM
#DLR - 68.3 ; Birmingham - 70.5 ; Newcastle - 103.0 ; Croydon - 44.3 ; Manchester  39.5 ; Nottingham (no data - so use national average) 71 ; Sheffield 96.8 ; 

#National rail - 53.4 ; 
#London Underground - 73.1;


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

query <- "

SELECT 
\"Spring_Census_2011_WD_NOutlier\".\"id\",
\"Spring_Census_2011_WD_NOutlier\".\"URN_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LAEstab_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LA..code.\", 
  \"Spring_Census_2011_WD_NOutlier\".\"NCyearActual_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LSOA\", 
  \"Spring_Census_2011_WD_NOutlier\".\"CASWARD\",
  \"Spring_Census_2011_WD_NOutlier\".\"EthnicGroupMajor_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".acorntype, 
  \"Spring_Census_2011_WD_NOutlier\".\"TypeOfEstablishment..name.\", 
  \"Spring_Census_2011_WD_NOutlier\".\"IMD_2010_D\", 
  \"Spring_Census_2011_WD_NOutlier\".\"IMD_2010_R\", 
  \"Spring_Census_2011_WD_NOutlier\".\"PhaseOfEducation..name.\", 
  \"Spring_Census_2011_WD_NOutlier\".sl_distance, 
  \"Spring_Census_2011_WD_NOutlier\".final_distance2, 
  \"Spring_Census_2011_WD_NOutlier\".\"Mode\", 
  \"Spring_Census_2011_WD_NOutlier\".\"network_type\",
  \"Spring_Census_2011_WD_NOutlier\".outlier_binary
FROM 
public.\"Spring_Census_2011_WD_NOutlier\"
WHERE 
\"Spring_Census_2011_WD_NOutlier\".outlier_binary = 0 AND 
  \"Spring_Census_2011_WD_NOutlier\".\"Mode2\" = 'TRA';
"

rail <- dbGetQuery(con, query)




# 
# rail$LA <- substr(rail$LAEstab_SPR11,1,3)
# 
# rail$London <- ifelse(rail$LA %in% c('201', '202', '203', '204', '205', '206', '207', '208', '209', '210', '211', '212', '213', '301', '302', '303', '304', '305', '306', '307', '308', '309', '310', '311', '312', '313', '314', '315', '316', '317', '318', '319', '320'),1 ,0)

#light rail
#DLR - 68.3 ; Birmingham - 70.5 ; Newcastle - 103.0 ; Croydon - 44.3 ; Manchester  39.5 ; Nottingham (no data - so use national average) 71 ; Sheffield 96.8 ; 

#National rail - 53.4 ; 
#London Underground - 73.1;

rail$CO2g_passenger_km <- NA

#Change values for Train
rail$CO2g_passenger_km <- ifelse(rail$Mode == "TRN",53.4,rail$CO2g_passenger_km) 

#Change values for underground
rail$CO2g_passenger_km <- ifelse(rail$Mode == "LUL",73.1,rail$CO2g_passenger_km) 

#Trams

rail$CO2g_passenger_km <- ifelse(rail$Mode == "MTL" & rail$network_type == "blackpool",71,rail$CO2g_passenger_km) 
rail$CO2g_passenger_km <- ifelse(rail$Mode == "MTL" & rail$network_type == "croydon",44.3,rail$CO2g_passenger_km)
rail$CO2g_passenger_km <- ifelse(rail$Mode == "MTL" & rail$network_type == "docklands",68.3,rail$CO2g_passenger_km)
rail$CO2g_passenger_km <- ifelse(rail$Mode == "MTL" & rail$network_type == "manchester",39.5,rail$CO2g_passenger_km)
rail$CO2g_passenger_km <- ifelse(rail$Mode == "MTL" & rail$network_type == "midland",70.5,rail$CO2g_passenger_km)
rail$CO2g_passenger_km <- ifelse(rail$Mode == "MTL" & rail$network_type == "newcastle",103,rail$CO2g_passenger_km)
rail$CO2g_passenger_km <- ifelse(rail$Mode == "MTL" & rail$network_type == "nottingham",71,rail$CO2g_passenger_km)
rail$CO2g_passenger_km <- ifelse(rail$Mode == "MTL" & rail$network_type == "sheffield",96.8,rail$CO2g_passenger_km)

#CO2 Calculation
rail$co2g_route <- rail$final_distance2 * rail$CO2g_passenger_km





###########################################
###Create CO2 Estimates for Walk / Cycle###
###########################################
#http://www.sciencedirect.com/science/article/pii/S0301421501000611 - 11.4 (walking) ; 8.3 (cycling)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

query <- "

SELECT 
\"Spring_Census_2011_WD_NOutlier\".\"id\",
\"Spring_Census_2011_WD_NOutlier\".\"URN_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LAEstab_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LA..code.\", 
  \"Spring_Census_2011_WD_NOutlier\".\"NCyearActual_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".\"LSOA\", 
  \"Spring_Census_2011_WD_NOutlier\".\"CASWARD\",
  \"Spring_Census_2011_WD_NOutlier\".\"EthnicGroupMajor_SPR11\", 
  \"Spring_Census_2011_WD_NOutlier\".acorntype, 
  \"Spring_Census_2011_WD_NOutlier\".\"TypeOfEstablishment..name.\", 
  \"Spring_Census_2011_WD_NOutlier\".\"IMD_2010_D\", 
  \"Spring_Census_2011_WD_NOutlier\".\"IMD_2010_R\", 
  \"Spring_Census_2011_WD_NOutlier\".\"PhaseOfEducation..name.\", 
  \"Spring_Census_2011_WD_NOutlier\".sl_distance, 
  \"Spring_Census_2011_WD_NOutlier\".final_distance2, 
  \"Spring_Census_2011_WD_NOutlier\".\"Mode\", 
  \"Spring_Census_2011_WD_NOutlier\".outlier_binary
FROM 
public.\"Spring_Census_2011_WD_NOutlier\"
WHERE 
\"Spring_Census_2011_WD_NOutlier\".outlier_binary = 0 AND 
  \"Spring_Census_2011_WD_NOutlier\".\"Mode2\" = 'NON';
"

walk <- dbGetQuery(con, query)





walk$co2g_route <- ifelse(walk$Mode == "WLK", walk$final_distance2 * 11.4, ifelse(walk$Mode == "CYC",walk$final_distance2 * 8.3,NA))






###################################################
########Cut down files and output to DB#############
###################################################




bus <- subset(bus, select = c(id,co2g_route))
rail <- subset(rail, select = c(id,co2g_route))
cars <- subset(cars, select = c(id,co2g_route))
walk <- subset(walk, select = c(id,co2g_route))

output <- rbind (bus,rail,cars,walk)


#Get the full data from DB 

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

query <- "

SELECT *
FROM 
public.\"Spring_Census_2011_WD_NOutlier\";
"

Spring_Census_2011_WD_NOutlier <- dbGetQuery(con, query)



#Create final output to DB



Spring_Census_2011_WD_NOutlier_CO2 <- merge(Spring_Census_2011_WD_NOutlier, output, by = "id", all.x=TRUE)

Spring_Census_2011_WD_NOutlier_CO2$id  <- NULL
Spring_Census_2011_WD_NOutlier_CO2$row.names  <- NULL






#

library("RPostgreSQL")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

dbWriteTable(con, 'Spring_Census_2011_WD_NOutlier_CO2', Spring_Census_2011_WD_NOutlier_CO2)

dbDisconnect(con)

