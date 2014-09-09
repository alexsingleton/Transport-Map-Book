#Requires Create_Pupil_OD_data_V2 to have been run - creates the OD file


#router --transport=motorcar --prefix=gb --quickest --lon1=-0.220510 --lat1=51.76016 --lon2=-0.216146   --lat2=51.76058 --quiet --output-text-all

###Setup libraries####
#Also need: sudo apt-get install libpq-dev
install.packages("RPostgreSQL", dependencies = TRUE)

library("RPostgreSQL")

install.packages("rjson", dependencies = TRUE)
library("rjson")
#Also need to run: sudo apt-get install libcurl4-openssl-dev
install.packages("RCurl", dependencies = TRUE)
library(RCurl)


############ Setup connection to DB############
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="Projects", user="postgres", password="agra123") 
###############################################

OD <- dbReadTable(con, "OD")


OD_Sample <- OD[sample(nrow(OD), size=5000), ]


#Routino


final <- NULL
final_error <- NULL
fileloc <- "/home/alex/quickest-all.txt"


for (i in 1:nrow(OD_Sample))  {
  
   UID <- OD_Sample[i,]$KS4_PupilMatchingRefAnonymous
   Mode <-OD_Sample[i,]$MODEOFTRAVEL_SPR08
   Pupil_Lat <-OD_Sample[i,]$Pupil_Lat
   Pupil_Lon <-OD_Sample[i,]$Pupil_Lon
   School_Lat <-OD_Sample[i,]$School_Lat
   School_Lon <-OD_Sample[i,]$School_Lon
   Postcode <-OD_Sample[i,]$Postcode
   PCD <-OD_Sample[i,]$PCD
 
   if (Postcode != PCD){
     
      #Loop to assign the transport variabe
      if (Mode %in% c('Walk','WLK')) {
        
        transport <- "foot"
        
      } else {
        
        transport <- "motorcar"
      }
   
   router <- paste("router --transport=",transport," --prefix=gb --quickest  --lon1=",Pupil_Lon," --lat1=",Pupil_Lat," --lon2=",School_Lon,"   --lat2=",School_Lat,"  --output-text-all --quiet",sep='')
     
   system(router, wait=TRUE) # Send the routing command
   
  suppressWarnings(try_read <- try(read.delim(fileloc, header = FALSE, sep = "\t", skip = 4)))
  
   
   if ((file.exists(fileloc)) && (!inherits(try_read, 'try-error'))){
   
   routeresults <- read.delim(fileloc, header = FALSE, sep = "\t", skip = 4)
   distance <- max(routeresults$V7)
   tmp <- c(UID,distance)
   final <- rbind(final,tmp)
   
   unlink("/home/alex/quickest-all.txt")
   
   remove("try_read","tmp","distance","routeresults","router","transport","School_Lon","School_Lat","Pupil_Lon","Pupil_Lat","Mode","UID")

   
   } else {
     
   tmp <- c(UID,PCD,Postcode)
   final_error <- rbind(final_error,tmp)
   remove("try_read","tmp","router","transport","School_Lon","School_Lat","Pupil_Lon","Pupil_Lat","Mode","UID")

     
   }
   
    }
   }


#####################Create sample from Routino data of 500###

OD_Sample <- data.frame(final[sample(nrow(final), size=500), ])

names(OD_Sample) <- c("KS4_PupilMatchingRefAnonymous","Routino_Dist")

temp <- subset(OD, select=c(KS4_PupilMatchingRefAnonymous, MODEOFTRAVEL_SPR08, Pupil_Lat, Pupil_Lon,School_Lat,School_Lon,Postcode,PCD))

OD_Sample <- merge(OD_Sample, temp, all.x=TRUE, by = "KS4_PupilMatchingRefAnonymous")




#########################Google#################################



OD_Out <-NULL

i <- 1

for (i in 392:500)  {
   Sys.sleep((runif(1, 1, 5)))
   
   UID <- as.character(OD_Sample[i,]$KS4_PupilMatchingRefAnonymous)
   Routino_Dist <- as.numeric(as.character((OD_Sample[i,]$Routino_Dist)))
   Pupil_Lat <- (OD_Sample[i,]$Pupil_Lat)
   Pupil_Lon <- (OD_Sample[i,]$Pupil_Lon)
   School_Lat <- (OD_Sample[i,]$School_Lat)
   School_Lon <-(OD_Sample[i,]$School_Lon)
   Postcode <- (OD_Sample[i,]$Postcode)
   PCD <- (OD_Sample[i,]$PCD)
   O <- paste(Pupil_Lat,",",Pupil_Lon,sep='')
   D <- paste(School_Lat,",",School_Lon,sep='')
   Mode <-OD_Sample[i,]$MODEOFTRAVEL_SPR08

 #Loop to assign the transport variabe
      if (Mode %in% c('Walk','WLK')) {
        
        transport <- "walking"
        
      } else {
        
        transport <- "driving"
      }
   
   
   
   #Get directions
   parameters <- paste("json?origin=",O,"&destination=",D,"&sensor=false&mode=",transport,sep = '')

   host <- "http://maps.googleapis.com"
   path <- paste("/maps/api/directions/",parameters,sep='')
   url <- paste(host,path,sep='')

   JSON <-getURL(URLencode(url),.opts = list(timeout = 1, maxredirs = 2, verbose = TRUE))

   json_data <- fromJSON(JSON) 
   
   if (json_data$status == "OK") {
     
     distance <- ((json_data$routes[[1]]$legs[[1]]$distance$value)/1000)
     export <- c(UID,distance, Routino_Dist, Pupil_Lat, Pupil_Lon, School_Lat, School_Lon, Postcode, PCD)
     
   } else {
     
     export <- c(UID,"error", Routino_Dist, Pupil_Lat, Pupil_Lon, School_Lat, School_Lon, Postcode, PCD)
     
   }
   
   OD_Out <- rbind(OD_Out,export)
   print(i)
   export <- NULL
   
}


################################################################

OD_Out <- as.data.frame(OD_Out)

names(OD_Out) <- c("KS4_PupilMatchingRefAnonymous","Google_Dist","Routino_Dist","Pupil_Lat", "Pupil_Lon", "School_Lat", "School_Lon", "Postcode", "PCD")

temp <- subset(OD, select=c(KS4_PupilMatchingRefAnonymous, MODEOFTRAVEL_SPR08))

OD_Sample <- merge(OD_Out, temp, all.x=TRUE, by = "KS4_PupilMatchingRefAnonymous")



#################################Bing##############################




key <- "Aq1nl7Ve2pYARIKm7p1ohImyjEo3yRKfj1pm4UVwhjqg_Jtaq-wcLPDmHzqFZi7O"
host <- "http://dev.virtualearth.net/REST/V1/Routes/"
export_type <- "?o=json"

OD <- OD_Sample


OD_Out <- NULL


#nrow(OD_Subset)

for (i in 1:nrow(OD))  {
  
   Pupil_Lat <- as.character(OD[i,]$Pupil_Lat)
   Pupil_Lon <- as.character(OD[i,]$Pupil_Lon)
   O <- paste(Pupil_Lat,",",Pupil_Lon,sep="")
   School_Lat <- as.character(OD[i,]$School_Lat)
   School_Lon <- as.character(OD[i,]$School_Lon)
   D <- paste(School_Lat,",",School_Lon,sep="")   
   
   UID <- as.character(OD[i,]$KS4_PupilMatchingRefAnonymous)
   Transport <- as.character(OD[i,]$MODEOFTRAVEL_SPR08)
   Google_Dist <- as.character(OD[i,]$Google_Dist)
   Routino_Dist <- as.character(OD[i,]$Routino_Dist)

   
 #Loop to assign the transport variabe
      if (Transport %in% c('Walk','WLK')) {
        
        type <- "Walking"
        
      } else {
        
        type <- "Driving"
      }
 
   
  

   
   #Get directions
   parameters <- paste("&wp.0=",O,"&wp.1=",D,"&key=",sep = '')
   
   url <- paste(host,type,export_type,parameters,key, sep='')

   JSON <-getURL(URLencode(url),.opts = list(timeout = 1, maxredirs = 2, verbose = TRUE))

   json_data <- fromJSON(JSON) 
   
   if (json_data$statusDescription == "OK") {
     
     distance <- json_data$resourceSets[[1]]$resources[[1]]$travelDistance
     export <- c(UID,Google_Dist,Routino_Dist,distance)
     
   } else {
     
     export <- c(UID,Google_Dist,Routino_Dist,"error")
     
   }
   
  OD_Out <- rbind(OD_Out,export)
   
   #assign((as.character(UID)),export)

   print(i)
   #print(url)
   #print(UID)
   rm(list=c("export","O","D","UID","JSON","json_data","parameters","url","type","Routino_Dist","Google_Dist","Transport"))
  
   Sys.sleep((runif(1, 1, 5)))

}


############################Prep#########################

OD_Out <- as.data.frame(OD_Out)

names(OD_Out) <- c("KS4_PupilMatchingRefAnonymous","Google_Dist","Routino_Dist","Bing_Dist")

temp <- subset(OD, select=c(KS4_PupilMatchingRefAnonymous, Pupil_East,Pupil_North, School_East, School_North))

OD_Sample <- merge(OD_Out, temp, all.x=TRUE, by = "KS4_PupilMatchingRefAnonymous")




############################straight line #################



  OD_Sample$Straight_Dist <- sqrt((OD_Sample$Pupil_North-OD_Sample$School_North)^2+(OD_Sample$Pupil_East-OD_Sample$School_East)^2)/1000

write.csv(OD_Sample,file="/home/alex/Dropbox/Publications/Journals/Area/Compare_Distances.csv")

##########################################################
