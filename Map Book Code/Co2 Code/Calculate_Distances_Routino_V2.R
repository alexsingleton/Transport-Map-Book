#Requires Education_Data_Prep.R to be run

###Setup libraries####
install.packages("rjson", dependencies = TRUE)
library("rjson")
install.packages("RCurl", dependencies = TRUE)
library(RCurl)
install.packages("geosphere", dependencies = TRUE)
library("geosphere")

#### Create OD

#OD <- subset(ed_data,NCyearActual_SPR11 == "8")

#OD <- ed_data[sample(nrow(ed_data), 1000), ] 


OD_split <- split(ed_data,ed_data$LA..code.)


all_LA <- NULL



for(j in 1:length(names(OD_split))) { 

  LA <- names(OD_split)[j]
  nam <- paste("LA_",LA, sep="")
  
  #assign(paste(nam),subset(ed_data,LA..code. == paste(LA)))
  
  OD <- subset(ed_data,LA..code. == paste(LA))


final <- NULL
fileloc <- "/Users/alex/quickest-all.txt"

#Type Codes - R=Routino; SL=strightline SP = Same Postcode MV = missing values

for (i in 1:nrow(OD))  {
  
   UID <- OD[i,]$PupilMatchingRefAnonymous_SPR11
   Mode <-OD[i,]$ModeOfTravel_SPR11
   Pupil_Lat <-OD[i,]$Pupil_Lat
   Pupil_Lon <-OD[i,]$Pupil_Lon
   School_Lat <-OD[i,]$School_Lat
   School_Lon <-OD[i,]$School_Lon
   Postcode <-OD[i,]$Postcode
   PCD <-OD[i,]$Postcode_SPR11
   URN_SPR11 <-OD[i,]$URN_SPR11
   LSOA<-OD[i,]$LSOA
   EthnicGroupMajor_SPR11 <-OD[i,]$EthnicGroupMajor_SPR11
   Gender_SPR11 <-OD[i,]$Gender_SPR11
   LAEstab_SPR11 <-OD[i,]$LAEstab_SPR11
   NCyearActual_SPR11 <-OD[i,]$NCyearActual_SPR11
   acorncategory <-OD[i,]$acorncategory
   acorngroup <-OD[i,]$acorngroup
   acorntype <-OD[i,]$acorntype
   EstablishmentName <-OD[i,]$EstablishmentName
   LA..code.<-OD[i,]$LA..code.
   TypeOfEstablishment..name. <-OD[i,]$TypeOfEstablishment..name.
   IMD_2010_S <-OD[i,]$IMD_2010_S
   IMD_2010_D <-OD[i,]$IMD_2010_D
   IMD_2010_R <-OD[i,]$IMD_2010_R
   PhaseOfEducation..name. <-OD[i,]$PhaseOfEducation..name.

   
   
   if (is.na(Pupil_Lat)||is.na(Pupil_Lon)||is.na(School_Lat)||is.na(School_Lat)) {
     
     type <- "MV"
     
     distance <- NA
     
     tmp <- c(UID, Mode, Pupil_Lat, Pupil_Lon, School_Lat, School_Lon, Postcode, PCD, URN_SPR11, LSOA, EthnicGroupMajor_SPR11, Gender_SPR11, LAEstab_SPR11, NCyearActual_SPR11, acorncategory, acorngroup, acorntype, EstablishmentName, LA..code., TypeOfEstablishment..name., IMD_2010_D, IMD_2010_S, IMD_2010_R, PhaseOfEducation..name., distance,type)
     
     
     final <- rbind(final,tmp)
     
     
     print(i) 
     
     rm(list=setdiff(ls(), c("ed_data","i","final","OD","fileloc","nam","j","OD_split","all_LA")))
     
     
   } else {
   
   
 
   if (Postcode != PCD){
     
      #Loop to assign the transport variabe
      if (Mode %in% c('Walk','WLK')) {
        
        transport <- "foot"
        
      } else {
        
        transport <- "motorcar"
      }
   
   router <- paste("router --transport=",transport," --prefix=gb --quickest  --lon1=",Pupil_Lon," --lat1=",Pupil_Lat," --lon2=",School_Lon,"   --lat2=",School_Lat,"  --output-text-all --quiet --profiles=/Users/alex/routino-2.2/xml/routino-profiles.xml --dir=/Users/alex/routino-2.2/",sep='')
      
     
   system(router, wait=TRUE) # Send the routing command
   
  suppressWarnings(try_read <- try(read.delim(fileloc, header = FALSE, sep = "\t", skip = 4)))
   
   if ((file.exists(fileloc)) && (!inherits(try_read, 'try-error'))){
   
     
   routeresults <- read.delim(fileloc, header = FALSE, sep = "\t", skip = 4)
   distance <- max(routeresults$V7)
   
   type <- "R"
   
   tmp <- c(UID, Mode, Pupil_Lat, Pupil_Lon, School_Lat, School_Lon, Postcode, PCD, URN_SPR11, LSOA, EthnicGroupMajor_SPR11, Gender_SPR11, LAEstab_SPR11, NCyearActual_SPR11, acorncategory, acorngroup, acorntype, EstablishmentName, LA..code., TypeOfEstablishment..name., IMD_2010_D, IMD_2010_S, IMD_2010_R, PhaseOfEducation..name., distance,type)
   
   
   final <- rbind(final,tmp)
      
  system(" rm /Users/alex/quickest-all.txt", wait=TRUE)
   
      
   rm(list=setdiff(ls(), c("ed_data","i","final","OD","fileloc","nam","j","OD_split","all_LA")))
   
   
   print(i)

   
   } else { #do straight line distance
     
     
     p1 <- c(-Pupil_Lon,Pupil_Lat) #pupil
     p2 <- c(-School_Lon,School_Lat) #pupil
     
    distance <-  distHaversine(p1,p2) / 1000
   
   
       type <- "SL"
       
     tmp <- c(UID, Mode, Pupil_Lat, Pupil_Lon, School_Lat, School_Lon, Postcode, PCD, URN_SPR11, LSOA, EthnicGroupMajor_SPR11, Gender_SPR11, LAEstab_SPR11, NCyearActual_SPR11, acorncategory, acorngroup, acorntype, EstablishmentName, LA..code., TypeOfEstablishment..name., IMD_2010_D, IMD_2010_S, IMD_2010_R, PhaseOfEducation..name., distance,type)
     
     
     final <- rbind(final,tmp)
   

        print(i) 
       
     rm(list=setdiff(ls(), c("ed_data","i","final","OD","fileloc","nam","j","OD_split","all_LA")))
     
   }
   
    } else {
      
      distance <- 0
      
      type <- "SP"
      
      tmp <- c(UID, Mode, Pupil_Lat, Pupil_Lon, School_Lat, School_Lon, Postcode, PCD, URN_SPR11, LSOA, EthnicGroupMajor_SPR11, Gender_SPR11, LAEstab_SPR11, NCyearActual_SPR11, acorncategory, acorngroup, acorntype, EstablishmentName, LA..code., TypeOfEstablishment..name., IMD_2010_D, IMD_2010_S, IMD_2010_R, PhaseOfEducation..name., distance, type)
      
      
      final <- rbind(final,tmp)

      rm(list=setdiff(ls(), c("ed_data","i","final","OD","fileloc","nam","j","OD_split","all_LA")))
      
      
      print(i)
      
    }
   
}
   
   
   }

colnames(final) <- c('UID', 'Mode', 'Pupil_Lat', 'Pupil_Lon', 'School_Lat', 'School_Lon', 'Postcode', 'PCD', 'URN_SPR11', 'LSOA', 'EthnicGroupMajor_SPR11', 'Gender_SPR11', 'LAEstab_SPR11', 'NCyearActual_SPR11', 'acorncategory', 'acorngroup', 'acorntype', 'EstablishmentName', 'LA..code.', 'TypeOfEstablishment..name.', 'IMD_2010_D', 'IMD_2010_S', 'IMD_2010_R', 'PhaseOfEducation..name.', 'distance', 'type')

assign(paste(nam),final)

write.csv(final,file=paste("/Volumes/Macintosh HD 2/Dropbox/Projects/School_Carbon/Final Version/",nam,".csv",sep=''))

all_LA <-rbind(all_LA,get(nam))
  
}






rm(list=setdiff(ls(), c("ed_data","OD_split")))


write.csv(all_LA,file="/Volumes/Macintosh HD 2/Dropbox/Projects/School_Carbon/Final Version/all_LA_With_Distances.csv")



#NEXT STEP RUN - Rail_Tram_Distances.R




