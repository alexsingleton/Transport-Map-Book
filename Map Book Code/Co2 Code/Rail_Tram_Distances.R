install.packages("FNN", dependencies = TRUE)
install.packages("rgdal", dependencies = TRUE)
install.packages("doBy", dependencies = TRUE)
install.packages("sqldf", dependencies = TRUE)
install.packages("RPostgreSQL", dependencies = TRUE)
install.packages("stringr", dependencies = TRUE)


library("FNN")
library("rgdal")
library(doBy)
library(sqldf)
library("RPostgreSQL")
library(stringr)

load("/Users/alex/Dropbox/Projects/School_Carbon/Final/Railway_Analysis.RData")


#Set working directory
setwd("/Volumes/Macintosh HD 2/Dropbox/Projects/School_Carbon/Final/")#work
setwd("/Users/alex/Dropbox/Projects/School_Carbon/Final")#laptop


#Read in previous results
all_LA <- read.csv(file="all_LA_With_Distances.csv")

#Create ID
id<- as.vector(seq(1:nrow(all_LA)))
all_LA<-cbind(all_LA,id)


#Download ONS Postcode Directory (ONSPD) from MySociety
temp <- tempfile()
download.file("http://parlvid.mysociety.org:81/os/ONSPD_FEB_2012_UK_O.zip",temp) 
NSPD <- read.csv(unz(temp, "ONSPD_FEB_2012_UK_O.csv"),head = FALSE) 
unlink(temp)

#Cut NSPD down and apply to all_LA...

NSPD2 <- subset(NSPD, !is.na(V10) & !is.na(V11), select=c("V1","V10","V11"))
NSPD2$V1   <- gsub(" ","", NSPD2$V1, fixed=TRUE)

colnames(NSPD2) <- c("Postcode","pupil_East","pupil_North")
all_LA <- merge(all_LA, NSPD2, by.x='PCD', by.y='Postcode', all.x=TRUE)

colnames(NSPD2) <- c("Postcode","school_East","school_North")
all_LA <- merge(all_LA, NSPD2, by.x='Postcode', by.y='Postcode', all.x=TRUE)



rail <- all_LA[(!is.na(all_LA$pupil_North) & !is.na(all_LA$pupil_East) & !is.na(all_LA$school_North) & !is.na(all_LA$school_East) & all_LA$Mode=='TRN'),]
tram <- all_LA[(!is.na(all_LA$pupil_North) & !is.na(all_LA$pupil_East) & !is.na(all_LA$school_North) & !is.na(all_LA$school_East) & all_LA$Mode=='MTL'),]
tube <- all_LA[(!is.na(all_LA$pupil_North) & !is.na(all_LA$pupil_East) & !is.na(all_LA$school_North) & !is.na(all_LA$school_East) & all_LA$Mode=='LUL'),]

###########################################
## TTTTTTT RRRRRR    AAA   MM    MM  SSSSS  
#    TTT   RR   RR  AAAAA  MMM  MMM SS      
#    TTT   RRRRRR  AA   AA MM MM MM  SSSSS  
#    TTT   RR  RR  AAAAAAA MM    MM      SS 
#    TTT   RR   RR AA   AA MM    MM  SSSSS  
##########################################




##########
#TRAMS >>> Pupils
##########


#Create pupil / Tram location objects

pupil_tram_xy <- as.matrix(tram[,c("pupil_East","pupil_North")])

tram_locations <- readOGR("Rail", "all_tram_light_rail_stations")

tram_tram_xy <- as.matrix(cbind(tram_locations@data$x,tram_locations@data$y))


#Nearest Neighbour - Find closest tram stop to a pupil home
nn = get.knnx(tram_tram_xy,pupil_tram_xy,1) #Nearest Neighbour
colnames(tram_locations@data) <- c("network","PUPIL_TRAM_ID","tram_x_pupil","tram_y_pupil")
tram_match <- cbind(tram,tram_locations@data[nn$nn.index,]) #Append Nearest Neighbour
tram_match <- cbind(tram_match,(nn$nn.dist/1000)) #Append Distance to Nearest Neighbour
colnames(tram_match)[37] <- "distance_station"


#Check the data for outliers caused by an error in mode choice

#Calculate Quartiles and IQR
Quartiles_tram <- summaryBy(distance_station ~ network, data = tram_match, 
                          FUN = function(x) { c(IQR = IQR(x,na.rm=TRUE, type= 7),m = quantile(x,na.rm=TRUE)) } )
colnames(Quartiles_tram) <- c("network","IQR","","Q1","","Q3","")
Quartiles_tram <- subset(Quartiles_tram,  select=c(network, IQR, Q1, Q3))

tram_match <- merge(tram_match,Quartiles_tram, by= "network", all.x=TRUE)

attach(tram_match)
tram_match$outlier_tram <- ifelse((distance_station < (Q1 - (1.5 * IQR)) | distance_station > (Q3 + (1.5 * IQR))),1,0)                    
detach(tram_match)

tram_match$IQR <- NULL
tram_match$Q1 <- NULL
tram_match$Q3 <- NULL



#remove those tram journeys where the distance to the tram stop is greater than the distance to the school
attach(tram_match)
tram_match$SL_to_school <- sqrt((pupil_East-school_East)^2 + (pupil_North-school_North)^2)  / 1000
detach(tram_match)

tram_match$outlier_tram[tram_match$SL_to_school < tram_match$distance_station] <- 1

                                       



##########
#TRAMS >>> Schools
##########


#Get a table of unique schools and their locations from the tram_match data
detach("package:RPostgreSQL")

schools <- sqldf("select LAEstab_SPR11, network, school_East, school_North from tram_match group by LAEstab_SPR11, school_East, school_North ")
school_tram_xy <- as.matrix(schools[,c("school_East","school_North")])


#Nearest Neighbour - Find closest tram stop to a school
nn = get.knnx(tram_tram_xy,school_tram_xy,1) #Nearest Neighbour
colnames(tram_locations@data) <- c("network","SCHOOL_TRAM_ID","tram_x_school","tram_y_school")
tram_school_match <- cbind(schools,tram_locations@data[nn$nn.index,]) #Append Nearest Neighbour
tram_school_match <- subset(tram_school_match,select=c("LAEstab_SPR11","network","SCHOOL_TRAM_ID","tram_x_school","tram_y_school"))
tram_school_match <- cbind(tram_school_match,(nn$nn.dist/1000)) #Append Distance to Nearest Neighbour

colnames(tram_school_match)[6] <- "distance_station"



#Find median SL distance of pupils travelling by tram to school & use to detect outliers
average_SL_to_school_tram <- aggregate(SL_to_school~LAEstab_SPR11, data=tram_match, FUN="median")
tram_school_match <- merge(tram_school_match, average_SL_to_school_tram, by.x='LAEstab_SPR11', by.y='LAEstab_SPR11', all.x=TRUE)

attach(tram_school_match)
tram_school_match$outlier_tram_school <- ifelse(distance_station > SL_to_school,1,0)                    
detach(tram_school_match)

tram_school_match <- subset(tram_school_match,select=c('LAEstab_SPR11','outlier_tram_school','SCHOOL_TRAM_ID'))

#Join schools back onto tram_match & add an outlier flag to the pupil record if the school is considered too far from a station to be viable
tram_match <- merge(tram_match, tram_school_match, by="LAEstab_SPR11", all.x=TRUE)
tram_match$outlier_tram <- ifelse(tram_match$outlier_tram_school == 1,1,tram_match$outlier_tram)                    

#Add outlier flag if the journeys cross netwok (usually found in london - i.e. docklands / croydon)

tram_match$pupil_net <- str_split_fixed(tram_match$PUPIL_TRAM_ID, "_",2)[,1]
tram_match$school_net <- str_split_fixed(tram_match$SCHOOL_TRAM_ID, "_",2)[,1]

tram_match$outlier_tram <- ifelse(tram_match$pupil_net != tram_match$school_net,1,tram_match$outlier_tram)                    

  

#Remove records detected as outliers and add these to a new error dataset - also remove added variables and reorder to match all_LA
error_tram <- tram_match[tram_match$outlier_tram == 1,]
error_tram <- subset(error_tram,select=colnames(all_LA))
tram_match <- tram_match[tram_match$outlier_tram == 0,]




#Cleanup
remove(list= c('Quartiles_tram','average_SL_to_school_tram','pupil_tram_xy','school_tram_xy','schools','tram_school_match','tram_tram_xy','id','nn','temp'))


#################################
#  RRRRRR    AAA   IIIII LL      
#  RR   RR  AAAAA   III  LL      
#  RRRRRR  AA   AA  III  LL      
#  RR  RR  AAAAAAA  III  LL      
#  RR   RR AA   AA IIIII LLLLLLL 
#################################






##########
#RAIL >>> Pupils
##########


#Create pupil / Tram location objects

pupil_rail_xy <- as.matrix(rail[,c("pupil_East","pupil_North")])

rail_locations <- readOGR("Rail", "station_point")

rail_rail_xy <- as.matrix(cbind(rail_locations@data$x,rail_locations@data$y))

#Manual intervention to prevent an issue with an equidistant point
pupil_rail_xy[27269,1] <- pupil_rail_xy[27269,1] +1

#Nearest Neighbour - Find closest tram stop to a pupil home
nn = get.knnx(rail_rail_xy,pupil_rail_xy,1) #Nearest Neighbour
colnames(rail_locations@data) <- c("","","","rail_x_pupil","rail_y_pupil","PUPIL_RAIL_ID")
rail_locations@data <- rail_locations@data[,c("PUPIL_RAIL_ID","rail_x_pupil","rail_y_pupil")]
rail_match <- cbind(rail,rail_locations@data[nn$nn.index,]) #Append Nearest Neighbour
rail_match <- cbind(rail_match,(nn$nn.dist/1000)) #Append Distance to Nearest Neighbour
colnames(rail_match)[36] <- "distance_station"




#Check the data for outliers caused by an error in mode choice

#Calculate Quartiles and IQR
rail_match$all <- "all"
Quartiles_rail <- summaryBy(distance_station~all, data = rail_match, 
                            FUN = function(x) { c(IQR = IQR(x,na.rm=TRUE, type= 7),m = quantile(x,na.rm=TRUE)) } )

colnames(Quartiles_rail) <- c("network","IQR","","Q1","","Q3","")
Quartiles_rail <- subset(Quartiles_rail,  select=c(network, IQR, Q1, Q3))


Q1 <- as.numeric(Quartiles_rail["Q1"])
Q3 <- as.numeric(Quartiles_rail["Q3"])
IQR <- as.numeric(Quartiles_rail["IQR"])

attach(rail_match)
rail_match$outlier_rail <- ifelse((distance_station < (Q1 - (1.5 * IQR)) | distance_station > (Q3 + (1.5 * IQR))),1,0)                    
detach(rail_match)




#remove those tram journeys where the distance to the tram stop is greater than the distance to the school

rail_match$SL_to_school <- sqrt((rail_match$pupil_East-rail_match$school_East)^2 + (rail_match$pupil_North-rail_match$school_North)^2)  / 1000
rail_match$outlier_rail[rail_match$SL_to_school < rail_match$distance_station] <- 1





##########
#Rail >>> Schools
##########


#Get a table of unique schools and their locations from the tram_match data
schools <- sqldf("select LAEstab_SPR11, school_East, school_North from rail_match group by LAEstab_SPR11, school_East, school_North ")
school_rail_xy <- as.matrix(schools[,c("school_East","school_North")])


#Nearest Neighbour - Find closest tram stop to a school
nn = get.knnx(rail_rail_xy,school_rail_xy,1) #Nearest Neighbour
colnames(rail_locations@data) <- c("SCHOOL_RAIL_ID","rail_x_school","rail_y_school")
rail_school_match <- cbind(schools,rail_locations@data[nn$nn.index,]) #Append Nearest Neighbour
rail_school_match <- subset(rail_school_match,select=c("LAEstab_SPR11","SCHOOL_RAIL_ID","rail_x_school","rail_y_school"))
rail_school_match <- cbind(rail_school_match,(nn$nn.dist/1000)) #Append Distance to Nearest Neighbour

colnames(rail_school_match)[5] <- "distance_station"



#Find median SL distance of pupils travelling by tram to school & use to detect outliers
average_SL_to_school_rail <- aggregate(SL_to_school~LAEstab_SPR11, data=rail_match, FUN="median")
rail_school_match <- merge(rail_school_match, average_SL_to_school_rail, by.x='LAEstab_SPR11', by.y='LAEstab_SPR11', all.x=TRUE)

attach(rail_school_match)
rail_school_match$outlier_rail_school <- ifelse(distance_station > SL_to_school,1,0)                    
detach(rail_school_match)

rail_school_match <- subset(rail_school_match,select=c('LAEstab_SPR11','outlier_rail_school','SCHOOL_RAIL_ID'))

#Join schools back onto tram_match & add an outlier flag to the pupil record if the school is considered too far from a station to be viable
rail_match <- merge(rail_match, rail_school_match, by="LAEstab_SPR11", all.x=TRUE)

rail_match$outlier_rail <- ifelse(rail_match$outlier_rail_school == 1,1,rail_school_match$outlier_rail)                    

#Remove records detected as outliers and add these to a new error dataset - also remove added variables and reorder to match all_LA
error_rail <- rail_match[rail_match$outlier_rail == 1,]
error_rail <- subset(error_rail,select=colnames(all_LA))
rail_match <- rail_match[rail_match$outlier_rail == 0,]




#Cleanup
remove(list= c('Quartiles_rail','average_SL_to_school_rail','pupil_rail_xy','school_rail_xy','schools','rail_school_match','rail_rail_xy','nn','IQR','Q1','Q3'))

  
  
  
#  TTTTTTT UU   UU BBBBB   EEEEEEE 
#    TTT   UU   UU BB   B  EE      
#    TTT   UU   UU BBBBBB  EEEEE   
#    TTT   UU   UU BB   BB EE      
#    TTT    UUUUU  BBBBBB  EEEEEEE 
#




##########
#TUBE >>> Pupils
##########


#Create pupil / Tube location objects

pupil_tube_xy <- as.matrix(tube[,c("pupil_East","pupil_North")])

tube_locations <- readOGR("Rail", "tube_stations")

tube_tube_xy <- as.matrix(cbind(tube_locations@data$x,tube_locations@data$y))



#Nearest Neighbour - Find closest tube stop to a pupil home
nn = get.knnx(tube_tube_xy,pupil_tube_xy,1) #Nearest Neighbour
colnames(tube_locations@data) <- c("tube_x_pupil","tube_y_pupil","PUPIL_TUBE_ID")
tube_locations@data <- tube_locations@data[,c("PUPIL_TUBE_ID","tube_x_pupil","tube_y_pupil")]
tube_match <- cbind(tube,tube_locations@data[nn$nn.index,]) #Append Nearest Neighbour
tube_match <- cbind(tube_match,(nn$nn.dist/1000)) #Append Distance to Nearest Neighbour
colnames(tube_match)[36] <- "distance_station"




#Check the data for outliers caused by an error in mode choice

#Calculate Quartiles and IQR
tube_match$all <- "all"
Quartiles_tube <- summaryBy(distance_station~all, data = tube_match, 
                            FUN = function(x) { c(IQR = IQR(x,na.rm=TRUE, type= 7),m = quantile(x,na.rm=TRUE)) } )

colnames(Quartiles_tube) <- c("network","IQR","","Q1","","Q3","")
Quartiles_tube <- subset(Quartiles_tube,  select=c(network, IQR, Q1, Q3))


Q1 <- as.numeric(Quartiles_tube["Q1"])
Q3 <- as.numeric(Quartiles_tube["Q3"])
IQR <- as.numeric(Quartiles_tube["IQR"])

attach(tube_match)
tube_match$outlier_tube <- ifelse((distance_station < (Q1 - (1.5 * IQR)) | distance_station > (Q3 + (1.5 * IQR))),1,0)                    
detach(tube_match)




#remove those tram journeys where the distance to the tram stop is greater than the distance to the school

tube_match$SL_to_school <- sqrt((tube_match$pupil_East-tube_match$school_East)^2 + (tube_match$pupil_North-tube_match$school_North)^2)  / 1000
tube_match$outlier_tube[tube_match$SL_to_school < tube_match$distance_station] <- 1





##########
#Tube >>> Schools
##########


#Get a table of unique schools and their locations from the tube_match data
detach("package:RPostgreSQL")
schools <- sqldf("select LAEstab_SPR11, school_East, school_North from tube_match group by LAEstab_SPR11, school_East, school_North ")
school_tube_xy <- as.matrix(schools[,c("school_East","school_North")])


#Nearest Neighbour - Find closest tram stop to a school
nn = get.knnx(tube_tube_xy,school_tube_xy,1) #Nearest Neighbour
colnames(tube_locations@data) <- c("SCHOOL_TUBE_ID","tube_x_school","tube_y_school")
tube_school_match <- cbind(schools,tube_locations@data[nn$nn.index,]) #Append Nearest Neighbour
tube_school_match <- subset(tube_school_match,select=c("LAEstab_SPR11","SCHOOL_TUBE_ID","tube_x_school","tube_y_school"))
tube_school_match <- cbind(tube_school_match,(nn$nn.dist/1000)) #Append Distance to Nearest Neighbour

colnames(tube_school_match)[5] <- "distance_station"



#Find median SL distance of pupils travelling by tube to school & use to detect outliers
average_SL_to_school_tube <- aggregate(SL_to_school~LAEstab_SPR11, data=tube_match, FUN="median")
tube_school_match <- merge(tube_school_match, average_SL_to_school_tube, by.x='LAEstab_SPR11', by.y='LAEstab_SPR11', all.x=TRUE)

attach(tube_school_match)
tube_school_match$outlier_tube_school <- ifelse(distance_station > SL_to_school,1,0)                    
detach(tube_school_match)

tube_school_match <- subset(tube_school_match,select=c('LAEstab_SPR11','outlier_tube_school','SCHOOL_TUBE_ID'))

#Join schools back onto tram_match & add an outlier flag to the pupil record if the school is considered too far from a station to be viable
tube_match <- merge(tube_match, tube_school_match, by="LAEstab_SPR11", all.x=TRUE)

tube_match$outlier_tube <- ifelse(tube_match$outlier_tube_school == 1,1,tube_school_match$outlier_tube)                    

#Remove records detected as outliers and add these to a new error dataset - also remove added variables and reorder to match all_LA
error_tube <- tube_match[tube_match$outlier_tube == 1,]
error_tube <- subset(error_tube,select=colnames(all_LA))
tube_match <- tube_match[tube_match$outlier_tube == 0,]




#Cleanup
remove(list= c('Quartiles_tube','average_SL_to_school_tube','pupil_tube_xy','school_tube_xy','schools','tube_school_match','tube_tube_xy','nn','IQR','Q1','Q3'))



#################################################
# Calculate network distances for TUBE
library("RPostgreSQL")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

tube_out <- NULL

for (i in 1:nrow(tube_match)) {

pupil_tube_id <- tube_match[i,'PUPIL_TUBE_ID']
school_tube_id <- tube_match[i,'SCHOOL_TUBE_ID']
row_id <- tube_match[i,'id']

o_q <-  paste("SELECT tube_id.id FROM public.tube_id where tube_id.\"UID\" = '",pupil_tube_id,"';",sep="")
d_q <-  paste("SELECT tube_id.id FROM public.tube_id where tube_id.\"UID\" = '",school_tube_id,"';",sep="")

origin <- dbGetQuery(con, o_q)
destination <- dbGetQuery(con, d_q)


query <- paste("select * from shortest_path('select gid as id, start_id::int4 as source,end_id::int4 as target,shape_leng::float8 as cost from tube_network',",origin,",",destination,", false, false);", sep='')
temp <- dbGetQuery(con, query)
temp_d <- sum(temp$cost)

temp_out <- cbind(row_id,temp_d)

tube_out <- rbind(tube_out,temp_out)

remove(list= c('pupil_tube_id','school_tube_id','o_q','d_q','origin','destination','query','temp','temp_d','temp_out'))

}
detach("package:RPostgreSQL")

tube_out <- as.data.frame(tube_out)
colnames(tube_out) <- c("row_id","Net_dist")
tube_match <- sqldf("select tube_match.*,tube_out.Net_dist from tube_match left join tube_out where tube_match.id = tube_out.row_id")
remove(tube_out)
#############################





#################################################
# Calculate network distances for RAIL
library("RPostgreSQL")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

rail_out <- NULL


for (i in 1:nrow(rail_match)) {
  
  pupil_rail_id <- rail_match[i,'PUPIL_RAIL_ID']
  school_rail_id <- rail_match[i,'SCHOOL_RAIL_ID']
  row_id <- rail_match[i,'id']
  
  o_q <-  paste("SELECT railway_id.id FROM public.railway_id where railway_id.\"UID\" = '",pupil_rail_id,"';",sep="")
  d_q <-  paste("SELECT railway_id.id FROM public.railway_id where railway_id.\"UID\" = '",school_rail_id,"';",sep="")
  
  origin <- dbGetQuery(con, o_q)
  destination <- dbGetQuery(con, d_q)
  
  
  query <- paste("select * from shortest_path('select gid as id, start_id::int4 as source,end_id::int4 as target,shape_leng::float8 as cost from railway_network',",origin,",",destination,", false, false);", sep='')
  temp <- dbGetQuery(con, query)
  temp_d <- sum(temp$cost)
  
  temp_out <- cbind(row_id,temp_d)
  
  rail_out <- rbind(rail_out,temp_out)
  
  remove(list= c('pupil_rail_id','school_rail_id','o_q','d_q','origin','destination','query','temp','temp_d','temp_out'))
  
}
detach("package:RPostgreSQL")

rail_out <- as.data.frame(rail_out)
colnames(rail_out) <- c("row_id","Net_dist")

rail_match<- sqldf("select rail_match.*,rail_out.Net_dist from rail_match left join rail_out where rail_match.id = rail_out.row_id")
remove(rail_out)
#############################





#################################################
# Calculate network distances for TRAM
library("RPostgreSQL")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

tram_out <- NULL


for (i in 1:nrow(tram_match)) {
  
  pupil_tram_id <- tram_match[i,'PUPIL_TRAM_ID']
  school_tram_id <- tram_match[i,'SCHOOL_TRAM_ID']
  row_id <- tram_match[i,'id']
  network <- tram_match[i,'pupil_net']#both pupil_net and school_net should be the same as differences are excluded
  
  
  o_q <-  paste("SELECT ",network,"_id.id FROM public.",network,"_id where ",network,"_id.\"UID\" = '",pupil_tram_id,"';",sep="")
  d_q <-  paste("SELECT ",network,"_id.id FROM public.",network,"_id where ",network,"_id.\"UID\" = '",school_tram_id,"';",sep="")
  
  origin <- dbGetQuery(con, o_q)
  destination <- dbGetQuery(con, d_q)
  
  
  query <- paste("select * from shortest_path('select gid as id, start_id::int4 as source,end_id::int4 as target,shape_leng::float8 as cost from ",network,"_network',",origin,",",destination,", false, false);", sep='')
  temp <- dbGetQuery(con, query)
  temp_d <- sum(temp$cost)
  
  temp_out <- cbind(row_id,temp_d)
  
  tram_out <- rbind(tram_out,temp_out)
  
  remove(list= c('pupil_tram_id','school_tram_id','o_q','d_q','origin','destination','query','temp','temp_d','temp_out','network'))
print(i)  
}
detach("package:RPostgreSQL")

tram_out <- as.data.frame(tram_out)
colnames(tram_out) <- c("row_id","Net_dist")

tram_match<- sqldf("select tram_match.*,tram_out.Net_dist from tram_match left join tram_out where tram_match.id = tram_out.row_id")
remove(tram_out)
#############################




#Create cutdown dataset for merge back onto the original data
#Error
error_rail_cutdown <- cbind(subset(error_rail, select = c('id')),"E")
error_tube_cutdown <- cbind(subset(error_tube, select = c('id')),"E")
error_tram_cutdown <- cbind(subset(error_tram, select = c('id')),"E")
error_all_cutdown <- rbind(error_rail_cutdown,error_tube_cutdown,error_tram_cutdown)
error_all_cutdown <- cbind(error_all_cutdown,NA)
colnames(error_all_cutdown) <- c('id','type','net_distance')
error_all_cutdown$network_type <- "error"

#Non Error
rail_match_cutdown <- cbind(subset(rail_match, select = c('id','Net_dist')),"OK")
tube_match_cutdown <- cbind(subset(tube_match, select = c('id','Net_dist')),"OK")
tram_match_cutdown <- cbind(subset(tram_match, select = c('id','Net_dist','pupil_net')),"OK")
rail_match_cutdown$pupil_net <- "rail"
tube_match_cutdown$pupil_net <- "tube"

all_match_cutdown <- rbind(rail_match_cutdown,tube_match_cutdown,tram_match_cutdown)
colnames(all_match_cutdown) <- c('id','net_distance','type')

all_match_cutdown <- subset(all_match_cutdown, select = c('id','type','net_distance'))
colnames(all_match_cutdown) <- c('id','type','net_distance','network_type')

final_net_distances <- rbind(all_match_cutdown,error_all_cutdown)

#Big merge!!!

all_LA2 <- merge(all_LA, final_net_distances, by='id', all.x=TRUE)



all_LA2$final_distance <- ifelse(!is.na(all_LA2$net_distance),all_LA2$net_distance,all_LA2$distance) #Network distances
all_LA2$final_type <- ifelse(!is.na(all_LA2$net_distance),"ND",as.character(all_LA2$type.x))#Change type field on the basis of above

all_LA2$final_type <- ifelse(all_LA2$Mode %in% c('TRN','MTL','LUL') & is.na(all_LA2$type.y),"MV",as.character(all_LA2$final_type))

#Calculate straight line

all_LA2$sl_distance <- ifelse(!is.na(all_LA2$final_distance),(sqrt((all_LA2$pupil_East - all_LA2$school_East)^2 + (all_LA2$pupil_North - all_LA2$school_North)^2) / 1000),NA)



#Write out the distances to PostgresDB

library("RPostgreSQL")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="localhost", dbname="school_carbon", user="postgres", password="") 

dbWriteTable(con, 'Spring_Census_2011_WD', all_LA2)

dbDisconnect(con)


#Next run CO2 model which also contains the outlier detection
