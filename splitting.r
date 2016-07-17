#bird<- Bird Data
#vms<- VMS Data

#Instantiate vector
closest<-NULL

#Loop through each of the birds
for(i in 1:nrow(birds))
{
  #Output to confirm script is actually doing something
  print(i)
  
  #reset vmsUse so we can refill it per bird
  vmsUse <- NULL
  
  #Get time 30 mins before and after bird tracker ping
  birdsMin <- format( as.POSIXct(strptime(paste(birds$MinTime[i]), "%H:%M:%S",tz="GMT")), '%H:%M:%S')
  birdsMax <- format( as.POSIXct(strptime(paste(birds$MaxTime[i]), "%H:%M:%S",tz="GMT")), '%H:%M:%S')
  
  #Start Boat data loop
  for( j in 1:nrow(vms))
  {
    
    #Get boat VMS data ping time
    boatTime <- format( as.POSIXct(strptime(paste(vms$timeofday[j]), "%H:%M:%S",tz="GMT")), '%H:%M:%S')
      
    #Check - if boat time between min & max bird times
    if( ( birdsMin < boatTime ) & ( boatTime < birdsMax ) ){
      #if TRUE, save current Boat information to vmsUse
      vmsUse <- rbind( vmsUse, data.frame(vms$ID[j], vms$timeofday[j], vms$lat[j], vms$long[j], vms$speed[j], vms$geartype[j]) )
    }
    
    #Output every 12000 boat Records for confirmation of life
    if( j %% 12000 == 0 ) {
      print(j)
    }
  
  }
  
  #Set colnames for vmsUse
  colnames(vmsUse) <- c("ID", "timeofday", "lat", "long", "speed", "geartype")
  
  #extract x and y coordinates of vessels
  temp <- vmsUse[, match(c("lat", "long"), colnames(vmsUse))]
  
  #calculate distance from current vessel
  temp$long <- temp$long - birds$x[i]
  temp$lat <- temp$lat - birds$y[i]
  temp <- temp ^ 2
  temp <- sqrt(rowSums(temp))
  
  #grab first value in case multiple (boat might not move?)
  tempRow<-head(which(temp == min(temp)),1)
  
  #Append dataset to closest, our final output for closest boat to bird
  closest<-rbind(closest,data.frame(vmsUse$ID[tempRow], min(temp), vmsUse$speed[tempRow], vmsUse$geartype[tempRow]))
}

#set colnames for closest
colnames(closest) <- c("closestBoatID", "distancetoBoat", "boatSpeed", "boatGearType")

#write to dataframe
name<-data.frame(closest)

#output to CSV
write.csv(name,"C:/Users/Edd/Documents/R_Files/Gannets/Scotland/Scotland_Closest_new.csv", row.names = F)
