#bird<- Bird Data
#vms<- VMS Data

closest <- matrix(NA, nrow(birds), 3)
colnames(closest) <- c("ID", "Distance", "Speed")
for(i in 1:nrow(birds))
{
  #extract x and y coordinates of vessels
  temp <- vms[, match(c("lat", "long"), colnames(vms))]
  
  #calculate distance from current vessel
  temp$long <- temp$long - birds$x[i]
  temp$lat <- temp$lat - birds$y[i]
  temp <- temp ^ 2
  temp <- sqrt(rowSums(temp))
  
  #In case this returns more than one boat with the same distance - use the first one
  tempRow<-head(which(temp == min(temp)),1)
    
  #extract closest vessel
  closest[i,1] <- vms$ID[tempRow]
  closest[i,2] <- min(temp)
  closest[i,3] <- vms$speed[tempRow]
}
