#bird<- Bird Data
#vms<- VMS Data

closest <- matrix(NA, nrow(birds), 3)
colnames(closest) <- c("ID", "Distance", "Speed")
for(i in 1:nrow(birds))
{
  
  i<-67
  
  #extract x and y coordinates of vessels
  temp <- vms[, match(c("lat", "long"), colnames(vms))]
  
  #calculate distance from current vessel
  temp$long <- temp$long - birds$x[i]
  temp$lat <- temp$lat - birds$y[i]
  temp <- temp ^ 2
  temp <- sqrt(rowSums(temp))
  
  tempRow<-which(temp == min(temp))
  
  
  
  print(tempRow)
    
  #extract closest vessel
  closest[i,1] <- vms$ID[tempRow]
  closest[i,2] <- min(temp)
  closest[i,3] <- vms$speed[tempRow]
}
