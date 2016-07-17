
D<-read.csv("AllBirds1.csv",header=T)

#Set all NA values to NA string
D$TORT_9L[is.na(D$TORT_9L)] <- "NA"

#IfElse speed and tort parametres
for(j in 1:length(D$TORT_9L) ) {

  if ( (D$TORT_9L[j] >= 0) & ( D$TORT_9L[j] <= 0.9) ){
    D$behaviour[j] <- "foraging"
  } else if (D$TORT_9L[j] == "NA") {
    D$behaviour[j] <- "NA"
  } else if (D$Speed[j] >= 1.5 & D$Speed[j] <= 9) {
    D$behaviour[j] <- "foraging"
  } else {
    D$behaviour[j] <- "none-foraging"
  }

  
}


write.csv(D,"C:/Users/Zoe/Documents/Masters/Research Project/Data analysis/All Data/AllBirdsBehaviour.csv", row.names=FALSE)  

 
# Exclude all values at night 
# set up time/date stamp 

D$DT<-as.POSIXct(strptime(paste(D$date,D$time), "%d/%m/%Y %H:%M:%S",tz="GMT"))

#Set day, month and year into seperate column for NOAA excel 

D$Day <- format( as.POSIXct(strptime(paste(D$date,D$time), "%d/%m/%Y %H:%M:%S",tz="GMT")), '%d')
D$Month <- format( as.POSIXct(strptime(paste(D$date,D$time), "%d/%m/%Y %H:%M:%S",tz="GMT")), '%m')
D$Year <- format( as.POSIXct(strptime(paste(D$date,D$time), "%d/%m/%Y %H:%M:%S",tz="GMT")), '%Y')



write.csv(D,"C:/Users/Zoe/Documents/Masters/Research Project/Data analysis/All Data/AllBirdsBehaviour.csv", row.names=FALSE)



#Subset via night/ day column after NOAA calculation

NOAA<-read.csv("AllBirdsBehaviour1.csv",header=T)

NOAA$time[""] <- "0:00"

D_NOAA<- subset(NOAA, NOAA$Day_Night == "day")

write.csv(D_NOAA,"C:/Users/Zoe/Documents/Masters/Research Project/Data analysis/All Data/AllBirdsBehaviourExclusive.csv", row.names=FALSE)


################################################################################################

#Subset by time 
DT <- subset(D, format(D$DT, '%H') >= '05' & format(D$DT, '%H') <= '21')

D<-read.csv("AllBirds1.csv",header=T)


h <- c(800, 220000,200,230000)
j<-  c(0,0, 0, 0)

dtt <- sprintf("%04d", h)
as.POSIXlt(dtt, format="%H%M")

for(i in 1:length(h)) {
  
  if (dtt[i] <= 210000){
    j[i]<- dtt[i]
  } else if (dtt[i] >= 210000){
    j[i]<- print ("")
  }
  
}

head(j)
DATA<- data.frame(j)
DATA[DATA==""]<-NA
head(DATA)

#subset if NA 
ALLDATA<-subset(DATA, !is.na(DATA$j))


#############################################

for(i in 1:length(D$DT)) {
  
  if (D$DT[i] <= 21:00:00){
    D$DDT[i]<- D$DT[i]
  } else if (D$DT[i] >= 21:00:00){
    D$DDT[i]<- print ("")
  }
  
}


DATA[DATA==""]<-NA
head(DATA)

#subset if NA 
ALLDATA<-subset(DATA, !is.na(DATA$j))


###########################################

df <- data.frame(POSIXtime = seq(as.POSIXct('2013-08-02 12:00'),
                                 as.POSIXct('2013-08-06 05:00'), len = 45),
                 x = seq(45))

sub.4 <- subset(df, D$DT2 >= as.POSIXct('2013-08-05 18:00') &
                  D$DT2 <= as.POSIXct('2013-08-05 22:45'))

sub.3 <- subset(df, format(POSIXtime, '%H') %in% c('12', '13', '14', '15', '16'))



#t <- subset(D, D$DT >= as.POSIXct('21:00') &
#D$DT <= as.POSIXct('05:00'))


###########################################
D<-read.csv("AllBirds1.csv",header=T)

D$DT<-as.POSIXct(strptime(paste(D$date,D$time), "%d/%m/%Y %H:%M:%S",tz="GMT"))


library(lubridate)

date1 <- as.POSIXct("2010-07-06 05:00:00")
date2 <- as.POSIXct("2010-08-03 21:00:00")
int <- interval(date1, date2)

DAT<-D[D$DT %within% int,]

###########################################

D<-read.csv("AllBirds1.csv",header=T)

D$DT<-as.POSIXct(strptime(paste(D$date,D$time), "%d/%m/%Y %H:%M:%S",tz="GMT"))


require(xts)
XT<-xts(D[,-1],order.by=D$DT)
XT["T02:00:00/T05:00:00"]






