
# This step creates the count of bat passes per night and per site 
# Table created for species: table Max_Mean_contacts_ModActiSp.csv
# Table created for guilds: table Max_Mean_contacts_ModActiGuilde.csv

# Because 4 microphones were used we can choose to use 
# the mean, median or max count of bat passes among the 4 microphones

library(plyr)
library(gtools)
library(stringr)

# Load density table (all bat passes semi-automatically validated for all nights and sites)
Valid00 = read.table("./bat-road-collision-risks/Tables/IdValid_Total_Sites.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "",quote="")

# Load metadata 
# DateNuit = operational nights for each sampled site
# Variables = Coordinates and landscape description
DateNuit = read.table("./bat-road-collision-risks/Tables/Liste_DateNuit.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "",quote="")
Variables = read.table("./bat-road-collision-risks/Tables/Table_points_obs_2016_et_2017_COORDONNEES.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "",quote="")
lsx=unique(as.character(DateNuit$Point2)) # Vector of site names

# Create unique ID by date and site
Valid00$TimestampValid=paste(Valid00$Site,Valid00$Date_nuit,sep="_")
DateNuit$TimestampDateNuit=paste(DateNuit$Point,DateNuit$Date,sep="_")

# Merge density table with metadata
Valid0=merge(Valid00,DateNuit[,3:4],by.x="TimestampValid",by.y="TimestampDateNuit")

# Standardise species names
Valid0$ID<-ifelse(substring(Valid0$ID,1,3)=="Ple", "Plesp", as.character(Valid0$ID))
Valid0$ID<-ifelse(substring(Valid0$ID,1,6)=="Pipkuh"| substring(Valid0$ID,1,6)=="Pipnat", 
                  "Pip35", as.character(Valid0$ID))
Valid0$ID<-ifelse(substring(Valid0$ID,1,6)=="Myomyo"| substring(Valid0$ID,1,6)=="Myobly", 
                  "MyoGT", as.character(Valid0$ID))
Valid0$ID<-ifelse(substring(Valid0$ID,1,3)=="Chi"|substring(Valid0$ID,1,3)=="mic"|
                    substring(Valid0$ID,1,3)=="ois"|substring(Valid0$ID,1,3)=="ort"|
                    substring(Valid0$ID,1,3)=="ros"|substring(Valid0$ID,1,3)=="veh", 
                  "parasi", as.character(Valid0$ID))
Valid=subset(Valid0,Valid0$ID!="parasi"&Valid0$ID!="Pipsp")

# Add a column for Guild affiliation
Valid$Guilde<-ifelse(substring(Valid$ID,1,3)=="Rhi"|substring(Valid$ID,1,5)=="Myosp"|
                       substring(Valid$ID,1,3)=="Bar"|substring(Valid$ID,1,4)=="MyoG"|
                       substring(Valid$ID,1,3)=="Ple", 
                     "Ferme",
                     
                     (ifelse(substring(Valid$ID,1,3)=="Min"|substring(Valid$ID,1,4)=="Pipk"|
                               substring(Valid$ID,1,4)=="Pipp"|substring(Valid$ID,1,4)=="Pip3"|
                               substring(Valid$ID,1,3)=="Hyp"|substring(Valid$ID,1,4)=="Pipn",
                             "SemiOuvert",
                             
                             (ifelse(substring(Valid$ID,1,3)=="Ept"|substring(Valid$ID,1,3)=="Nyc"|
                                       substring(Valid$ID,1,3)=="Tad", 
                                     "Ouvert", "NA")))))

# Aggregate count of bat passes by site, night and microphone
ValidAggrSp <- count(Valid, c('Point2','Date_nuit','Lieu','ID')) # Lieu indicates microphone ID
ValidGuilde=subset(Valid,Valid$Guilde!="NA")
ValidAggrGuilde <- count(ValidGuilde, c('Point2','Date_nuit','Lieu','Guilde'))

# Compute max, median and mean of bat passes by site and by night among all 4 microphones

{ # For each Species
  
  Point2<-vector(length=0)
  ValidAggrSpMax<-matrix(nrow=0,ncol=0)
  ValidAggrSpMean<-matrix(nrow=0,ncol=0)
  ValidAggrSpMed<-matrix(nrow=0,ncol=0)
  for (k in 1:length(lsx)){
    Site_k<-sub( "_.*", "", as.character(lsx[k]))
    ValidAggrSp0=subset(ValidAggrSp, ValidAggrSp$Point2 == Site_k)
    ValidAggrSp00=ValidAggrSp0
    ValidAggrSpMax_Site<-aggregate(ValidAggrSp00$freq,by=list(as.character(ValidAggrSp00$Date_nuit),ValidAggrSp00$ID),max)
    ValidAggrSpMean_Site<-aggregate(ValidAggrSp00$freq,by=list(as.character(ValidAggrSp00$Date_nuit),ValidAggrSp00$ID),mean)
    ValidAggrSpMed_Site<-aggregate(ValidAggrSp00$freq,by=list(as.character(ValidAggrSp00$Date_nuit),ValidAggrSp00$ID),median)
    ValidAggrSpMax=rbind(ValidAggrSpMax,ValidAggrSpMax_Site)
    ValidAggrSpMean=rbind(ValidAggrSpMean,ValidAggrSpMean_Site)
    ValidAggrSpMed=rbind(ValidAggrSpMed,ValidAggrSpMed_Site)
    Nom_Site<-rep(Site_k,nrow(ValidAggrSpMax_Site))
    Point2<-c(Point2,Nom_Site)
  }
  
  Max_contacts_ModActiSp=cbind(Point2,ValidAggrSpMax,ValidAggrSpMean[,3],ValidAggrSpMed[,3])
  colnames(Max_contacts_ModActiSp)=c("Point2","Date_nuit","Espece","Max","Mean","Median")
}

{ # For each Guild
  Point2<-vector(length=0)
  ValidAggrMax<-matrix(nrow=0,ncol=0)
  ValidAggrMean<-matrix(nrow=0,ncol=0)
  ValidAggrMed<-matrix(nrow=0,ncol=0)
  for (k in 1:length(lsx)){
    Site_k<-sub( "_.*", "", as.character(lsx[k]))
    ValidAggr0=subset(ValidAggrGuilde, ValidAggrGuilde$Point2 == Site_k)
    ValidAggr00=ValidAggr0
    ValidAggrMax_Site<-aggregate(ValidAggr00$freq,by=list(as.character(ValidAggr00$Date_nuit),ValidAggr00$Guilde),max)
    ValidAggrMean_Site<-aggregate(ValidAggr00$freq,by=list(as.character(ValidAggr00$Date_nuit),ValidAggr00$Guilde),mean)
    ValidAggrMed_Site<-aggregate(ValidAggr00$freq,by=list(as.character(ValidAggr00$Date_nuit),ValidAggr00$Guilde),median)
    ValidAggrMax=rbind(ValidAggrMax,ValidAggrMax_Site)
    ValidAggrMean=rbind(ValidAggrMean,ValidAggrMean_Site)
    ValidAggrMed=rbind(ValidAggrMed,ValidAggrMed_Site)
    Nom_Site<-rep(Site_k,nrow(ValidAggrMax_Site))
    Point2<-c(Point2,Nom_Site)
  }
  
  Max_contacts_ModActiGuilde=cbind(Point2,ValidAggrMax,ValidAggrMean[,3],ValidAggrMed[,3])
  colnames(Max_contacts_ModActiGuilde)=c("Point2","Date_nuit","Espece","Max","Mean","Median")
  
}

# Detection of incomplete nights (because of weak batteries in bat recorders)
# Count hours containing recordings fo each night
Valid$timestamp <- as.POSIXct(paste(Valid$Date,paste(Valid$Heure,Valid$Minute,Valid$Seconde,sep=":"),sep=" "), format = "%d/%m/%Y %H:%M:%S")
Valid$timestampTrunc <- trunc(Valid$timestamp, "hours")
ValidAggr2 <- count(Valid, c('Point2',"Date_nuit",'timestampTrunc','Lieu'))
NHparN=count(ValidAggr2[,1:4],c('Point2','timestampTrunc','Date_nuit'))
NHparN2=count(NHparN[,1:3],c('Point2','Date_nuit'))
colnames(NHparN2)=c("Point2","Date_nuit","N_heures")
NHparN2$Annee=str_sub(NHparN2$Date_nuit,-4,-1)

# Remove incomplete nights (less than 8 hours) for the species table
Max_contacts_ModActiSp2=merge(Max_contacts_ModActiSp,NHparN2)
Max_contacts_ModActiSp3=subset(Max_contacts_ModActiSp2,Max_contacts_ModActiSp2$N_heures>7)

# Remove incomplete nights (less than 8 hours) for the Guild table
Max_contacts_ModActiGuilde2=merge(Max_contacts_ModActiGuilde,NHparN2)
Max_contacts_ModActiGuilde3=subset(Max_contacts_ModActiGuilde2,Max_contacts_ModActiGuilde2$N_heures>7)

# Merge with coordinates and landscape description metadata
Paysage=cbind(as.character(Variables$id),as.character(Variables$Paysage),
              as.character(Variables$H_moy),as.character(Variables$Larg_route),
              as.character(Variables$Dist_veg),as.character(Variables$X),
              as.character(Variables$Y))
Paysage=as.data.frame(Paysage)
colnames(Paysage)=c("id","Paysage","H_veg","L_route","D_veg","X","Y")

Max_contacts_ModActiSp3$Site=gsub( "b", "", as.character(Max_contacts_ModActiSp3$Point2))
Max_contacts_ModActiSp4=merge(Max_contacts_ModActiSp3,Paysage,by.x="Site",by.y="id")
Max_contacts_ModActiGuilde3$Site=gsub( "b", "", as.character(Max_contacts_ModActiGuilde3$Point2))
Max_contacts_ModActiGuilde4=merge(Max_contacts_ModActiGuilde3,Paysage,by.x="Site",by.y="id")

# Save tables
write.table(Max_contacts_ModActiSp4,"./bat-road-collision-risks/Tables/Max_Mean_contacts_ModActiSp.csv",sep=";",row.names=F)
write.table(Max_contacts_ModActiGuilde4,"./bat-road-collision-risks/Tables/Max_Mean_contacts_ModActiGuilde.csv",sep=";",row.names=F)






