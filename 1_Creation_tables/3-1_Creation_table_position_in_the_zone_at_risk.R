
# This step attributes a binary position to each bat pass
# 1: in the zone at risk
# 0: not in the zone at risk
# NA: precision insufficient to obtain a position
# Table created: MaDataRisk_nonTrajectoegal0.csv

# 0 values are attributed to all bat passes that were not recorded by the four microphones simultaneously

library(stringr)
library(lubridate)
library(Hmisc)
library(plyr)
library(dplyr)
library (gtools)
library(bazar)

# Load bat trajectory location table (each row is a flight path with SegTraj as ID and containing N bat calls)
# The column Espece (P) stands for primary species and (S) for secondary species
# The column Risque locates the bat trajectory in the zone at risk (RiskCertain) or in the safe zone (SafeCertain)
# NA in the column Risque stand for trajectories that could not provide data precise enough
MaDataRisk0 = read.csv("./bat-road-collision-risks/Tables/RisqueXYZ_Total_Sites.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "",check.names=FALSE)

# Load the same table but each row is a call
MaDataRiskCri = read.csv("./bat-road-collision-risks/Tables/Cri_par_cri_Risque_XYZ_Total_Sites.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "",check.names=FALSE)

# Load density table when all 4 mics were working (all bat passes semi-automatically validated for all nights and sites)
# This special table was created because at the end of some nights, some bat recorders had weak batteries
IDValid = read.csv("./bat-road-collision-risks/Tables/IdValid_Total_Sites_4mics_fonctionnels.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "",check.names=FALSE)

# Standardise species names
IDValid$ID[IDValid$ID == "Pipkuh"] <- "Pip35"
IDValid$ID[IDValid$ID == "Pipnat"] <- "Pip35"
IDValid$ID[IDValid$ID == "Myobly"] <- "MyoGT"
IDValid=IDValid[!grepl("parasi", IDValid$ID),]
IDValid=IDValid[!grepl("ortho", IDValid$ID),]

# If an individual is recorded on more than one microphone simultaneously in the table IDValid
# Keeps only one microphone and deletes the others
rm(IDValid_Site_TRIE_temp)
# For each site
for (k in 1:length(names(table(IDValid$Site)))){
  Site_k=as.numeric(names(table(IDValid$Site)[k]))
  print(paste("Site = ",Site_k))
  IDValid_Site=subset(IDValid,IDValid$Site==Site_k)
  IDValid_Site$MiliSec=as.numeric(substring(IDValid_Site$Fichier,(nchar(as.character(IDValid_Site$Fichier))-6),(nchar(as.character(IDValid_Site$Fichier))-4)))
  IDValid_Site$Timestamp=IDValid_Site$Heure*3600+IDValid_Site$Minute*60+IDValid_Site$Seconde+IDValid_Site$MiliSec/1000
  
  rm(IDValid_Site_Sp_TRIE_temp)
  # For each species
  for (j in 1:length(names(table(droplevels(IDValid_Site$ID))))){
    Sp_j=names(table(droplevels(IDValid_Site$ID))[j])
    print(Sp_j)
    IDValid_Site_Sp=subset(IDValid_Site,IDValid_Site$ID==Sp_j)
    rownames(IDValid_Site_Sp) <- NULL
    IDValidpourCorrRound1=data.frame(as.numeric(as.factor(IDValid_Site_Sp$Date_nuit)), IDValid_Site_Sp$Timestamp)
    colnames(IDValidpourCorrRound1)=c("Date_nuit","Timestamp")
    IDValidCorrRound1a=find.matches(IDValidpourCorrRound1[1:nrow(IDValidpourCorrRound1),],IDValidpourCorrRound1[1:nrow(IDValidpourCorrRound1),],tol=c(0,4.99))
    if(!is.null(dim(IDValidCorrRound1a[[1]]))){
      IDValidCorrRound1b=t(apply(IDValidCorrRound1a[[1]], 1, sort))
      IDValidCorrRound1c=unique(IDValidCorrRound1b)
      mode(IDValidCorrRound1c) = "numeric"
      
      if(nrow(IDValidCorrRound1c)>3){
        Table_Travail=data.frame(IDValidCorrRound1c)
        Table_Travail2=Table_Travail[1:(nrow(Table_Travail)-1),]
        print(nrow(Table_Travail))
        
        while (nrow(Table_Travail)>nrow(Table_Travail2)){
          Table_Travail=Table_Travail2
          
          Table_Travail_A=IDValidpourCorrRound1[rownames(Table_Travail),]
          Table_Travail_B=find.matches(Table_Travail_A[1:nrow(Table_Travail_A),],Table_Travail_A[1:nrow(Table_Travail_A),],tol=c(0,4.99))
          if(!is.null(dim(Table_Travail_B[[1]]))){
            Table_Travail_C=t(apply(Table_Travail_B[[1]], 1, sort))
            mode(Table_Travail_C) = "numeric"
            Table_Travail_C=data.frame(Table_Travail_C)
            
            Table_Travail_C$Garder=TRUE
            for (i in 2:(nrow(Table_Travail_C)-1)){
              if((rowSums(!is.na(Table_Travail_C[i-1,1:(ncol(Table_Travail_C)-1)]))<3) & (rowSums(!is.na(Table_Travail_C[i,1:(ncol(Table_Travail_C)-1)]))>2)
                 & (rowSums(!is.na(Table_Travail_C[i+1,1:(ncol(Table_Travail_C)-1)]))>2)){
                Table_Travail_C$Garder[i]=FALSE
              }
              if((rowSums(!is.na(Table_Travail_C[i-1,1:(ncol(Table_Travail_C)-1)]))>2) & (rowSums(!is.na(Table_Travail_C[i,1:(ncol(Table_Travail_C)-1)]))>2)
                 & (rowSums(!is.na(Table_Travail_C[i+1,1:(ncol(Table_Travail_C)-1)]))<3)){
                Table_Travail_C$Garder[i]=FALSE
              }
              if((rowSums(!is.na(Table_Travail_C[i-1,1:(ncol(Table_Travail_C)-1)]))<3) & (rowSums(!is.na(Table_Travail_C[i,1:(ncol(Table_Travail_C)-1)]))>2)
                 & (rowSums(!is.na(Table_Travail_C[i+1,1:(ncol(Table_Travail_C)-1)]))<3)){
                Table_Travail_C$Garder[i]=FALSE
              }
            }
            Table_Travail2<-Table_Travail_C[!(Table_Travail_C$Garder=="FALSE"),]
            print(nrow(Table_Travail2))
          }}
        if (exists("IDValid_Site_Sp_TRIE_temp")){
          IDValid_Site_Sp_TRIE_temp=rbind(IDValid_Site_Sp_TRIE_temp,
                                          IDValid_Site_Sp[rownames(Table_Travail2),])
        }else{IDValid_Site_Sp_TRIE_temp=IDValid_Site_Sp[rownames(Table_Travail2),]}}
    }else{if (exists("IDValid_Site_Sp_TRIE_temp")){
      IDValid_Site_Sp_TRIE_temp=rbind(IDValid_Site_Sp_TRIE_temp,
                                      IDValid_Site_Sp)
    }else{IDValid_Site_Sp_TRIE_temp=IDValid_Site_Sp}}
  }
  if (exists("IDValid_Site_TRIE_temp")){
    IDValid_Site_TRIE_temp=rbind(IDValid_Site_TRIE_temp,
                                 IDValid_Site_Sp_TRIE_temp)
  }else{IDValid_Site_TRIE_temp=IDValid_Site_Sp_TRIE_temp}
}

MaDataRisk0$Type="Trajectoire"

# If a bat pass in the IDValid table is not in the MaDataRiskCri table
# then the bat pass is considered as in the safe zone and receives a value of 0
# Else the bat pass in the IDValid table is removed

# If the filename of the bat pass in IDValid does not exist in MaDataRiskCri$Fich1 (mic 1)
# the row of IDValid is kept
IDmergeCri1merge=merge(IDValid_Site_TRIE_temp,MaDataRiskCri,by.x="Fichier",by.y="Fich1",all.x=T)
IDmergeCri1sub=subset(IDmergeCri1merge,is.na(IDmergeCri1merge$Fich2))
IDmergeCri1=cbind(as.character(IDmergeCri1sub$Site.x),as.character(IDmergeCri1sub$Fichier),as.character(IDmergeCri1sub$ID))
colnames(IDmergeCri1)=c("Site","Fichier","EspeceP")
IDmergeCri1=as.data.frame(IDmergeCri1)
rm(IDValid_Site_TRIE_temp)

# If the filename of the bat pass in IDValid does not exist in MaDataRiskCri$Fich2 (mic 2)
# the row of IDValid is kept
IDmergeCri2merge=merge(IDmergeCri1,MaDataRiskCri,by.x="Fichier",by.y="Fich2",all.x=T)
IDmergeCri2sub=subset(IDmergeCri2merge,is.na(IDmergeCri2merge$Fich1))
IDmergeCri2=cbind(as.character(IDmergeCri2sub$Site.x),as.character(IDmergeCri2sub$Fichier),as.character(IDmergeCri2sub$EspeceP))
colnames(IDmergeCri2)=c("Site","Fichier","EspeceP")
IDmergeCri2=as.data.frame(IDmergeCri2)
rm(IDmergeCri1merge)
rm(IDmergeCri1sub)
rm(IDmergeCri1)

# If the filename of the bat pass in IDValid does not exist in MaDataRiskCri$Fich3 (mic 3)
# the row of IDValid is kept
IDmergeCri3merge=merge(IDmergeCri2,MaDataRiskCri,by.x="Fichier",by.y="Fich3",all.x=T)
IDmergeCri3sub=subset(IDmergeCri3merge,is.na(IDmergeCri3merge$Fich1))
IDmergeCri3=cbind(as.character(IDmergeCri3sub$Site.x),as.character(IDmergeCri3sub$Fichier),as.character(IDmergeCri3sub$EspeceP))
colnames(IDmergeCri3)=c("Site","Fichier","EspeceP")
IDmergeCri3=as.data.frame(IDmergeCri3)
rm(IDmergeCri2merge)
rm(IDmergeCri2sub)
rm(IDmergeCri2)

# If the filename of the bat pass in IDValid does not exist in MaDataRiskCri$Fich3 (mic 3)
# the row of IDValid is kept
IDmergeCri4merge=merge(IDmergeCri3,MaDataRiskCri,by.x="Fichier",by.y="Fich4",all.x=T)
IDmergeCri4sub=subset(IDmergeCri4merge,is.na(IDmergeCri4merge$Fich1))
IDmergeCri4=cbind(as.character(IDmergeCri4sub$Site.x),as.character(IDmergeCri4sub$Fichier),as.character(IDmergeCri4sub$EspeceP))
colnames(IDmergeCri4)=c("Site","Fichier","EspeceP")
IDmergeCri4=as.data.frame(IDmergeCri4)
rm(IDmergeCri3merge)
rm(IDmergeCri3sub)
rm(IDmergeCri3)

IDnonTrajecto=IDmergeCri4
IDnonTrajecto$SegTraj=NA
IDnonTrajecto$N=NA
IDnonTrajecto$EspeceS=NA
IDnonTrajecto$RisqueXYZ="SafeCertain"
IDnonTrajecto$Type="Contact"
rm(IDmergeCri4merge)
rm(IDmergeCri4sub)
rm(IDmergeCri4)

colnames(MaDataRisk0)[which(names(MaDataRisk0) == "Risque")] <- "RisqueXYZ"
MaDataRisk0=rbind(MaDataRisk0,IDnonTrajecto)
rm(IDnonTrajecto)

# Remove rows that do not correspond to a bat
MaDataRisk0=subset(MaDataRisk0,MaDataRisk0$EspeceP!="parasi" &
                     MaDataRisk0$EspeceP!="oiseau" &
                     MaDataRisk0$EspeceP!="ChiroSp" &
                     MaDataRisk0$EspeceP!="micromamm" &
                     MaDataRisk0$EspeceP!="micomamm" &
                     MaDataRisk0$EspeceP!="micromamm?" &
                     MaDataRisk0$EspeceP!="micromamm?joli" &
                     MaDataRisk0$EspeceP!="rossignol" &
                     MaDataRisk0$EspeceP!="vehiculebizarre" &
                     MaDataRisk0$EspeceP!="ortho" &
                     MaDataRisk0$EspeceP!="ortho?" &
                     MaDataRisk0$EspeceP!="ortho?chelou" &
                     MaDataRisk0$EspeceP!="socialPippyg" &
                     MaDataRisk0$EspeceP!="vehicule" &
                     MaDataRisk0$EspeceP!="vehicule2roues?" 
)

# Create time and date vectors
Fich <- as.character(MaDataRisk0$Fichier)
MaDataRisk0$Date<-substr(Fich,nchar(Fich)-22,nchar(Fich)-15)
MaDataRisk0$Annee<-substr(Fich,nchar(Fich)-22,nchar(Fich)-19)
MaDataRisk0$Mois<-substr(Fich,nchar(Fich)-18,nchar(Fich)-17)
MaDataRisk0$Jour<-substr(Fich,nchar(Fich)-16,nchar(Fich)-15)
MaDataRisk0$Heure<-substr(Fich,nchar(Fich)-13,nchar(Fich)-12)
MaDataRisk0$Minute<-substr(Fich,nchar(Fich)-11,nchar(Fich)-10)
MaDataRisk0$Seconde<-substr(Fich,nchar(Fich)-9,nchar(Fich)-8)

# Create date of the night vector (after midnight, the date remains the same as the previous day)
MaDataRisk0$Sub=substr(Fich,nchar(Fich)-13,nchar(Fich)-13)
Sub0=subset(MaDataRisk0,MaDataRisk0$Sub==0) # table hours after midnight
Sub1=subset(MaDataRisk0,MaDataRisk0$Sub!=0) # table hours before midnight
Sub0$Date_Nuit= as.character(as.Date(paste(Sub0$Annee,Sub0$Mois,Sub0$Jour,sep="-"))-1)
Sub1$Date_Nuit= as.character(as.Date(paste(Sub1$Annee,Sub1$Mois,Sub1$Jour,sep="-")))
MaDataRisk1=rbind(Sub0,Sub1)
rm(MaDataRisk0)
rm(Sub0)
rm(Sub1)
rm(Fich)

# Create Julian day vector
MaDataRisk1$JJulien=yday(MaDataRisk1$Date_Nuit)

# Create unique ID by date and site
MaDataRisk1$TimestampRisk=paste(MaDataRisk1$Site,as.character(MaDataRisk1$Date_Nuit),sep="_")

# Standardise species names
MaDataRisk1$EspeceP[MaDataRisk1$EspeceP == "Pipkuh"] <- "Pip35"
MaDataRisk1$EspeceP[MaDataRisk1$EspeceP == "Pipnat"] <- "Pip35"
MaDataRisk1$EspeceP[MaDataRisk1$EspeceP == "Myobly"] <- "MyoGT"

# Add the three guilds
MaDataRisk1$Guilde<-ifelse(substring(MaDataRisk1$EspeceP,1,3)=="Rhi"|substring(MaDataRisk1$EspeceP,1,4)=="Myoa"|
                             substring(MaDataRisk1$EspeceP,1,4)=="Myob"|substring(MaDataRisk1$EspeceP,1,4)=="Myoc"|
                             substring(MaDataRisk1$EspeceP,1,4)=="Myod"|substring(MaDataRisk1$EspeceP,1,4)=="Myoe"|
                             substring(MaDataRisk1$EspeceP,1,4)=="Myon"|substring(MaDataRisk1$EspeceP,1,4)=="Myos"|
                             substring(MaDataRisk1$EspeceP,1,3)=="Bar"|substring(MaDataRisk1$EspeceP,1,4)=="MyoG"|
                             substring(MaDataRisk1$EspeceP,1,3)=="Ple", 
                           "Ferme",
                           
                           (ifelse(substring(MaDataRisk1$EspeceP,1,3)=="Min"|substring(MaDataRisk1$EspeceP,1,3)=="Pip"| 
                                     substring(MaDataRisk1$EspeceP,1,3)=="Hyp",
                                   "SemiOuvert",
                                   
                                   (ifelse(substring(MaDataRisk1$EspeceP,1,3)=="Ept"|substring(MaDataRisk1$EspeceP,1,3)=="Nyc"|
                                             substring(MaDataRisk1$EspeceP,1,3)=="Tad"|substring(MaDataRisk1$EspeceP,1,4)=="ENVs", 
                                           "Ouvert", "NA")))))


MaDataRisk1$SP=MaDataRisk1$EspeceP
MaDataRisk1.2=MaDataRisk1
MaDataRisk1.2$EspeceP=MaDataRisk1$Guilde
MaDataRisk1.3=rbind(MaDataRisk1,MaDataRisk1.2) #  rbind tables SP and Guild
MaDataRisk1.3$Guilde <- NULL
MaDataRisk1.3$Sub <- NULL
MaDataRisk1.3$TimestampRisk <- NULL
rm(MaDataRisk1.2)

Fich2=as.character(MaDataRisk1.3$Fichier)
MaDataRisk1.3$Min<-as.numeric(substr(Fich2,nchar(Fich2)-11,nchar(Fich2)-10))
MaDataRisk1.3$Sec<-as.numeric(substr(Fich2,nchar(Fich2)-9,nchar(Fich2)-8))
MaDataRisk1.3$Timestamp=as.numeric(MaDataRisk1.3$Heure)*3600+MaDataRisk1.3$Min*60+MaDataRisk1.3$Sec
rm(MaDataRisk1)
rm(Fich2)

# Match bat passes with vehicle passes
# Load vehicle pass table
Vehicule = read.csv("./bat-road-collision-risks/Tables/Vehicules_TOTAL.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "",check.names=FALSE)
FichV=as.character(Vehicule$Fichier)

# Create time and date vectors
Vehicule$Date=as.numeric(substr(FichV,nchar(FichV)-22,nchar(FichV)-15))
Vehicule$Heure<-as.numeric(substr(FichV,nchar(FichV)-13,nchar(FichV)-12))
Vehicule$Min<-as.numeric(substr(FichV,nchar(FichV)-11,nchar(FichV)-10))
Vehicule$Sec<-as.numeric(substr(FichV,nchar(FichV)-9,nchar(FichV)-8))
Vehicule$Timestamp=Vehicule$Heure*3600+Vehicule$Min*60+Vehicule$Sec
Vehicule$Site=sub( "_.*", "", Vehicule$Fichier)
Vehicule$Site[Vehicule$Site == "Pignan"] <- 104
Vehicule$Site[Vehicule$Site == "Plaissan"] <- 109

# Subset vehicle passes
# noise > 1200 ms corresponds to a vehicle pass
VehiculeSub=subset(Vehicule,Vehicule$Duree_Noise_Max>1200) 
rm(Vehicule)
rm(FichV)

# Find time matches between bat passes and vehicle passes
rm(TableFinal)
for (k in 1:length(levels(as.factor(MaDataRisk1.3$Site)))){
  
  Site_k=levels(as.factor(MaDataRisk1.3$Site))[k]
  print(Site_k)
  
  TableSubRisk_Site=subset(MaDataRisk1.3,MaDataRisk1.3$Site==Site_k)
  TableSubVehi_Site=subset(VehiculeSub,VehiculeSub$Site==Site_k)
  
  TableSubRisk=as.data.frame(cbind(as.numeric(TableSubRisk_Site$Date),TableSubRisk_Site$Timestamp))
  colnames(TableSubRisk)=c("Date","Timestamp")
  TableSubVehi=as.data.frame(cbind(as.numeric(TableSubVehi_Site$Date),TableSubVehi_Site$Timestamp))
  colnames(TableSubVehi)=c("Date","Timestamp")
  
  CorrVehi=find.matches(TableSubRisk,TableSubVehi,tol=c(0,3600),maxmatch=1) # cherche lags de 1h max
  CorrVehi$matches[CorrVehi$matches == "0"] <- NA
  
  MatchVehiRisk=TableSubVehi_Site[CorrVehi$matches,]
  TableTemp=cbind(TableSubRisk_Site,MatchVehiRisk)
  TableTemp$Date=NULL
  TableTemp$Heure=NULL
  TableTemp$Min=NULL
  TableTemp$Sec=NULL
  TableTemp$Date=NULL
  TableTemp$Heure=NULL
  TableTemp$Min=NULL
  TableTemp$Sec=NULL
  TableTemp[,ncol(TableTemp)]=NULL
  
  if(exists("TableFinal")){TableFinal=rbind(TableFinal,TableTemp)}else{TableFinal=TableTemp}
}

# Calculate the time lag (LagVehi) between a bat pass and a vehicle pass
colnames(TableFinal)[which(names(TableFinal) == "Timestamp")[1]] <- "TimestampChiro"
colnames(TableFinal)[which(names(TableFinal) == "Timestamp")] <- "TimestampVehi"
TableFinal$LagVehi=TableFinal$TimestampVehi-TableFinal$TimestampChiro

TableFinal$Annee=NULL
TableFinal$Mois=NULL
TableFinal$Jour=NULL
TableFinal$Minute=NULL
TableFinal$Seconde=NULL
TableFinal$timestamp=NULL
TableFinal$timestampTrunc=NULL

# Save table
write.table(TableFinal,"./bat-road-collision-risks/Tables/MaDataRisk_nonTrajectoegal0.csv",sep=";",row.names=F)


