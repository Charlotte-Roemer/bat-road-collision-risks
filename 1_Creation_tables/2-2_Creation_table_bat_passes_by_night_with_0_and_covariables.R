
# This step adds an absence of observation (count of bat passes per night = 0) if a species was absent
# and merges with scaled variables
# Table created: MaDataActiNew_pour_Modelisation.csv

library(stringr)
library(lubridate)

# Load density tables created in the previous step
MaDataActi = read.csv("./bat-road-collision-risks/Tables/Max_Mean_contacts_ModActiSp.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE, stringsAsFactors=F)

# Create unique ID by date and site
MaDataActi$TimestampActi=paste(MaDataActi$Site,MaDataActi$Date_nuit,sep="_")

# List of operational nights by site
DateSite=aggregate(MaDataActi$Median,by=list(Point=MaDataActi$Site,Date=MaDataActi$Date_nuit),sum)
DateSite$TimestampDateNuit=paste(DateSite$Point,DateSite$Date,sep="_")

# Add rows with count of bat passes = 0 if species was absent
MaDataActiAvec0=matrix(nrow = 0,ncol=0)
for (k in 1:length(table(MaDataActi$Espece))){
  
  Sp=names(table(MaDataActi$Espece))[k]
  MaDataActiSpX=subset(MaDataActi,MaDataActi$Espece==Sp)
  
  MaDataActiSp=merge(DateSite,MaDataActiSpX,by.x="TimestampDateNuit",by.y="TimestampActi",all.x=T)
  MaDataActiSp=cbind(MaDataActiSp[,1:3],MaDataActiSp[,8:11])
  
  MaDataActiSp1=MaDataActiSp
  MaDataActiSp1$Espece=Sp
  MaDataActiSp1[,5:7][is.na(MaDataActiSp1[,5:7])] <- 0
  
  MaDataActiSp1$Jour=str_sub(as.character(MaDataActiSp1$Date),-10,-9)
  MaDataActiSp1$Mois=str_sub(as.character(MaDataActiSp1$Date),-7,-6)
  MaDataActiSp1$Annee=str_sub(as.character(MaDataActiSp1$Date),-4,-1)
  DateFormat=paste(MaDataActiSp1$Annee,MaDataActiSp1$Mois,MaDataActiSp1$Jour,sep="-")
  MaDataActiSp1$JJulien=yday(DateFormat)
  
  MaDataActiAvec0temp=MaDataActiSp1
  MaDataActiAvec0=rbind(MaDataActiAvec0,MaDataActiAvec0temp)
}


colnames(MaDataActiAvec0)[which(names(MaDataActiAvec0) == "Point")] <- "Site"

# Add Guild
MaDataActiAvec0$SP=MaDataActiAvec0$Espece
MaDataActiAvec0_pourGuilde=MaDataActiAvec0

MaDataActiAvec0_pourGuilde$Espece<-ifelse(substring(MaDataActiAvec0_pourGuilde$Espece,1,3)=="Rhi"|substring(MaDataActiAvec0_pourGuilde$Espece,1,4)=="Myoa"|
                                            substring(MaDataActiAvec0_pourGuilde$Espece,1,4)=="Myob"|substring(MaDataActiAvec0_pourGuilde$Espece,1,4)=="Myoc"|
                                            substring(MaDataActiAvec0_pourGuilde$Espece,1,4)=="Myod"|substring(MaDataActiAvec0_pourGuilde$Espece,1,4)=="Myoe"|
                                            substring(MaDataActiAvec0_pourGuilde$Espece,1,4)=="Myon"|substring(MaDataActiAvec0_pourGuilde$Espece,1,4)=="Myos"|
                                            substring(MaDataActiAvec0_pourGuilde$Espece,1,3)=="Bar"|substring(MaDataActiAvec0_pourGuilde$Espece,1,4)=="MyoG"|
                                            substring(MaDataActiAvec0_pourGuilde$Espece,1,3)=="Ple", 
                                          "Ferme",
                                          
                                          (ifelse(substring(MaDataActiAvec0_pourGuilde$Espece,1,3)=="Min"|substring(MaDataActiAvec0_pourGuilde$Espece,1,3)=="Pip"| 
                                                    substring(MaDataActiAvec0_pourGuilde$Espece,1,3)=="Hyp",
                                                  "SemiOuvert",
                                                  
                                                  (ifelse(substring(MaDataActiAvec0_pourGuilde$Espece,1,3)=="Ept"|substring(MaDataActiAvec0_pourGuilde$Espece,1,3)=="Nyc"|
                                                            substring(MaDataActiAvec0_pourGuilde$Espece,1,3)=="Tad"|substring(MaDataActiAvec0_pourGuilde$Espece,1,4)=="ENVs", 
                                                          "Ouvert", "NA")))))

#Aggregate rows to obtain a count of bat passes per night and site for each guild
MaDataActiAvec0_pourGuildeAggr=aggregate(MaDataActiAvec0_pourGuilde$Median, 
                                         by=list(TimestampDateNuit=MaDataActiAvec0_pourGuilde$TimestampDateNuit,
                                                 Site=MaDataActiAvec0_pourGuilde$Site,
                                                 Date=MaDataActiAvec0_pourGuilde$Date,
                                                 Espece=MaDataActiAvec0_pourGuilde$Espece,
                                                 Jour=MaDataActiAvec0_pourGuilde$Jour,
                                                 Mois=MaDataActiAvec0_pourGuilde$Mois,
                                                 Annee=MaDataActiAvec0_pourGuilde$Annee,
                                                 JJulien=MaDataActiAvec0_pourGuilde$JJulien ),
                                         FUN=sum)
colnames(MaDataActiAvec0_pourGuildeAggr)[which(names(MaDataActiAvec0_pourGuildeAggr) == "x")] <- "Median"

# Concatenate the tables
MaDataActiAvec0_pourGuildeAggr$SP=NA
MaDataActiAvec0$Max=NULL
MaDataActiAvec0$Mean=NULL
col_order <- names(MaDataActiAvec0)
MaDataActiAvec0_pourGuildeAggr <- MaDataActiAvec0_pourGuildeAggr[, col_order]
MaDataActiAvec0.2=rbind(MaDataActiAvec0,MaDataActiAvec0_pourGuildeAggr)

# Load scaled variables
Variables2= read.table("D:/Trajectographie/THESE CRO/Fichiers_BILAN_THESE_ROUTES/Variables_scalees_a_la_main_pour_modelisation.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
Variables2$JJulien = yday(as.Date(as.character(Variables2$Date), "%d/%m/%Y"))

# Merge
MaDataActiNew=merge(MaDataActiAvec0.2,Variables2)

# Creation of five binomial variables for Landscape type (useful for model selection only)
MaDataActiNew$Double_haie=as.factor((MaDataActiNew$Paysage=="Double_haie"))
MaDataActiNew$Haie_parallele=as.factor((MaDataActiNew$Paysage=="Haie_parallele"))
MaDataActiNew$Haie_perpendiculaire=as.factor((MaDataActiNew$Paysage=="Haie_perpendiculaire"))
MaDataActiNew$Lisiere=as.factor((MaDataActiNew$Paysage=="Lisiere"))
MaDataActiNew$Allee_forestiere=as.factor((MaDataActiNew$Paysage=="Allee_forestiere"))
MaDataActiNew$Non_arbore=as.factor((MaDataActiNew$Paysage=="Non_arbore"))

# Change levels for Landscape to have No_vegetation (Non_arbore) as intercept
MaDataActiNew$Paysage=as.character(MaDataActiNew$Paysage)
MaDataActiNew$Paysage[which(MaDataActiNew$Paysage=="Non_arbore")]="NV"
MaDataActiNew$Paysage[which(MaDataActiNew$Paysage=="Allee_forestiere")]="F"
MaDataActiNew$Paysage[which(MaDataActiNew$Paysage=="Double_haie")]="DPT"
MaDataActiNew$Paysage[which(MaDataActiNew$Paysage=="Haie_parallele")]="SPT"
MaDataActiNew$Paysage[which(MaDataActiNew$Paysage=="Haie_perpendiculaire")]="PT"
MaDataActiNew$Paysage[which(MaDataActiNew$Paysage=="Lisiere")]="FE"
MaDataActiNew$Paysage <- factor(MaDataActiNew$Paysage, levels = c("NV","F", "FE", "DPT","SPT","PT"))

MaDataActiNew$Median=as.integer(MaDataActiNew$Median)
MaDataActiNew$Site=as.factor(MaDataActiNew$Site)

# Save table
write.table(MaDataActiNew,"./bat-road-collision-risks/Tables/MaDataActiNew_pour_Modelisation.csv",sep=";",row.names=F)



