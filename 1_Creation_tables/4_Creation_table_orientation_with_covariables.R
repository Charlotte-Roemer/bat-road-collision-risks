
# This step attributes an orientation (360°) to each bat trajectory
# Table created: MaData_Orientation.csv

library(stringr)
require(lubridate)
library(dplyr)

# Load bat trajectory location table (each row is a is a call with SegTraj as trajectory ID)
Coordonnees0= read.table("./bat-road-collision-risks/Tables/Cri_par_cri_Risque_XYZ_Total_Sites.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")

# Remove rows that do not correspond to a bat
Coordonnees=subset(Coordonnees0,Coordonnees0$IdP!="parasi" &
                     Coordonnees0$IdP!="oiseau" &
                     Coordonnees0$IdP!="ChiroSp" &
                     Coordonnees0$IdP!="ortho" &
                     Coordonnees0$IdP!="ortho?" &
                     Coordonnees0$IdP!="vehicule" 
)

# Keep only coordinates for which precision is sufficient
Coordonnees_Precise<-subset(Coordonnees,Coordonnees$Precision==T)

# Remove unnecessary columns
Coordonnees_Precise[,2:29]=NULL
Coordonnees_Precise$bary_x=NULL
Coordonnees_Precise$bary_y=NULL
Coordonnees_Precise$bary_z=NULL
Coordonnees_Precise$Dist_bary=NULL
Coordonnees_Precise$Precision=NULL
Coordonnees_Precise$SegTraj=NULL
Coordonnees_Precise$V=NULL
Coordonnees_Precise$CvV=NULL
Coordonnees_Precise$round=NULL
Coordonnees_Precise$Vfinal=NULL

# Create time and date vectors
Fich <- as.character(Coordonnees_Precise$Fich1)
Coordonnees_Precise$Date=substr(Fich,nchar(Fich)-22,nchar(Fich)-15)
Coordonnees_Precise$Annee<-substr(Fich,nchar(Fich)-22,nchar(Fich)-19)
Coordonnees_Precise$Mois<-substr(Fich,nchar(Fich)-18,nchar(Fich)-17)
Coordonnees_Precise$Jour<-substr(Fich,nchar(Fich)-16,nchar(Fich)-15)
Coordonnees_Precise$Heure<-substr(Fich,nchar(Fich)-13,nchar(Fich)-12)
Coordonnees_Precise$Minute<-substr(Fich,nchar(Fich)-11,nchar(Fich)-10)
Coordonnees_Precise$Seconde<-substr(Fich,nchar(Fich)-9,nchar(Fich)-8)

# Create date of the night vector (after midnight, the date remains the same as the previous day)
Coordonnees_Precise$Sub=substr(Fich,nchar(Fich)-13,nchar(Fich)-13)
Sub0=subset(Coordonnees_Precise,Coordonnees_Precise$Sub==0) # table hours after midnight
Sub1=subset(Coordonnees_Precise,Coordonnees_Precise$Sub!=0) # table hours before midnight
Sub0$Date_Nuit= as.character(as.Date(paste(Sub0$Annee,Sub0$Mois,Sub0$Jour,sep="-"))-1)
Sub1$Date_Nuit= as.character(as.Date(paste(Sub1$Annee,Sub1$Mois,Sub1$Jour,sep="-")))
Coordonnees_Precise2=rbind(Sub0,Sub1)
rm(Coordonnees_Precise)
rm(Sub0)
rm(Sub1)
rm(Fich)

# Create Julian day vector
Coordonnees_Precise2$JJulien=yday(Coordonnees_Precise2$Date_Nuit)

Coord4=Coordonnees_Precise2
Coord4$x=as.numeric(as.character(Coord4$x))
Coord4$y=as.numeric(as.character(Coord4$y))
Coord4$IdP=as.character(Coord4$IdP)

# Standardise species names
Coord4$IdP[Coord4$IdP == "Pipkuh"] <- "Pip35"
Coord4$IdP[Coord4$IdP == "Pipnat"] <- "Pip35"
Coord4$IdP[Coord4$IdP == "Myobly"] <- "MyoGT"
Coord4$IdP[Coord4$IdP == "Myomyo"] <- "MyoGT"
Coord4$IdP[(substring(Coord4$IdP,1,3)=="Myo")&(substring(Coord4$IdP,1,4)!="MyoG")]<- "Myosp"

Coord5<-Coord4[order(Coord4$Site,Coord4$Date_Nuit,Coord4$SegTrajFinal),]

# Calculate vector in the X dimension for each trajectory
xdeb<-vector(length=0)
xfin<-vector(length=0)

for (i in 2:(nrow(Coord5)-1)){ 
  if (Coord5$SegTrajFinal[i]!=Coord5$SegTrajFinal[i-1])
  {
    xdeb[i]<-Coord5$x[i]
  }
  if (Coord5$SegTrajFinal[i]!=Coord5$SegTrajFinal[i+1])
  {
    xfin[i]<-Coord5$x[i]
  }
}

xdeb=xdeb[!is.na(xdeb)]
xfin=xfin[!is.na(xfin)]
xdeb=c(Coord5$x[1],xdeb)
xfin=c(xfin,Coord5$x[length(Coord5$x)])
x_vecteur<-vector(length=0)
for (i in 1:length(xfin)){ 
  x_vecteur[i]=xfin[i]-xdeb[i]
}

x_vecteur2=x_vecteur

# Calculate vector in the Y dimension for each trajectory
ydeb<-vector(length=0)
yfin<-vector(length=0)

for (i in 2:(nrow(Coord5)-1)){ 
  if (Coord5$SegTrajFinal[i]!=Coord5$SegTrajFinal[i-1])
  {
    ydeb[i]<-Coord5$y[i]
  }
  if (Coord5$SegTrajFinal[i]!=Coord5$SegTrajFinal[i+1])
  {
    yfin[i]<-Coord5$y[i]
  }
}

ydeb=ydeb[!is.na(ydeb)]
yfin=yfin[!is.na(yfin)]
ydeb=c(Coord5$y[1],ydeb)
yfin=c(yfin,Coord5$y[length(Coord5$y)])
y_vecteur<-vector(length=0)

for (i in 1:length(yfin)){ 
  y_vecteur[i]=yfin[i]-ydeb[i]
}

y_vecteur2=y_vecteur

# For each vector, calculate the orientation of the trajectory in function of the X axis (road axis)

if (length(x_vecteur2)>0){
  orientation<-vector(length=0)
  
  for (i in 1:length(x_vecteur2)){ 
    if (x_vecteur2[i]>=0 & y_vecteur2[i]>=0) { 
      orientation[i]=180*(atan(y_vecteur2[i]/x_vecteur2[i])/pi)
    }
    else {
      if (x_vecteur2[i]<=0 & y_vecteur2[i]>0) { 
        orientation[i]=(180*(atan(-x_vecteur2[i]/(y_vecteur2[i]))/pi))+90
      }
      else {
        if (x_vecteur2[i]<=0 & y_vecteur2[i]<=0) { 
          orientation[i]=(180*(atan((-y_vecteur2[i])/(-x_vecteur2[i]))/pi))+180
        }
        else {
          if (x_vecteur2[i]>0 & y_vecteur2[i]<=0) { 
            orientation[i]=(180*(atan(x_vecteur2[i]/(-y_vecteur2[i]))/pi))+270
          }
        }}}}}


# Aggregate dataset by trajectory ID (SegTraj)
CoordAggr=aggregate(Coord5$SegTrajFinal,by=list(Site=Coord5$Site,SegTrajFinal=Coord5$SegTrajFinal,Date_Nuit=Coord5$Date_Nuit),FUN=length)
CoordAggr$x=NULL

# Bind all vectors
BindCoordOrientation=cbind(orientation,x_vecteur,y_vecteur,CoordAggr)

# Remove trajectories where the bat did not move (X and Y vectors = 0)
BindCoordOrientation2=subset(BindCoordOrientation,(BindCoordOrientation$x_vecteur!=0)|
                               (BindCoordOrientation$y_vecteur!=0))

# Remove unnecessary columns
Coord6=Coord5
Coord6$x=NULL
Coord6$y=NULL
Coord6$z=NULL
Coord6$Temps2=NULL
Coord6$IcsP=NULL
Coord6$IbuzP=NULL
Coord6$IdS=NULL
Coord6$IcsS=NULL
Coord6$IbuzS=NULL
Coord6$Date=NULL
Coord6$Annee=NULL
Coord6$Mois=NULL
Coord6$Jour=NULL
Coord6$Heure=NULL
Coord6$Minute=NULL
Coord6$Seconde=NULL
Coord6$Sub=NULL

# Some trajectories start with one species and end with another
# In that case, the species with the most ID is kept

Coord6Aggr=aggregate(Coord6$Date2,list(Site=Coord6$Site,Date_Nuit=Coord6$Date_Nuit,
                                       JJulien=Coord6$JJulien,Fich1=Coord6$Fich1,
                                       SegTrajFinal=Coord6$SegTrajFinal,
                                       IdP=Coord6$IdP),FUN=length)
Coord6Aggr2 <- Coord6Aggr[order(Coord6Aggr$Site,Coord6Aggr$SegTrajFinal, -abs(Coord6Aggr$x) ), ]
Coord7a=Coord6Aggr2[ !duplicated(Coord6Aggr2[,c(1:3,5)]), ]     
Coord7b=Coord7a[,c(1,2,5,6)]
Coord62=Coord6
Coord62$IdP=NULL
Coord7c=merge(Coord62,Coord7b)
Coord7d <- Coord7c[order(Coord7c$Site,Coord7c$SegTrajFinal, Coord7c$Fich1), ]
Coord7e=Coord7d[ !duplicated(Coord7d[,c(1:3)]), ]     

Coord7=merge(BindCoordOrientation2,Coord7e)

# Add the 3 guilds
Coord7$Guilde<-ifelse(substring(Coord7$IdP,1,3)=="Rhi"|substring(Coord7$IdP,1,4)=="Myoa"|
                        substring(Coord7$IdP,1,4)=="Myob"|substring(Coord7$IdP,1,4)=="Myoc"|
                        substring(Coord7$IdP,1,4)=="Myod"|substring(Coord7$IdP,1,4)=="Myoe"|
                        substring(Coord7$IdP,1,4)=="Myon"|substring(Coord7$IdP,1,4)=="Myos"|
                        substring(Coord7$IdP,1,3)=="Bar"|substring(Coord7$IdP,1,4)=="MyoG"|
                        substring(Coord7$IdP,1,3)=="Ple", 
                      "Ferme",
                      
                      (ifelse(substring(Coord7$IdP,1,3)=="Min"|substring(Coord7$IdP,1,3)=="Pip"| 
                                substring(Coord7$IdP,1,3)=="Hyp",
                              "SemiOuvert",
                              
                              (ifelse(substring(Coord7$IdP,1,3)=="Ept"|substring(Coord7$IdP,1,3)=="Nyc"|
                                        substring(Coord7$IdP,1,3)=="Tad"|substring(Coord7$IdP,1,4)=="ENVs", 
                                      "Ouvert", "NA")))))

# Remove unnecessary columns
Coord7$SP=Coord7$IdP
Coord8=Coord7
Coord8$IdP=Coord7$Guilde
MaDataOrientationNew00=rbind(Coord7,Coord8) # On rbind les tables SP et Guilde
MaDataOrientationNew00$Guilde <- NULL
MaDataOrientationNew00$Sub <- NULL
MaDataOrientationNew00$Fichier=NULL

# Create time and date vectors
MaDataOrientationNew00$Heure=substr(as.character(MaDataOrientationNew00$Fich1),nchar(as.character(MaDataOrientationNew00$Fich1))-13,
                                    nchar(as.character(MaDataOrientationNew00$Fich1))-12)
MaDataOrientationNew00$Minute=substr(as.character(MaDataOrientationNew00$Fich1),nchar(as.character(MaDataOrientationNew00$Fich1))-11,
                                     nchar(as.character(MaDataOrientationNew00$Fich1))-10)
MaDataOrientationNew00$Seconde=substr(as.character(MaDataOrientationNew00$Fich1),nchar(as.character(MaDataOrientationNew00$Fich1))-9,
                                      nchar(as.character(MaDataOrientationNew00$Fich1))-8)
MaDataOrientationNew00$Date=substr(as.character(MaDataOrientationNew00$Fich1),nchar(as.character(MaDataOrientationNew00$Fich1))-22,
                                   nchar(as.character(MaDataOrientationNew00$Fich1))-15)

# Remove sites for which there was no trajectory (one of the bat detectors had technical problems)
MaDataOrientationNew0=subset(MaDataOrientationNew00,(MaDataOrientationNew00$Site!=123)&(MaDataOrientationNew00$Site!=55)) # on enlève les sites pour lequel on a pas de trajecto

# Create categories for trajectory orientation, where 0 = perpendicular and 1 = parallel to road axis 
MaDataOrientationNew0$parallele[(MaDataOrientationNew0$orientation<45)|
                                  (MaDataOrientationNew0$orientation>=315)|
                                  (MaDataOrientationNew0$orientation>=135 & MaDataOrientationNew0$orientation<225)]=1
MaDataOrientationNew0$parallele[(MaDataOrientationNew0$orientation>=45 & MaDataOrientationNew0$orientation<135)|
                                  (MaDataOrientationNew0$orientation>=225 & MaDataOrientationNew0$orientation<315)]=0

# Remove unnecessary columns
MaDataOrientationNew0$x_vecteur=NULL
MaDataOrientationNew0$y_vecteur=NULL
MaDataOrientationNew0$Type=NULL
MaDataOrientationNew0$X.Fichier=NULL
MaDataOrientationNew0$Date=NULL

# Load scaled variables
Variables2= read.table("./bat-road-collision-risks/Tables/Variables_scalees_a_la_main_pour_modelisation.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
Variables2$JJulien=yday(as.Date(Variables2$Date, "%d/%m/%Y"))

# Merge
MaDataOrientationNew=left_join(MaDataOrientationNew0, Variables2)

# Creation of five binomial variables for Landscape type (useful for model selection only)
MaDataOrientationNew$Double_haie=as.factor((MaDataOrientationNew$Paysage=="Double_haie"))
MaDataOrientationNew$Haie_parallele=as.factor((MaDataOrientationNew$Paysage=="Haie_parallele"))
MaDataOrientationNew$Haie_perpendiculaire=as.factor((MaDataOrientationNew$Paysage=="Haie_perpendiculaire"))
MaDataOrientationNew$Lisiere=as.factor((MaDataOrientationNew$Paysage=="Lisiere"))
MaDataOrientationNew$Allee_forestiere=as.factor((MaDataOrientationNew$Paysage=="Allee_forestiere"))
MaDataOrientationNew$Non_arbore=as.factor((MaDataOrientationNew$Paysage=="Non_arbore"))

# Change levels for Landscape to have Forest as intercept
MaDataOrientationNew$Paysage=as.character(MaDataOrientationNew$Paysage)
MaDataOrientationNew$Paysage[which(MaDataOrientationNew$Paysage=="Non_arbore")]="NV"
MaDataOrientationNew$Paysage[which(MaDataOrientationNew$Paysage=="Allee_forestiere")]="F"
MaDataOrientationNew$Paysage[which(MaDataOrientationNew$Paysage=="Double_haie")]="DPT"
MaDataOrientationNew$Paysage[which(MaDataOrientationNew$Paysage=="Haie_parallele")]="SPT"
MaDataOrientationNew$Paysage[which(MaDataOrientationNew$Paysage=="Haie_perpendiculaire")]="PT"
MaDataOrientationNew$Paysage[which(MaDataOrientationNew$Paysage=="Lisiere")]="FE"
MaDataOrientationNew$Paysage <- factor(MaDataOrientationNew$Paysage, levels = c("F", "FE", "DPT","SPT","PT","NV"))

MaDataOrientationNew$Site=as.factor(MaDataOrientationNew$Site)

# Save table
write.table(MaDataOrientationNew,"./bat-road-collision-risks/Tables/MaDataOrientationNew_pour_Modelisation.csv",sep=";",row.names=F)





