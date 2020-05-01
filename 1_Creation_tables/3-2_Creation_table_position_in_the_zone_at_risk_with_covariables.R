
# This step merges the table with positions in the zone at risk with landscape variables
# Creates table MaDataRiskNew_pour_Modelisation.csv

library(stringr)
library(suncalc)
library(lubridate)
library(dplyr)

# Load table with positions in the zone at risk
MaDataRiskNew00= read.table("./bat-road-collision-risks/Tables/MaDataRisk_nonTrajectoegal0.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")

# Create time and date vectors
MaDataRiskNew00$Heure=substr(as.character(MaDataRiskNew00$Fichier),nchar(as.character(MaDataRiskNew00$Fichier))-13,
                             nchar(as.character(MaDataRiskNew00$Fichier))-12)
MaDataRiskNew00$Minute=substr(as.character(MaDataRiskNew00$Fichier),nchar(as.character(MaDataRiskNew00$Fichier))-11,
                              nchar(as.character(MaDataRiskNew00$Fichier))-10)
MaDataRiskNew00$Seconde=substr(as.character(MaDataRiskNew00$Fichier),nchar(as.character(MaDataRiskNew00$Fichier))-9,
                               nchar(as.character(MaDataRiskNew00$Fichier))-8)

# Remove unnecessary columns
MaDataRiskNew00$SegTraj=NULL
MaDataRiskNew00$N=NULL
MaDataRiskNew00$Fichier=NULL
MaDataRiskNew00$EspeceS=NULL
MaDataRiskNew00$Type=NULL
MaDataRiskNew00$TimestampChiro=NULL
MaDataRiskNew00$Fichier.1=NULL
MaDataRiskNew00$Duree_Noise_Max=NULL
MaDataRiskNew00$TimestampVehi=NULL

# Load scaled variables
Variables2= read.table("./bat-road-collision-risks/Tables/Variables_scalees_a_la_main_pour_modelisation.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
Variables2$JJulien = yday(as.Date(as.character(Variables2$Date), "%d/%m/%Y"))

# Merge
MaDataRiskNew0=left_join(MaDataRiskNew00, Variables2)
MaDataRiskNew0=subset(MaDataRiskNew0, !is.na(MaDataRiskNew0$L_route))

# Remove sites for which there was no trajectory (on sites #123 and 55 one of the bat detectors had technical problems)
MaDataRiskNew=subset(MaDataRiskNew0,(MaDataRiskNew0$Site!=123)&(MaDataRiskNew0$Site!=55)) 

MaDataRiskNew$RisqueXYZ=as.character(MaDataRiskNew$RisqueXYZ)
MaDataRiskNew$RisqueXYZ[MaDataRiskNew$RisqueXYZ=="SafeCertain"] <- 0
MaDataRiskNew$RisqueXYZ[MaDataRiskNew$RisqueXYZ=="RiskCertain"] <- 1
MaDataRiskNew$RisqueXYZ=as.numeric(MaDataRiskNew$RisqueXYZ)
MaDataRiskNew=MaDataRiskNew[!is.na(MaDataRiskNew$RisqueXYZ),]

# Creation of five binomial variables for Landscape type (useful for model selection only)
MaDataRiskNew$Double_haie=as.factor((MaDataRiskNew$Paysage=="Double_haie"))
MaDataRiskNew$Haie_parallele=as.factor((MaDataRiskNew$Paysage=="Haie_parallele"))
MaDataRiskNew$Haie_perpendiculaire=as.factor((MaDataRiskNew$Paysage=="Haie_perpendiculaire"))
MaDataRiskNew$Lisiere=as.factor((MaDataRiskNew$Paysage=="Lisiere"))
MaDataRiskNew$Allee_forestiere=as.factor((MaDataRiskNew$Paysage=="Allee_forestiere"))
MaDataRiskNew$Non_arbore=as.factor((MaDataRiskNew$Paysage=="Non_arbore"))

# Change levels for Landscape to have Forest as intercept
MaDataRiskNew$Paysage=as.character(MaDataRiskNew$Paysage)
MaDataRiskNew$Paysage[which(MaDataRiskNew$Paysage=="Non_arbore")]="NV"
MaDataRiskNew$Paysage[which(MaDataRiskNew$Paysage=="Allee_forestiere")]="F"
MaDataRiskNew$Paysage[which(MaDataRiskNew$Paysage=="Double_haie")]="DPT"
MaDataRiskNew$Paysage[which(MaDataRiskNew$Paysage=="Haie_parallele")]="SPT"
MaDataRiskNew$Paysage[which(MaDataRiskNew$Paysage=="Haie_perpendiculaire")]="PT"
MaDataRiskNew$Paysage[which(MaDataRiskNew$Paysage=="Lisiere")]="FE"
MaDataRiskNew$Paysage <- factor(MaDataRiskNew$Paysage, levels = c("F", "FE", "DPT","SPT","PT","NV"))

MaDataRiskNew$Site=as.factor(MaDataRiskNew$Site)

# Save table
write.table(MaDataRiskNew,"./bat-road-collision-risks/Tables/MaDataRiskNew_pour_Modelisation.csv",sep=";",row.names=F)

