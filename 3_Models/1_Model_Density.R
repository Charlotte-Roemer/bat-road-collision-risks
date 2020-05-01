
# This step uses the formulae selected during model selection, creates model objects 
# and a summary table for all species models
# Model objects created : Speciesname_Acti_BestModel.rds
# Table created : Summary_Activite.csv

library(glmmTMB)

# Load density table
MaDataActiNewBIND= read.table("./bat-road-collision-risks/Tables/MaDataActiNew_pour_Modelisation.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
MaDataActiNewBIND$Paysage <- factor(MaDataActiNewBIND$Paysage, levels = c("NV","F", "FE", "DPT","SPT","PT"))

# List of species and guilds for which we want to build models
Liste_Sp=c("Pippip","Pip35","Pippyg","Hypsav","Minsch","Nyclei","MyoGT","Plesp","Myodau","Myoema", "Eptser",
           "Ferme", "Ouvert", "SemiOuvert")

# Build model formula for each species and guild
for (i in 1:length(Liste_Sp)){
  
  Sp=Liste_Sp[i]
  print(Sp)
  
  MaDataActiNew=subset(MaDataActiNewBIND,MaDataActiNewBIND$Espece==Sp)
  MaDataActiNew$Median=as.integer(MaDataActiNew$Median)
  MaDataActiNew$Site=as.factor(MaDataActiNew$Site)
  
  if (Sp=="Pippip") {
    ModActiPippip<-glmmTMB(Median ~ JJulien_scaled+I(JJulien_scaled^2)+
                             Paysage+
                             D_houppier_scaled+
                             (1 | Site) , 
                           data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiPippip
    
  }else if (Sp=="Myoema") {
    ModActiMyoema<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2)+
                              (1 | Site) , 
                            data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiMyoema
    
  }else if (Sp=="Pip35") {
    ModActiPip35<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2)+
                             (1 | Site) , 
                           data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiPip35
    
  }else if (Sp=="Pippyg") {
    ModActiPippyg<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2)+
                              D_houppier_scaled+
                              Paysage+
                              (1 | Site) , 
                            data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiPippyg
    
  }else if (Sp=="Nyclei") {
    ModActiNyclei<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2)+
                              D_houppier_scaled+
                              (1 | Site) , 
                            data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiNyclei
    
  }else if (Sp=="Minsch") {
    ModActiMinsch<-glmmTMB( Median ~  JJulien_scaled+I(JJulien_scaled^2)+
                              Paysage+
                              D_houppier_scaled+
                              (1 | Site) , 
                            data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiMinsch
    
  }else if (Sp=="Hypsav") {
    ModActiHypsav<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2)+
                              Paysage+
                              (1 | Site) , 
                            data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiHypsav
    
  }else if (Sp=="Plesp") {
    ModActiPlesp<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2)+
                             D_houppier_scaled+
                             N_vehi_nuit_1200_scaled+
                             (1 | Site) , 
                           data=MaDataActiNew, family=nbinom2)
    
    BestModel=ModActiPlesp
    
  }else if (Sp=="MyoGT") {
    ModActiMyoGT<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2)+
                             H_veg_scaled+
                             (1 | Site) , 
                           data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiMyoGT
    
  }else if (Sp=="Eptser") {
    ModActiEptser<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2)+
                              (1 | Site) , 
                            data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiEptser
    
  }else if (Sp=="Myodau") {
    ModActiMyodau<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2)+
                              D_houppier_scaled+
                              (1 | Site) , 
                            data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiMyodau
    
  }else if (Sp=="Ferme") {
    ModActiFerme<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2) +
                             N_vehi_nuit_1200_scaled+
                             (1 | Site) , 
                           data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiFerme
    
  }else if (Sp=="Ouvert") {
    ModActiOuvert<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2) + 
                              H_veg_scaled +
                              (1 | Site) , 
                            data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiOuvert
    
  }else if (Sp=="SemiOuvert") {
    ModActiSemiOuvert<-glmmTMB( Median ~ JJulien_scaled+I(JJulien_scaled^2) + 
                                  Paysage +
                                  D_houppier_scaled +
                                  (1 | Site), 
                                data=MaDataActiNew, family=nbinom2)
    BestModel=ModActiSemiOuvert
    
  }
  
  # Save model object
  saveRDS(BestModel, file = paste("./bat-road-collision-risks/SAVED MODELS/", Sp, "_Acti_BestModel.rds",sep=""))
  
  # Create summary table for each species and guild
  summaryModelTemp <-summary(BestModel)$coefficients$cond
  summaryModelTemp2=cbind(rep(Sp,nrow(summaryModelTemp)),rownames(summaryModelTemp),summaryModelTemp)
  colnames(summaryModelTemp2)[2]="Variables"
  row.names(summaryModelTemp2)=1:nrow(summaryModelTemp2)
  
  # Concatenate all summary tables
  if (exists ("summaryModelF")){summaryModelF=rbind(summaryModelF,summaryModelTemp2)}else{
    summaryModelF=summaryModelTemp2}
  
}

# Save table
write.table(summaryModelF,"./bat-road-collision-risks/SUMMARY/Summary_Activite.csv", sep=";",row.names=F)

