
# This step uses the formulae selected during model selection, creates model objects 
# and a summary table for all species models
# Model objects created : Speciesname_Risk_BestModel.rds
# Table created : Summary_Risque.csv

library(glmmTMB)

# Load positions in the zone at risk table
MaDataRiskNewBIND= read.table("./bat-road-collision-risks/Tables/MaDataRiskNew_pour_Modelisation.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
MaDataRiskNewBIND$Paysage <- factor(MaDataRiskNewBIND$Paysage, levels = c("F", "FE", "DPT","SPT","PT","NV"))

# List of species and guilds for which we want to build models
Liste_Sp=c("Pippip","Pip35","Pippyg","Hypsav","Minsch","Nyclei","MyoGT","Plesp","Myodau","Eptser",
           "Ouvert","SemiOuvert","Ferme")

for (i in 1:length(Liste_Sp)){
  
  Sp=Liste_Sp[i]
  print(Sp)
  
  MaDataRiskNew=subset(MaDataRiskNewBIND,MaDataRiskNewBIND$EspeceP==Sp)
  
  if (Sp=="Pippip") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ))~JJulien_scaled+I(JJulien_scaled^2)+
                      Paysage")
    FormulaRandom=paste0("+(1 | Site) ")
    
  }else if (Sp=="Pip35") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~ JJulien_scaled+I(JJulien_scaled^2)+
                      N_vehi_nuit_1200_scaled+
                      Paysage")
    FormulaRandom=paste0("+(1 | Site) ")
    
  }else if (Sp=="Pippyg") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~JJulien_scaled+I(JJulien_scaled^2)")
    FormulaRandom=paste0("+(1 | Site) ")
    
  }else if (Sp=="Nyclei") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~ JJulien_scaled+I(JJulien_scaled^2)+
                      Paysage")
    FormulaRandom=paste0("+(1 | Site) ")
    
  }else if (Sp=="Minsch") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~ JJulien_scaled+I(JJulien_scaled^2)")
    FormulaRandom=paste0("+(1 | Site) ")
    
  }else if (Sp=="Hypsav") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~ JJulien_scaled+I(JJulien_scaled^2)+
                      Paysage")
    FormulaRandom=paste0("+(1 | Site) ")
    
  }else if (Sp=="Plesp") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~ JJulien_scaled+I(JJulien_scaled^2)")
    FormulaRandom=paste0("+(1 | Site) ")
    
  }else if (Sp=="MyoGT") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~JJulien_scaled+I(JJulien_scaled^2)")
    FormulaRandom=paste0("+(1 | Site) ")
    
  }else if (Sp=="Eptser") { # convergence problem for interaction Paysage:D_houppier_scaled
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~ JJulien_scaled+I(JJulien_scaled^2)+
                      Paysage+
                      D_houppier_scaled")
    FormulaRandom=paste0("+(1 | Site) ")
    
  }else if (Sp=="Myodau") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~ JJulien_scaled+I(JJulien_scaled^2)
                      H_veg_scaled")
    FormulaRandom=paste0("+(1 | Site) ")
    
  }else if (Sp=="Ferme") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~JJulien_scaled+I(JJulien_scaled^2)")
    FormulaRandom=paste0("+(1 | Site) + (1|SP) ")
    
  }else if (Sp=="SemiOuvert") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~ JJulien_scaled+I(JJulien_scaled^2)+
                      Paysage")
    FormulaRandom=paste0("+(1 | Site) + (1|SP) ")
    
  }else if (Sp=="Ouvert") {
    FormulaFix=paste0("cbind(RisqueXYZ,(1-RisqueXYZ)) ~ JJulien_scaled+I(JJulien_scaled^2)+
                      Paysage")
    FormulaRandom=paste0("+(1 | Site) + (1|SP) ")
  }
  
  
  Formula=as.formula(paste0(as.character(FormulaFix),FormulaRandom))
  BestModel=glmmTMB(Formula, data=MaDataRiskNew, family=binomial)
  
  # Save model object
  saveRDS(BestModel, file = paste("./bat-road-collision-risks/SAVED MODELS/", Sp, "_Risk_BestModel.rds",sep=""))
  
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
write.table(summaryModelF,"./bat-road-collision-risks/SUMMARY/Summary_Risque.csv", sep=";",row.names=F)

