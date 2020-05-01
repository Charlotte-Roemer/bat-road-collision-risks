library(beepr)
library(plyr)
library(Rfast)
library(MuMIn)
library(glmmTMB)

# Function to calculate VIF
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)$cond
  nam <- names(fixef(fit)$cond)
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

# Function for model selection
ModSelection<- function (VarDispo,FormulaFix,FormulaRandom,familyMod) {
  VarSimple=VarDispo
  BestVar=vector(length=0)
  DeltaAIC=3
  i=0
  VarSelect=vector()
  VarSelect_sansI=vector()
  InteractSelect=vector()
  FormulaF=vector()
  standE=vector()
  
  Formula=as.formula(paste0(as.character(FormulaFix),FormulaRandom))
  AIC_prec=AICc(glmmTMB(Formula,data=MaDataActiNew, family=familyMod))
  
  
  Fixeffects=list()
  degree=vector()
  logLik=vector()
  AICF=vector()
  
  
  if(length(VarSimple)>0){
    while(DeltaAIC>2)
    {
      i=i+1
      
      VarSelect_sansIsub=setdiff(VarSelect_sansI, VarDispoSansInter)
      if(length(VarSelect_sansIsub)>1)
      {
        VarPaysage=grepl("Non_arbore+",VarSelect_sansIsub)
        VarSelect_sansIsub[VarPaysage==T]=sub("Non_arbore+","",VarSelect_sansIsub[VarPaysage==T])
        InteractPossible=combn(VarSelect_sansIsub,2,FUN=paste, collapse=':')
        VarDispo=c(VarDispo,InteractPossible)
        VarDispo=VarDispo[!VarDispo %in% VarSelect]
        
      }
      
      AIC=vector()
      for (h in 1:(length(VarDispo)))
      {
        FormulaFixTemp=paste0(as.character(FormulaFix),"+",VarDispo[h])
        Formula=as.formula(paste0(as.character(FormulaFixTemp),FormulaRandom))
        ModTemp=glmmTMB(Formula, data=MaDataActiNew, family=familyMod)
        if (!is.nan(max(vif.mer(ModTemp)))){
          if (max(vif.mer(ModTemp))<3){
            if(!(grepl("D_houppier", Formula[3])&grepl("H_veg", Formula[3]))){
            AIC[h]=AICc(ModTemp)}else{AIC[h]=AIC_prec*2}
            }else{AIC[h]=AIC_prec*2}
        }else{AIC[h]=NA}
        print(paste(h,length(VarDispo),AIC[h],FormulaFixTemp))
        Fixeffects=c(Fixeffects,list(t(as.data.frame(fixef(ModTemp)$cond))))
        degree=rbind(degree,attr(logLik(ModTemp), "df"))
        logLik=rbind(logLik,logLik(ModTemp)[1])
        AICF=rbind(AICF,AIC[h])
        standE=c(standE,list(summary(ModTemp)$coefficients$cond[,2]))
        FormulaF=rbind(FormulaF,FormulaFixTemp)
      }
      
      for (i in 1:length(AIC)){
        if (is.na(AIC[i])){print(paste(i,"Warning : AIC=NA"))}
      }
      DeltaAIC=AIC_prec-min(AIC,na.rm=T)
      print(paste("Delta AIC =",DeltaAIC,sep=" "))
      if(DeltaAIC>2)
      {
        N=which(AIC==(min(AIC,na.rm=T)))[1]
        BestVar[i]=VarDispo[N]
        VarDispo=VarDispo[!VarDispo %in% VarDispo[N]]
        VarSelect=c(VarSelect,BestVar[i])
        VarSelect_sansI=subset(VarSelect,grepl(":",VarSelect)==F)
        
        FormulaFix=paste0(as.character(FormulaFix),"+",BestVar[i])
        AIC_prec=min(AIC,na.rm=T)
      }
    }
  }
  
  # Retrieves tables for fixed effects and standard errors
  FixeffectsF=ldply(Fixeffects,function(x){data.frame(unlist(x))}) 
  colnames(FixeffectsF)[which(names(FixeffectsF) == "X.Intercept.")] <- "Intercept"
  standEF=ldply(standE,function(x){t(as.data.frame(unlist(x)))})
  colnames(standEF)[which(names(standEF) == "(Intercept)")] <- "Intercept"
  colnames(standEF)=paste("SE", colnames(standEF), sep = "_")
  
  # Join all information for candidate models
  Bilan=cbind(FixeffectsF,standEF,degree,logLik,AICF,(AICF-min(AICF)))
  colnames(Bilan)=c(colnames(FixeffectsF),colnames(standEF),"df","logLik", "AIC","delta")
  Bilansub=subset(Bilan,Bilan$delta<2)
  Bilansub$weight=Weights(Bilansub$AIC)
  Bilansub=Bilansub[order(Bilansub$AIC),]
  
  print(FormulaFix)
  beepr::beep(2)
}

# Null Model
FormulaFix=paste0("Median~JJulien_scaled+I(JJulien_scaled^2)")
# Variables to select and to test in interaction
VarDispo=as.character(c("H_veg_scaled","N_vehi_nuit_1200_scaled","D_houppier_scaled","(Non_arbore+Double_haie+Haie_parallele+Haie_perpendiculaire+Lisiere)"))
# Among those variables which ones must not be tested in interaction
VarDispoSansInter=as.character(c("(JJulien_scaled+I(JJulien_scaled^2))"))
# Random effect
FormulaRandom=paste0("+(1|Site)")
# Family
familyMod="nbinom2"

# Load density table
MaDataActiNewBIND= read.table("./bat-road-collision-risks/Tables/MaDataActiNew_pour_Modelisation.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
MaDataActiNewBIND$Paysage <- factor(MaDataActiNewBIND$Paysage, levels = c("NV","F", "FE", "DPT","SPT","PT"))

# CHOOSE SPECIES
Sp = "Pippip"
MaDataActiNew=subset(MaDataActiNewBIND,MaDataActiNewBIND$Espece==Sp)
MaDataActiNew$Median=as.integer(MaDataActiNew$Median)
MaDataActiNew$Site=as.factor(MaDataActiNew$Site)

# RUN MODEL SELECTION
ModSelection(VarDispo,FormulaFix,FormulaRandom,familyMod)

