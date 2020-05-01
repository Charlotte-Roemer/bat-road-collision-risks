
# This step creates a table newdata with 60 values for each covariable
# Those values are predicted 20,000 times along a normal distribution of parameters mean=cond$fit and SE=cond$se.fit
# Creates a table for each species and each variable

library(ggeffects)
library(ggplot2)
library(stringr)
library(beepr)
library(glmmTMB)

# Load scaled variables
Variables2= read.table("./bat-road-collision-risks/Tables/Variables_scalees_a_la_main_pour_modelisation.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")

Liste_Sp=c("Pippip","Pip35","Pippyg","Hypsav","Minsch","Nyclei","MyoGT","Myodau", "Eptser",
           "Ferme", "Ouvert", "SemiOuvert")

for (i in 1:length(Liste_Sp)){
  
  Sp=Liste_Sp[i]
  print(Sp)
  
  # Load model of the species for density
  BestModel <- readRDS(paste("./bat-road-collision-risks/SAVED MODELS/", Sp, "_LagVehi_BestModel.rds", sep=""))
  
  ############################################################# D_houppier ####
  if(grepl("D_houppier", formula(BestModel)[3])){
    print("D_houppier")
    
    # Create simulated table
    newdata = data.frame(JJulien_scaled=0,
                         Paysage="F",
                         D_houppier_scaled=(c(-30:30)/10),
                         N_vehi_nuit_1200_scaled=0,
                         H_veg_scaled=0,
                         Site="bla",
                         SP="bli")
    # Predicts response
    cond <- predict(BestModel, newdata=newdata, type="link",se.fit=T)
    
    # Bootstrap for 20000 responses for each value of the descriptor gradient
    predictedRaw=vector()
    for (z in 1:20000)
    {
      predictedRaw=c(predictedRaw,rnorm(length(cond$fit),mean = cond$fit, sd=cond$se.fit))
    }
    pr1=data.frame("D_houppier_scaled"=rep(newdata$D_houppier_scaled,20000),predictedRaw)
    
    # Backtransformation of response
    pr1$predicted=1/(1+exp(-pr1$predictedRaw))
    
    # Backtransform before scaling
    pr1$D_houppier=((pr1$D_houppier_scaled*sd(sqrt(Variables2$D_houppier+4)))+mean(sqrt(Variables2$D_houppier+4)))^2-4
    
    # Save table
    PredictTable=cbind(rep(Sp,nrow(pr1)),as.data.frame(pr1))
    colnames(PredictTable)[1]="Species"
    write.table(as.data.frame(PredictTable),paste("./bat-road-collision-risks/SIM_PREDICT/", Sp,"LagVehi_D_houppier_predict_sim_table",".csv",sep=""),sep=";",row.names=F)

  }
  
  ############################################################# N_vehi_nuit_1200 ####
  if(grepl("N_vehi_nuit_1200", formula(BestModel)[3])){
    print("N_vehi_nuit_1200")
    
    # Create simulated table
    newdata = data.frame(JJulien_scaled=0,
                         Paysage="F",
                         D_houppier_scaled= 0,
                         N_vehi_nuit_1200_scaled= (c(-30:30)/10),
                         H_veg_scaled=0,
                         Site="bla",
                         SP="bli")
    # Predicts response
    cond <- predict(BestModel, newdata=newdata, type="link",se.fit=T)
    
    # Bootstrap for 20000 responses for each value of the descriptor gradient
    predictedRaw=vector()
    for (z in 1:20000)
    {
      predictedRaw=c(predictedRaw,rnorm(length(cond$fit),mean = cond$fit, sd=cond$se.fit))
    }
    pr2=data.frame("N_vehi_nuit_1200_scaled"=rep(newdata$N_vehi_nuit_1200_scaled,20000),predictedRaw)
    
    # Backtransformation of response
    pr2$predicted=1/(1+exp(-pr2$predictedRaw))
    
    # Backtransform before scaling
    pr2$N_vehi_nuit_1200=((pr2$N_vehi_nuit_1200_scaled*sd(sqrt(Variables2$N_vehi_nuit_1200)))+mean(sqrt(Variables2$N_vehi_nuit_1200)))^2
    
    # Save table
    PredictTable=cbind(rep(Sp,nrow(pr2)),as.data.frame(pr2))
    colnames(PredictTable)[1]="Species"
    write.table(as.data.frame(PredictTable),paste("./bat-road-collision-risks/SIM_PREDICT/", Sp,"LagVehi_N_vehi_nuit_1200_predict_sim_table",".csv",sep=""),sep=";",row.names=F)

  }
  
  ############################################################# H_veg ####
  if(grepl("H_veg", formula(BestModel)[3])){
    print("H_veg")
    # Create simulated table
    newdata = data.frame(JJulien_scaled=0,
                         Paysage="F",
                         D_houppier_scaled= 0,
                         N_vehi_nuit_1200_scaled= 0,
                         H_veg_scaled=(c(-30:30)/10),
                         Site="bla",
                         SP="bli")
    # Predicts response
    cond <- predict(BestModel, newdata=newdata, type="link",se.fit=T)
    
    # Bootstrap for 20000 responses for each value of the descriptor gradient
    predictedRaw=vector()
    for (z in 1:20000)
    {
      predictedRaw=c(predictedRaw,rnorm(length(cond$fit),mean = cond$fit, sd=cond$se.fit))
    }
    pr3=data.frame("H_veg_scaled"=rep(newdata$H_veg_scaled,20000),predictedRaw)
    
    # Backtransformation of response
    pr3$predicted=1/(1+exp(-pr3$predictedRaw))
    
    # Backtransform before scaling
    pr3$H_veg=((pr3$H_veg_scaled*sd(Variables2$H_veg))+mean(Variables2$H_veg))
    
    # Save table
    PredictTable=cbind(rep(Sp,nrow(pr3)),as.data.frame(pr3))
    colnames(PredictTable)[1]="Species"
    write.table(as.data.frame(PredictTable),paste("./bat-road-collision-risks/SIM_PREDICT/", Sp,"LagVehi_H_veg_predict_sim_table",".csv",sep=""),sep=";",row.names=F)
    
  }
  
  ############################################################# Paysage ####
  if(grepl("Paysage", formula(BestModel)[3])){
    print("Paysage")
    
    # Create simulated table
    newdata = data.frame(JJulien_scaled=0,
                         Paysage=c("F", "FE", "DPT","SPT","PT", "NV"),
                         D_houppier_scaled= 0,
                         N_vehi_nuit_1200_scaled= 0,
                         H_veg_scaled=0,
                         Site="bla",
                         SP="bli")
    newdata$Paysage=factor(newdata$Paysage, levels = c("F", "FE", "DPT","SPT","PT", "NV"))
    
    # Predicts response
    cond <- predict(BestModel, newdata=newdata, type="link",se.fit=T)
    
    # Bootstrap for 20000 responses for each value of the descriptor gradient
    predictedRaw=vector()
    for (z in 1:20000)
    {
      predictedRaw=c(predictedRaw,rnorm(length(cond$fit),mean = cond$fit, sd=cond$se.fit))
    }
    pr4=data.frame("Paysage"=rep(newdata$Paysage,20000),predictedRaw)
    
    # Backtransformation of response
    pr4$predicted=1/(1+exp(-pr4$predictedRaw))
    
    # Save table
    PredictTable=cbind(rep(Sp,nrow(pr4)),as.data.frame(pr4))
    colnames(PredictTable)[1]="Species"
    write.table(as.data.frame(PredictTable),paste("./bat-road-collision-risks/SIM_PREDICT/", Sp,"LagVehi_Paysage_predict_sim_table",".csv",sep=""),sep=";",row.names=F)
    
   }
  
  ############################################################# JJulien ####
  if(grepl("JJulien", formula(BestModel)[3])){
    print("JJulien")
    
    # Create simulated table
    newdata = data.frame(JJulien_scaled=(c(-30:30)/10),
                         Paysage="F",
                         D_houppier_scaled= 0,
                         N_vehi_nuit_1200_scaled= 0,
                         H_veg_scaled=0,
                         Site="bla",
                         SP="bli")
    # Predicts response
    cond <- predict(BestModel, newdata=newdata, type="link",se.fit=T)
    
    # Bootstrap for 20000 responses for each value of the descriptor gradient
    predictedRaw=vector()
    for (z in 1:20000)
    {
      predictedRaw=c(predictedRaw,rnorm(length(cond$fit),mean = cond$fit, sd=cond$se.fit))
    }
    pr5=data.frame("JJulien_scaled"=rep(newdata$JJulien_scaled,20000),predictedRaw)
    
    # Backtransformation of response
    pr5$predicted=1/(1+exp(-pr5$predictedRaw))
    
    # Backtransform before scaling
    pr5$JJulien=(pr5$JJulien_scaled*sd(Variables2$JJulien))+mean(Variables2$JJulien)
    pr5$Date=as.Date(strptime(pr5$JJulien, "%j"))
    
    # Save table
    PredictTable=cbind(rep(Sp,nrow(pr5)),as.data.frame(pr5))
    colnames(PredictTable)[1]="Species"
    write.table(as.data.frame(PredictTable),paste("./bat-road-collision-risks/SIM_PREDICT/", Sp,"LagVehi_JJulien_predict_sim_table",".csv",sep=""),sep=";",row.names=F)
    
  }
  
}

beep(2)

