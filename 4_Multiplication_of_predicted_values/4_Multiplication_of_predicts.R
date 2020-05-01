
# This step loads all predicted values for the same covariable selected in at least two quantitative models
# Then multiplies the predicts to obtain the product
# If a model is missing, the mean values of the raw data is used

library(dplyr)
library(tibble)
library(ggplot2)
library(Hmisc)
library(purrr)

#List of predict tables for each species and each model and each variable
ls1 <- list.files("./bat-road-collision-risks/SIM_PREDICT",
                  recursive=FALSE,pattern="*predict_sim_table.csv$")

# Tables of raw values
MaDataActiNew=read.table("./bat-road-collision-risks/Tables/MaDataActiNew_pour_Modelisation.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
MaDataRiskNew=read.table("./bat-road-collision-risks/Tables/MaDataRiskNew_pour_Modelisation.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
MaDataLagVehiNew=read.table("./bat-road-collision-risks/Tables/MaDataLagVehiNew_pour_Modelisation.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")

# List of species
ListeVariables=data.frame(Species=substr(ls1,0,6))
ListeVariables$Species=as.character(ListeVariables$Species)

# Standardise names
ListeVariables$Species[ListeVariables$Species == "FermeL"] <- "Ferme"
ListeVariables$Species[ListeVariables$Species == "FermeR"] <- "Ferme"
ListeVariables$Species[ListeVariables$Species == "FermeA"] <- "Ferme"
ListeVariables$Species[ListeVariables$Species == "MyoGTA"] <- "MyoGT"
ListeVariables$Species[ListeVariables$Species == "MyoGTL"] <- "MyoGT"
ListeVariables$Species[ListeVariables$Species == "MyoGTR"] <- "MyoGT"
ListeVariables$Species[ListeVariables$Species == "Pip35A"] <- "Pip35"
ListeVariables$Species[ListeVariables$Species == "Pip35R"] <- "Pip35"
ListeVariables$Species[ListeVariables$Species == "Pip35L"] <- "Pip35"
ListeVariables$Species[ListeVariables$Species == "PlespA"] <- "Plesp"
ListeVariables$Species[ListeVariables$Species == "PlespR"] <- "Plesp"
ListeVariables$Species[ListeVariables$Species == "SemiOu"] <- "SemiOuvert"

# Table with Species and Model (Density, Risk, Vehicle-avoidance) as column
ListeVariables$Modele=substr(sub( "_.*", "", ls1),
                             nchar(sub( "_.*", "", ls1))-3,
                             nchar(sub( "_.*", "", ls1)))

# Add a column with the variables
ListeVariables$Variable=sub( ".*_", "", sub( "_predict_sim_table.csv", "", ls1))
ListeVariables$Variable[ListeVariables$Variable == "1200"] <- "N_vehi_nuit_1200"
ListeVariables$Variable[ListeVariables$Variable == "houppier"] <- "D_houppier"
ListeVariables$Variable[ListeVariables$Variable == "veg"] <- "H_veg"

ListeVariables2=unique(ListeVariables)

# Read tables and predicts
# Merge 
# rbind all tables
Liste_Sp=names(table(ListeVariables$Species))
Pred_FINAL=data.frame()
for (i in 1:length(Liste_Sp)){
  
  Sp=Liste_Sp[i]
  print(Sp)
  SubSp=subset(ListeVariables2,ListeVariables2$Species==Sp)
  
  Liste_Variables=names(table(SubSp$Variable))
  for(j in 1:length(Liste_Variables)){
    
    NomVariable=Liste_Variables[j]
    SubSpVariable = subset(SubSp, SubSp$Variable == NomVariable)
    
    # For each species, if at least 2 models per variable
    if(length(names(table(SubSpVariable$Modele)))>1){
      
      # DENSITY
      if ("Acti" %in% SubSpVariable$Modele){
        Pred_Acti= read.table(paste("./bat-road-collision-risks/SIM_PREDICT/",Sp,"Acti_", NomVariable, "_predict_sim_table.csv",sep=""), header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
        names(Pred_Acti)[names(Pred_Acti) == 'predicted'] <- 'Acti_predicted'
        Pred_Acti$predictedRaw=NULL
        if(NomVariable=="Paysage"){
          Pred_Acti=Pred_Acti[order(Pred_Acti$Paysage),]
        }
      }
      
      # RISK
      if ("Risk" %in% SubSpVariable$Modele){
        Pred_Risk= read.table(paste("./bat-road-collision-risks/SIM_PREDICT/",Sp,"Risk_", NomVariable, "_predict_sim_table.csv",sep=""), header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
        names(Pred_Risk)[names(Pred_Risk) == 'predicted'] <- 'Risk_predicted'
        Pred_Risk$predictedRaw=NULL
        if(NomVariable=="Paysage"){
          Pred_Risk=Pred_Risk[order(Pred_Risk$Paysage),]
        }
      }
      
      
      # VEHICLE AVOIDANCE
      if ("Vehi" %in% SubSpVariable$Modele){
        Pred_Lag= read.table(paste("./bat-road-collision-risks/SIM_PREDICT/",Sp,"LagVehi_", NomVariable, "_predict_sim_table.csv",sep=""), header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")
        names(Pred_Lag)[names(Pred_Lag) == 'predicted'] <- 'Lag_predicted'
        Pred_Lag$predictedRaw=NULL
        if(NomVariable=="Paysage"){
          Pred_Lag=Pred_Lag[order(Pred_Lag$Paysage),]
        }
      }
      
      
      # Predicts are aligned for all 3 models
      
      if (exists("Pred_Acti")){
        if(exists("Pred_Risk")){
          Predicted_merge1=cbind(Pred_Acti,Pred_Risk$Risk_predicted)
          names(Predicted_merge1)[names(Predicted_merge1) == 'Pred_Risk$Risk_predicted'] <- 'Risk_predicted'
        }else{
          Predicted_merge1=Pred_Acti
          Predicted_merge1$Risk_predicted=NA
        }}else if(exists("Pred_Risk")){
          Predicted_merge1=Pred_Risk
          Predicted_merge1$Acti_predicted=NA
        }
      if(exists("Pred_Lag")){
        Predicted_merge2=cbind(Predicted_merge1,Pred_Lag$Lag_predicted)
        names(Predicted_merge2)[names(Predicted_merge2) == 'Pred_Lag$Lag_predicted'] <- 'Lag_predicted'
      }else{
        Predicted_merge2=Predicted_merge1
        Predicted_merge2$Lag_predicted=NA
      }
      
      
      Pred_FINAL=bind_rows(Pred_FINAL,Predicted_merge2)
      
      rm(Pred_Acti)
      rm(Pred_Risk)
      rm(Pred_Lag)
      
    }
  }
}

# Inverse of column vehicle avoidance because we want a proportion of NOT avoided vehicles
Pred_FINAL$LagNotAvoided_predicted=1-Pred_FINAL$Lag_predicted

# Add constants (mean of raw values) in empty columns and multiply predicts

Pred_FINAL$Predicted_Product=NA

# Hypsav
Pred_FINAL$LagNotAvoided_predicted[which(!is.na(Pred_FINAL$Paysage) & Pred_FINAL$Species == "Hypsav")]=
  mean(1-MaDataLagVehiNew$LagvehiCat[which(MaDataLagVehiNew$Espece == "Hypsav")])

# Minsch
# Pred_FINAL$LagNotAvoided_predicted[which(!is.na(Pred_FINAL$Paysage) & Pred_FINAL$Species == "Minsch")]=
#   mean(1-MaDataLagVehiNew$LagvehiCat[which(MaDataLagVehiNew$Espece=="Minsch")])

# Myodau
# Pred_FINAL$Acti_predicted[which(!is.na(Pred_FINAL$H_veg) & Pred_FINAL$Species == "Myodau")]=
#   mean(MaDataActiNew$Median[which(MaDataActiNew$Espece=="Myodau")])

# MyoGT
# Pred_FINAL$Acti_predicted[which(!is.na(Pred_FINAL$N_vehi_nuit_1200) & Pred_FINAL$Species == "MyoGT")]=
#   mean(MaDataActiNew$Median[which(MaDataActiNew$Espece=="MyoGT")])

# Pip35
Pred_FINAL$Acti_predicted[which(!is.na(Pred_FINAL$N_vehi_nuit_1200) & Pred_FINAL$Species == "Pip35")]=
  mean(MaDataActiNew$Median[which(MaDataActiNew$Espece=="Pip35")])

# Pippip
Pred_FINAL$LagNotAvoided_predicted[which(!is.na(Pred_FINAL$Paysage) & Pred_FINAL$Species == "Pippip")]=
  mean(1-MaDataLagVehiNew$LagvehiCat[which(MaDataLagVehiNew$Espece=="Pippip")])
# Pred_FINAL$Acti_predicted[which(!is.na(Pred_FINAL$N_vehi_nuit_1200) & Pred_FINAL$Species == "Pippip")]=
#   mean(MaDataActiNew$Median[which(MaDataActiNew$Espece=="Pippip")])

# Pippyg
# Pred_FINAL$Acti_predicted[which(!is.na(Pred_FINAL$N_vehi_nuit_1200) & Pred_FINAL$Species == "Pippyg")]=
#   mean(MaDataActiNew$Median[which(MaDataActiNew$Espece=="Pippyg")])

# Plesp
# Pred_FINAL$LagNotAvoided_predicted[which(!is.na(Pred_FINAL$N_vehi_nuit_1200) & Pred_FINAL$Species == "Plesp")]=
#   mean(1-MaDataLagVehiNew$LagvehiCat[which(MaDataLagVehiNew$Espece=="Plesp")])
Pred_FINAL$LagNotAvoided_predicted[which(!is.na(Pred_FINAL$JJulien) & Pred_FINAL$Species == "Plesp")]=
  mean(1-MaDataLagVehiNew$LagvehiCat[which(MaDataLagVehiNew$Espece=="Plesp")])

# SRE
Pred_FINAL$Acti_predicted[which(!is.na(Pred_FINAL$N_vehi_nuit_1200) & Pred_FINAL$Species == "Ferme")]=
  mean(MaDataActiNew$Median[which(MaDataActiNew$Espece=="Ferme")])

# Create a new table where unused rows are deleted: subset rows where there is a value in each column: Acti, Risk, Lag
Pred_FINALSub=subset(Pred_FINAL, (!is.na(Pred_FINAL$Acti_predicted) & !is.na(Pred_FINAL$Risk_predicted) &
                                    !is.na(Pred_FINAL$LagNotAvoided_predicted)))

# Multiplication of predicts
Pred_FINALSub$Predicted_Product=
  Pred_FINALSub$Acti_predicted*
  Pred_FINALSub$Risk_predicted*
  Pred_FINALSub$LagNotAvoided_predicted


# Calculate median and confidence intervals for each value in the gradient of variables

Liste_Sp=names(table(ListeVariables$Species))
for (i in 1:length(Liste_Sp)){
  
  Sp=Liste_Sp[i]
  print(Sp)
  Pred_FINAL_Sp=subset(Pred_FINALSub, Pred_FINALSub$Species==Sp)
  
  # JJulien
  if(TRUE %in% !is.na(Pred_FINAL_Sp$JJulien)){
    Pred_FINAL_Sp_JJulien=subset(Pred_FINAL_Sp, !is.na(Pred_FINAL_Sp$JJulien))
    Pred_FINAL_Sp_JJulien_aggr=aggregate(Predicted_Product ~ Species + JJulien,
                                          data = Pred_FINAL_Sp_JJulien,
                                          FUN = function(x) c(Mean = mean(x),
                                                              (Quantiles = quantile(x, probs=c(2.5, 50, 97.5)/100) ))) 
    
    
    Quantiles_JJulien=as.data.frame(Pred_FINAL_Sp_JJulien_aggr[,3])
    Pred_FINAL_Sp_JJulien_aggr2=cbind(Pred_FINAL_Sp_JJulien_aggr[,1:2], Quantiles_JJulien)
    colnames(Pred_FINAL_Sp_JJulien_aggr2)[3:6]=c("Mean", "Q2_5", "Q50", "Q97_5")
    Pred_FINAL_Sp_JJulien_aggr2$Date=as.Date(strptime(Pred_FINAL_Sp_JJulien_aggr2$JJulien, "%j"))
    
    # Plot
    png(filename=paste("./bat-road-collision-risks/GRAPHS/", Sp,"_Predict_Multiplication_JJulien",".png",sep=""), height=1200, width=1400,res=300)
    plot1=ggplot(Pred_FINAL_Sp_JJulien_aggr2, aes(Date, Q50)) +
      geom_line(size=1)  +
      geom_ribbon(aes(ymin = Q2_5, ymax = Q97_5), alpha = .1)+
      labs(x = "Month",
           y = "Number of bat passes \nat collision risk/night") +
      scale_x_date(date_breaks = "1 month", 
                   labels = function(x) format(x, "%m")) +
      scale_y_log10()+
      ggtitle(Sp) + 
      theme_bw(base_size = 15)
    print(plot1)
    dev.off()
  }
  
  # Hveg
  if(TRUE %in% !is.na(Pred_FINAL_Sp$H_veg)){
    Pred_FINAL_Sp_Hveg=subset(Pred_FINAL_Sp, !is.na(Pred_FINAL_Sp$H_veg))
    Pred_FINAL_Sp_Hveg_aggr=aggregate(Predicted_Product ~ Species + H_veg,
                                          data = Pred_FINAL_Sp_Hveg,
                                          FUN = function(x) c(Mean = mean(x),
                                                              (Quantiles = quantile(x, probs=c(2.5, 50, 97.5)/100) ))) 
    
    Quantiles_Hveg=as.data.frame(Pred_FINAL_Sp_Hveg_aggr[,3])
    Pred_FINAL_Sp_Hveg_aggr2=cbind(Pred_FINAL_Sp_Hveg_aggr[,1:2], Quantiles_Hveg)
    colnames(Pred_FINAL_Sp_Hveg_aggr2)[3:6]=c("Mean", "Q2_5", "Q50", "Q97_5")
    
    # Plot 
    png(filename=paste("./bat-road-collision-risks/GRAPHS/",  Sp,"_Predict_Multiplication_Hveg",".png",sep=""), height=1200, width=1400,res=300)
    plot2=ggplot(Pred_FINAL_Sp_Hveg_aggr2, aes(H_veg, Q50)) +
      geom_line(size=1)  +
      geom_ribbon(aes(ymin = Q2_5, ymax = Q97_5), alpha = .1)+
      labs(x = "Tree height (m)",
           y = "Number of bat passes \nat collision risk/night") +
      scale_fill_discrete(guide=FALSE)+
      scale_y_log10()+
      ggtitle(Sp) + 
      theme_bw(base_size = 15)
    print(plot2)
    dev.off()
  }
  
  # Paysage
  if(TRUE %in% !is.na(Pred_FINAL_Sp$Paysage)){
    Pred_FINAL_Sp_Paysage=subset(Pred_FINAL_Sp, !is.na(Pred_FINAL_Sp$Paysage))
    Pred_FINAL_Sp_Paysage_aggr=aggregate(Predicted_Product ~ Species + Paysage,
              data = Pred_FINAL_Sp_Paysage,
              FUN = function(x) c(Mean = mean(x),
                                  (Quantiles = quantile(x, probs=c(2.5, 50, 97.5)/100) ))) 

    Quantiles_Paysage=as.data.frame(Pred_FINAL_Sp_Paysage_aggr[,3])
    Pred_FINAL_Sp_Paysage_aggr2=cbind(Pred_FINAL_Sp_Paysage_aggr[,1:2], Quantiles_Paysage)
    colnames(Pred_FINAL_Sp_Paysage_aggr2)[3:6]=c("Mean", "Q2_5", "Q50", "Q97_5")
    Pred_FINAL_Sp_Paysage_aggr2$Paysage=as.factor(Pred_FINAL_Sp_Paysage_aggr2$Paysage)
    
    # Plot 
    Pred_FINAL_Sp_Paysage_aggr2$Paysage=factor(Pred_FINAL_Sp_Paysage_aggr2$Paysage, levels = c("F", "FE", "DPT","SPT","PT","NV"))
    png(filename=paste("./bat-road-collision-risks/GRAPHS/",  Sp,"_Predict_Multiplication_Landscape",".png",sep=""), width=1050, height=600, res=300)
    plot3=ggplot(Pred_FINAL_Sp_Paysage_aggr2, aes(Paysage, Q50)) +
      geom_point(position = position_dodge(.1)) +
      geom_errorbar(aes(ymin = Q2_5, ymax = Q97_5), width = 0.3) +
      #  coord_cartesian(ylim = c(0, 1))+
      labs(x="Landscape", 
           y="Number of bat passes \nat collision risk/night") +
      scale_x_discrete(labels = levels(Pred_FINAL_Sp_Paysage_aggr2$Paysage))+ 
      scale_y_log10()+
      theme_bw(base_size = 12)+
      ggtitle(Sp)
    print(plot3)
    dev.off()
  }
  
  # N_vehi_nuit_1200
  if(TRUE %in% !is.na(Pred_FINAL_Sp$N_vehi_nuit_1200)){
    Pred_FINAL_Sp_N_vehi_nuit_1200=subset(Pred_FINAL_Sp, !is.na(Pred_FINAL_Sp$N_vehi_nuit_1200))
    Pred_FINAL_Sp_N_vehi_nuit_1200_aggr=aggregate(Predicted_Product ~ Species + N_vehi_nuit_1200,
                                       data = Pred_FINAL_Sp_N_vehi_nuit_1200,
                                       FUN = function(x) c(Mean = mean(x),
                                                           (Quantiles = quantile(x, probs=c(2.5, 50, 97.5)/100) )))
    
    Quantiles_N_vehi_nuit_1200=as.data.frame(Pred_FINAL_Sp_N_vehi_nuit_1200_aggr[,3])
    Pred_FINAL_Sp_N_vehi_nuit_1200_aggr2=cbind(Pred_FINAL_Sp_N_vehi_nuit_1200_aggr[,1:2], Quantiles_N_vehi_nuit_1200)
    colnames(Pred_FINAL_Sp_N_vehi_nuit_1200_aggr2)[3:6]=c("Mean", "Q2_5", "Q50", "Q97_5")
    
    # Plot
    png(filename=paste("./bat-road-collision-risks/GRAPHS/",  Sp,"_Predict_Multiplication_Nvehi_Nuit",".png",sep=""), height=1200, width=1400,res=300)
    plot4=ggplot(Pred_FINAL_Sp_N_vehi_nuit_1200_aggr2, aes(N_vehi_nuit_1200, Q50)) +
      geom_line(size=1)  +
      geom_ribbon(aes(ymin = Q2_5, ymax = Q97_5), alpha = .1)+
      labs(x = "Number of vehicles/night",
           y = "Number of bat passes \nat collision risk/night") +
      scale_fill_discrete(guide=FALSE)+
      scale_y_log10()+
      ggtitle(Sp) + 
      theme_bw(base_size = 15)
    print(plot4)
    dev.off()
  }
  
  
}

