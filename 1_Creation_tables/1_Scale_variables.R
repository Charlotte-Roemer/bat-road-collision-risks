
# In this step, all variables are scaled and this table will be used for backtransformation later
# Table created: Variables_scalees_a_la_main_pour_modelisation.csv

library(dplyr)
library(lubridate)

# Load metadata 
# DateNuit = operational nights for each sampled site
# Variables = Coordinates and landscape description
# N_vehi = count of vehciles per night and per site
Variables00 = read.csv("./bat-road-collision-risks/Tables/Table_points_obs_2016_et_2017_COORDONNEES.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "",quote="")
DateNuit = read.table("./bat-road-collision-risks/Tables/Liste_DateNuit.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "",quote="")
N_vehi= read.table("./bat-road-collision-risks/Tables/N_vehi_nuit_site.csv", header = TRUE, sep= ";", dec = ".", fill = TRUE,comment.char = "")

# Remove unnecessary columns
Variables0=cbind(Variables00$id,as.character(Variables00$Paysage),Variables00$H_moy ,Variables00$Larg_route ,Variables00$Dist_houppier, Variables00$X, Variables00$Y)
colnames(Variables0)=c("id","Paysage","H_veg","L_route","D_houppier","X","Y")
Variables0=as.data.frame(Variables0)
Variables0$H_veg=as.numeric(as.character(Variables0$H_veg))
Variables0$L_route=as.numeric(as.character(Variables0$L_route))
Variables0$D_houppier=as.numeric(as.character(Variables0$D_houppier))

# Merge Variables with DateNuit
DateNuit$Point=as.character(DateNuit$Point)
Variables0$id=as.character(Variables0$id)
Variables01=full_join(Variables0, DateNuit, by=c("id"="Point"))

# Merge Variables with N_vehicule_nuit
Variables=merge(N_vehi,Variables01, by.x="Site",by.y="id",all.y=T)
Variables$N_vehi_nuit_1200[Variables$Site==10] <- 400
Variables$N_vehi_nuit_1200[Variables$Site==123] <- Variables$N_vehi_nuit_1200[Variables$Site==116]

# For landscape type = No_vegetation (Non_arbore)
# use the mean value of all other sites for tree height (H_veg) and distance to foliage (D_houppier)
for (i in 1:nrow(Variables)){
  if(Variables$Paysage[i]=="Non_arbore"){
    Variables$H_veg[i]<-10.46
    Variables$D_houppier[i]<-0.24
  }
  else {
    Variables$H_veg[i]<-Variables$H_veg[i]
    Variables$D_houppier[i]<-Variables$D_houppier[i]
  }
}

# Create Julian Day vector
Variables$JJulien = yday(as.Date(Variables$Date, "%d/%m/%y")) #as.Date(strptime(Variables$JJulien, "%j")) #backtransform

#Center-scale
Variables$H_veg_scaled=(Variables$H_veg-mean(Variables$H_veg))/sd(Variables$H_veg)
Variables$D_houppier_scaled=(sqrt(Variables$D_houppier+4)-mean(sqrt(Variables$D_houppier+4)))/sd(sqrt(Variables$D_houppier+4))
Variables$JJulien_scaled=(Variables$JJulien-mean(Variables$JJulien))/sd(Variables$JJulien)
Variables$N_vehi_nuit_1200_scaled=(sqrt(Variables$N_vehi_nuit_1200)-mean(sqrt(Variables$N_vehi_nuit_1200)))/sd(sqrt(Variables$N_vehi_nuit_1200))
Variables$L_route_scaled=(Variables$L_route-mean(Variables$L_route))/sd(Variables$L_route)
Variables$X=as.numeric(Variables$X)
Variables$Y=as.numeric(Variables$Y)
Variables$X_scaled=(Variables$X-mean(Variables$X))/sd(Variables$X)
Variables$Y_scaled=(Variables$Y-mean(Variables$Y))/sd(Variables$Y)

# Save table
write.table(Variables,"./bat-road-collision-risks/Tables/Variables_scalees_a_la_main_pour_modelisation.csv",sep=";",row.names=F)




