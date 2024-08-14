########## 2 - Creation of the virtual species's pseudo-absences, using EcoPA package ##########

library(devtools)
install_github("JosephineBroussin/EcoPA")
library(EcoPA)


########## 1) Creation of the species's niche inside an ecological space ##########
### Set the working directory to the path where you downloaded the "Example_Virtual_Species" folder

### Import environmental predictors 

setwd("./Exemple_Virtual_Species")

presences = read.csv("Presences_VS.csv")

niche = SpeciesNiche(data = presences[, -c(1,2)],
                     bins_sizes = c(1, # SBT dimension will be represented with bins of size 1
                                    1, # SST dimension will be represented with bins of size 1
                                    1, # Salinity dimension will be represented with bins of size 1
                                    0.5, # Wind dimension will be represented with bins of size 0.5
                                    10), # Ice dimension will be represented with bins of size 10
                     niche_border = c(-2, 35, # SBT dimension goes from -2 to 35°C
                                      -2, 35, # SST dimension goes from -2 to 35°C
                                      0 ,40, # Salinity dimension goes from 0 to 40°C
                                      0, 15, # Wind dimension goes from 0 to 15 m/s
                                      0, 100)) # Ice dimension goes from 0 to 100% coverage



########## 2) Generation of the species's pseudo-absence (PA) in this ecological space ##########
pseudo_abs = PAGeneration(data = niche,
                          nb_pa = c(10000*10, 4000*10, 2000*10, 1000*10), ### Different number of PA * 10 to test the influence of the set on modeling outputs
                          ratio_pa_in = c(0, 0.5, 1)) ### Different ratio of PA : 0 PA inside the niche, half of them inside, all of them inside

########## 3) Saving outputs ##########
### Rounded presences

rounded_pres = niche[[3]]
colnames(rounded_pres) = niche[[2]]

write.csv(rounded_pres, 
          "Rounded_presences_VS.csv", 
          row.names = F)

### Pseudo-absences

for (i in 1:length(pseudo_abs)){
  
  pseudo_abs_step = pseudo_abs[[i]]
  
  write.csv(pseudo_abs_step, 
            paste0("PA_", names(pseudo_abs)[i], ".csv"), 
            row.names = T)
}
