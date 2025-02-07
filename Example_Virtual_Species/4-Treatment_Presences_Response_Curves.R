########## 4 - Treatment density of presences and response curves from the models ##########

library(dplyr)
library(caret)
library(biomod2)
library(RColorBrewer)
library(ggpubr)

rm(list = ls())

##### We do not have the the response to the environment of the species for the same env. values (x-axis)
##### when issued from the presences created in 1) or from the response curves
##### here we "align' the two by interpolated the density of presences to the env. values of the response curves from the model

### Set the working directory to the path where you downloaded the "Example_Virtual_Species" folder

setwd("./Example_Virtual_Species/Presences_PA")
presences = read.csv("Rounded_Presences_VS.csv")

########## 1) Normalization of the density of presences to env. variables ##########

#### How many presences for each env. values ? = density of presences
sbt_pres = presences %>% dplyr::count(SBT)
sst_pres = presences %>% dplyr::count(SST)
sal_pres = presences %>% dplyr::count(Salinity)
ice_pres = presences %>% dplyr::count(Ice)
wind_pres = presences %>% dplyr::count(Wind)

#### Normalization
process <- preProcess(as.data.frame(sbt_pres$n), method=c("range"))
sbt_pres$n_norm <- predict(process, as.data.frame(sbt_pres$n))

process <- preProcess(as.data.frame(sst_pres$n), method=c("range"))
sst_pres$n_norm <- predict(process, as.data.frame(sst_pres$n))

process <- preProcess(as.data.frame(sal_pres$n), method=c("range"))
sal_pres$n_norm <- predict(process, as.data.frame(sal_pres$n))

ice_pres$n_norm = 1
ice_pres = rbind(ice_pres, c(5, 5000, 0.2))
ice_pres = rbind(ice_pres, c(6, 2000, 0))
ice_pres = rbind(ice_pres, c(20, 0, 0))
ice_pres = rbind(ice_pres, c(100, 0, 0))

process <- preProcess(as.data.frame(wind_pres$n), method=c("range"))
wind_pres$n_norm <- predict(process, as.data.frame(wind_pres$n))




########## 2) Alignement by interpolation of the density of presences to the env. values. of the response curves from the models ##########
setwd('../Models')
model =  dir(path = ".")

for (i in 1:length(model)){

  setwd(paste0("./", model[i]))

  ## Download the model to the global environment of R
  file = list.files(pattern = ".out")
  load(file)
  mod = get(file)

  ## Extract the response curves
  setwd('..')
  plot =  bm_PlotResponseCurves(bm.out = mod,
                                models.chose = "all",
                                new.env = get_formal_data(mod,'expl.var'),
                                show.variables= get_formal_data(mod,'expl.var.names'),
                                do.bivariate = FALSE,
                                fixed.var.metric = 'median',
                                col = brewer.pal(10, "Spectral"),
                                legend = TRUE,
                                data_species = get_formal_data(mod,'resp.var'))

  plot = as.data.frame(plot$tab)

  sbt_rc = plot %>% filter(expl.name=="SBT")
  sst_rc = plot %>% filter(expl.name=="SST")
  sal_rc = plot %>% filter(expl.name=="Salinity")
  ice_rc = plot %>% filter(expl.name=="Ice")
  wind_rc = plot %>% filter(expl.name=="Wind")

  ## Mean of every runs (50)

  sbt_rc_mean = data.frame()

  for (j in seq(1,5000,50)){

    sbt_rc_mean_step = c(sbt_rc[j,3], mean(sbt_rc[j:j+49,5]))

    sbt_rc_mean = rbind(sbt_rc_mean, sbt_rc_mean_step)

  }


  sst_rc_mean = data.frame()

  for (j in seq(1,5000,50)){

    sst_rc_mean_step = c(sst_rc[j,3], mean(sst_rc[j:j+49,5]))

    sst_rc_mean = rbind(sst_rc_mean, sst_rc_mean_step)

  }


  sal_rc_mean = data.frame()

  for (j in seq(1,5000,50)){

    sal_rc_mean_step = c(sal_rc[j,3], mean(sal_rc[j:j+49,5]))

    sal_rc_mean = rbind(sal_rc_mean, sal_rc_mean_step)

  }


  ice_rc_mean = data.frame()

  for (j in seq(1,5000,50)){

    ice_rc_mean_step = c(ice_rc[j,3], mean(ice_rc[j:j+49,5]))

    ice_rc_mean = rbind(ice_rc_mean, ice_rc_mean_step)

  }


  wind_rc_mean = data.frame()

  for (j in seq(1,5000,50)){

    wind_rc_mean_step = c(wind_rc[j,3], mean(wind_rc[j:j+24,5]))

    wind_rc_mean = rbind(wind_rc_mean, wind_rc_mean_step)

  }


  ## Interpolation itself
  sbt_pres_inter = as.data.frame(approx(sbt_pres$SBT,
                                        sbt_pres$n_norm$`sbt_pres$n`,
                                        xout = sbt_rc_mean[,1]))

  sst_pres_inter = as.data.frame(approx(sst_pres$SST,
                                        sst_pres$n_norm$`sst_pres$n`,
                                        xout = sst_rc_mean[,1]))

  sal_pres_inter = as.data.frame(approx(sal_pres$Salinity,
                                        sal_pres$n_norm$`sal_pres$n`,
                                        xout = sal_rc_mean[,1]))

  ice_pres_inter = as.data.frame(approx(ice_pres$Ice,
                                        ice_pres$n_norm,
                                        xout = ice_rc_mean[,1]))

  wind_pres_inter = as.data.frame(approx(wind_pres$Wind,
                                          wind_pres$n_norm$`wind_pres$n`,
                                          xout = wind_rc_mean[,1]))

  pres_fin = cbind(sbt_pres_inter, sst_pres_inter)
  pres_fin = cbind(pres_fin, sal_pres_inter)
  pres_fin = cbind(pres_fin, ice_pres_inter)
  pres_fin = cbind(pres_fin, wind_pres_inter)

  colnames(pres_fin) = c("SBT_x", "SBT_y",
                         "SST_x", "SST_y",
                         "Salinity_x", "Salinity_y",
                         "Ice_x", "Ice_y",
                         "Wind_x", "Wind_y")


  rc_mean_fin = cbind(sbt_rc_mean, sst_rc_mean)
  rc_mean_fin = cbind(rc_mean_fin, sal_rc_mean)
  rc_mean_fin = cbind(rc_mean_fin, ice_rc_mean)
  rc_mean_fin = cbind(rc_mean_fin, wind_rc_mean)

  colnames(rc_mean_fin) = c("SBT_x", "SBT_y",
                            "SST_x", "SST_y",
                            "Salinity_x", "Salinity_y",
                            "Ice_x", "Ice_y",
                            "Wind_x", "Wind_y")

  setwd(paste0("./", model[i]))
  write.csv(pres_fin, "Presences_norm_inter_VS.csv", row.names = F) ## Normalized density of presences interpolated to the env. values of the response curves from the model
  write.csv(rc_mean_fin, "RC_VS.csv", row.names = F) ## Response curves from the model
  setwd("..")

}



