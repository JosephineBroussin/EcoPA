########## 1 - Creation of a virtual species with 5 predictors, climato 2000-2020 ##########
#### Download the "Example_Virtual_Species" folder


library(raster)
library(ggplot2)
library(virtualspecies) ## package by Leroy used to create the virtual species. Every details about this package can be found on his website.
library(caret)
library(dplyr)
library(plyr)


########## 1) Responses curves to predictors ##########
### Set the working directory to the path where you downloaded the "Example_Virtual_Species" folder

### Import environmental predictors

setwd("./Exemple_Virtual_Species/Environmental_Data")

sbt = stack("SBT_climato_2000-2020.grd")
sst = stack("SST_climato_2000-2020.grd")
sal = stack("Salinity_climato_2000-2020.grd")
wind = stack("Wind_climato_2000-2020.grd")
ice = stack("Ice_climato_2000-2020.grd")

# The bathymetry is added only to remove the high sea areas in the suitability estimation
# and therefore in the presences. If these zones (the majority in terms of size, with SBT values
# between 0 and 3°C) are included, the majority of presences will have low SBT values, leading to a skewed response curve.
bathy = stack("Bathy.grd")

### Creation of the responses curves (gaussian)
my_parameters <- formatFunctions(SBT = c(fun = 'dnorm', mean = 0, sd = 15),
                                 SST = c(fun = 'dnorm', mean = 0, sd = 15),
                                 Salinity = c(fun = 'dnorm', mean = 35, sd = 10),
                                 Ice = c(fun = 'dnorm', mean = 0, sd = 0.5),
                                 Wind = c(fun = 'dnorm', mean = 0, sd = 10),
                                 Bathy = c(fun = 'betaFun', p1 = -200, p2 = 0, alpha = 0.9, gamma = 0.08))


########## 2) Environment suitability according to those responses curves ##########
my_env = stack(sbt, sst, sal, wind, ice, bathy)

## Focus on the North-East Atlantic
my_env = crop(my_env,  extent(-35,50,36,90))


virtspecies = generateSpFromFun(raster.stack = my_env[[c("SBT","SST", "Salinity", "Wind", "Ice", "Bathy")]],
                                parameters = my_parameters,
                                plot = TRUE)

virtspecies_suit = virtspecies$suitab.raster



########## 3) Extraction of presence-absence ##########

### Conversion of env. suit. to presence-absence
virtspecies_PA <- convertToPA(virtspecies,
                              beta = 0.2, alpha = -0.0000001, plot = TRUE)

pres_abs_virtspecies = raster(virtspecies_PA$pa.raster)


### Extraction of the coordinates (latitude x longitude) of 10,000 presences

coord_presences = data.frame(coordinates(pres_abs_virtspecies)[sample(which(pres_abs_virtspecies[] == 1),
                                                                      10000, replace=TRUE),1:2])


### Extraction of environmental values found at each coordinate

for(i in 1:nlayers(my_env)){

  coord_presences[,i+2] = raster::extract(my_env[[i]], coord_presences[,1:2])
}

coord_presences = coord_presences[,-c(ncol(coord_presences))]

colnames(coord_presences) =
  c("X", "Y", "SBT", "SST", "Salinity", "Wind", "Ice")


setwd('..')
write.csv(coord_presences, "Presences_VS.csv", row.names = F)
