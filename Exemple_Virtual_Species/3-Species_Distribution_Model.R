########## 3 - Species Distribution Model of the virtual species ##########

library(biomod2)

rm(list = ls())

########## 1) Import presences and pseudo-absences ##########
### Set the working directory to the path where you downloaded the "Example_Virtual_Species" folder


setwd("./Example_Virtual_Species/Presences_PA")

presences = read.csv("Rounded_Presences_VS.csv")
pseudo_absences = list.files(pattern = "PA")

setwd('../Models')

########## 2) Modeling using BIOMOD2 (Thuiller et al. https://biomodhub.github.io/biomod2/ ) ##########

for (i in 1:length(pseudo_absences)){

  ## Creation of the modeling matrix

  setwd('..')
  setwd("./Presences_PA")

  pseudo_absences_file = read.csv(pseudo_absences[i])
  mat_mod = rbind(presences, pseudo_absences_file[,1:5])

  ## Number of presences (10,000) and pseudo-absences
  pres = 10000
  pa = nrow(pseudo_absences_file)/10  ### There is 10 set of PA to test the influence in modeling outputs

  ## We tell to biomod that the first set of PA is from row 10,001 to row 10,001 + pa (PA1),
                            ## second set of PA is from row 10,001 + pa to 10,001 + 2*pa (PA2) etc...
  pa.tab = data.frame(PA1 = c(rep(T, pres), rep(T, pa), rep(F, 9*pa)),
                      PA2 = c(rep(T,pres), rep(F, pa), rep(T, pa), rep(F, 8*pa)),
                      PA3 = c(rep(T, pres), rep(F, 2*pa), rep(T, pa), rep(F, 7*pa)),
                      PA4 = c(rep(T, pres), rep(F, 3*pa), rep(T, pa), rep(F, 6*pa)),
                      PA5 = c(rep(T, pres), rep(F, 4*pa), rep(T, pa), rep(F, 5*pa)),
                      PA6 = c(rep(T, pres), rep(F, 5*pa), rep(T, pa), rep(F, 4*pa)),
                      PA7 = c(rep(T, pres), rep(F, 6*pa), rep(T, pa), rep(F, 3*pa)),
                      PA8 = c(rep(T, pres), rep(F, 7*pa), rep(T, pa), rep(F, 2*pa)),
                      PA9 = c(rep(T, pres), rep(F, 8*pa), rep(T, pa), rep(F, pa)),
                      PA10 = c(rep(T, pres), rep(F, 9*pa), rep(T, pa)))

  ## We tell to biomod that the 10,000 rows are presences (1), the rest is pseudo-absences (NA)
  resp.data = c(rep(1, pres), rep(NA, pa*10))

  ## No influence of coordinates
  resp.xy = data.frame(X=as.numeric(rep(1, nrow(mat_mod))),
                        Y=as.numeric(rep(1, nrow(mat_mod))))

  resp_name = "test_species"

  bmData = BIOMOD_FormatingData(resp.var = resp.data,
                                 resp.xy = resp.xy,
                                 expl.var = mat_mod[c("SBT", 'SST', "Salinity", "Wind", "Ice")],
                                 resp.name = resp_name[i],
                                 PA.strategy = "user.defined",
                                 PA.user.table = pa.tab)

  myBiomodOption = bm_ModelingOptions(data.type = "binary",
                                      models = "RF",
                                      strategy = "bigboss",
                                      bm.format = bmData) ## "default parameter values are updated with values predefined by biomod2 team"
                                                          ## If you want to tune your model see bm_Tuning for more info

  setwd('../Models')
  dir.create(paste0("./", pseudo-absences[i]))

  mySDMModel = BIOMOD_Modeling(bm.format = bmData,
                                models = c('RF'),
                                bm.options = myBiomodOption,
                                CV.nb.rep = 5, # Number of Evaluation run for one PA set to see influence of algorithm on modeling outputs (*10 PA set = 50 runs)
                                CV.do.full.models = FALSE, # if true, models calibrated and evaluated with the whole dataset
                                CV.perc = 0.8, # % of data used to calibrate the models, the remaining part will be used for testing
                                var.import = 3,# Number of permutation to estimate variable importance
                                metric.eval = c('KAPPA','TSS','ROC'), # names of evaluation metrics
                                modeling.id = resp_name[i])

  setwd('../Models')

}
