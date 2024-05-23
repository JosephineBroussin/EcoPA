#' PAGeneration
#'
#' @param data list of length 2 created by SpeciesNiche (1 = an n-dimensions array, 2 = name of each dimensions (environmental variables))
#' @param nb_pa a vector of number of pseudo-absences you want to generate
#' @param ratio_pa_InOut a vector of ratio of pseudo-absences in and out the species niche (for details and advice see help file at : )
#'
#' @return a list of data-frame of length nb_pa*ratio_pa_InOut
#' @export
#'
#' @importFrom dplyr mutate_if
#' @importFrom dplyr sample_n
#' @importFrom dplyr filter
#' @import magrittr
#'
#' @examples
#' set.seed(10)
#' data_species = data.frame(Env_Var1= rnorm(100),
#'                               Env_Var2 = rnorm(100,3,1),
#'                               Env_Var3 = rnorm(100,5,2))
#'
#' species_niche = SpeciesNiche(data = data_species, hist_type = c("FD", "FD", "FD"), niche_border = c(-3,3, 0,6, 0,10))
#'
#' PAGeneration(data = species_niche, nb_pa = c(1000, 3000, 5000), ratio_pa_InOut = c(1, 2/3))
#'
#'
PAGeneration = function(data, nb_pa, ratio_pa_InOut){

  ##### Verifications #####

  ## Verify that data is a list with [[1]] array and [[2]] colnames

  #if ( ! is.data.frame(data))
    #stop("data must be a dataframe")


  env_var_names = data[[2]]

  data = data[[1]]

  ##### Create the PA niche (inverse of the species niche) ####

  data[is.na(data)] = 0

  data_t = max(data, na.rm=T) - data

  data_t = as.data.frame.table(data_t)
  colnames(data_t) = c(env_var_names, "Freq")
  data_t = dplyr::mutate_if(data_t, is.factor, ~ as.numeric(as.character(.x)))

  ## Split the niche in 2 : (1) PA outside of the species niche ; (2) PA inside the species niche

  data_t_out = data_t %>% dplyr::filter(Freq == max(Freq))
  data_t_in = data_t %>% dplyr::filter(Freq < max(Freq))


  ##### Generate the PA from the PA niche #####

  list_pa_sampled = list()

  for (i in 1:length(nb_pa)){

    for (j in 1:length(ratio_pa_InOut)){


      nb_pa_in = ceiling(round(nb_pa[i] * ratio_pa_InOut[j]))
      nb_pa_out = ceiling(round(nb_pa[i] * (1 - ratio_pa_InOut[j])))


      pa_in_sampled = sample_n(data_t_in, nb_pa_in, replace = TRUE, weight = Freq)

      if (nrow(pa_in_sampled) > 0){

        pa_in_sampled$resp.var = "in"

      }


      pa_out_sampled = sample_n(data_t_out, nb_pa_out, replace = TRUE)

      if (nrow(pa_out_sampled) > 0) {

        pa_out_sampled$resp.var = "out"

      }


      pa_sampled = rbind(pa_in_sampled, pa_out_sampled)
      pa_sampled = pa_sampled[sample(1:nrow(pa_sampled)),]

      list_pa_sampled[[j + (length(ratio_pa_InOut) * (i - 1))]] = pa_sampled

      names(list_pa_sampled)[[j + (length(ratio_pa_InOut) * (i - 1))]] = paste0(nb_pa[i],"PA_", nb_pa_in, "In_", nb_pa_out, "Out")

    }
  }

  list_pa_sampled <<- list_pa_sampled

}


