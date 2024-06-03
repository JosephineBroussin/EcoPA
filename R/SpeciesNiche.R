#' SpeciesNiche
#'
#' @param data a dataframe with m rows = presences of the species and n columns = environmental variables
#' @param hist_type wich formula is going to be used to calculate breaks for each environmental variables, based on the 3 types of the base hist funtion in r : "Sturges"; "Scott"; "FD" (Freedman-Diaconis)
#' @param niche_border by default (if not specified) the minimun and maximum of each environmental variables in the data; if specified must be : min of env var 1, max of env var 1, min of env var 2, max of env var 2, ..., min of env var n, max of env var n
#'
#' @return a list of length 2 with (1) an n-dimensions array, each dimensions being an environmental variables, filled with the number of presences for each associations of those variables and (2) the names of each dimensions (environmental variables)
#' @export
#'
#'
#'
#' @import magrittr
#' @importFrom graphics hist
#' @importFrom stats filter na.omit
#' @importFrom dplyr count_
#' @importFrom plyr round_any
#'
#' @examples
#' set.seed(10)
#' data_species = data.frame(Env_Var1= rnorm(100),
#'                               Env_Var2 = rnorm(100,3,1),
#'                               Env_Var3 = rnorm(100,5,2))
#'
#' SpeciesNiche(data = data_species, hist_type = c("FD", "FD", "FD"), niche_border = c(-3,3, 0,6, 0,10))
#'
SpeciesNiche = function(data, hist_type, niche_border = NULL){

  ##### Verifications #####

  ## Verify that data is a dataframe

  if ( ! is.data.frame(data))
    stop("data must be a dataframe")

  ## Verify that length of histo vector = number of col of data

  if ( ! length(hist_type) == ncol(data))
    stop("Length of hist_type must be equal to the number of environmental variables (column of data)")

  ## Verify that number of couple of niche border = number of col of data

  if ( ! length(niche_border) == 0 & ! length(niche_border) == 2*ncol(data))
    stop("If specified, length of niche border must be equal to 2*environmental variables (column of data)")


  ##### Preparation of data #####

  ## Delete rows of data with missing informations

  data = na.omit(data)


  ## Calculate the breaks of each environmental variable (based on the choosen hist_type in arguments)

  list_hist = list()
  breaks_size = vector()

  for (i in 1:ncol(data)){

    list_hist[[i]] = hist(data[,i], breaks = hist_type[i])

    breaks_size[i] = list_hist[[i]]$breaks[2] - list_hist[[i]]$breaks[1]

  }


  #Round each env. var according to the species_niche array breaks

  data_round = data.frame()

  for (i in 1:ncol(data)){

    data_round_step = plyr::round_any(data[,i], breaks_size[i])

    data_round = rbind(data_round, data_round_step)
  }

  data_round = as.data.frame(t(data_round))


  ##### Create the niche #####

  dim_niche = vector()
  dim_names = list()


  if(length(niche_border) == 0){

    for(i in 1:ncol(data)){

      niche_border[i*2-1] = plyr::round_any(min(data[,i]), breaks_size[i])
      niche_border[i*2] = plyr::round_any(max(data[,i]), breaks_size[i])
    }
  } else {

    niche_border = niche_border
  }


  for (i in 1:ncol(data)){


    dim_niche[i] = ceiling(((niche_border[i*2] - niche_border[i*2-1]) / breaks_size[i]) + 1)


    niche_border_round_min = plyr::round_any(niche_border[i*2-1], breaks_size[i])
    niche_border_round_max = plyr::round_any(niche_border[i*2], breaks_size[i])
    dim_names[[i]] = seq(niche_border_round_min, niche_border_round_max, breaks_size[i])
  }



  species_niche = array( dim = dim_niche, dimnames = dim_names)


  ###### Fill the niche #####


  #Count the number of observations for each association of environmental variables

  names_stock = names(data_round)

  data_round = data_round %>% dplyr::count(., !!!.)

  #Fill the niche

  for (i in 1:nrow(data_round)){

    subset = as.character(data_round[i,1:ncol(data_round)-1])
    nombre = as.numeric(data_round[i,ncol(data_round)])

    species_niche[t(subset)] = nombre[1]

  }

  species_niche = list(species_niche, colnames(data))
  return(species_niche)


}







