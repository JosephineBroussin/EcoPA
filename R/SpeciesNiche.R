#' SpeciesNiche
#' This function construct an ecological space, each of its dimensions being one of the environmental predictors composing "data".
#' This ecological space is an array. An array need two informations to be constructed (1) the minimum and maximum values of each dimensions in "niche_border" and (2) each dimensions being sliced into bins regularly space, the size of those bins in "bins_sizes".
#' Once the array is constructed it is filled with the number of presences observed for each associations of environmental values.
#'
#'
#' @param data a dataframe with m rows = presences of the species and n columns = environmental variables
#' @param niche_border by default (if not specified) the minimum and maximum of each environmental variables in the data; if specified must be : min of env var 1, max of env var 1, min of env var 2, max of env var 2, ..., min of env var n, max of env var n
#' @param bins_sizes a vector of length number of predictors (column of data) specifying the size of the bins constructing each dimension of the array. Can be numeric values decided by the user or algorithms calculating the optimal size of the bin ("Sturges", "Freedman-Diaconis", "Scott", see details in hist base r function). Numeric values and algorithm can not be mixed.
#'
#' @return a list of length 3 with (1) an n-dimensions array, each dimensions being an environmental variables, filled with the number of presences for each associations of those variables, (2) the names of each dimensions (environmental variables) and (3) the input presences of "data" round to the size of the bins. We strongly recommend to use the round presences (+ pseudo-absences generated in PAGeneration) in your Species Distribution Modeling procedure.
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
#' data_species = data.frame(Pred_1= rnorm(100),
#'                           Pred_2 = rnorm(100,3,1),
#'                           Pred_3 = rnorm(100,5,2))
#'
#' SpeciesNiche(data = data_species, niche_border = c(-3,3, 0,6, 0,10), bins_sizes = c("FD", "FD", "FD"))
#'
SpeciesNiche = function(data, niche_border = NULL, bins_sizes){

  ##### Verifications #####

  ## Verify that data is a dataframe

  if ( ! is.data.frame(data))
    stop("data must be a dataframe")

  ## Verify that length of breaks_sizes vector = number of col of data

  if ( ! length(bins_sizes) == ncol(data))
    stop("Length of bins_sizes must be equal to the number of environmental variables (column of data)")


  ## Verify that number of couple of niche border = number of col of data

  if ( ! length(niche_border) == 0 & ! length(niche_border) == 2*ncol(data))
    stop("If specified, length of niche border must be equal to 2*environmental variables (column of data)")


  ##### Preparation of data #####

  ## Delete rows of data with missing informations

  data = na.omit(data)


  ## Calculate the breaks of each environmental variable (based on the choosen formula to calculate breaks with argument "breaks) if not stipulated

  list_hist = list()
  breaks_size = vector()

  if (class(bins_sizes) == "numeric") {

    breaks_size = bins_sizes

  } else {

    for (i in 1:ncol(data)){

      list_hist[[i]] = hist(data[,i], breaks = bins_sizes[i],
                            plot = TRUE, main = colnames(data)[i], xlab = colnames(data)[i])

      breaks_size[i] = list_hist[[i]]$breaks[2] - list_hist[[i]]$breaks[1]

    }
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


    niche_border_round_min = plyr::round_any(niche_border[i*2-1], breaks_size[i])
    niche_border_round_max = plyr::round_any(niche_border[i*2], breaks_size[i])

    dim_names[[i]] = round(seq(niche_border_round_min, niche_border_round_max, breaks_size[i]), digits = 10)
        # round the sequence because sometimes R changes x.1 to x.10000000000001 (depends which computer I am using,
                                                                                  #do not do it all the time
                                                                                  #do not know why --> this solution for the moment)

    dim_niche[i] = length(dim_names[[i]])
  }



  species_niche = array(dim = dim_niche, dimnames = dim_names)


  ###### Fill the niche #####


  #Count the number of observations for each association of environmental variables

  names_stock = names(data_round)

  data_round_count = data_round %>% dplyr::count(., !!!.)
  data_round_count = round(data_round_count, digits = 10) ## same as for the creation of the niche, sometimes R changes x.1 to x.10000000000001

  #Fill the niche

  for (i in 1:nrow(data_round_count)){

    subset = as.character(data_round_count[i,1:ncol(data_round_count)-1])
    nombre = as.numeric(data_round_count[i,ncol(data_round_count)])

    species_niche[t(subset)] = nombre[1]

  }

  species_niche = list(species_niche, colnames(data), data_round)
  return(species_niche)


}







