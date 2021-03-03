#' assign_life_history
#'
#' \code{assign_life_history} adds in and fills in missing life history information for PRM
#'
#' @param dat the data to be used in the regression etc
#' @param FishBase potential fishbase data
#' @param LifeHistoryVars
#'
#' @return data frame with life history
#' @export
assign_life_history <-
  function(dat,
           FishBase = NA,
           LifeHistoryVars = c(
             'MaxLength',
             'AgeMat',
             'VonBertK',
             'Temp',
             'SpeciesCat',
             'SpeciesCatName',
             'b_to_k_ratio'
           )) {
    # Add in life history variables if missing --------------------------------
    missing <-
      LifeHistoryVars[!LifeHistoryVars %in% colnames(dat)] # find life history data needed but not present

    add_in <-
      as.data.frame(matrix(
        as.numeric(NA),
        nrow = dim(dat)[1],
        ncol = length(missing)
      ))

    colnames(add_in) <- missing

    out_dat <-
      cbind(dat, add_in) #tack on empty matrices for missing life history data

    # Fill in present but missing variables --------------------------------

    # load('Data/fishbase_data.Rdata')

    # isscaap <- read.csv('Data/ISSCAAP Codes.csv', stringsAsFactors = F)

    fishbase_vars <-
      colnames(fishbase_lifehistory)[colnames(fishbase_lifehistory) %in% LifeHistoryVars[LifeHistoryVars != 'SpeciesCat']]

    fishbase_lifehistory$has_none = apply(is.na(fishbase_lifehistory[, fishbase_vars]), 1, all)

    filtered_fblh = fishbase_lifehistory %>%
      filter(has_none == F)

    life_bank <- out_dat %>%
      select(IdOrig, SciName, SpeciesCat) %>%
      left_join(select(filtered_fblh,-SpeciesCat), by = 'SciName') %>%
      left_join(isscaap, by = 'SpeciesCat') %>%
      mutate(b_to_k_ratio = 0.4)

    # lifenames <- c('MaxLength','AgeMat','VonBertK','Temp', 'SpeciesCat','SpeciesCatName')

    for (i in 1:length(LifeHistoryVars)) {
      missing <- is.na(out_dat[, LifeHistoryVars[i]])

      out_dat[missing, LifeHistoryVars[i]] <-
        life_bank[missing, LifeHistoryVars[i]]
    }

    out_dat <- filter(out_dat, is.na(SpeciesCatName) == F)
    return(out_dat)
  }
