#' assign_spcategory
#'
#' \code{assign_spcategory} adds in and fills in missing life history information for PRM
#'
#' @param dat the data to assign species categories to
#' @param asfismatch ASFIS data to search for matches in
#'
#' @return data frame with assigned species categories
#' @export

assign_spcategory<-function(dat, afsismatch = afsis)
{
  browser()
  # rename columns to match input data
  colnames(afsismatch)<-c('SciName', 'SpeciesCat')

  # pull out data with SpeciesCat
  hasdata<- dat %>%
    filter(is.na(SpeciesCat)==F)

  # subset dat to include data missing SpeciesCat
  temp<- dat %>%
    filter(is.na(SpeciesCat)) %>%
    dplyr::select(-SpeciesCat) %>% # drop SpeciesCat variable so as not to duplicate it with join
    left_join(afsismatch, by = c('SciName'))

  temp<-data.frame(rbind(hasdata,temp))
  browser()
  return(temp)
}
