#' run_gum_assessment
#'
#' Run assessment part of global upside model (GUM)
#' @param dat data frame with at minimum colnames Year,Catch, SciName
#'
#' @return results a data frame with assessment results
#' @export
#' 
run_gum_assessmentKK <- function(dat){


temp_predicted <- list()

# add in species category info

if (any(c('SpeciesCat', 'SpeciesCatName') %in% colnames(dat)) == F & 'SciName' %in% colnames(dat)) {

  dat <- dat %>%
    left_join(isscaap_links, by = 'SciName')

  warning('No species category number or name provided - looking up from FAO database')
}
else if ((any(c('SpeciesCat', 'SpeciesCatName') %in% colnames(dat)) == F & ('SciName' %in% colnames(dat)) == F)){

  stop(" Can't run model without either species catgory number / name (SpeciesCat/SpeciesCatName) and/or scientific name.
       Provide at least one of these")

}

# Obtain predicted log B/Bmsy from each model


for (i in 1 :length(regs) ){
  temp_predicted[[i]] = apply_prm(dat = dat, reg = regs[[i]]) %>%
    mutate(model = names(regs[i]), model_number = as.numeric(gsub('M','',model)))
}
data <- bind_rows(temp_predicted) %>%
  mutate(model_worked = is.na(LogBvBmsy) == F) %>%
  filter(model_worked == T) %>% #drop models that didn't work
  ungroup() %>%
  group_by(IdOrig) %>%
  filter(model_number == min(model_number)) %>%
  mutate(BvBmsy = exp(LogBvBmsy)) %>%
  rename(year = Year, catch = Catch)#keep the best model that ran for each fishery

data <- FindResilience(data) #%>%
  #rename(res = Res1)

stocks <- unique(data$IdOrig)

# sub <- sample(stocks, 10, replace = F)
#
# data <- filter(data, IdOrig %in% sub)
#
# stocks <- unique(data$IdOrig)
#

if ('IdOrig' %in% colnames(data)){

  data$id = data$IdOrig

}

apply_fun <- function(i,data,stocks){
  out = run_post_prm_pt_cmsy(dat = filter(data, IdOrig == stocks[i]))$CatchMSY
}

results <- lapply(1:length(stocks), apply_fun, data = data, stocks = stocks) %>%
  bind_rows()

return(results)

}
