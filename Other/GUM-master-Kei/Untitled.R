# source('RunProjections.R')
# source('SnowProjections.R')

# devtools::install_github('DanOvando/GUM', build_vignettes = T)
library(GUM)
library(tidyverse)

# devtools::load_all('../GUM')

head(sample_data)

vignette('GUM')

stocks = unique(sample_data$IdOrig)

sub = sample(stocks, 10, replace = F)

small_dat = filter(sample_data, IdOrig %in% sub)

less_dat = small_dat %>% dplyr::select(IdOrig,SciName,SpeciesCat,CommName,Year,Catch,BvBmsy) %>%
  mutate(IdOrig = as.character(IdOrig))

no_dat <- less_dat %>%
  mutate(SpeciesCat = 36, SciName = 'Blah')

results = run_gum_assessment(dat = less_dat)

head(results)

StatusQuoPolicy<-'StatusQuoA' # 'StatusQuoA'

results$CatchShare <- FALSE

results$Price <- 1000

results$BvBmsyOpenAccess <- 0.5

test_projection <- as.data.frame(results) %>%
  # filter(is.na(FvFmsy) == F) %>%
  RunProjection(
              BaselineYear = 2012,
              NumCPUs = 1)

a = ggKobe(test_projection$DataPlus %>% filter(Year == 2012, Policy == 'Historic'), plot_density = T)

ggplot(test_projection$DataPlus, aes(Year, BvBmsy, fill = Policy)) +
  geom_point(shape = 21) +
  facet_grid(IdOrig ~.)
