mtcars <- as_tibble(mtcars) # for nicer printing

mtcars %>% rename_all(toupper)

c('ichigo', 'aoi', 'ran') %>% purrr::map(function(.){paste(., 'chan')})
c('ichigo', 'aoi', 'ran') %>% purrr::map(~paste(., 'chan'))

filterd_lm <- function(dat, .tag, 
                       .key = "Species", .x = "Sepal.Width", .y = "Petal.Length"){
  dat %>% 
    select(.key, .x, .y) %>%              # selectは文字列を引数に取れる。
    #rename_all(~c("key", "x", "y")) %>%   # NSE問題を回避するために関数の中でカラム名を指定する。
    rename("key"=.key, "x"=.x, "y"=.y) %>% 
    filter(key == .tag) %>%
    lm(y ~ x, data = .)
}

model_lm_setosa <- filterd_lm(dat, "setosa")

a=iris %>%
  group_by(Species) %>%
  nest()

N=10
for(i in 1:9){data[i] <- dbinom(i,10,i/10)}
data <- lapply(1:10,dbinom,size=10,prob=.1)%>%
  bind_rows()

newdata <- UpsideResult %>%
  select(year,
         prefecture,
         prefecture_alphabet = alphabet,
         management,
         total_pref_catch_ton = total_pref_catch) 

saveRDS(newdata,file="UpsideData7Sep21.rds")

valid <- newdata %>%
  group_by(management,year) %>%
  summarise(total=sum(total_pref_catch_ton))

ggplot(valid, aes(x=year,y=total,col=management))+
  geom_line()
