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
