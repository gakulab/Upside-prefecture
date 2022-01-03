#' Find Resilience
#'
#' Finds Fishbase resilience
#' @param Data the data
#'
#' @return Fishbase resilience
#' @export
FindResilience<-function(Data)
{
  ResData<-unique(Data[c("IdOrig","SciName", "VonBertK","AgeMat")])
  #ResNames<-validate_names(ResData$SciName)
  
  RESout<-stocks(ResData$SciName, fields="Resilience")
  RESout<-RESout%>%
    drop_na(Resilience)%>% 
    rename(SciName = sciname, Res = Resilience)
    
  ResData<-merge(ResData, RESout, all.x=T, all.y=F)
    

  # ResData$k<-NA
  # ResData$tm<-NA
  # ResData$Res<-NA
  # 
  Data$Res = NA
  # 
  Data$Value = NA
  # 
  # ResData<-ResData[is.na(ResData$VonBertK)==F | is.na(ResData$AgeMat)==F,]
  # 
  # # Assign resilience based on life history values
  # ResData$k[ResData$VonBertK>0.3]<-"High"
  # ResData$k[ResData$VonBertK>=0.16 & ResData$VonBertK<=0.3]<-"Medium"
  # ResData$k[ResData$VonBertK>=0.05 & ResData$VonBertK<0.16]<-"Low"
  # ResData$k[ResData$VonBertK<0.05]<-"Very Low"
  # 
  # ResData$tm[ResData$AgeMat<1]<-"High"
  # ResData$tm[ResData$AgeMat>=1 & ResData$AgeMat<=4]<-"Medium"
  # ResData$tm[ResData$AgeMat>4 & ResData$AgeMat<=10]<-"Low"
  # ResData$tm[ResData$AgeMat>10]<-"Very Low"
  # 
  # ResData$Res[grepl("Very Low", ResData$k) | grepl("Very Low", ResData$tm)]<-"Very low"
  # ResData$Res[(grepl("Low", ResData$k) | grepl("Low", ResData$tm)) & is.na(ResData$Res)==T]<-"Low"
  # ResData$Res[(grepl("Medium", ResData$k) | grepl("Medium", ResData$tm)) & is.na(ResData$Res)==T]<-"Medium"
  # ResData$Res[(grepl("High", ResData$k) | grepl("High", ResData$tm)) & is.na(ResData$Res)==T]<-"High"

  # Fill in resilience for stocks with data
  for(a in 1:nrow(ResData))
  {
    WhereRes<-Data$IdOrig==ResData$IdOrig[a]

    Data$Res[WhereRes]<-ResData$Res[a]

    show(paste((a/nrow(ResData)*100),"% Done with Resilience",sep=""))
  }

  Data$Res[is.na(Data$Res)]<-'Medium'

  # Calculate frequency of resilience categories for each ISSCAAP group
  Data$Value[is.na(Data$Res)==F]<-1

  ResCount<- Data %>%
    group_by(SpeciesCatName,Res) %>%
    summarize(Count=sum(Value,na.rm=T))

  # ResCount<-ddply(Data,c('SpeciesCatName','Res'),summarize,Count=sum(Value,na.rm=T))

  cats<-unique(ResCount$SpeciesCatName)

  DefaultRes<-data.frame(matrix(NA,nrow=length(cats),ncol=2))

  colnames(DefaultRes)<-c('SpeciesCatName','Res')

  # Determine default resilience category for each ISSCAAP group
  for(a in 1:length(cats))
  {
    DefaultRes$SpeciesCatName[a]<-cats[a]

    temp<-ResCount[ResCount$SpeciesCatName==cats[a],]

    DefaultRes$Res[a]<-temp$Res[temp$Count==max(temp$Count,na.rm=T)]
  }

  # If no default is calculated, assign "Medium" resilience
  DefaultRes$Res[is.na(DefaultRes$Res)==T]<-'Medium'

#   for(b in 1:nrow(Data))
#   {
#     if(is.na(Data$Res[b]))
#     {
#       Data$Res[b]<-DefaultRes$Res[DefaultRes$SpeciesCatName==Data$SpeciesCatName[b]]
#     }
#   }

  # write.csv(file=paste(ResultFolder,'ISSCAAP Default Resiliency.csv',sep=''),DefaultRes)
  #
  # pdf(file=paste(FigureFolder,'Resilience Histograms by ISSCAAP.pdf',sep=''),width=12,height=10)
  # print(ggplot(Data,aes(x=factor(Res))) +
  #   geom_bar() +
  #   facet_wrap(~SpeciesCatName,scales='free'))
  # dev.off()

  return(FullData=Data)

}
