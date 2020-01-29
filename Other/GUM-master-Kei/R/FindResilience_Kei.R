#' Find Resilience
#'
#' Finds Fishbase resilience
#' @param Data the data
#'
#' @return Fishbase resilience
#' @export
FindResilience_Kei<-function(Data)
{
  ResData<-unique(Data[c("IdOrig","SciName", "VonBertK","AgeMat")])
  #ResNames<-validate_names(ResData$SciName)
  
  ## 以下を変更　##
  ResData$SciName <- as.character(ResData$SciName)
  #　初期状態ではstocks()のオプションであるfieldsにResilienceのみが指定されており、
  #魚種と対応していない結果だけをreturnし、それをNAを取り除いて用いてmergeして
  #ResilienceデータをResDataに付与していた。これは危険な手法であるとともに、正常に動いてすらいなかった。
  #unique()も必要である。
  RESout<-stocks(ResData$SciName,fields = c("Species","Resilience")) #fieldsオプションはreturnする列の指定を行う
  RESout <- RESout %>%
    drop_na(Resilience) %>%
    rename(SciName=Species,Res = Resilience) %>%
    unique()
  #stocks()のfieldオプションで選択できる情報一覧
  #[1] "SpecCode"          "Species"           "StockCode"         "SynOC"             "StockDefs"        
  #[6] "StockDefsGeneral"  "Level"             "LocalUnique"       "IUCN_Code"         "IUCN_Assessment"  
  #[11] "IUCN_DateAssessed" "Protected"         "StocksRefNo"       "CITES_Code"        "CITES_Date"       
  #[16] "CITES_Ref"         "CITES_Remarks"     "CMS"               "Northernmost"      "NorthSouthN"      
  #[21] "Southermost"       "NorthSouthS"       "Westernmost"       "WestEastW"         "Easternmost"      
  #[26] "WestEastE"         "BoundingRef"       "BoundingMethod"    "TempMin"           "TempMax"          
  #[31] "TempRef"           "TempPreferred"     "TempPref25"        "TempPref50"        "TempPref75"       
  #[36] "TempPrefRef"       "EnvTemp"           "Resilience"        "ResilienceRemark"  "pHMin"            
  #[41] "pHMax"             "pHRef"             "dHMin"             "dHMax"             "dHRef"            
  #[46] "GenBankID"         "RfeID"             "FIGIS_ID"          "EcotoxID"          "SCRFA_data"       
  #[51] "GMAD_ID"           "SAUP"              "SAUP_ID"           "SAUP_Group"        "AusMuseum"        
  #[56] "FishTrace"         "IUCN_ID"           "IUCN_IDAssess"     "BOLD_ID"           "IGFAName"         
  #[61] "EssayID"           "ICESStockID"       "OsteoBaseID"       "DORIS_ID"          "Aquamaps"         
  #[66] "Morphology"        "Occurrence"        "Strains"           "Ecology"           "Diseases"         
  #[71] "Abnorm"            "Metabolism"        "Predators"         "Spawning"          "Fecundity"        
  #[76] "Speed"             "Diet"              "Eggs"              "EggDevelop"        "Food"             
  #[81] "Larvae"            "LarvDyn"           "LarvSpeed"         "PopDyn"            "LengthWeight"     
  #[86] "Gillarea"          "Maturity"          "MatSizes"          "Processing"        "Reproduction"     
  #[91] "Introductions"     "Abundance"         "Vision"            "Genetics"          "Aquaculture"      
  #[96] "CountryComp"       "Allele"            "GeneticStudies"    "Ration"            "Foods"            
  #[101] "Ecotoxicology"     "Brains"            "Catches"           "FAOAqua"           "LengthRelations"  
  #[106] "LengthFrequency"   "Sounds"            "Broodstock"        "EggNursery"        "FryNursery"       
  #[111] "LarvalNursery"     "Entered"           "DateEntered"       "Modified"          "DateModified"     
  #[116] "Expert"            "DateChecked"       "TS"            
  #resilience情報がある種は情報が増えたが、無い種は無いままである。従って、RESoutよりもResDataの方がデータは大きい
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
  
  # VonBertKは成長率を表す。
  # # Assign resilience based on life history values
  # ResData$k[ResData$VonBertK>0.3]<-"High" 
  # ResData$k[ResData$VonBertK>=0.16 & ResData$VonBertK<=0.3]<-"Medium"
  # ResData$k[ResData$VonBertK>=0.05 & ResData$VonBertK<0.16]<-"Low"
  # ResData$k[ResData$VonBertK<0.05]<-"Very Low"
  # 
  
  # AgeMatは成熟年齢を指す。
  # ResData$tm[ResData$AgeMat<1]<-"High" #1年で成熟するというのはresilienceが高いことを示唆していると思われる
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
    #このコードの目的はresilience情報をDataに付与することである。つまり、既存の(Data内の)Res列をResDataのResに置き換えたいのである。
    #mergeやjoin系で結合することも可能である。しかし、下手に変更せず以下の通りに行うのが安全であろう。
    
    #IdOrigごとに試行していく。まずは、IdOrigの場所をWhereResに格納
    WhereRes<-Data$IdOrig==ResData$IdOrig[a] 
　　
    #DataにResilienceを付与。mergeで一括して操作すれば良いのでは？とこの時点では思う。
    #こちら方mergeよりもが結合過程(進捗状況やerror表示地点)が見られて安心ではあるが、speedはどうなのだろうか？
    Data$Res[WhereRes]<-ResData$Res[a]
   
    #試行の進捗状況を表示
    show(paste((a/nrow(ResData)*100),"% Done with Resilience",sep=""))
  }

  #resilience情報がない場合はMediumにする
  Data$Res[is.na(Data$Res)]<-'Medium'

  # Calculate frequency of resilience categories for each ISSCAAP group 
  Data$Value[is.na(Data$Res)==F]<-1

  ResCount<- Data %>%
    group_by(SpeciesCatName,Res) %>%
    summarize(Count=sum(Value,na.rm=T)) #各カテゴリーの頻度を集計

  # ResCount<-ddply(Data,c('SpeciesCatName','Res'),summarize,Count=sum(Value,na.rm=T))

  cats<-unique(ResCount$SpeciesCatName)

  DefaultRes<-data.frame(matrix(NA,nrow=length(cats),ncol=2))

  colnames(DefaultRes)<-c('SpeciesCatName','Res')

  # Determine default resilience category for each ISSCAAP group 
  for(a in 1:length(cats))
  {
    DefaultRes$SpeciesCatName[a]<-as.character(cats[a]) #元々はas.character()がなかったが、ないとfactorで読み込んでくれないので追加

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
