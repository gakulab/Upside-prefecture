library(tidyverse)

#ディレクトリの指定
current_path <- getwd()
setwd("/Volumes/GoogleDrive/Shared drives/gakuLab_Research_Upside/Upside-prefecture/Other/GUM-master-Kei/data")

#データの読み込み
RAM_JP <- read.csv("BvBmsy_RAM_JP_&_Landing_Kei.csv",stringsAsFactors = F) #RAMのデータから日本の資源におけるBvBmsyを抽出し、水揚げデータと合併
lifehistory <- read.csv("FishLifeHistory_JP.csv",stringsAsFactors = F) #RAMデータに含まれる魚種を中心とした、各魚種の生物情報データフレーム
lifehistory <- lifehistory %>% select(-c(IdOrig,SpeciesCatName)) %>% left_join(isscaap_codes,by=c("SpeciesCat"))

#上記の２つのデータを合併
RAM_JP_2 <- RAM_JP %>%
  select(-SpeciesCatName) %>%
  left_join(lifehistory,by=c("SciName","SpeciesCat"))


#より効率的な処理 (mapやlapply)を用いるためにデータフレームのRAM_JPをリストへ変形する
#魚種毎にfor loopを回したくないので、applyを用いる

#魚種を抜き出す
data_fish_list <- unique(RAM_JP$CommStockName)

#データフレームを分割する関数
split_data <- function(dat){

  result <- list() #空のリストの作成
  #name <- as.character(unique(dat$CommStockName))
  result[["Year"]] <- dat$Year
  result[["BvBmsy"]] <- dat$BvBmsy #リストの第2成分の1にBvBmsyを格納
  result[["Landing_t"]] <- dat$Landing_t #リストの第2成分の2にLanding_tを格納
  result[["SpeciesCatName"]] <- dat$SpeciesCatName #種のカテゴリー名を付与
  #assign(name,result)

  #生物情報の付与
  result[["MaxLength"]] <- dat$MaxLength
  result[["AgeMat"]] <- dat$AgeMat
  result[["VonBertK"]] <- dat$VonBertK
  result[["Temp"]] <- dat$Temp

  return(result)
}

#上記の関数へ入れるデータを魚種別にするようにした関数
split_data_apply <- function(i,data,data_fish_list){
  out <- split_data(dat = filter(data, CommStockName == data_fish_list[i]))
}

#lapplyで魚種のリストデータにsplit_data(split_data_apply)を使用
results <- lapply(1:length(data_fish_list),split_data_apply,data=RAM_JP_2,data_fish_list = data_fish_list)
names(results) <- as.character(data_fish_list) #リストに名前を付与


#モデルのためのデータ設計を行う関数を作成(GUMのFormatForRegression.Rの様なもの)
FormatForRegreesion_JP <- function(data,InitialScale=1:6){
  data1 <- bind_rows(data)
  years <- data1$Year
  catches <- data1$Landing_t
  max_catch <- as.numeric(data1[data1$Landing_t==max(data1$Landing_t),]$Year)
  data2 <- data1 %>%
    mutate(
      YearsBack = rev(1:length(catches)),
      LogBvBmsy = log(BvBmsy), #対数をとったBvBmsy
      ScaledCatch = Landing_t/max(Landing_t), #各時点の漁獲量と最大漁獲量の比率
      ScaledCatch1Back = lag(Landing_t,n=1), #1時点遅れている漁獲量
      ScaledCatch2Back = lag(Landing_t,n=2), #2時点遅れている漁獲量
      ScaledCatch3Back = lag(Landing_t,n=3), #3時点遅れている漁獲量
      ScaledCatch4Back = lag(Landing_t,n=4), #4時点遅れている漁獲量
      MaxCatch = max(Landing_t), #最大漁獲量
      TimeToMaxCatch = which.max(Landing_t), #初年度から何年で最大漁獲量になるのか
      InitialScaledCatchSlope = lm(ScaledCatch[1:6] ~ InitialScale,na.action='na.omit')$coefficients[2], #始め6年間で回帰して、傾きを求める
      MeanScaledCatch = mean(ScaledCatch), #各時点の漁獲量と最大漁獲量の比率の平均値
      CatchToRollingMax = Landing_t
    )

  for (i in 1:length(catches)) {
    data2$CatchToRollingMax[i] <- catches[i]/max(catches[1:i]) #各時点において、過去からその時点までの間で最大の漁獲量とその時点の漁獲量の割合
  }
  data3 <- select(data2,c(LogBvBmsy,YearsBack,ScaledCatch,ScaledCatch1Back,ScaledCatch2Back,ScaledCatch3Back,ScaledCatch4Back,MaxCatch,TimeToMaxCatch,InitialScaledCatchSlope,MeanScaledCatch,CatchToRollingMax,MaxLength,AgeMat,VonBertK,Temp,SpeciesCatName))
  data3 <- as.list(data3)
  return(data3)
}

#モデルのデータ設計
Model_data <- map(results,FormatForRegreesion_JP) %>%
  bind_rows()

#各回帰モデルを回す
#モデルの説明変数(生物情報パート)は、所有している種数が少ない説明変数を順番に抜いていく。全情報のモデルから無情報のモデルへの遷移を作りたいのである。
#日本の魚種の場合、MaxLength,
M1 <-lm(LogBvBmsy ~ YearsBack + ScaledCatch + ScaledCatch1Back + ScaledCatch2Back +
           ScaledCatch3Back + ScaledCatch4Back + MaxCatch + TimeToMaxCatch +
           InitialScaledCatchSlope + MeanScaledCatch + CatchToRollingMax +
           MaxLength + AgeMat + VonBertK + Temp + SpeciesCatName,data=Model_data)

M2 <-lm(LogBvBmsy ~ YearsBack + ScaledCatch + ScaledCatch1Back + ScaledCatch2Back +
          ScaledCatch3Back + ScaledCatch4Back + MaxCatch + TimeToMaxCatch +
          InitialScaledCatchSlope + MeanScaledCatch + CatchToRollingMax +
          MaxLength + Temp + VonBertK + SpeciesCatName,data=Model_data) #AgeMatが抜ける

M3 <-lm(LogBvBmsy ~ YearsBack + ScaledCatch + ScaledCatch1Back + ScaledCatch2Back +
          ScaledCatch3Back + ScaledCatch4Back + MaxCatch + TimeToMaxCatch +
          InitialScaledCatchSlope + MeanScaledCatch + CatchToRollingMax +
          Temp + MaxLength + SpeciesCatName,data=Model_data) #VonBertKが抜ける

M4 <-lm(LogBvBmsy ~ YearsBack + ScaledCatch + ScaledCatch1Back + ScaledCatch2Back +
          ScaledCatch3Back + ScaledCatch4Back + MaxCatch + TimeToMaxCatch +
          InitialScaledCatchSlope + MeanScaledCatch + CatchToRollingMax +
          MaxLength + SpeciesCatName,data=Model_data) #Tempが抜ける

M6 <-lm(LogBvBmsy ~ YearsBack + ScaledCatch + ScaledCatch1Back + ScaledCatch2Back +
          ScaledCatch3Back + ScaledCatch4Back + MaxCatch + TimeToMaxCatch +
          InitialScaledCatchSlope + MeanScaledCatch + CatchToRollingMax + SpeciesCatName,data=Model_data)

#モデルのリストを作成
regs_JP <- list("M1"=M1,"M2"=M2,"M3"=M3,"M4"=M4,"M6"=M6)
rm(M1,M2,M3,M4,M6)

#ディレクトリを戻す
setwd(current_path)
