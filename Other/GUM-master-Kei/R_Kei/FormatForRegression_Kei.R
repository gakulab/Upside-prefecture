#' FormatForRegression
#'
#' \code{FormatForRegression} takes data and prepares it for PRM regression
#' @param f fishery
#' @param Data raw data
#' @param Fisheries the total list of all possible fisheries
#' @param DependentVariable the left hand variable
#' @param CatchVariables vector of catch related variables
#' @param CatchLags number of years of catch lags to create
#' @param LifeHistoryVars life history variables to include
#' @param IsLog is the dependent variable in log space
#' @param IdVar the variable marking the id
#'
#' @return formatted data frame
#' @export
FormatForRegression_Kei<- function(f, Data,Fisheries,DependentVariable,CatchVariables,CatchLags,LifeHistoryVars,IsLog,IdVar)

{

  # Format Data For Regression  --------
  #This code reformats data frames into
  # the structure for a panel regression #このコードはパネル回帰分析用にデータフレーム構造を再構築する。

  # Create Regression Data Frame --------------------------------------------
  Where <- Data[,IdVar]==Fisheries[f] #Fisheriesはrun_gum_assessment()で入れたデータのIdOrigをunique()したもので魚種リストである。全体データから魚種ごとにデータを抜き出している

  TempFrame<- Data[Where,] #フィルターのようなこと
  
  #TempFrame <- Data[Data$IdOrig==Fisheries[f],]

  Where<- TempFrame$IdOrig==Fisheries[f] #再確認？

  LifeHistoryVars<- sort(LifeHistoryVars) #LifeHistoryVarsはapply_prm()のオプションで定められている。

  DependentName<- DependentVariable #従属変数(目的変数)。GLMではBvBmsyとしている

  DependentName<- if (IsLog==T){paste('Log',DependentVariable,sep='')} #IsLog==TRUEの時は"Log"を従属変数名に付与する

  RegNames<- c(DependentName,CatchVariables) #推測だが、CatchVariablesは説明変数。従って、回帰モデルに用いる変数名の一覧を作成している。

  RegFrame<- as.data.frame(matrix(NA,nrow=dim(TempFrame)[1],ncol=length(RegNames))) #結果を入れる枠組みを作成

  colnames(RegFrame)<- RegNames #枠組みに先ほど格納した回帰モデルに用いる変数名一覧を挿入

  DependentTemp<-  TempFrame[,DependentVariable] #

  if (IsLog==T){DependentTemp<- log(DependentTemp)} #IsLogという対数化オプションがTRUEの場合は対数をとる

  DependentTemp[is.infinite(DependentTemp)]<- NA #無限を取り除く

  RegFrame[,DependentName]<-DependentTemp #

  # Loop Over Fisheries -----------------------------------------------------


  SlopeWindow<- 1:6

  #   for (f in 1:length(Fisheries))
  #   {

  #     if (is.integer(f/50)){   }

  # write.table(paste(round(100*(f/length(Fisheries))),"% Done with Regression Formating",sep=''), file = 'Regression Formatting Progress.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)

  MaxCatch<- max(TempFrame$Catch,na.rm=T) #TempFrameのCatchが最大のものをMaxCatchに格納

  TempCatch<- TempFrame$Catch #TempFrameのCatchをそのまま格納

  ScaledCatch <- TempCatch/MaxCatch #比率の作成

  RegFrame[Where,'ScaledCatch']<- ScaledCatch #Create scaled catch #最大漁獲量に対する各漁獲量の比率の列の追加

  RegFrame[Where,'MaxCatch']<- MaxCatch #Maximum recorded catch

  RegFrame[Where,'MeanScaledCatch']<- mean(ScaledCatch ,na.rm=T)#Create scaled catch #最大漁獲量と比較した漁獲量の割合の平均

  #1魚種毎に行うため、データフレームの列数がその魚種が存在する期間を表す。
  RegFrame[Where,'TimeToMaxCatch']<- which(TempCatch==MaxCatch)[1] #Create time till max catch 漁獲量が最大となるまでの年数。(具体的な年数(ex:1960)を求めないのは各魚種データがどの期間において存在するのか不明なため)

  RegFrame[Where,'YearsBack']<-rev(1:length(TempCatch)) #Create time till max catch #rev()はベクトル内の逆順を示す。YearsBackは最新のデータから漁業を開始した年が何年前なのかを表す。


  InitialSlope<- NA 

  FirstCatch<- which(is.na(ScaledCatch)==F)[1] #先述したScaledCatchのNAでないものの上から１個目をFirstCatchに格納

  #FirstCatchがNAではないとき(1個以上正常な値がある時)、目的変数(ScaledCatchの上から6個のデータ)、説明変数(1:6)で回帰する。恐らくデータは時系列のためデータが存在する初期の6年間での傾向(傾き)を表している
  if ((is.na(FirstCatch)==F)) 
  {
    InitialSlope<- lm(formula=ScaledCatch[FirstCatch:(FirstCatch+5)] ~  SlopeWindow,na.action='na.omit')$coefficients[2] 
  }
  RegFrame[Where,'InitialScaledCatchSlope']<- InitialSlope #Create initial slope of scaled catch

  BlankCatch<- matrix(NA,nrow=length(ScaledCatch),ncol=1) #ScaledCatch(最大漁獲量に対する各年の漁獲量の割合)の列数の枠組みを作成

  MaxFrame<- BlankCatch #複製
  
　#BlankCatch(ScaledCatch:最大漁獲量に対する各年の漁獲量の割合)の長さのfor loop、内容は1年目から1年単位で範囲を拡げ、最大の漁獲量を算出している。
  for (c in FirstCatch:length(BlankCatch))
  {
    MaxFrame[c]<- max(TempCatch[FirstCatch:c],na.rm=T)
  }

  MaxFrame[is.infinite(MaxFrame)]<- NA #無限大はNAにする 

  RegFrame[Where,'CatchToRollingMax']<- TempCatch/MaxFrame #Create rolling scaled catch #時系列だからこそ意味のある指標。過去の漁獲量の年に比べ当年の漁獲量がどの割合なのか算出。当年が過去最大の場合1となる。

  ## Populate lagged catches ##

  for (l in 1:CatchLags) #CatchLagsの意味が不明
  {

    TempLag<- BlankCatch

    LagIndex<- pmax(0,(1:length(BlankCatch))-l) #pmax()は並列最大値であり、各ベクトルにおいて要素の順番ごとに最大値を返す。

    TempLag[(1+l):length(BlankCatch)]<- ScaledCatch[LagIndex] #上記のLogIndexは下に1つずれた形になっている。

    WhereCol<- colnames(RegFrame)==paste('ScaledCatch',l,'Back',sep='') #ScaledCatch系は1年ごとにずれた漁獲量を取るだけである。

    RegFrame[Where,WhereCol]<- TempLag # Create lagged scaled catches

  }

  #   }#Close fisheries loop


  RegFrame<- cbind(TempFrame,RegFrame)
  return(RegFrame)

}


