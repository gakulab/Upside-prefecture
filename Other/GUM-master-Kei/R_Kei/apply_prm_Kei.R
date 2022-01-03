#' apply_prm
#'
#' \code{apply_prm} applied panel regression model to new data
#' @param dat data frame データフレーム
#' @param reg the regression model being used 回帰モデル
#' @param CatchLags number of years of catch lags #ScaledCatchの年数を決める。何年ずれにするか
#' @param LifeHistoryVars life history variables needed 必要とされる生活史情報
#' @param IdVar the variable marking id idを作る変数
#' @param CatchVariables catch related variables 
#' @param min_catch_years minimum number of catch years 最底必要漁獲データ年数
#'
#' @return data with PRM predicted log BvBmsy データに基づくパネルリグレッション
#' @export
apply_prm_Kei <- function(dat,reg,CatchLags = 4, LifeHistoryVars = c('MaxLength','AgeMat','VonBertK','Temp','SpeciesCat','SpeciesCatName','b_to_k_ratio'),
                      IdVar = 'IdOrig', CatchVariables =  c('YearsBack','ScaledCatch',paste('ScaledCatch',1:CatchLags,'Back',sep=''),'MaxCatch','TimeToMaxCatch','InitialScaledCatchSlope'
                                                              ,'MeanScaledCatch','CatchToRollingMax'),
                      min_catch_years = 10){

  dat$BvBmsy <- NA #housekeeping #BvBmsyをNAに統一する必要あるのか？？->回帰分析でBvBmsyの推定値を出すので既に与えられているBvBmsyは不要にある。
  if(is.numeric(dat$Year) == F){dat$Year == as.numeric(dat$Year)} #3/27kei:year->Yearに変更。入力データはYearなのでyearではなくYearにすべき

  # Filter out things that don't have enough catch years　#データ数が不足しているIdOrigを抽出
  not_enough_catch  <- dat %>%
    group_by(IdOrig) %>%
    summarize(catch_years = sum(is.na(Catch) == F & Catch > 0)) %>% #研究に使える年の集計(NAが無く、Catchが0以上)
    subset(catch_years < min_catch_years) #データ数の下限値を設定(例：10年以上)

  dat <- dat %>%
    filter(!IdOrig %in% not_enough_catch$IdOrig) #not_enough_catchにデータ数が不足している魚種をリストアップしたが、ここではデータ数が不足している魚種を弾いている。

  Fisheries <- unique(dat$IdOrig) #魚種リストの作成

  # Format data for regression #
  formatted <- lapply(1:length(Fisheries),FormatForRegression_Kei, Data = dat, Fisheries = Fisheries, DependentVariable = 'BvBmsy',
                      CatchVariables = CatchVariables, CatchLags = 4, LifeHistoryVars = LifeHistoryVars,
                      IsLog = T, IdVar = 'IdOrig') %>%
    bind_rows()

  #3/31kei:SciName列がfactorだと以下のassign_life_history()でcharacterへの強制変換メッセージが返ってくるので予め変換しておく
  formatted$SciName <- as.character(formatted$SciName)
  # Add in life history
  formatted <- assign_life_history_Kei(dat = formatted,LifeHistoryVars = LifeHistoryVars)

  # Change species category factors to match model #regでのカテゴリーを抽出
  reg_factors <- reg$xlevels$SpeciesCatName

  # isscaapでの全カテゴリー
  AllPossible <- isscaap_codes

  # AllPossible = formatted %>%
  #   select(SpeciesCatName, SpeciesCat) %>%
  #   unique()

  #Dataの魚種のカテゴリー名をisscaapの魚種のカテゴリー名に変換する
  adjusted_data = AssignNearestSpeciesCategory_Kei(Data = formatted, AvailableCategories = reg_factors, AllCategories = AllPossible)

  # Predict log B/Bmsy(BvBmsy)
  predicted = predict.lm(reg,adjusted_data$Data, se.fit = T)

  formatted$LogBvBmsy = predicted$fit

  formatted$regression_LogBvBmsy = predicted$fit

  formatted$BvBmsySD = predicted$se.fit

  return(formatted)
}
