#' run_gum_assessment
#'
#' Run assessment part of global upside model (GUM)
#' @param dat data frame with at minimum colnames Year,Catch, SciName
#'
#' @return results a data frame with assessment results
#' @export

run_gum_assessment_Kei <- function(dat){
  
  
  #空データの作成
  temp_predicted <- list()
  
  # add in species category info (種分類情報の追加)
  #列名にSpeciesCat,SpeciesCatName,を含まずにSciNameを含むデータの場合、データにisscaap_linksというデータを付与。
  if (any(c('SpeciesCat', 'SpeciesCatName') %in% colnames(dat)) == F & 'SciName' %in% colnames(dat)) {
    #この意味は、SpeciesCat,SpeciesCatNameを持っていない場合には補填するということでろう。
    dat <- dat %>%
      left_join(isscaap_links, by = 'SciName') #isscaap_linksは魚種名変換用ファイル
    #警告文も付与。種のカテゴリ番号・名前を表す列がないので、FAOデータベースを検索と言っている。
    warning('No species category number or name provided - looking up from FAO database')
  }else if ((any(c('SpeciesCat', 'SpeciesCatName') %in% colnames(dat)) == F & ('SciName' %in% colnames(dat)) == F)){
    #列名にSpeciesCat,SpeciesCatName,を含まずにSciNameも含まないデータの場合、試行を停止して警告文を発している。
    #最低でもカテゴリ番号・名前、学術名の内一つがないとモデルを試行することができないと言っている
    stop(" Can't run model without either species catgory number / name (SpeciesCat/SpeciesCatName) and/or scientific name.
       Provide at least one of these")
    
  }
  
  # Obtain predicted log B/Bmsy from each model　ー　各モデルから推測された対数B/Bmsyを取得する
  #見知らぬ関数apply_prm()が出現。後に解説する。
  #regsの詳細が不明。sysdata.rdaに入っている。regsの中身を見ると回帰モデル?が6つ入っている
  for (i in 1 :length(regs) ){ 
    temp_predicted[[i]] = apply_prm(dat = dat, reg = regs[[i]]) %>% 
      mutate(model = names(regs[i]), model_number = as.numeric(gsub('M','',model))) 
  }
  data <- bind_rows(temp_predicted) %>% #dataに初めに作った空のリストtemp_predictedを積み重ね。bind_rows()はリストを引数にできる
    mutate(model_worked = is.na(LogBvBmsy) == F) %>% #LogBvBmsyがNAである場合model_workedの列にFを付与する
    filter(model_worked == T) %>% #drop models that didn't work 
    ungroup() %>% 
    group_by(IdOrig) %>% 
    filter(model_number == min(model_number)) %>% #各IdOrigにおいて、モデル番号が小さいもの１つを残す。理由は不明。
    mutate(BvBmsy = exp(LogBvBmsy)) %>% #対数を元に戻す
    rename(year = Year, catch = Catch)#keep the best model that ran for each fishery
  
  data <- FindResilience_Kei(data) %>% #FindResilience()でレジリエンスを付与
    rename(res = Res)
  
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
  
  #run_post_prm_pt_cmsy()の説明は後述
  apply_fun <- function(i,data,stocks){
    out = run_post_prm_pt_cmsy(dat = filter(data, IdOrig == stocks[i]))$CatchMSY
  }
  
  results <- lapply(1:length(stocks), apply_fun, data = data, stocks = stocks) %>%
    bind_rows()
  
  return(results)
  
}