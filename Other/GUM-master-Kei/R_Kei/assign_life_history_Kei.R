#' assign_life_history
#'
#' \code{assign_life_history} adds in and fills in missing life history information for PRM
#'
#' @param dat the data to be used in the regression etc #回帰等に使用されるデータ
#' @param FishBase potential fishbase data 
#' @param LifeHistoryVars
#'
#' @return data frame with life history
#' @export
assign_life_history_Kei <-
  function(dat,
           FishBase = NA,
           LifeHistoryVars = c(
             'MaxLength',
             'AgeMat',
             'VonBertK',
             'Temp',
             'SpeciesCat',
             'SpeciesCatName',
             'b_to_k_ratio'
           )) {
    # Add in life history variables if missing --------------------------------
    #上記のLifeHistoryVarsに含まれていない生活史情報を抜き出す、つまり、足りない情報が何かを明確にする。
    missing <-
      LifeHistoryVars[!LifeHistoryVars %in% colnames(dat)] # find life history data needed but not present

    #空のデータフレームを作成
    add_in <-
      as.data.frame(matrix(
        as.numeric(NA),
        nrow = dim(dat)[1], 
        ncol = length(missing)
      ))

    colnames(add_in) <- missing #足りない情報のデータフレームを作成

    #空の状態で、まずは合併する
    out_dat <- cbind(dat, add_in) #tack on empty matrices for missing life history data
    out_dat$SciName <- as.character(out_dat$SciName)
    
    # Fill in present but missing variables --------------------------------

    # load('Data/fishbase_data.Rdata')

    # isscaap <- read.csv('Data/ISSCAAP Codes.csv', stringsAsFactors = F)
    
    # fishbase_lifehistoryはsysdata.rdaに入っている。fishbase_varsにfishbase_lifehistoryの列名の中でLifeHistoryVarsの列名(SpeciesCat以外)にも存在する列名を格納
    fishbase_lifehistory <- lifehistory %>%
      select(SciName,SpeciesCat,SpeciesCatName,Temp,AgeMat,MaxLength,VonBertK) %>%
      unique()
    fishbase_vars <-
      colnames(fishbase_lifehistory)[colnames(fishbase_lifehistory) %in% LifeHistoryVars[LifeHistoryVars != 'SpeciesCat']]
  

    #fishbase_lifehistoryのhas_noneの列に情報の状態をチェック。全てNAの場合TRUEを返し、一つでも情報がある場合はFALSEを返す。
    #all()は与えられた論理ベクトルが全てTRUEならTRUEを返す。any()は一つでもTRUEならTRUEを返す。
    fishbase_lifehistory$has_none = apply(is.na(fishbase_lifehistory[, fishbase_vars]), 1, all)

    #任意の生活史データが一つ以上あるデータのみを抽出
    filtered_fblh = fishbase_lifehistory %>%
      filter(has_none == F)

    #データの合併(SciNameで紐付けして情報を合併している)
    life_bank <- out_dat %>% 
      select(IdOrig, SciName, SpeciesCat) %>% #結局始めの3行しか使わないのならout_datを作成した意味がわからない。
      left_join(select(filtered_fblh,-SpeciesCat), by = 'SciName') %>% #-SpeciesCatという形にせずに、byで一緒に指定すれば良いのではと思う。
      left_join(isscaap, by = c('SpeciesCat',"SpeciesCatName")) %>% #ISSCAAPの分類(魚種グループ)を付与
      mutate(b_to_k_ratio = 0.4) #この割合の根拠が欲しい

    # lifenames <- c('MaxLength','AgeMat','VonBertK','Temp', 'SpeciesCat','SpeciesCatName')
    
    #初期に作成したout_datに生活史情報を入れていく。
    for (i in 1:length(LifeHistoryVars)) {
      missing <- is.na(out_dat[, LifeHistoryVars[i]])

      out_dat[missing, LifeHistoryVars[i]] <-
        life_bank[missing, LifeHistoryVars[i]]
    }

    out_dat <- filter(out_dat, is.na(SpeciesCatName) == F)
    return(out_dat)
  }
