#' Run Post PRM Pella-Tomlinson parameterized CatchMSY
#'
#' @param dat the catch data and life history information #漁獲情報と生活史情報が含まれたデータ
#' @param b_to_k_ratio ratio of biomass at bmsy to biomass at K #bmsyの時の資源量とKの時の資源量の割合
#' @param res resilience of the species #種ごとのレジリエンス
#' @param CommonError default sigma of b/bmsy #b/bmsyの標準偏差
#' @param sigR standard deviation of recruitment deviates 
#' @param Smooth T/F to smooth catch histories with running median #漁獲量がsmoothか否か。Fの場合は移動中央値平滑化を行う。
#' @param Display T/F to show outputs as they are created
#' @param n number of iterations to try #試行の繰り返し数
#' @param NumCPUs number of CPUs to use for parallel processing #並行処理に用いるCPU数
#' @return output of CatchMSY 
#' @export

run_post_prm_pt_cmsy_Kei <-
  function(dat,
           b_to_k_ratio = 0.4,
           res = 'Medium',
           start_bio = NA,
           mid_bio = NA,
           final_bio = NA,
           CommonError = 0.05,
           sigR = 0,
           Smooth = F,
           Display = T,
           n = 10000,
           NumCPUs = 1)
  {
  
    #4/2kei:r-kのペアのプロットを表示するコードを追加する関数 -> 理由：r-kペアがどのように選ばれたか見えないと不安に感じるため
    MatrixCmsy_Kei <- function(parbound, n, interbio, finalbio, startbt)
    {
      #with()でparboundを第一引数に固定。
      with(as.list(parbound),
           {
             #レジリエンスから割り当てた初期成長率の範囲から離散型一様分布としてランダムに値をn(デフォルトは10000)個取り出す作業を10回繰り返す
             #ri = rep(exp(runif(n, log(start_g[1]), log(start_g[2]))), length(startbt))  ## get N values between g[1] and g[2], assign to ri
             #4/2kei:上記のコードは範囲がstart_gに固定されている。r,kの更新範囲はparboudに格納されるため、本来はparbound$gを範囲にする必要がある。
             ri = rep(exp(runif(n, log(g[1]), log(g[2]))), length(startbt))
             
             
             #初期環境収容力の範囲から離散型一様分布としてランダムに値をn(デフォルトは10000)個取り出す作業を10回繰り返す
             #ki = rep(exp(runif(n, log(start_k[1]), log(start_k[2]))), length(startbt))  ## get N
             #4/2kei:上記のコードは範囲がstart_gに固定されている。r,kの更新範囲はparboudに格納されるため、本来はparbound$gを範囲にする必要がある。
             ki = rep(exp(runif(n, log(k[1]), log(k[2]))), length(startbt))
             
             #デフォルトではstartbt(B/Kの10%信頼区間を10等分したもの)は10個で、nは10000なので100000個のソートされた値をstartbiに格納する。
             startbti <- sort(rep(startbt, n)) #sortしている意味が分からない。sortしなければ上記の試行１セットごとに10等分が割り当てられると思うのだが？？してもしなくても変わらない
             
             ParamSpace <-
               as.data.frame(cbind(
                 phi, #推定されたphi
                 ri, #初期成長率
                 ki, #初期環境収容力
                 interbio[1], #データ中央年10%信頼区間B/K推定値の下限値(累積確率0.45地点)
                 interbio[2], #データ中央年10%信頼区間B/K推定値の上限値(累積確率0.55地点)
                 finalbio[1], #データ最終年10%信頼区間B/K推定値の下限値(累積確率0.45地点)
                 finalbio[2], #データ最終年10%信頼区間B/K推定値の上限値(累積確率0.45地点)
                 sigR, #プロセスエラー
                 startbti #データ開始年10%信頼区間B/K推定値を10等分した値
               ))
             
             colnames(ParamSpace) <-
               c(
                 'phi',
                 'r',
                 'K',
                 'InterBio1',
                 'InterBio2',
                 'FinalBio1',
                 'FinalBio2',
                 'sigR',
                 'StartBio'
               )
             
             #3/27kei:r-kペアを図示して経過を見たい
             #plot(ParamSpace$g,ParamSpace$K)
             
             #ct(各年漁獲量)が10000回繰り返されたマトリックスを生成
             CatchMat <-
               matrix(
                 rep(ct, dim(ParamSpace)[1]),
                 nrow = dim(ParamSpace)[1],
                 ncol = length(ct),
                 byrow = T
               )
             
             #CatchMatと同じ規格のNAだけが格納されたデータフレームを生成
             btMat <- matrix(NA, nrow = dim(CatchMat)[1], dim(CatchMat)[2])
             
             #1行目に初期環境収容力×データ開始年10%信頼区間B/K推定値を10等分した値×プロセスエラーを行い、"初期資源量"を推定する
             btMat[, 1] <-
               ParamSpace$K * ParamSpace$StartBio * exp(rnorm(dim(btMat)[1], 0, ParamSpace$sigR))
             
             for (y in 2:length(ct))
             {
               #プロセスエラーパート
               xt <- exp(rnorm(dim(btMat)[1], 0, sigR))
               
               #surplus-production-model(Upside)に当てはめ、芋づる式で漁獲量データが存在する年の資源量を推定している。
               btMat[, y] <-
                 (btMat[, y - 1] + ((phi + 1) / phi) * ParamSpace$r * btMat[, y - 1] * (1 - (btMat[, y - 1] / ParamSpace$K) ^ phi) - ct[y - 1]) * (xt)
             }
             
             #4/6kei:B/Kの時系列の行列を作成 -> 条件の採用プロセスを可視化するため
             #btMat_condi <- data.frame(t(data.frame(btMat/ParamSpace$K)))
             #btMat_condi <- btMat_condi %>% gather(key=sampleX,value=Rate,X1:X100000)
             
             #names(btMat_condi) <- CatchYears
             
             
             
             #ID付け
             ItId <- 1:dim(btMat)[1]
             
             ResultMat <- data.frame(ItId, btMat, ParamSpace)
             
             BioDat <- ResultMat[, grepl('X', colnames(ResultMat))]
             
             #round()は四捨五入ではなく、条件に沿って実数を丸める。(1.一番近い実数、2.一番近い実数が二つある場合は末尾が偶数の方を選択)
             #従って、2.5は2と3が最も近く、末尾が偶数なのは2なので2に丸める。このように五捨が起こりうる。
             interyr <- round(median(1:nyr))
             
             EllBio <-
               data.frame(
                 apply(BioDat, 1, min), #その初期資源量(一定条件のもとでランダムに生成したKとB/K)においてSPMを用いて推定した場合のその年の最小資源量を算出
                 apply(BioDat, 1, max), #その初期資源量(一定条件のもとでランダムに生成したKとB/K)においてSPMを用いて推定した場合のその年の最大資源量を算出
                 BioDat[, interyr] / ResultMat$K, #データ中間年でのB/K
                 BioDat[, nyr] / ResultMat$K #データ最終年でのB/K
               )
             
             colnames(EllBio) <-
               c('MinBio_最小資源量', 'MaxBio_最大資源量', 'InterBio_データ中間年でのBvK', 'FinalBio_データ最終年でのBvK')
             
             #論理的に食い違っていないかチェック StartBioが最小のものを条件にしているか不明
             #条件に用いられているFinalBio1&2,InterBio1&2はapply_prm()のpredict.lm()で回帰分析し区間推定したものであり、それと比較している。
             #Ell = ResultMat$StartBio == min(ResultMat$StartBio) & #StartBioが最小のもの(10%信頼区間B/K推定値の下限値(累積確率0.45地点))
             Ell = ResultMat$StartBio == min(ResultMat[!is.na(ResultMat[nyr+1]),]$StartBio) &#4/2kei:元々のコードは絶対的に累積確率0.45地点のstartbtiという条件だったが、こちらは最終年まで資源推定できた結果の中で最小のstartbtiを選ぶ
               EllBio$FinalBio_データ最終年でのBvK >= ResultMat$FinalBio1 & #データ最終年のB/K(finalBio_データ最終年でのB/K)がデータ最終年10%信頼区間B/K推定値の下限値(累積確率0.45地点)を下回っていないか
               EllBio$FinalBio_データ最終年でのBvK <= ResultMat$FinalBio2 & #データ最終年のB/K(finalBio_データ最終年でのB/K)がデータ最終年10%信頼区間B/K推定値の上限値(累積確率0.55地点)を上回っていないか
               EllBio$InterBio_データ中間年でのBvK >= ResultMat$InterBio1 & #データ中央年のB/K(finalBio_データ中央年でのB/K)がデータ中央年10%信頼区間B/K推定値の下限値(累積確率0.45地点)を下回っていないか
               EllBio$InterBio_データ中間年でのBvK <= ResultMat$InterBio2 & #データ中央年のB/K(finalBio_データ中央年でのB/K)がデータ中央年10%信頼区間B/K推定値の上限値(累積確率0.55地点)を下回っていないか
               EllBio$MinBio_最小資源量 > 0 & EllBio$MaxBio_最大資源量 < ResultMat$K #資源量が0以上か & 資源量が環境収容力を上回っていないか
             
             #条件に当てはまらない値の場所をMissing格納
             Missing <- is.na(EllBio$FinalBio_データ最終年でのBvK)
             
             #必要な結果のみをPssibleRunsに格納。
             PossibleRuns <- ResultMat[Ell & !Missing, ]
             
             #3/27kei:図示
             #plot(PossibleRuns$g,PossibleRuns$K)
             
             #4/2kei:図示
             Ell_sum <- ResultMat
             #Ell_fulfill <- na.omit(ResultMat[Ell,])         
             
             #4/2kei:各条件を格納
             Ell1 = EllBio$FinalBio_データ最終年でのBvK >= ResultMat$FinalBio1 & EllBio$FinalBio_データ最終年でのBvK <= ResultMat$FinalBio2
             Ell2 = EllBio$InterBio_データ中間年でのBvK >= ResultMat$InterBio1 & EllBio$InterBio_データ中間年でのBvK <= ResultMat$InterBio2 
             Ell3 = EllBio$MinBio_最小資源量 > 0
             Ell4 = EllBio$MaxBio_最大資源量 < ResultMat$K
             Ell5 = ResultMat$StartBio == min(ResultMat$StartBio)
             
             #4/2kei:最終年まで推定できたかどうかの可否を列(Work)に格納
             Ell_sum$Run_to_end <- ifelse(!is.na(Ell_sum[nyr]),TRUE,FALSE)
             Ell_sum$Run_to_end <- factor(Ell_sum$Run_to_end,levels = c(TRUE,FALSE)) #4/2kei:水準の入れ替え(デフォルトはFの方が先に来る)
             
             #4/2kei:条件を満たすかどうかの可否を列に追加 -> どの条件がどれだけ結果に左右しているかが可視化できる
             Ell_sum$Ell_all <- ifelse(is.na(Ell_sum[nyr]),NA,ifelse(Ell==T,"Fulfill","Not"))
             Ell_sum$Ell_lambda_final <- ifelse(is.na(Ell_sum[nyr]),NA,ifelse(Ell1==T,"Fulfill","Not"))
             Ell_sum$Ell_lambda_inter <- ifelse(is.na(Ell_sum[nyr]),NA,ifelse(Ell2==T,"Fulfill","Not"))
             Ell_sum$Ell_b_over_0 <- ifelse(is.na(Ell_sum[nyr]),NA,ifelse(Ell3==T,"Fulfill","Not"))
             Ell_sum$Ell_b_smaller_k <- ifelse(is.na(Ell_sum[nyr]),NA,ifelse(Ell4==T,"Fulfill","Not"))
             Ell_sum$Ell_min_startbti <- ifelse(is.na(Ell_sum[nyr]),NA,ifelse(Ell5==T,"Fulfill","Not"))
             Ell_sum$Ell_geom <- ifelse(Ell_sum$Run==FALSE,"Collapse",ifelse(Ell_sum$Ell_all=="Fulfill","Fulfill","Doen't_fulfill"))
             
             a <- na.omit(Ell_sum[Ell_sum$Ell_all=="Fulfill",])
             b <- sum(Ell,na.rm = T)
             
             v <- ggplot(Ell_sum,aes(x=r,y=K))+
               geom_point(aes(col=Run_to_end))+
               xlim(parbound$g[1],parbound$g[2])+
               ylim(parbound$k[1],parbound$k[2])+
               labs(title = paste(dat$CommName,num,"trial",sep = " "),subtitle = paste("Black point","(","find",b,"points",")","fulfill all condition",sep = " "))+
               geom_point(data=a,mapping=aes(x=r,y=K),fill="Black")
             
             if (num == "Final")
             {
               vv <- ggplot(Ell_sum,aes(x=r,y=K))+
                 geom_point(aes(col=Run_to_end))+
                 xlim(start_g[1],start_g[2])+
                 ylim(start_k[1],start_k[2])+
                 labs(title = paste(dat$CommName,num,"trial","#scale is prior range",sep = " "),subtitle = paste("Black point","(","find",b,"points",")","fulfill all condition",sep = " "))+
                 geom_point(data=a,mapping=aes(x=r,y=K),fill="Black")
             }else{
               vv <- NA
             }
             
             return(list(PossibleRuns=PossibleRuns,graph=v,graph_final=vv))
             
           })
    }
    
####################    
    
    #当コードでは、target_msy_ratioはb_to_k_ratioを指定している。
    find_phi <- function(phi_guess = 0.188, target_msy_ratio)
    {
      func <- function(phi, target_msy_ratio)
      {
        #パラメーターphiを推定する。
        ratio <- 1 / ((phi + 1) ^ (1 / phi))

        #最小二乗法でbmsy/kがtarget_msy_ratioになるようなphiを算出。
        obj <- (target_msy_ratio - ratio) ^ 2
        return(obj)
      }
  

      #optim関数は最適化を行う関数である。最適化にも様々あるが、L-BFGS-Bを選択しているので、目的関数の最小化を図っている。
      #target_msy_ratio=b_to_k_ratioであり、b_to_k_raitoは0.4とオプションで指定されている。この条件下ではΦ=0.188....の時に極値を持つ。
      #b_to_k_ratio(Bmsy/K)はBmsyが2/Kであるならば0.5になると思っているため、b_to_k_ratioに0.5を指定して最適化を行うとφ=1となり、scheaferのモデルと一致する
      phi = optim(
        phi_guess, #初期値。変な値をセットすると収束しない可能性がある。
        func, #最適化する目的変数を含む数式
        target_msy_ratio = target_msy_ratio, #数式のオプション
        lower = -1, #phiは-1よりも大きい
        upper = 20, #phiは20よりも小さい
        method = 'L-BFGS-B' #最適化手法 制約条件を許す準ニュートン法とのこと 制約条件を許すため探索値の範囲lowerとupperを指定できる
      )$par #parameterであるphiのみ知ることができれば良いので$parとしている。他にもparameterを入れた時の数式の結果であるvalueや試行回数counts、収束したかを表すconvergenceがある

      return(phi)

    }
   

    
#####################
    
    #MatrixCmsy()は条件を満たした結果を返すが、自作関数find_prior_range()は条件を見たした結果の確率を返す。
    #しかし、r,kをランダムにとっているため毎試行結果が異なる。解決策としてはランダムにとらず等分割して計算するか、ランダムにとった結果を大数の法則に準じて何回も繰り返すかである。
    find_prior_range <- function(par=c(start_g[1],start_g[2],start_k[1],start_k[2]),n=10000,mean_n=100){
      
      result_true <- numeric(mean_n)
      
      for(i in 1:mean_n){
        
      #ri <- seq(par[1],par[2],length=10000) #deternimisticにしたい場合
      #ki <- seq(par[1],par[2],length=10000) #deternimisticにしたい場合
      ri <- rep(exp(runif(n,log(par[1]), log(par[2]))),length(startbt)) #rの事前範囲からランダムに獲得 3/31kei:対数で一様分布から抽出して元の尺度に戻しているので、r,k共に小さい値が多くなる
      ki <- rep(exp(runif(n,log(par[3]), log(par[4]))),length(startbt)) #kの事前範囲からランダムに獲得 3/31kei:現時点での考察だが、r-kは反比例関係なのでこのようにしている可能性が示唆される
      startbti <- sort(rep(startbt, n)) #B/Kの45%-55%を10等分して複製
      
      
      ParamSpace <-
        as.data.frame(cbind(
          phi, #推定されたphi
          ri, #初期成長率
          ki, #初期環境収容力
          interbio[1], #データ中央年10%信頼区間B/K推定値の下限値(累積確率0.45地点)
          interbio[2], #データ中央年10%信頼区間B/K推定値の上限値(累積確率0.55地点)
          finalbio[1], #データ最終年10%信頼区間B/K推定値の下限値(累積確率0.45地点)
          finalbio[2], #データ最終年10%信頼区間B/K推定値の上限値(累積確率0.45地点)
          sigR, #プロセスエラー
          startbti #データ開始年10%信頼区間B/K推定値を10等分した値
        ))
      
      colnames(ParamSpace) <-
        c(
          'phi',
          'r',
          'K',
          'InterBio1',
          'InterBio2',
          'FinalBio1',
          'FinalBio2',
          'sigR',
          'StartBio'
        )
      
      #3/27kei:r-kペアを図示して経過を見たい
      #plot(ParamSpace$r,ParamSpace$K)
      
      #ct(各年漁獲量)が10000回繰り返されたマトリックスを生成
      CatchMat <-
        matrix(
          rep(ct, dim(ParamSpace)[1]),
          nrow = dim(ParamSpace)[1],
          ncol = length(ct),
          byrow = T
        )
      
      #CatchMatと同じ規格のNAだけが格納されたデータフレームを生成
      btMat <- matrix(NA, nrow = dim(CatchMat)[1], dim(CatchMat)[2])
      
      #1行目に初期環境収容力×データ開始年10%信頼区間B/K推定値を10等分した値×プロセスエラーを行い、"初期資源量"を推定する
      btMat[, 1] <-
        ParamSpace$K * ParamSpace$StartBio * exp(rnorm(dim(btMat)[1], 0, ParamSpace$sigR))
      
      for (y in 2:length(ct))
      {
        #プロセスエラーパート
        xt <- exp(rnorm(dim(btMat)[1], 0, sigR))
        
        #surplus-production-model(Upside)に当てはめ、芋づる式で漁獲量データが存在する年の資源量を推定している。
        btMat[, y] <-
          (btMat[, y - 1] + ((phi + 1) / phi) * ParamSpace$r * btMat[, y - 1] * (1 - (btMat[, y - 1] / ParamSpace$K) ^ phi) - ct[y - 1]) * (xt)
      }
      
      #ID付け
      ItId <- 1:dim(btMat)[1]
      
      ResultMat <- data.frame(ItId, btMat, ParamSpace)
      
      BioDat <- ResultMat[, grepl('X', colnames(ResultMat))]
      
      #round()は四捨五入ではなく、条件に沿って実数を丸める。(1.一番近い実数、2.一番近い実数が二つある場合は末尾が偶数の方を選択)
      #従って、2.5は2と3が最も近く、末尾が偶数なのは2なので2に丸める。このように五捨が起こりうる。
      interyr <- round(median(1:nyr))
      
      EllBio <-
        data.frame(
          apply(BioDat, 1, min), #その初期資源量(一定条件のもとでランダムに生成したKとB/K)においてSPMを用いて推定した場合のその年の最小資源量を算出
          apply(BioDat, 1, max), #その初期資源量(一定条件のもとでランダムに生成したKとB/K)においてSPMを用いて推定した場合のその年の最大資源量を算出
          BioDat[, interyr] / ResultMat$K, #データ中間年でのB/K
          BioDat[, nyr] / ResultMat$K #データ最終年でのB/K
        )
      
      colnames(EllBio) <-
        c('MinBio_最小資源量', 'MaxBio_最大資源量', 'InterBio_データ中間年でのBvK', 'FinalBio_データ最終年でのBvK')
      
      #論理的に食い違っていないかチェック StartBioが最小のものを条件にしているか不明
      #条件に用いられているFinalBio1&2,InterBio1&2はapply_prm()のpredict.lm()で回帰分析し区間推定したものであり、それと比較している。
      Ell =
        #ResultMat$StartBio == min(ResultMat$StartBio) & #StartBioが最小のもの(10%信頼区間B/K推定値の下限値(累積確率0.45地点))
        EllBio$FinalBio_データ最終年でのBvK >= ResultMat$FinalBio1 & #データ最終年のB/K(finalBio_データ最終年でのB/K)がデータ最終年10%信頼区間B/K推定値の下限値(累積確率0.45地点)を下回っていないか
        EllBio$FinalBio_データ最終年でのBvK <= ResultMat$FinalBio2 & #データ最終年のB/K(finalBio_データ最終年でのB/K)がデータ最終年10%信頼区間B/K推定値の上限値(累積確率0.55地点)を上回っていないか
        EllBio$InterBio_データ中間年でのBvK >= ResultMat$InterBio1 & #データ中央年のB/K(finalBio_データ中央年でのB/K)がデータ中央年10%信頼区間B/K推定値の下限値(累積確率0.45地点)を下回っていないか
        EllBio$InterBio_データ中間年でのBvK <= ResultMat$InterBio2 & #データ中央年のB/K(finalBio_データ中央年でのB/K)がデータ中央年10%信頼区間B/K推定値の上限値(累積確率0.55地点)を下回っていないか
        EllBio$MinBio_最小資源量 > 0 &
        EllBio$MaxBio_最大資源量 < ResultMat$K #資源量が0以上か & 資源量が環境収容力を上回っていないか
    
      
      mean(Ell,na.rm=T)
      result_true[i] <- mean(Ell,na.rm = T)
      
      }
    
    a <- mean(result_true)
      
      
    return(a)
    }
    
    #find_prior_range()
    #system.time(find_prior_range())
    #kei:関数を繰り返す関数を自作(3/30)
    #repeat_fun <- function(func,index,n=100){
     # for (i in 1:n) {
      #  result <- do.call(what=func,arg=index)
       # 
        #if(i==1){
         # result_sum <- data.frame(result)
      #  }else{
       #   result_sum <- cbind(result_sum,result) 
        #  names(result_sum)[i] <- paste("result",i,sep="")
        #}
        #
      #  print(result)
      #}
      #
      #func_sum <- t(result_sum)
      #return(func_sum)
  #  }
    #likelihood_rk <- function(n=10,par=c(start_g[1],start_g[2],start_k[1],start_k[2])){
    #for (i in 1:n) {
     # b[i] <- find_prior_range(par = par)
    #}
    #mean(b)
    #}
    
    #お化けコード(かかる時間的に)
    #estimated_param <- optim(
    #  c(start_g[1],start_g[2],start_k[1],start_k[2]),
      #c(start_g[1],start_g[2],start_k[1],start_k[2]),
    #  find_prior_range(),
    #  control = list(fnscale=-1)
   # )$par

    
    
    
    
    
    
    
### ここからコードスタート　以前はファンクションメイキングパート
    #datにb_to_k_ratioという列が存在しない場合、b_to_k_ratioという列を追加
    if (all(is.na(dat$b_to_k_ratio))) {
      dat$b_to_k_ratio <- b_to_k_ratio
    }

    #論理値TRUEは1とみなされるので、datのcathchがNAである場合は０をかけ、NAがない場合は１をかけることでNA処理をしている
    CatchYears <- (dat$year * as.numeric(is.na(dat$catch) == F))

    #上記の処理によりcatchにおいてNAを含む場合は０に置き換えられているので、NAを付与する。
    CatchYears[CatchYears == 0] <- NA
  
    #dataに存在する最古の漁獲年数の列番号ををFirstCatchYearに抽出、NAを含めるとerrorが返ってくるのでna.rmオプションをTRUEにしてNAを無視している
    FirstCatchYear <- which(dat$year == min(CatchYears, na.rm = T))[1]
    
    #dataに存在する最新の漁獲年数の列番号をLastCatchYearに抽出
    LastCatchYear <- which(dat$year == max(CatchYears, na.rm = T))[1]

    #datに先ほど算出した最新年ー最古年間に存在するデータを抽出している。
    dat <- dat[FirstCatchYear:LastCatchYear, ]

    #データに含まれる年数の格納
    yr   <- dat$year #dat$Year[(dat[,IdVar])==stock]

    #対応する漁獲量も格納
    ct   <-
      dat$catch #(dat$Catch[(dat[,IdVar])==stock])  ## assumes that catch is given in tonnes, transforms to 1'000 tonnes

    #pmin()は並列最小値を算出する関数。BvBmsy*Bmsy/K(b_to_k_ratio)=B/Kを求めている。B/Kを1と比較して最小値を出している。1で頭打ちにしている。
    #BvBmsyはapply_prm()でlogBvBmsyをexp()で戻して作成している
    bio <-
      pmin(1, (dat$BvBmsy * dat$b_to_k_ratio)) #pull out bvbmsy (transposed to B/K)

    #BvBmsyの標準誤差(BvBmsySDの中身はpredict.lm()で生成したse.fitなのでSD:標準偏差と付いているがSE:標準誤差である)
    bioerror <- dat$BvBmsySD

    #BvBmsySDがNAの場合、CommonErrorとして指定した値を付与(デフォルトでは0.05)
    bioerror[is.na(bioerror)] <- CommonError

    PossibleRuns <- NA

    #ct(各年漁獲量)が存在し、LastCatchYearが存在するならば、
    if (sum(ct, na.rm = T) > 0 &
        length(LastCatchYear) > 0 & length(ct) > 1)
    {
      #NAを内挿(interpolation)する。na.approx()はzooパッケージに入っている関数である。線形補間を行う(従って2つ以上の非NA値が必要)
      ct <- na.approx(ct)

      if (Smooth == F) {
        #runmed()は移動中央値平滑化を行う関数である。最も頑強な散布図平滑化である。任意の整数幅で中央値を移動しながら算出する。
        #そのため整数幅は奇数である必要がある。また、整数幅が3の時が最小で頑強である。
        #"平滑化"は初めての知った概念なので、丁寧に記していく。例：1,2,3,4,5,6という行列があるとする。runmed(n,3)を行なった場合、1には1~3の中央値を付与。
        #2には1~3の中央値を付与。3には2~4の中央値を付与していくというように移動しながら、任意の整数幅で中央値を算出する。
        ct <- runmed(ct, 3)
      }

      
      #     res  <- (dat$Res[(dat[,IdVar])==stock])[1] ## resilience from FishBase, if needed, enable in PARAMETER SECTION
      #レジリエンスを抽出
      res <- unique(dat$res)

      #res:レジリエンスがNAの場合は0.5を付与する
      if (all(is.na(res))) {
        res <- 0.5
      }

      #レジリエンスごとにstart_gに値を付与していく。start_gは恐らくstart growth rateのようなものであろう。
      #レジリエンスが高いということは成長が大きいということである。コードもそれを表している。
      for (i in 1) {
        start_g  <- if (res == "Very low") {
          c(0.001, 0.05)
        }
        else if (res == "Low") {
          c(0.05, 0.15)
        }
        else if (res == "Medium") {
          c(0.15, 0.5)
        }
        else if (res == "High") {
          c(0.5, 1)
        }
        else {
          c(0.15, 0.5)
        } ## Medium, or default if no res is found
      }

      #find_phi()でtarget_msy_ratioの値でのphiの最適化を行い、phiを求める。
      phi <- find_phi(target_msy_ratio = b_to_k_ratio)

      #rの代わりにgを用いる
      #4/14
      #start_g <- start_g * ((phi + 1) / phi) #論文を見るかぎり下記の式ではなくこちらでは？？
      start_g <- start_g * (phi / (1 + phi)) #To account for g instead of r

      #時系列の年数
      nyr  <- length(yr)    ## number of years in the time series

      flush.console()

      ## PARAMETER SECTION

      #start_kに最大漁獲量と50倍された最大漁獲量を格納。
      start_k     <-
        c(max(ct, na.rm = T), 50 * max(ct, na.rm = T)) ## default for upper k e.g. 100 * max catch

      #     startbio 	<- c(0.6,1)   ## assumed biomass range at start of time series, as fraction of k

      #qnorm(確率,平均,標準偏差)累積確率、pnorm()は確率密度の時の値を返す。bioerrorはdat$BvBmsySDのこと。bioはB/Kのこと(1未満になるように条件がかかっている)
      #start_bioということで統計開始年のB/Kが正規分布しているとした時の0.45,0.55%地点を計算し、０以上1未満を格納。10%信頼区間のような状態。
      start_bio <-
        pmin(1, pmax(0, c(
          qnorm(0.45, bio[1], bioerror[1]), qnorm(0.55, bio[1], bioerror[1])
        )))

      #start_bioが全てNAの場合(apply_prm()でBvBmsyを推定できなかった場合)、以下を実行
      #ct(各年漁獲量)の1年目が最大漁獲量の50%以下であるならstartbioにc(0.5,0.9)、50%以上ならc(0.3,0.6)を付与する。個人的には逆だと思う！
      #startbio(start_bio)はB/Kの10%信頼区間値である。
      #デフォルトバージョン
      #if (all(is.na(start_bio)))
      #{
      #  startbio    <-
      #    if (ct[1] / max(ct, na.rm = T) < 0.5) {
      #      c(0.5, 0.9)
      #    } else {
      #      c(0.3, 0.6)
      #    } ## use for batch processing #SUB IN BVBMSY VALUES
      #} else {
      #  startbio <- start_bio
      #}
       
      ###########  Bmsy/K
      if (all(is.na(start_bio)))
      {
        startbio    <-
          if (ct[1] / max(ct, na.rm = T) < 0.5) {
            c(0.5, 0.9)
          } else {
            c(0.3, 0.6)
          } ## use for batch processing #SUB IN BVBMSY VALUES
      } else {
        startbio <- start_bio
      }

      #データ年の中央値を算出
      interyr 	<-
        median(1:length(yr))   ## interim year within time series for which biomass estimate is available; set to yr[2] if no estimates are available #SUB IN INTERMIN YEAR

      mid_bio    <-
        NA # pmin(1,pmax(0,c(qnorm(0.45,bio[1],bioerror[1]),qnorm(0.55,bio[1],bioerror[1]))))

      #上記で強制NAにされているので、ifを使うまでも無いが、interbioにc(0,1)を付与する。→データ中間年での推定B/Kは0-1(0-100%)になる。
      if (all(is.na(mid_bio)))
      {
        interbio 	<-
          c(0, 1) ## biomass range for interim year, as fraction of k; set to 0 and 1 if not available
      } else{
        interbio <- mid_bio
      }

      #データの中央年を抜き出す。
      interyr <- yr[interyr]

      #データ最終年でのB/Kの10%信頼区間を算出。
      final_bio    <-
        pmin(1, pmax(0, c(
          qnorm(0.45, bio[nyr], bioerror[nyr]), qnorm(0.55, bio[nyr], bioerror[nyr])
        )))

      #final_bioが全てNAの場合、以下を実行
      #ct(各年漁獲量)の最終年が最大漁獲量の50%以上であるならfinalbioにc(0.3,0.7)、50%以上ならc(0.01,0.4)を付与する。
      #finalbio(final_bio)はB/Kの10%信頼区間値である。
      if (all(is.na(final_bio)))
      {
        finalbio    <-
          if (ct[nyr] / max(ct, na.rm = T) > 0.5) {
            c(0.3, 0.7)
          } else {
            c(0.01, 0.4)
          } ## use for batch processing #SET TO KNOWN B/BMSY RANGE

      } else{
        finalbio <- final_bio
      }

      #startbioを10等分し、startbtに格納。データ開始年のB/Kの10%信頼区間値を10等分している。
      startbt     <-
        seq(startbio[1], startbio[2], length.out = 10) ## apply range of start biomass in steps of 0.05

      parbound <-
        list(
          g = start_g, #レジリエンスごとに固定値を付与している。(phi/phi+1がかけられていることに注意。単純なgではない)
          k = start_k, #最大漁獲量と50倍された最大漁獲量
          lambda = finalbio, #データ最終年でのB/Kの10%信頼区間推定値
          sigR = sigR, #オプションで固定値0とされている。プロセスエラー
          phi = phi #optim()でphiを最適化して算出したもの
        )
      
      #オプションでDisplayがTRUEならば、各種情報を表示
      if (Display == T)
      { 
        cat(unique(dat$SciName),"\n")
        cat("Last year:データ最新年 =", max(yr), ", last catch:データ最新年漁獲量 =", ct[nyr], "\n")
        cat("Resilience:レジリエンス =", res, "\n")
        cat("Process error:プロセスエラー =", sigR, "\n")
        cat("Assumed initial biomass (B/k) :データ初期の推定B/K・データ初期B/Kの回帰の45%-55%点 =",
            startbio[1],
            "-",
            startbio[2],
            " k",
            "\n")
        cat(
          "Assumed intermediate biomass (B/K) in",
          interyr,
          ":データ中期の推定B/K =",
          interbio[1],
          "-",
          interbio[2],
          " k",
          "\n"
        )
        cat(
          "Assumed final biomass (B/K):データ終期の推定B/K・データ終期B/Kの回帰の45%-55%点 =",
          parbound$lambda[1],
          "-",
          parbound$lambda[2],
          " k",
          "\n"
        )
        cat("Initial bounds for g:レジリエンスに対応したgをphiで変形した後の事前範囲 =",
            parbound$g[1],
            "-",
            parbound$g[2],
            "\n")
        cat(
          "Initial bounds for k:Kの事前範囲(漁獲量の1~50倍) =",
          format(parbound$k[1], digits = 3),
          "-",
          format(parbound$k[2], digits = 3),
          "\n"
        )
      }
      flush.console()

      #4/14kei:事前分布での結果をみる
      num = "1st" #4/2kei:plot用
      Summary1 <- MatrixCmsy_Kei(parbound, n, interbio, finalbio, startbt)
      PossibleRuns <- Summary1$PossibleRuns
      
      #4/2kei:図を見たい場合
      if (Plot == T) {
        plot(Summary1$graph)
      }else{
        cat("*****Plot option is FALSE now. If you want to see the plot of r-k pair, change the plot option to TRUE*****")
      }
      
      
      #お化けコード(かかる時間的に)
      estimated_param <- optim(
        c(start_g[1],start_g[2],start_k[1],start_k[2]),
        #c(start_g[1],start_g[2],start_k[1],start_k[2]),
        find_prior_range,
        control = list(fnscale=-1)
      )$par
      
      #最適化した範囲を付与
      parbound$g <- c(estimated_param[1],estimated_param[2])
      parbound$k <- c(estimated_param[3],estimated_param[4])
      
      
      num = "Final" #4/2kei:plot用
      
      #4/14kei:
      Summary3 <-
        MatrixCmsy_Kei(parbound, n, interbio, finalbio, startbt)
      PossibleRuns <- Summary3$PossibleRuns
      
      if (Plot == TRUE){
        
        plot(Summary3$graph_final)  
        plot(Summary3$graph)
        
      }else{
        cat("*****Plot option is FALSE now. If you want to see the plot of r-k pair, change the plot option to TRUE*****")
      }
      
      #Fail列を追加
      PossibleRuns$Fail <- 0
      
      #id列を追加(一見、何の紐付けもなく付与していて危険に見えるが、1魚種ずつ試行しているため安全である)
      PossibleRuns$id <- unique(dat$id)
      
      ## Get statistics on g, k and msy
      g   <- PossibleRuns$r
      k 	<- PossibleRuns$K
      
      #msyの算出を行なっている
      PossibleRuns$MSY <- (g * k) * b_to_k_ratio
      
      #資源量の列のみを選択し、(B/K)/(Bmsy/K)を行なっている。つまり、B/Bmsyの算出を行なっている。　
      bvbmsy <-
        (PossibleRuns[, grepl('X', colnames(PossibleRuns))] / k) / b_to_k_ratio
      
      #PossibleRunsには条件を満たしたgとkの候補ごとの推定資源量が格納されている。
      CatchMat = matrix(
        rep(ct, dim(PossibleRuns)[1]), #漁獲量をPossibleRunsの候補分用意する
        nrow = dim(PossibleRuns)[1],
        ncol = length(ct),
        byrow = T
      )
      
      #FvFmsyの算出
      fvfmsy <- CatchMat / PossibleRuns$MSY / bvbmsy
      
      #FvFmsyをPossibleRunsにFinalFvFmsy列に追加
      PossibleRuns$FinalFvFmsy <- fvfmsy[, dim(fvfmsy)[2]]
      
      #BvBmsyをPossibleRunsにFinalBvBmsy列に追加
      PossibleRuns$FinalBvBmsy <- bvbmsy[, dim(bvbmsy)[2]]
      
      #各年のBvBmsyの対数平均をtime_bvbmsyに追加
      time_bvbmsy <- (apply(bvbmsy, 2, function(x)
        exp(mean(log(
          x
        )))))
      #各g-kの候補ごとに全年度通じてのBvBmsyの平均を算出し、最終的に全候補通じてのBvBmsyの平均を算出する。
      mean_bvbmsy <-
        mean(apply(bvbmsy, 1, function(x)
          exp(mean(log(
            x
          )))))
      #各g-kの候補ごとにBvBmsyの対数標準偏差を算出
      LogSD_bvbmsy <- mean(apply(bvbmsy, 1, function(x)
        (sd(log(
          x
        )))))
      
      #各g-kの候補によるmsyの算出を行う(kが打ち消され、g*BmsyとなりBmsy時点でのg:増加量＝MSYなので求めることができる)
      msy <-  (g * k) * b_to_k_ratio
      
      #gとkを用いてmsyを設定しているため、g=Fmsyとなる。
      Fmsy <- g
      
      #対数を取ったmsyの平均ををmean_ln_msyに格納する
      mean_ln_msy = mean(log(msy), na.rm = T)
      
      negative_g <- F
      #       if (any(g < 0))
      #       {
      #         negative_g <- T
      #         g <- abs(g)
      #       }
      #
      #対数をとったgの平均をmean_ln_gに格納する:つまり、候補となったgの平均をとる
      mean_ln_g <- mean(log(g), na.rm = T)
      
      #対数をとったkの平均をmean_ln_kに格納する:つまり、候補となったkの平均をとる
      mean_ln_k <- mean(log(k), na.rm = T)
      
      #         dat$MSY[Where]<- mean(msy,na.rm=T)
      
      # dat$RanCatchMSY[Where]<- TRUE
      
      #datにMSY列を追加する。mean_ln_msyは対数をとっているのでexp()を咬ませれば純粋なmsyが算出可能
      dat$MSY <- exp(mean_ln_msy)
      
      #datにphi列を追加する。phiが前の部分でoptim()を用いて推定済み
      dat$phi <- phi
      
      #       if (negative_g == T)
      #       {
      #         dat$g[Where]<- -exp(mean_ln_g)
      #       }
      #       if (negative_g == F)
      #       {
      #datに候補となったg(成長率)平均の列を追加。上記のnegative_gは成長率がマイナスである魚種に対する処理であったがコメントアウトされている。
      dat$r <- exp(mean_ln_g)
      # }
      
      dat$k <- exp(mean_ln_k) #候補となったk(環境収容力)平均の列を追加
      
      dat$MSYLogSd <- (sd(log(msy))) #推定されたg-kによって算出されたmsyの標準偏差の対数を追加
      
      dat$gLogSd <- (sd(log(g), na.rm = T)) #推定されたgの標準偏差の対数を追加
      
      dat$KLogSd <- (sd(log(k), na.rm = T)) #推定さrたkの標準偏差の対数を追加
      
      dat$CatchMSYBvBmsy <- time_bvbmsy #g-kの候補によって、算出されたbvbmsyの全候補の平均を格納
      
      dat$CatchMSYBvBmsy_LogSd <- LogSD_bvbmsy #上記の対数版
      
      dat$BvBmsy <- time_bvbmsy #g-kの候補によって、算出されたbvbmsyの全候補の平均を格納
      
      dat$FvFmsy <- (dat$catch / dat$MSY) / dat$BvBmsy #FvFmsyの算出
      
      
      #4/13kei:Biomassも比較用に算出
      #1行目に初期環境収容力×データ開始年10%信頼区間B/K推定値を10等分した値×プロセスエラーを行い、"初期資源量"を推定する
      dat$Biomass <- NA
      dat[1,"Biomass"] <- dat$k[1] * mean(PossibleRuns$StartBio) * exp(rnorm(1, 0, sigR))
      
      for (y in 2:length(ct))
      {
        #プロセスエラーパート
        xt <- exp(rnorm(1, 0, sigR))
        
        #surplus-production-model(Upside)に当てはめ、芋づる式で漁獲量データが存在する年の資源量を推定している。
        dat[y, "Biomass"] <-
          (dat[y-1,"Biomass"] + ((phi + 1) / phi) * dat$r[y] * dat[y-1,"Biomass"] * (1 - (dat[y-1,"Biomass"] / dat$k[y]) ^ phi) - ct[y - 1]) * (xt)
      }
      
      
      
      ## plot MSY over catch data
      if(Display == T & NumCPUs == 1 & length(g) > 10) #g-kの候補が11個以上の時、g-kの候補による結果をプロットする
      {
        par(mfcol = c(2, 3)) #プロットエリアを2行3列に分割
        #実際のデータによる漁獲量推移と推定msyと95%信頼区間
        plot(
          yr,
          ct,
          type = "l",
          ylim = c(0, max(ct)),
          xlab = "Year",
          ylab = "Catch (MT)",
          main = "Timeseries of catch",
          sub = paste("Common name:",unique(dat$CommName),sep=" ")
        )
        abline(h = exp(mean(log(msy))),
               col = "red",
               lwd = 2)
        abline(h = exp(mean_ln_msy - 1.96 * sd(log(msy))), col = "red") #95%信頼区間にしたかったためデフォルトでは±2だったが、±1.96に変更した。
        abline(h = exp(mean_ln_msy + 1.96 * sd(log(msy))), col = "red")
        
        #推定したgのヒストグラム
        hist(
          g,
          freq = F,
          xlim = c(0, 1.2 * max(g, na.rm = T)),
          main = paste("Scientific name:",unique(dat$SciName),sep = " "),
          sub = "Timeseries of catch"
        )
        abline(v = exp(mean(log(g))),
               col = "red",
               lwd = 2)
        abline(v = exp(mean(log(g)) - 1.96 * sd(log(g))), col = "red") #95%信頼区間にしたかったためデフォルトでは±2だったが、±1.96に変更した。
        abline(v = exp(mean(log(g)) + 1.96 * sd(log(g))), col = "red")
        
        #初期範囲で採用されたgとkのplot
        plot(
          g1,
          k1,
          xlim = start_g,
          ylim = start_k,
          xlab = "g",
          ylab = "k (MT)",
          main = "Scatter plot ",
          sub = "(r-k prior range)"
        )
        
        #推定したkのヒストグラム
        hist(
          k,
          freq = F,
          xlim = c(0, 1.2 * max(k)),
          xlab = "k (MT)",
          main = "Histogram of K"
        )
        abline(v = exp(mean(log(k))),
               col = "red",
               lwd = 2)
        abline(v = exp(mean(log(k)) - 1.96 * sd(log(k))), col = "red")　#95%信頼区間にしたかったためデフォルトでは±2だったが、±1.96に変更した。
        abline(v = exp(mean(log(k)) + 1.96 * sd(log(k))), col = "red")
        
        #最終的なr-kペアのプロット(最終的なr,kの範囲で採用されたr,kのプロット)
        plot(log(g), log(k), xlab = "ln(g)", ylab = "ln(k)",main="Logarithmic scatter plot",sub = "(r-k new range)")
        abline(v = mean(log(g)))
        abline(h = mean(log(k)))
        abline(mean(log(msy)) + log(4), -1, col = "red", lwd = 2)
        abline(mean(log(msy)) - 1.96 * sd(log(msy)) + log(4), -1, col = "red")
        abline(mean(log(msy)) + 1.96 * sd(log(msy)) + log(4), -1, col = "red")
        
        #msyのヒストグラム
        hist(
          msy,
          freq = F,
          xlim = c(0, 1.2 * max(c(msy))),
          xlab = "MSY (MT)",
          main = "Histogram of MSY"
        )
        abline(v = exp(mean(log(msy))),
               col = "red",
               lwd = 2)
        abline(v = exp(mean_ln_msy - 1.96 * sd(log(msy))), col = "red") #95%信頼区間にしたかったためデフォルトでは±2だったが、±1.96に変更した。
        abline(v = exp(mean_ln_msy + 1.96 * sd(log(msy))), col = "red")
      }
      
      if (Display == T)
      {
        cat("Possible combinations (パラメーターgとkの候補数) = ", length(g), "\n")
        cat("geom. mean g (推定されたgの候補の平均) =", format(exp(mean(log(
          g
        ))), digits = 3), "\n")
        cat("g +/- 1.96 SD (gの95%信頼区間) =",
            format(exp(mean(log(
              g
            )) - 1.96 * sd(log(
              g
            ))), digits = 3),
            "-",
            format(exp(mean(log(
              g
            )) + 1.96 * sd(log(
              g
            ))), digits = 3),
            "\n")
        cat("geom. mean k (推定されたkの候補の平均)=", format(exp(mean(log(
          k
        ))), digits = 3), "\n")
        cat("k +/- 1.96 SD (kの95%信頼区間) =",
            format(exp(mean(log(
              k
            )) - 1.96 * sd(log(
              k
            ))), digits = 3),
            "-",
            format(exp(mean(log(
              k
            )) + 1.96 * sd(log(
              k
            ))), digits = 3),
            "\n")
        cat("geom. mean MSY (推定されたMSYの平均)=", format(exp(mean(log(
          msy
        ))), digits = 3), "\n")
        cat("MSY +/- 1.96 SD (95%信頼区間) =",
            format(exp(mean_ln_msy - 1.96 * sd(log(
              msy
            ))), digits = 3),
            "-",
            format(exp(mean_ln_msy + 1.96 * sd(log(
              msy
            ))), digits = 3),
            "\n")
        
      }
      
      #graph <- NA
      #graph <- list(first = Summary1$graph, second = Summary2$graph, third_scale = Summary3$graph_final, third =Summary3$graph)
      
    } #Close if r1 is greater than 10
    
  } #Close if there is catch loop

return(list(CatchMSY = dat, PossibleParams = PossibleRuns))#, graph = graph))
} #Close function

