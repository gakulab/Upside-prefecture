#Catch-MSY論文（A simple method for estimating MSY from catch and resilience. Martell,S. and R.Froese, 2013）のRコード
#出典：上記論文の共同著者であるRainer FroeseさんのHPより（https://www.fishbase.de/rfroese/）
#編集日：2021/3/3

##############

# 2021/07/29
# Catch-MSYの手法にベイズ推論を導入
# Catch-MSYではr,kのペアごとにCatchデータとSurplus Production Modelを用いて尤度を算出する。r,kは区間としてアウトプットされる。しかし、我々はr,kにも分布があるのではないかと仮定してベイズ推論を用いてr,kの事後分布の算出を試みた
# 具体的な手法としては、Catch-MSYによりr,kの区間を算出し、その区間の事前分布と設定してStanを用いてr,kなどの事後分布を求める

##############


#環境の初期化
rm(list = ls(all=T))

set.seed(999)  ## for same random sequence
#require(hacks) ## 既にCRAN上からアーカイブ（もはや削除かも）されてしまったパッケージなのでコメントアウト

## Read Data for stock, year=yr, catch=ct, and resilience=res. Expects space delimited file with header yr  ct and years in integer and catch in real with decimal point
## For example
## stock	res	 yr     ct       
## cap-icel	Medium 1984  1234.32 
## filename <- "RAM_MSY.csv"

## set working directory
setwd("/Volumes/GoogleDrive/Shared drives/gakuLab_Research_Upside/Upside-prefecture/Other/Catch-MSY")

filename <- "ICESct2.csv"
outfile  <- "CatchMSY_Output.csv"
cdat <- read.csv2(filename, header=T, dec=".") #read.csv2：ヨーロッパではcsvなのに区切り文字が;セミコロンのファイルが存在し、それに対応する関数
cat("\n", "File", filename, "read successfully","\n") #cat()は文字列表示関数、"\n"は改行を意味する。"\t"はタブ、"\f"は改ページ、"\\"は\マークらしい

#データ内に含まれる魚種リストを作成
stock_id <- unique(as.character(cdat$stock)) 
## stock_id <- "cod-2224" ## for selecting individual stocks

#中身を見るために適当な魚種を一つ抜き出す"cap-icel"
#cdat <- filter(cdat,stock=="cap-icel")
#stock = "cap-icel"
## Loop through stocks
for(stock in stock_id) {
  yr   <- cdat$yr[as.character(cdat$stock)==stock]
  ct   <- as.numeric(cdat$ct[as.character(cdat$stock)==stock])/1000  ## assumes that catch is given in tonnes, transforms to '000 tonnes
  res  <- unique(as.character(cdat$res[as.character(cdat$stock)==stock])) ## resilience from FishBase, if needed, enable in PARAMETER SECTION
  nyr  <- length(yr)    ## number of years in the time series

## 不必要かもしれないが説明
  ## resilience（レジリエンス）は弾力、回復力、復元力などの意味を持ち、様々な環境・状況に対して適応し、生き延びる力として使われる
  ## レジリエンスが弱い魚種に高強度の漁獲を行うと乱獲になりやすい。一方でレジリエンスが強い魚種であれば高強度の漁獲にも応じ、資源の回復速度が早いとも言える
  ## 魚種に限った話ではなく、漁業においてもレジリエンスがある方が望ましいとされる
  ## 類似した単語でrobustness(ロバストネス)：頑強性・堅牢性がある。こちらは変化・撹乱に抗い機能を維持する特性を指す
  ## 要するにresilience:回復する力、robustness:変わらない力

  cat("\n","Stock",stock,"\n")
  flush.console() #flush.console()は逐次更新を行う関数。通常のforループでは一定の容量に達して初めて出力されるが、
                  ##flush.console()を用いることで、各試行でコンソールを更新し、途中の出力を表示する
                  ##しかし、これはコンソールベースのRguiでの話でRstudioでは関係ない
  
  ## PARAMETER SECTION
  
  ## If resilience is to be used, delete ## in rows 1-4 below and set ## in row 5	below : レジリエンスに応じて内的自然増加率rを設定
  start_r  <- if(res == "Very low"){c(0.015, 0.1)}else if(res == "Low") {c(0.05,0.5)}else if(res == "High") {c(0.6,1.5)}else {c(0.2,1)} ## Medium, or default if no res is found	
  ## start_r     <- c(0.5,1.5)  ## disable this line if you use resilience
  start_k     <- c(max(ct),50*max(ct)) ## default for upper k e.g. 100 * max catch
  ## startbio 	<- c(0.8,1)   ## assumed biomass range at start of time series, as fraction of k
  #startbioは初年度のB/Kの範囲を定めたもので、start_kを掛けて初期資源量の算出に用いる。
  #Upsideではstartbioは回帰分析で求めたB/Bmsyを平均とする正規分布の45%-55%の範囲としている。
  startbio    <- if(ct[1]/max(ct) < 0.5) {c(0.5,0.9)} else {c(0.3,0.6)} ## use for batch processing : バッチ処理は一括処理である。データ内容に左右されない処理である
  interyr 	<- yr[2]   ## interim year within time series for which biomass estimate is available; set to yr[2] if no estimates are available
  #interbioはr-kのペアの採択条件になるが、0~1なので絞り込みとしては機能してい無い。
  interbio 	<- c(0, 1) ## biomass range for interim year, as fraction of k; set to 0 and 1 if not available : 暫定的な資源水準（環境収容力に対する資源の比率）の範囲を指す。分からない場合は0%-100%と全部を対象に入れる
  ## finalbio 	<- c(0.8, 0.9) ## biomass range after last catches, as fraction of k
  #finalbioはr-kのペアの採択条件になる。以下の数字の根拠は別論文に記載。最終年の漁獲量と最高漁獲量を比較して算出する。
  finalbio    <- if(ct[nyr]/max(ct) > 0.5) {c(0.3,0.7)} else {c(0.01,0.4)} ## use for batch processing
  n           <- 30000  ## number of iterations, e.g. 100000
  sigR        <- 0.0      ## process error; 0 if deterministic model; 0.05 reasonable value? 0.2 is too high：プロセスエラーの適正値の設定については別途議論されるだろう
  
  #Upsideでは10等分であった
  startbt     <- seq(startbio[1], startbio[2], by = 0.05) ## apply range of start biomass in steps of 0.05	
  parbound <- list(r = start_r, k = start_k, lambda = finalbio, sigR)
  
  #情報をまとめて出力
  cat("Last year =",max(yr),", last catch =",1000*ct[nyr],"\n")
  cat("Resilience =",res,"\n")
  cat("Process error =", sigR,"\n")
  cat("Assumed initial biomass (B/k) =", startbio[1],"-", startbio[2], " k","\n")
  cat("Assumed intermediate biomass (B/k) in", interyr, " =", interbio[1],"-",interbio[2]," k","\n")
  cat("Assumed final biomass (B/k) =", parbound$lambda[1],"-",parbound$lambda[2]," k","\n")
  cat("Initial bounds for r =", parbound$r[1], "-", parbound$r[2],"\n")
  cat("Initial bounds for k =", format(1000*parbound$k[1], digits=3), "-", format(1000*parbound$k[2],digits=3),"\n")
  
  flush.console()
  
  ######
  # Stanの導入
  library(rstan)
  library(bayesplot)
  #計算の高速化
  rstan_options(aut_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  # 前部で導出されたr,kの初期範囲を事前分布の範囲にする
  data <- list(Year = yr,N = nyr, Catch = ct, lower_r = start_r[1],upper_r=start_r[2],lower_k=start_k[1],upper_k=start_k[2])
  
  setwd("~/研究/R_Stan_C-MSY")
  stan_result <- stan(
    file = "stan_bayes3.stan",
    data = data,
    seed = 1,                              # 乱数の種
    chains = 4,                             # チェーン数
    iter = 2000,                            # 乱数生成の繰り返し数
    #warmup = 1000,                          # バーンイン期間
    thin = 1                                # 間引き数(1なら間引き無し) 
  )
  
  # 結果の表示
  print(
    stan_result,                   # MCMCサンプリングの結果
    probs = c(0.025, 0.5, 0.975)   # 中央値と95%信用区間を出力
  )
  
  
  traceplot(stan_result,pars = c("r","K"))
  
  traceplot(stan_result,inc_warmup=T)
  mcmc_dens(stan_result,pars = c("r","K"))
  
  ######
  
  
  ## FUNCTIONS
  .schaefer	<- function(theta) #通常引数は数字や文字列などの"値"を入れるが、ここでは"リスト"を入れている
  {
    with(as.list(theta), {  ## for all combinations of ri & ki   ##with関数はパイプみたいなもん
      bt=vector()
      ell = 0  ## initialize ell
      for (j in startbt) ## 0.05刻みにしたstartbtの数だけ試行
      {
        if(ell == 0) 
        {
          bt[1]=j*k*exp(rnorm(1,0, sigR))  ## set biomass in first year
          for(i in 1:nyr) ## for all years in the time series
          {
            xt=rnorm(1,0, sigR)
            bt[i+1]=(bt[i]+r*bt[i]*(1-bt[i]/k)-ct[i])*exp(xt) ## calculate biomass as function of previous year's biomass plus net production minus catch
          }
          
          #Upsideであった不明な条件(startbtが最小のものにする)が無い->startbtの値が小さいものから試行を繰り返しているので、同様の結果が得られる。
          #Bernoulli likelihood, assign 0 or 1 to each combination of r and k  ##&&は論理積。つまり全部満たす時にellに1を代入
          ell = 0
          if(bt[nyr+1]/k>=lam1 && bt[nyr+1]/k <=lam2 && min(bt) > 0 && max(bt) <=k && bt[which(yr==interyr)]/k>=interbio[1] && bt[which(yr==interyr)]/k<=interbio[2]) 
            ell = 1 ##全ての条件を満たし、ellに1が代入された時にforループが停止する。startbtを下から試行しているので、条件を満たす中で最小のstartbtの時の結果が出る
                    ## 条件１：推定最終年度における環境収容力に対する資源量の割合が"lam1"以上 #lamはfinal_bioの範囲
                    ## 条件２：推定最終年度における環境収容力に対する資源量の割合が"lam2"以下
                    ## 条件３：最小漁獲量が０以上
                    ## 条件４：最大漁獲量が環境収容力以下
                    ## 条件５：interyr(途中で資源量が分かっている年があればその年を指定：なければ２年目)がinterbioの下限値以上
                    ## 条件６：interyr(途中で資源量が分かっている年があればその年を指定：なければ２年目)がinterbioの上限値以下
        } ## elseは省略	
      }
      return(list(ell=ell)) #ell(条件を満たしたか否か)しか返さない。startbtが小さいとか関係ないらしい
      
      
    })
  }
  
  sraMSY	<-function(theta, N)
  {
    #This function conducts the stock reduction
    #analysis for N trials
    #args:
    #	theta - a list object containing:
    #		r (lower and upper bounds for r)
    #		k (lower and upper bounds for k)
    #		lambda (limits for current depletion)
    
    
    with(as.list(theta), 
         {
           ri = exp(runif(N, log(r[1]), log(r[2])))  ## get N values between r[1] and r[2], assign to ri 一様確率分布からN個のサンプルを取得
           ki = exp(runif(N, log(k[1]), log(k[2])))  ## get N values between k[1] and k[2], assing to ki
           itheta=cbind(r=ri,k=ki, lam1=lambda[1],lam2=lambda[2], sigR=sigR) ## assign ri, ki, and final biomass range to itheta　サンプルを入れた行列を作成（cbindで蓄積）
           M = apply(itheta,1,.schaefer) ## call Schaefer function with parameters in itheta 行(サンプルセット)ごとにシェーファーの式に入れ、各サンプルにおける最小のstartbtでの結果を返す
           i=1:N
           ## prototype objective function
           get.ell=function(i) M[[i]]$ell
           ell = sapply(i, get.ell)  #名前属性付きベクトルを返すapplyファミリー
           return(list(r=ri,k=ki, ell=ell))	 #ell=1は条件を満たしたことを意味する。このデータより
         })
  }
  
  ## MAIN
  R1 = sraMSY(parbound, n)  
  
  
  ## Get statistics on r, k, MSY and determine new bounds for r and k
  #ell==1は条件を満たすという意味である。条件を満たしたr,kのペアを選択。
  r1 	<- R1$r[R1$ell==1]
  k1 	<- R1$k[R1$ell==1]
  
  #MSY(Maximum Sustainable Yield)を算出:MSY = r * K / 4
  msy1  <- r1*k1/4
  #幾何平均を用いて（理由は後述）MSYの代表値を算出
  mean_msy1 <- exp(mean(log(msy1))) #幾何平均（こっちは応用版の幾何平均の計算式なので解説サイト：http://www.wwq.jp/stacalcul/geomean1.htm）
  Summary <- cbind(r1,k1,msy1)
  max_k1a  <- min(k1[r1<1.1*parbound$r[1]]) ## smallest k1 near initial lower bound of r：最初にレジリエンスから算出したrに近い条件を満たしたrに対応するkの中で最小のkを抽出
  max_k1b  <- max(k1[r1*k1/4<mean_msy1]) ## largest k1 that gives mean MSY：幾何平均msyよりも小さいmsyを構成するkの中で最大のk
  max_k1 <- if(max_k1a < max_k1b) {max_k1a} else {max_k1b} ##上記のkを比較して小さい方をmax_k1とする
  
  #条件を満たすr1の数が10個未満の時は注意書きを出力
  if(length(r1)<10) {
    cat("Too few (", length(r1), ") possible r-k combinations, check input parameters","\n")
    flush.console()
  }
  
  ##　条件を満たすrの数が10個以上の時は、、、
  if(length(r1)>=10) {
    
    ## rとkの幅を少し広げて再度試行
    ## set new upper bound of r to 1.2 max r1
    parbound$r[2] <- 1.2*max(r1)
    ## set new lower bound for k to 0.9 min k1 and upper bound to max_k1 
    parbound$k 	  <- c(0.9 * min(k1), max_k1)
    
    
    cat("First MSY =", format(1000*mean_msy1, digits=3),"\n")
    cat("First r =", format(exp(mean(log(r1))), digits=3),"\n")
    cat("New upper bound for r =", format(parbound$r[2],digits=2),"\n")	
    cat("New range for k =", format(1000*parbound$k[1], digits=3), "-", format(1000*parbound$k[2],digits=3),"\n")
    
    
    ## Repeat analysis with new r-k bounds
    R1 = sraMSY(parbound, n)
    
    ## Get statistics on r, k and msy
    r = R1$r[R1$ell==1]
    k = R1$k[R1$ell==1]
    msy = r * k / 4
    mean_ln_msy = mean(log(msy))
    
    ## plot MSY over catch data
    #A
    par(mfcol=c(2,3))
    plot(yr, ct, type="l", ylim = c(0, max(ct)), xlab = "Year", ylab = "Catch (1000 t)", main = stock)
    abline(h=exp(mean(log(msy))),col="red", lwd=2)
    abline(h=exp(mean_ln_msy - 2 * sd(log(msy))),col="red")
    abline(h=exp(mean_ln_msy + 2 * sd(log(msy))),col="red")
    
    #D
    hist(r, freq=F, xlim=c(0, 1.2 * max(r)), main = "")
    abline(v=exp(mean(log(r))),col="red",lwd=2)
    abline(v=exp(mean(log(r))-2*sd(log(r))),col="red")
    abline(v=exp(mean(log(r))+2*sd(log(r))),col="red")
    
    #B
    plot(r1, k1, xlim = start_r, ylim = start_k, xlab="r", ylab="k (1000t)")
    plot(r, k, xlim = start_r, ylim = start_k, xlab="r", ylab="k (1000t)")
    #E
    hist(k, freq=F, xlim=c(0, 1.2 * max(k)), xlab="k (1000t)", main = "")
    abline(v=exp(mean(log(k))),col="red", lwd=2)	
    abline(v=exp(mean(log(k))-2*sd(log(k))),col="red")
    abline(v=exp(mean(log(k))+2*sd(log(k))),col="red")
    
    #C
    plot(log(r), log(k),xlab="ln(r)",ylab="ln(k)")
    abline(v=mean(log(r)))
    abline(h=mean(log(k)))
    abline(mean(log(msy))+log(4),-1, col="red",lwd=2)
    abline(mean(log(msy))-2*sd(log(msy))+log(4),-1, col="red")
    abline(mean(log(msy))+2*sd(log(msy))+log(4),-1, col="red")
    
    #F
    hist(msy, freq=F, xlim=c(0, 1.2 * max(msy)), xlab="MSY (1000t)",main = "")
    abline(v=exp(mean(log(msy))),col="red", lwd=2)
    abline(v=exp(mean_ln_msy - 2 * sd(log(msy))),col="red")
    abline(v=exp(mean_ln_msy + 2 * sd(log(msy))),col="red")
    
    ##最終的なアウトプット
    ##r,k,msy それぞれ幾何平均を取り、標準偏差*+/-2の値も表示
    cat("Possible combinations = ", length(r),"\n")
    cat("geom. mean r =", format(exp(mean(log(r))),digits=3), "\n")
    cat("r +/- 2 SD =", format(exp(mean(log(r))-2*sd(log(r))),digits=3),"-",format(exp(mean(log(r))+2*sd(log(r))),digits=3), "\n")
    cat("geom. mean k =", format(1000*exp(mean(log(k))),digits=3), "\n")
    cat("k +/- 2 SD =", format(1000*exp(mean(log(k))-2*sd(log(k))),digits=3),"-",format(1000*exp(mean(log(k))+2*sd(log(k))),digits=3), "\n")
    cat("geom. mean MSY =", format(1000*exp(mean(log(msy))),digits=3),"\n")
    cat("MSY +/- 2 SD =", format(1000*exp(mean_ln_msy - 2 * sd(log(msy))),digits=3), "-", format(1000*exp(mean_ln_msy + 2 * sd(log(msy))),digits=3), "\n")
    
    ## Write results into outfile, in append mode (no header in file, existing files will be continued)
    output = data.frame(stock, sigR, startbio[1], startbio[2], interbio[1], interbio[2], finalbio[1], finalbio[2], min(yr), max(yr), res, max(ct), ct[1], ct[nyr], length(r), exp(mean(log(r))), sd(log(r)), min(r), quantile(r,0.05), quantile(r,0.25), median(r), quantile(r,0.75), quantile(r,0.95), max(r), exp(mean(log(k))), sd(log(k)), min(k), quantile(k, 0.05), quantile(k, 0.25), median(k), quantile(k, 0.75), quantile(k, 0.95), max(k), exp(mean(log(msy))), sd(log(msy)), min(msy), quantile(msy, 0.05), quantile(msy, 0.25), median(msy), quantile(msy, 0.75), quantile(msy, 0.95), max(msy)) 
    
    write.table(output, file = outfile, append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)
    
  }
}  ## End of stock loop, get next stock or exit


#######
#catch-msyの結果を用いる
data <- list(Year = yr,N = nyr, Catch = ct, lower_r = min(r),upper_r=max(r),lower_k=min(k),upper_k=max(k))
setwd("~/研究/R_Stan_C-MSY")
stan_result <- stan(
  file = "stan_bayes3.stan",
  data = data,
  seed = 1,                              # 乱数の種
  chains = 4,                             # チェーン数
  iter = 2000,                            # 乱数生成の繰り返し数
  #warmup = 1000,                          # バーンイン期間
  thin = 1                                # 間引き数(1なら間引き無し) 
)

# 結果の表示
print(
  stan_result,                   # MCMCサンプリングの結果
  probs = c(0.025, 0.5, 0.975)   # 中央値と95%信用区間を出力
)


traceplot(stan_result,pars = c("r","K"))

traceplot(stan_result,inc_warmup=T)
mcmc_dens(stan_result,pars = c("r","K"))

######


##### 完全オリジナル #####
##幾何平均を用いる理由を体感しよう
#中央値シリーズ
med_msy1 = median(r1)*median(k1)/4
med_msy2 = median(msy1)
med_diff = abs(med_msy1 - med_msy2)
med_diff

#算術平均シリーズ
avg_msy1 = mean(r1)*mean(k1)/4
avg_msy2 = mean(msy1)
avg_diff = abs(avg_msy1 - avg_msy2)
avg_diff

#幾何平均シリーズ
geom_msy1 = exp(mean(log(r1)))*exp(mean(log(k1)))/4
geom_msy2 = exp(mean(log(msy1)))
geom_diff = abs(geom_msy1 - geom_msy2)
geom_diff

#圧倒的に幾何平均の差が小さい！！