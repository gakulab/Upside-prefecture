################################################
## Japan Analysis
## Functions file (exactly the same as the Cuba Project)
## Gavin McDonald (gmcdonald@bren.ucsb.edu)
## Updated by Gavin McDonald and Tracey Mangin (Feb 25, 2017)
## Sustainable Fisheries Group / Ocean Analytics
## Original coding: May 6, 2015
################################################

## Load packages
library(truncnorm)

################################################
## Define functions

################################################
## g_legend function
## Grab legend from ggplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

################################################
## bioModel function
## Biological model in terms of reference points b and f
## b defined as B/BMSY and f defined as F/FMSY
## Uses Pella-Tomlinson model


bioModel = function(b,phi,g,f_intervention1,f_intervention2,f_nonintervention)
{
  b_next = b + ((phi + 1) / phi ) * g * b * (1 -  (b ^ phi)  / (phi + 1)) - g * f_intervention1 * b - g * f_intervention2 * b - g * f_nonintervention * b
  bmax = (phi+1)^(1/phi) - 0.1
  return(max(0,min(bmax,b_next)))
}

################################################
## MSY function
## Calculates MSY using life history information
MSY = function(g,K,phi)
{
  msy = g * K / (phi + 1) ^ (1 / phi)
  return(msy)
}

################################################
## BMSY function
## Calculates BMSY using life history information
BMSY = function(K,phi)
{
  bmsy = K / (phi + 1) ^ (1 / phi)
  return(bmsy)
}

################################################
## econModel function
## Economic model in terms of reference points b and f
## b defined as B/BMSY and f defined as F/FMSY

econModel = function(g,K,phi,p,f,b,c,beta)
{
  revenue = p * f * b * MSY(g,K,phi)
  cost = c * (g * f) ^ beta
  pi =  revenue - cost
  return(list(pi=pi,
              revenue=revenue,
              cost=cost))
}

################################################
## profitMSY function
## Determine profit at MSY and BMSY

profitMSY = function(g,K,phi,p,b,c,beta)
{
  revenue = p * 1 * 1 * MSY(g,K,phi)
  cost = c * (g * 1) ^ beta
  pi =  revenue - cost
  return(pi)
}

################################################
## parameterPull function
## Pulls random value from truncated normal distribution
## lower,expected,upperが同じ時はexpectedを使用、同じではない時はrtruncnormという切断正規分布からランダム値を得る関数からランダム値を取得。

parameterPull = function(upper,lower,expected)
{
  if (lower == expected & expected == upper) parameter = expected else parameter = rtruncnorm(1, a=lower, b=upper, mean=expected, sd = (upper - lower)/4)
  return(parameter)
}
################################################
## log_normal_pull
## Pulls a random value from a lognormal distribution
## which mean is 1 and SD=log(CV(Harvest)) 
## Note: CV(Harvest) represents the coefficient of variation of the catch variable

log_normal_pull = function(mean, SD)
{
  location <- log(mean^2 / sqrt(SD^2 + mean^2))
  shape <- sqrt(log(1 + (SD^2 / mean^2)))
  draws_log <- rlnorm(n = 1, location, shape)
  return(draws_log)
}

################################################
## recoveryTime function
## Calculates time to recovery (first time period when vector value of bProjectionVec > recoveryCutoff)
## bProjectionVec defined in terms of B/BMSY and recoveryCutoff defined in terms of B/BMSY

# recoveryTime = function(bProjectionVec,recoveryCutoff,t)
# {
#   if (max(bProjectionVec) < recoveryCutoff) rectime = NA else rectime = min(which(bProjectionVec >= recoveryCutoff))
#   return(rectime)
# }

### Recovery year function
#########################
#50年のシミュレーションが終わった後に一括で処理する。bProjはBvBMSY。
recoveryTime = function(bProjectionVec,recoveryCutoff,t)
{
  #BvBMSYが全timeの中で最小値の位置を格納
  minimumSlot = which(bProjectionVec == min(bProjectionVec))[1]
  
  #最大のbProj(BvBMSY)がcutoff(デフォルト0.8)を下回る時timeをNAにし、cutoffを超える場合はcutoffを超える最小年を返す。which[1]にするため元のminimumSlotの場所が消えるので、再度minimumSlotを足す方法を取っている
  if (max(bProjectionVec) < recoveryCutoff) time = NA else time = which(bProjectionVec[minimumSlot:t] >= recoveryCutoff)[1] + minimumSlot - 1
  
  return(time)
}

################################################
## profitOptim function
## Internal Optimization Function 
## To be used with dynamicPolicy function
## Gives (negative) value function value for value function iteration code

profitOptim = function(f_intervention, f_nonintervention, b, p1, p2,  K, c1, c2, g, phi, beta, V, bvec, delta, split)
{  
  
  f_intervention = max(0,f_intervention)
  
  f_nonintervention = max(0,f_nonintervention)
  
  f_intervention1 = split * f_intervention
  f_intervention2 = (1-split) * f_intervention
  
  profit1 = econModel(g ,K, phi, p1, f_intervention1, b, c1, beta)$pi
  profit2 = econModel(g, K, phi, p2, f_intervention2, b, c2, beta)$pi
  
  profit = profit1 + profit2
  
  bnext = bioModel(b, phi, g, f_intervention1, f_intervention2, f_nonintervention)
  
  if (bnext < bvec[1]) Vnext = 0 else Vnext = approx(bvec,V,bnext, method = "linear")$y #spline(bvec,V,xout=bnext)
  
  negout= -(profit + delta * Vnext)
  
  if(!is.numeric(negout)) browser()
  if(is.na(negout)) browser()
  
  return(list(negout=negout))
}

################################################
## dynamicPolicy function
## Run Dynamic Optimization
## Solves for optimal policy function f (as function of bvec) given model parameters

dynamicPolicy = function(K,g,phi,p1,p2,c1,c2,beta,disc,bvec,f_nonintervention,split)
{
  
  tol=.01 ## Convergence tolerance
  
  delta= 1/(1+disc) ## Discount parameter
  t=0
  
  f1= matrix(1,length(bvec),1)
  Vnew= matrix(0,length(bvec),1)
  diff= 10*tol
  
  while (t<4 | diff>tol)
  {
    t= t+1
    V= Vnew
    oldf1= f1
    
    for (i in 1:length(bvec))
    {
      b= bvec[i]
      if(i==1)
      {guess= 1}
      else
      {guess= f1[i-1]}
      
      FishOut= optim(par=guess,fn=profitOptim,gr=NULL,lower=0,upper=((phi+1)/phi-0.01-f_nonintervention), f_nonintervention=f_nonintervention,b=b,p1=p1, p2=p2,K=K,c1=c1, c2=c2,g=g,phi=phi,beta=beta,V=V,bvec=bvec,delta=delta,split=split,method="L-BFGS-B")
      
      Vnew[i]= -FishOut$value
      f1[i]= FishOut$par
      
      
    } ## Close bvec loop
    
    diff= sum(abs(f1-oldf1))
    #print(diff)
    
  }## Close while loop
  
  
  return(list(f1=f1))
  
} ## Close function

################################################
## policy function
## Generates four policy functions, one for each scenario
## シナリオ(s)は1=SQ, 2=FMSY, 4=econOptである。 

policy = function(s,g,K,phi,p1,p2,f0Int1,f0Int2,f0NonInt,c1,c2,beta,disc,bvec,split)
{
  
  ## Policy vector for status quo; maintain f0_Intervention
  if (s==1) {f1 = rep(f0Int1,length(bvec))
  f2 = rep(f0Int2,length(bvec))}
  
  ## Policy vector that maximimzes food production; set at f=1 for all conditions, including f_NonIntervention
  ## Split betwen two legal fleets using split
  if (s==2) {f1 = rep(max(0, (1 - f0NonInt) * split),length(bvec))
  f2 = rep(max(0, (1 - f0NonInt) * (1 - split)), length(bvec))}
  
  ## Policy vector that minimizes recovery time; set at 0 until b=1 then set at f=1, including f_NonIntervention
  ## Split between two legal fleets using split
  zeros = which(bvec<1)
  ones = which(bvec>=1)
  if (s==3) {f1 = c(rep(0,length(zeros)),rep(max(0,(1-f0NonInt) * split),length(ones)))
  f2 = c(rep(0,length(zeros)),rep(max(0,(1-f0NonInt) * (1 - split)),length(ones)))}
  
  ## Policy vector that dynamically maximizes NPV
  ## Split between two legal fleets using split
  if (s==4) {f1 = dynamicPolicy(K,g,phi,p1,p2,c1,c2,beta,disc,bvec,f0NonInt,split)$f1 * split
  f2 = dynamicPolicy(K,g,phi,p1,p2,c1,c2,beta,disc,bvec,f0NonInt,split)$f1 * (1-split)}
  
  ## Policy vector for closing the fishery; f0_Intervention = 0
  if (s==5) {f1 = 0
  f2 = 0}
  
  ## Policy vector for open access; maintain f0_Intervention. 
  ## Mortality will be modified in projection loop depending on lambda
  if (s==6) {f1 = rep(f0Int1,length(bvec))
  f2 = rep(f0Int2,length(bvec))}
  
  return(list(f1=f1,
              f2=f2))
}

################################################
## NPV function
## Calculate NPV given a discount rate and vector of profits

NPV = function(discount,P)
{
  pv=vector()
  for (i in 1:length(P))
  {
    pv[i] = P[i] / (1+discount)^i
  }
  return(sum(pv))
}

################################################
## annuity function
## Calculate annuity given a discount rate and vector of profits
## 年単位のprofitを算出。　P:全年のProfit

annuityFunc = function(discount,P)
{
  n = length(P)  #年数
  
  # 割引率が0の時は、年単位で等分割する。そうでは無い場合、割引率を考慮して年単位のprofitを算出。
  if (discount == 0) annuity = NPV(discount,P)/n else{
    annuity = NPV(discount,P) / ((1 - 1/(1+discount)^n)/discount)
  }
  return(annuity)
}

################################################
## breakEvenNPV function
## Calculates NPV break-even point [year]

breakEvenNPV = function(discount,P1,P2)
{
  for (i in 1:length(P1))
  {
    npvCurrent1 = NPV(discount,P1[1:i])
    npvCurrent2 = NPV(discount,P2[1:i])
    if (npvCurrent1 + npvCurrent2 >= 0) pointer = i-1
    if (npvCurrent1 + npvCurrent2 >= 0) break
    if (i==length(P1) & npvCurrent1 + npvCurrent2 <= 0) pointer=NA
  }
  return(pointer)
}

################################################
## projectionModel

projectionModel = function(params,S,CatchShareLoop,LegalLoop) #paramは魚種別の生物モデルの結果パラメタセット、SはSenario、CatchShoreLoop,LegalLoopは文字通り
{ 
  ##Set up loops
  N = params$N ##固定値 1
  time = params$Time ##固定値 50
  if (CatchShareLoop == "yes") L = 2 else L = 1 ##デフォルトではFalse
  if (LegalLoop == "yes") M = 2 else M = 1  ##デフォルトではFalse
  
  ## Set recovery definition cutoff in terms of B/BMSY  ##推察するにB/Bmsyの回復基準値の設定
  cutoff = 0.8
  
  ## K / Bmsy = (phi + 1) ^ (1 / phi)となる。従って、K以上のBはあり得ないので、KをBと置き換えると、maxbは実質的なBvBmsyの上限値という意味を持つことが分かる。
  ## bioModelでBvBMSYの上限となる値
  maxb = (params$phi_expected+1)^(1/params$phi_expected)
  
  ## initialize vector that contains range of b values from 0 to maxbN  #phiに依存してbのレンジが変わる
  bVEC = seq(0,maxb+.1,.1) 
  
  #### 枠組み作り
  
  ## initialize policies array
  policies = array(NA,dim=c(length(S),N,L,M,length(bVEC)))
  
  ## initialize policies2 array
  policies2 = array(NA,dim=c(length(S),N,L,M,length(bVEC)))
  
  ## initialize biomass reference point projection array
  bProjections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize absolute biomass projection array
  BProjections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize intervention fishing mortality reference point projection array
  fInt1Projections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize intervention fishing mortality reference point projection array
  fInt2Projections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize non-intervention fishing mortality reference point projection array
  fNonIntProjections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize total fishing mortality reference point projection array
  fTotalProjections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize intervention absolute harvest projection array
  HInt1Projections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize intervention absolute harvest projection array
  HInt2Projections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize non-intervention absolute harvest projection array
  HNonIntProjections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize total absolute harvest projection array
  HTotalProjections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize revenue projection array
  revenue1Projections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize revenue projection array
  revenue2Projections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize cost projection array
  cost1Projections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize cost projection array
  cost2Projections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize profit projection array
  profit1Projections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize profit projection array
  profit2Projections = array(NA,dim=c(length(S),N,L,M,length(delayVec),time))
  
  ## initialize time to biological recovery matrix
  timeToRecovery = array(NA,dim=c(length(S),N,L,M,length(delayVec)))
  
  ## initialize NPV matrix
  npv1 = array(NA,dim=c(length(S),N,L,M,length(delayVec)))
  
  ## initialize NPV matrix
  npv2 = array(NA,dim=c(length(S),N,L,M,length(delayVec)))
  
  ## initialize annuity matrix
  annuity1 = array(NA,dim=c(length(S),N,L,M,length(delayVec)))
  
  ## initialize annuity matrix
  annuity2 = array(NA,dim=c(length(S),N,L,M,length(delayVec)))
  
  ## initialize NPV break-even point matrix
  breakEven = array(NA,dim=c(length(S),N,L,M,length(delayVec)))
  
  
  ### loop開始
  
  ## Loop over all delay scenarios
  for (h in 1:length(delayVec)){  ## added this loop #delayVecは固定値 2
    
    ## Loop over all policy scenarios #シナリオの数だけloop
    for (i in 1:length(S)){
      
      ## Loop over all Monte-Carlo iterations  #固定値 1
      for (j in 1:N){ 
        
        # ## Loop over legal theta values
        # for (k in 1:Thetas){
        
        ## Loop over catch share price and cost scalars  #固定値 1
        for (l in 1:L){
          
          ## Loop over illegal harvest elimination  #固定値 1
          for (m in 1:M){
            
            #loopはデフォルトでは、delayVecの2 * シナリオ5つで、合計10loopになる。
            
            
            ### パラメタの設定
            
            ## 各パラメにおいて、j==1の時Catch-MSYの結果を使う。しかし、デフォルトではjは1しか取らない
            ## For the first monte-carlo iteration, use the expected values of each parameter
            if (j == 1){
              discN = params$disc_expected   #割引率
              gN = params$g_expected   #成長率(C-MSY)
              phiN = params$phi_expected   #形状変数(0.188)
              maxFN = (phiN+1)/phiN   #phiから導く漁獲効率F
              KN = params$K_expected   #環境収容力(C-MSY)
              b0N = min(maxb,params$b0_expected)   #BvBMSY(PRM)
              f0_totalN = min(maxFN,params$f0_total_expected)   #FvFMSY
              theta1N_legal = params$theta_legal1_expected   #固定値 1
              theta2N_legal = params$theta_legal2_expected   #固定値 0
              thetaN_domestic = params$theta_domestic    #固定値 1
              p1N = params$p1_expected   #生産額の平均値
              p2N = params$p2_expected   #生産額の平均値
              betaN = params$beta_expected   #コストの形状変数
              c1N = params$c1_expected   #推定コストの平均値
              c2N = params$c2_expected   #推定コストの平均値
              gamma_p1N = params$gamma_p1   #econOptで価格が31%増える
              gamma_p2N = params$gamma_p2   #econOptで価格が31%増える
              gamma_c1N = params$gamma_c1   #econOptでコストが23%減る
              gamma_c2N = params$gamma_c2   #econOptでコストが23%減る
              lambdaN = params$lambda   #オープンアクセスの時にfを調整する変数 (fに対して、t時点とMSYの時で利益πの比率導出して乗じる)
              splitN = params$split   #下部のpolicy()で使う

              ## parameterPull()を使う：upper,expected,lowerがある時はexpectedを使用し、無い時は切断正規分布からサンプリングする。
              ## For all other iterations, pull a parameter from a truncated normal distribution
              ## defined by the upper bound, lower bound, and expected mean
            } else {
              discN = parameterPull(params$disc_upper,params$disc_lower,params$disc_expected)
              gN = parameterPull(params$g_upper,params$g_lower,params$g_expected)
              phiN = parameterPull(params$phi_upper,params$phi_lower,params$phi_expected)
              maxFN = (phiN+1)/phiN
              KN = parameterPull(params$K_upper,params$K_lower,params$K_expected)
              b0N = min(maxb,parameterPull(params$b0_upper,params$b0_lower,params$b0_expected))
              f0_totalN = min(maxFN,parameterPull(params$f0_total_upper,params$f0_total_lower,params$f0_total_expected))
              theta1N_legal = parameterPull(params$theta_legal1_upper,params$theta_legal1_lower,params$theta_legal1_expected)
              theta2N_legal = parameterPull(params$theta_legal2_upper,params$theta_legal2_lower,params$theta_legal2_expected)
              thetaN_domestic = params$theta_domestic
              p1N = parameterPull(params$p1_upper,params$p1_lower,params$p1_expected)
              p2N = parameterPull(params$p2_upper,params$p2_lower,params$p2_expected)
              betaN = parameterPull(params$beta_upper,params$beta_lower,params$beta_expected)
              c1N = parameterPull(params$c1_upper,params$c1_lower,params$c1_expected)
              c2N = parameterPull(params$c2_upper,params$c2_lower,params$c2_expected)
              gamma_p1N = params$gamma_p1
              gamma_p2N = params$gamma_p2
              gamma_c1N = params$gamma_c1
              gamma_c2N = params$gamma_c2
              lambdaN = params$lambda
              splitN = params$split
            }
            
            
            ## Set policies for current Monte-Carlo iteration parameters - assume perfect information
            ## theta1N_legalは1,theta2N_legalは0,thetaN_domesticは1がデフォルト。1や0ならcatch-MSYのf0を使うか否かという意味になる
            f0Int1N = f0_totalN * theta1N_legal * thetaN_domestic  #theta1N_legal,thetaN_domestic が 1なので、f0_totalNになる
            
            f0Int2N = f0_totalN * theta2N_legal * thetaN_domestic  #theta2N_legal が 0なので、0になる
            
            f0NonIntN = f0_totalN * (1 - (theta1N_legal * thetaN_domestic) - (theta2N_legal * thetaN_domestic))  #0になる。
            
            #iはシナリオ数(5)、j,l,mは固定値1
            policies[i,j,l,m,] = policy(S[i],gN,KN,phiN,p1N,p2N,f0Int1N,f0Int2N,f0NonIntN,c1N,c2N,betaN,discN,bVEC,splitN)$f1 
            policies2[i,j,l,m,] = policy(S[i],gN,KN,phiN,p1N,p2N,f0Int1N,f0Int2N,f0NonIntN,c1N,c2N,betaN,discN,bVEC,splitN)$f2
            
            ## Set non-intervention f0 depending on illegal harvest elimination loop   #m==1がデフォルト
            if (m == 1) f0NonIntN = f0_totalN * (1 - (theta1N_legal * thetaN_domestic) - (theta2N_legal * thetaN_domestic))   #0になる。
            if (m == 2) f0NonIntN = f0_totalN * (1 - thetaN_domestic)
            
            ## Set price and cost depending on catch share loop  #l == 1なのでgamma関係なし
            if (l == 2) { 
              p1N = p1N * gamma_p1N
              p2N = p2N * gamma_p2N
              c1N = c1N * gamma_c1N
              c2N = c2N * gamma_c2N
            }
            
            # policies[i,j,l,m,] = policy(S[i],gN,KN,phiN,pN,f0IntN,f0NonIntN,cN,betaN,discN,bVEC)$f1
            
            # 将来シミュレーションパート
            ## Loop over all time steps
            for (n in 1:time){
              
              #print(paste("Scenario ",i,"of",length(S),"; Monte Carlo Iteration",j,"of",N,"; Theta", k,"of",Thetas,"; Catch Share Loop",l,"of",L,"; Illegal Elimination Loop",m,"of",M,"; Time Step",n,"of",T,sep=" "))
              
              if (n == 1) {
                bProjections[i,j,l,m,h,n] = b0N
                fInt1Projections[i,j,l,m,h,n] = f0Int1N
                fInt2Projections[i,j,l,m,h,n] = f0Int2N
                fNonIntProjections[i,j,l,m,h,n] = f0NonIntN   #0
                p1N = params$p1_expected
                p2N = params$p2_expected
                c1N = params$c1_expected
                c2N = params$c2_expected
              } else {  ## l == 1がデフォルトであるため、gamma関係なし
                if (l == 2 & n >= delayVec[h]) {  ## added code here  ## gammaは2年目以降一定に掛けている。よく分からない。
                  p1N = params$p1_expected * gamma_p1N
                  p2N = params$p2_expected * gamma_p2N
                  c1N = params$c1_expected * gamma_c1N
                  c2N = params$c2_expected * gamma_c2N
                }
                if (m == 1) fNonIntProjections[i,j,l,m,h,n] = f0NonIntN   #2年目以降はこの行が実行される
                if (m == 2 & n < delayVec[h]) fNonIntProjections[i,j,l,m,h,n] = f0NonIntN
                if (m == 2 & n >= delayVec[h]) fNonIntProjections[i,j,l,m,h,n] = f0_totalN * (1 - thetaN_domestic)
                ## Open access policy scenario
                if (S[i] == 6){
                  if (n >= delayVec[h]) { # Only implement policy if implementation year is reached
                    fInt1Projections[i,j,l,m,h,n] = max(0,min(maxFN,fInt1Projections[i,j,l,m,h,n-1] + 
                                                                lambdaN * 
                                                                profit1Projections[i,j,l,m,h,n-1] / profitMSY(gN,KN,phiN,p1N,bProjections[i,j,l,m,h,n-1],c1N,betaN)))
                    
                    fInt2Projections[i,j,l,m,h,n] = max(0,min(maxFN,fInt2Projections[i,j,l,m,h,n-1] + 
                                                                lambdaN * 
                                                                profit2Projections[i,j,l,m,h,n-1] / profitMSY(gN,KN,phiN,p2N,bProjections[i,j,l,m,h,n-1],c2N,betaN)))
                  }
                  else {
                    fInt1Projections[i,j,l,m,h,n] = f0Int1N
                    fInt2Projections[i,j,l,m,h,n] = f0Int2N
                  }
                }  
                ## All other policy scenarios
                if (S[i] < 6) { #2年目以降は、ここが実行される
                  if (n >= delayVec[h]) { # Only implement improved policy if implementation year is reached
                    if (bProjections[i,j,l,m,h,n] < bVEC[1]) {
                      fInt1Projections[i,j,l,m,h,n] = approx(bVEC,policies[i,j,l,m,],bVEC[1])$y #approx関数は内挿関数
                      fInt2Projections[i,j,l,m,h,n] = approx(bVEC,policies2[i,j,l,m,],bVEC[1])$y 
                      
                    } else {  #政策でコントロールできるのはfであり、政策ごとに異なるfをfInt1,2に格納する
                      fInt1Projections[i,j,l,m,h,n] = approx(bVEC,policies[i,j,l,m,],bProjections[i,j,l,m,h,n])$y  ##approxという内挿関数は、x,yを指定して,xoutで指定したx値に相当するx,yの値を返す
                      fInt2Projections[i,j,l,m,h,n] = approx(bVEC,policies2[i,j,l,m,],bProjections[i,j,l,m,h,n])$y  ##yはpoliciesであり、この値はシナリオ別のfに相当する。b(BvBMSY)の値に依存して用いられるf(policies)も変わる。
                      #bVECはmaxbというBvBmsyの想定可能な範囲を表しており、想定可能なBvBMSYの範囲内でSPMによって得られたBvBmsy(bProj)に対応するf(policies)を返すという内容である。policiesは回帰の様に考えるとわかりやすい。
                    }
                  } else {   
                    fInt1Projections[i,j,l,m,h,n] = f0Int1N
                    fInt2Projections[i,j,l,m,h,n] = f0Int2N
                  }
                }
              }
              
              #fInt1ProjにはFvFMSYが入っているが、fInt2Projは0であり、fNonIntProjは0であるため、fTotalProjにはFvFMSYが入っている。
              fTotalProjections[i,j,l,m,h,n] = fInt1Projections[i,j,l,m,h,n] + fInt2Projections[i,j,l,m,h,n] + fNonIntProjections[i,j,l,m,h,n]
              
              #iはシナリオ,j,l,m,は固定値1,hは2,nはtime
              #bProjはBvBMSYなので、BMSYを乗じて約分し、Bとなる。従って、BProjはBが入る。
              BProjections[i,j,l,m,h,n] = bProjections[i,j,l,m,h,n] * BMSY(KN,phiN)
              
              #fInt1~はFvFMSY、gNは成長率、bPro~はBvBMSYとし、Ht=FtBtである。g=FMSY(gはBの最大増加割合でFMSYはBの最大漁獲割合であるため両者が等しくなるのはMSYと言える)と仮定すると、gとFMSY・BMSYが約分されてFとBのみが残りHt=FtBtが成立する
              HInt1Projections[i,j,l,m,h,n] = fInt1Projections[i,j,l,m,h,n] * gN * bProjections[i,j,l,m,h,n] * BMSY(KN,phiN)
              
              HInt2Projections[i,j,l,m,h,n] = fInt2Projections[i,j,l,m,h,n] * gN * bProjections[i,j,l,m,h,n] * BMSY(KN,phiN)    #fInt2Projが0なので結果も0
              
              #fNonIntProjは0なので、0になる
              HNonIntProjections[i,j,l,m,h,n] = fNonIntProjections[i,j,l,m,h,n] * gN * bProjections[i,j,l,m,h,n] * BMSY(KN,phiN)
              
              #Hの合計
              HTotalProjections[i,j,l,m,h,n] = HInt1Projections[i,j,l,m,h,n] + HInt2Projections[i,j,l,m,h,n] + HNonIntProjections[i,j,l,m,h,n]
              
              #生産額
              revenue1Projections[i,j,l,m,h,n] = econModel(gN,KN,phiN,p1N,fInt1Projections[i,j,l,m,h,n],bProjections[i,j,l,m,h,n],c1N,betaN)$revenue
              
              revenue2Projections[i,j,l,m,h,n] = econModel(gN,KN,phiN,p2N,fInt2Projections[i,j,l,m,h,n],bProjections[i,j,l,m,h,n],c2N,betaN)$revenue   #fInt2Projが0なので結果も0
              
              #費用
              cost1Projections[i,j,l,m,h,n] = econModel(gN,KN,phiN,p1N,fInt1Projections[i,j,l,m,h,n],bProjections[i,j,l,m,h,n],c1N,betaN)$cost
              
              cost2Projections[i,j,l,m,h,n] = econModel(gN,KN,phiN,p2N,fInt2Projections[i,j,l,m,h,n],bProjections[i,j,l,m,h,n],c2N,betaN)$cost   #fInt2Projが0なので結果も0
              
              #利益 = 生産額 - 費用
              profit1Projections[i,j,l,m,h,n] = econModel(gN,KN,phiN,p1N,fInt1Projections[i,j,l,m,h,n],bProjections[i,j,l,m,h,n],c1N,betaN)$pi
              
              profit2Projections[i,j,l,m,h,n] = econModel(gN,KN,phiN,p2N,fInt2Projections[i,j,l,m,h,n],bProjections[i,j,l,m,h,n],c2N,betaN)$pi   #fInt2Projが0なので結果も0
              
              # 最終年が来るまで、資源動態モデルを更新する
              if (n < time) bProjections[i,j,l,m,h,n+1] =  bioModel(bProjections[i,j,l,m,h,n],phiN,gN,fInt1Projections[i,j,l,m,h,n],fInt2Projections[i,j,l,m,h,n],fNonIntProjections[i,j,l,m,h,n])
              
            } ## End loop over time steps
            
            #BvBMSYがcutoff(デフォルト0.8)を超える最小timeを記録。全年共通。
            timeToRecovery[i,j,l,m,h] = recoveryTime(bProjections[i,j,l,m,h,],cutoff,time)
            
            #割引現在価値。割引現在価値にして、全年総額。
            npv1[i,j,l,m,h] = NPV(discN,profit1Projections[i,j,l,m,h,])
            
            npv2[i,j,l,m,h] = NPV(discN,profit2Projections[i,j,l,m,h,])
            
            #年単位のprofit
            annuity1[i,j,l,m,h] = annuityFunc(discN,profit1Projections[i,j,l,m,h,])
            
            annuity2[i,j,l,m,h] = annuityFunc(discN,profit2Projections[i,j,l,m,h,])
            
            
            #使わない
            breakEven[i,j,l,m,h] = breakEvenNPV(discN,profit1Projections[i,j,l,m,h,],profit2Projections[i,j,l,m,h,])
            
          } ## End loop over illegal harvest
          
        } ## End loop over catch schares
        
        # } ## End loop over thetas
        
      } ## End loop over Monte-Carlo iterations
      
    } ## End loop over policy scenarios
    
  } ## End loop over delay vec
  
  return(list(bVEC=bVEC,
              policies=policies,
              bProjections=bProjections, #BvBMSY
              BProjections=BProjections, #7列：management,MC,catchShare,illegalFishing,ImplementYear,time,biomass
              fInt1Projections=fInt1Projections, #FvFMSY1
              fInt2Projections=fInt2Projections, #FvFMSY2
              fNonIntProjections=fNonIntProjections, #FvFMSYill
              fTotalProjections=fTotalProjections, #FvFMSYtotal
              HInt1Projections=HInt1Projections, #harvest1
              HInt2Projections=HInt2Projections, #harvest2
              HNonIntProjections=HNonIntProjections, #harvest_ill_for
              HTotalProjections=HTotalProjections, 
              revenue1Projections=revenue1Projections,
              revenue2Projections=revenue2Projections,
              cost1Projections=cost1Projections,
              cost2Projections=cost2Projections,
              profit1Projections=profit1Projections, #profit1
              profit2Projections=profit2Projections, #profit2
              timeToRecovery=timeToRecovery, #6列：management,MC,catchShare,illegalFishing,implementYear,recTime
              npv1=npv1,
              npv2=npv2,
              breakEven=breakEven,
              cutoff=cutoff,
              annuity1=annuity1,
              annuity2=annuity2))
  
}