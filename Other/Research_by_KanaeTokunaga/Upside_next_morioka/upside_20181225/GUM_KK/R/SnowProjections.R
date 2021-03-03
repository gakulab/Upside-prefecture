#' Run Snow Projections
#'
#' @param s
#' @param Data
#' @param BaselineYear
#' @param Stocks
#' @param IdVar
#' @param bvec
#' @param Discount
#' @param tol
#' @param beta
#' @param CatchSharePrice
#' @param CatchShareCost
#' @param Policies
#' @param ProjectionTime
#' @param TempStockMatrix
#' @param StatusQuoPolicy
#'
#' @return
#' @export

SnowProjections <- function(s,
           Data,
           BaselineYear,
           Stocks,
           IdVar,
           bvec,
           Discount,
           tol,
           beta,
           CatchSharePrice,
           CatchShareCost,
           Policies,
           ProjectionTime,
           TempStockMatrix,
           StatusQuoPolicy)

  {

    RunDynamicOpt2 = function(MSY, g, phi, p, cost, beta, disc, bvec, tol)
    {
      delta = 1 / (1 + disc) #Discount parameter
      t = 0

      f1 = matrix(1, length(bvec), 1)
      Vnew = matrix(0, length(bvec), 1)
      diff = 10 * tol
      while (t < 4 | diff > tol)
      {
        t = t + 1
        V = Vnew
        oldf1 = f1
        for (i in 1:length(bvec))
        {
          b = bvec[i]
          if (i == 1)
          {
            guess = 1
          }
          else
          {
            guess = f1[i - 1]
          }
          #         FishOut= optim(par=guess,fn=GFRM_funR,lower=0.0001,upper=1.99,b=b,p=p,MSY=MSY,c=c,g=g,beta=beta,V=V,bvec=bvec,delta=delta,method="L-BFGS-B")
          #Optimize f for each bvec
          FishOut = nlminb(
            guess,
            GFRM_funR,
            lower = 0.0001,
            upper = 3,
            b = b,
            p = p,
            MSY = MSY,
            cost = cost,
            phi = phi,
            gar = g,
            beta = beta,
            V = V,
            bvec = bvec,
            delta = delta
          )
          Vnew[i] = -FishOut$objective
          f1[i] = FishOut$par


        } #Close bvec loop

        diff = sum(abs(f1 - oldf1))
        if (t > 200)
        {
          diff <- tol
          write.table(
            paste('Fishery ', Stocks[s], ' is stuck', sep = ''),
            file = 'Optimization Fail Log.txt',
            append = TRUE,
            sep = ";",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )
        }
      }# Close while loop

      #     write.table(paste(  'Number of Trys is ', t,sep=''), file = 'Optimization Testing.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)



      return(list(Policy = f1))

    } #Close function

    ######################################
    # Internal Optimization Function
    # To be used with RunDynamicOptimization2.g
    # Gives (negative) value function value for value function iteration code--------------------------------------------------
    ######################################

    GFRM_funR = function(f,
                         b,
                         p,
                         MSY,
                         cost,
                         phi,
                         gar,
                         beta,
                         V,
                         bvec,
                         delta)
    {
      #Dynamic Optimization workhorse function
      g <- gar

      profit = p * MSY * f * b - cost * (f * g) ^ beta

      bnext = max(min(bvec), b + ((phi + 1) / phi) * g * b * (1 - b ^ phi /
                                                                (phi + 1)) - g * b * f)

      #     bnext= max(min(bvec),b + g*b*(1-b/2) - g/2*b*f)
      bnext = min(max(bvec), bnext)
      out = approx(bvec, t(V), bnext) #spline(bvec,V,xout=bnext)
      Vnext = out$y

      negout = -(profit + delta * Vnext)
      return(negout)
    }

    ######################################
    # Forward Simulation of Policy Function
    # Inputs: polcy function, duration of simulation, and other model parameters
    # Outputs: variables over time (f, b, yield (y), profit (pi)--------------------------------------------------
    ######################################

    OpenAccessFleet <- function(f, pi, t, omega, MsyProfits)
    {
      #Function to adjust f in response to prior profits
      #     if (t==1)
      #     {
      #       f=f
      #     }
      #     if (t>1)
      #     {
      f <- pmin(4, pmax(f + omega * (pi / MsyProfits), .0001))
      # }
      return(f)
    }

    Sim_Forward = function(Policy,
                           fpolicy,
                           IsCatchShare,
                           bvec,
                           b0,
                           pre_f,
                           pre_profits,
                           Time,
                           p,
                           MSY,
                           c,
                           g,
                           phi,
                           beta)
    {
      #Function to simulate fishery populations over time
      b = matrix(0, Time, 1)
      f = b
      pi = b
      y = b
      b[1] = b0
      if (Policy == 'StatusQuoOpenAccess') {
        f[1] <- pre_f
      }
      if (Policy == 'CatchShare' &
          IsCatchShare == 0)
        # apply price cost effects of catch share policy to non-catch share stocks
      {
        p <- p * CatchSharePrice

        c <- c * CatchShareCost
      }

      if (Policy == 'StatusQuoOpenAccess' &
          IsCatchShare == 1)
        # revert previously applied price and cost effects of catch share fisheries for Open Access policy
      {
        p <- p / CatchSharePrice
        c <- c / CatchShareCost
      }

      MsyProfits <- MSY * p - c * (g) ^ beta

      Omega <- 0.1

      PastF <- f[1]

      PastPi <- pre_profits
      for (t in 1:Time)
      {
        if (Policy != 'StatusQuoOpenAccess') {
          f[t] = approx(bvec, fpolicy, b[t])$y
        }
        if (Policy == 'StatusQuoOpenAccess')
        {
          f[t] = OpenAccessFleet(PastF, PastPi, t, Omega, MsyProfits)

          PastF <- f[t]
        }
        pi[t] = p * MSY * f[t] * b[t] - c * (f[t] * g) ^ beta
        PastPi = pi[t]
        y[t] = MSY * f[t] * b[t]
        if (t < Time)
          #Move pella tomlinson forward
        {
          #         b[t+1] =max(min(bvec), b[t] + g*b[t]*(1-b[t]/2) - g/2*b[t]*f[t])
          b[t + 1] = max(min(bvec), b[t] + ((phi + 1) / phi) * g * b[t] *
                           (1 - b[t] ^ phi / (phi + 1)) - g * b[t] * f[t])

        }
      }

      Projection <- data.frame(f, b, y, pi, p, c)

      colnames(Projection) <-
        c('FvFmsy',
          'BvBmsy',
          'Yields',
          'Profits',
          'Price',
          'MarginalCost')

      return(Projection)
    }

    #   sapply(list.files(pattern="[.]g$", path="Functions", full.names=TRUE), source)

    counter <- 1

    #   show(paste(  round(100*(s/length(Stocks)),2),'% Done with Projections',sep=''))

    write.table(
      paste(round(100 * (s / length(
        Stocks
      )), 2), '% Done with Projections', sep = ''),
      file = 'Projection Analysis Progress.txt',
      append = TRUE,
      sep = ";",
      dec = ".",
      row.names = FALSE,
      col.names = FALSE
    )

    TempMat <- TempStockMatrix

    Where <- Data[, IdVar] == Stocks[s]

    #   Where<- Data[,IdVar]== '11776-FAO-67-31'

    StockData <- Data[Where, ]

    if (max(StockData$BvBmsy) > max(bvec))
    {
      maxb <- max(StockData$BvBmsy)

      bvec <- seq(from = 0.00000001,
                  to = maxb + 0.1,
                  length.out = 30)
    }

    RecentStockData <-  StockData[dim(StockData)[1], ]

    Price <- RecentStockData$Price
    MSY <- RecentStockData$MSY
    BOA <- RecentStockData$BvBmsyOpenAccess
    g <- RecentStockData$g
    phi <- RecentStockData$phi
    BtoKRatio <- RecentStockData$BtoKRatio
    FStatusQuo <- RecentStockData$FvFmsy
    IsCatchShare <- RecentStockData$CatchShare

    #     FStatusQuo<- ((RecentStockData$Catch)/MSY)/RecentStockData$BvBmsy

    FStatusQuo[is.na(FStatusQuo)] <- 0

    Where <- Data[, IdVar] == Stocks[s]

    FOA <- ((phi + 1) / phi) * (1 - BOA ^ phi / (phi + 1))

    FStatusQuo <- pmin(FStatusQuo, FOA)

    c_num <-  Price * FOA * BOA * MSY

    c_den = (g * FOA) ^ beta

    cost = c_num / c_den

    Data$MarginalCost[Where] <- cost

    if (IsCatchShare == 1)
      # adjust prices and costs for catch share fisheries before dynamic optimization
    {
      #     Price<-Price*CatchSharePrice
      #
      #     Data$Price[Where]<-Price

      cost <- cost * CatchShareCost

      Data$MarginalCost[Where] <- cost
    }

    MsyProfits = Price * MSY - cost * (g) ^ beta

    OptPolicy <-
      RunDynamicOpt2(MSY, g, phi, Price, cost, beta, Discount, bvec, tol)$Policy

    # Only apply catch share economic effects to non-catch share stocks. Should make Opt and CatchShare Identical for CS stocks
    #   if(IsCatchShare==0)
    #   {
    #     CatchSharePolicy<-  RunDynamicOpt2(MSY,g,CatchSharePrice*Price,CatchShareCost*cost,beta,Discount,bvec,tol)$Policy
    #   }
    #
    #   if(IsCatchShare==1)
    #   {
    #     CatchSharePolicy<-  RunDynamicOpt2(MSY,g,Price,cost,beta,Discount,bvec,tol)$Policy
    #   }
    #
    CatchSharePolicy <- OptPolicy

    #   FoodPolicy<-  RunDynamicOpt2(MSY,g,phi,Price,0,beta,0,bvec,tol)$Policy

    StatusQuoFForeverPolicy <-
      FStatusQuo * matrix(1,
                          nrow = dim(OptPolicy)[1],
                          ncol = dim(OptPolicy)[2])

    FSQ <-
      (RecentStockData$phi + 1) / (RecentStockData$phi) * (1 - RecentStockData$BvBmsy ^
                                                             RecentStockData$phi / (RecentStockData$phi + 1))

    StatusQuoBForeverPolicy <-
      (FSQ) * matrix(1,
                     nrow = dim(OptPolicy)[1],
                     ncol = dim(OptPolicy)[2])

    FmsyPolicy <-
      matrix(1,
             nrow = dim(OptPolicy)[1],
             ncol = dim(OptPolicy)[2])

    CloseDownPolicy <- bvec

    CloseDownPolicy[bvec < 1] <- 0

    CloseDownPolicy[bvec >= 1] <- 1

    StatusQuoOpenAccessPolicy <- FStatusQuo

    PolicyStorage <-
      as.data.frame(matrix(
        NA,
        nrow = length(bvec),
        ncol = 2 + length(Policies)
      ))

    colnames(PolicyStorage) <- c('IdOrig', 'b', Policies)

    PolicyStorage[, c('IdOrig', 'b')] <- data.frame(Stocks[s], bvec)

    for (p in 1:length(Policies))
    {
      eval(parse(text = paste('Policy<-', Policies[p], 'Policy', sep = '')))

      eval(parse(
        text = paste(
          'PolicyStorage$',
          Policies[p],
          '<-',
          Policies[p],
          'Policy',
          sep = ''
        )
      ))
      #     browser()
      #
      bvbmsy_start_projection <-
        min(max(bvec), max(
          min(bvec),
          with(
            RecentStockData,
            BvBmsy + ((phi + 1) / phi) * g * BvBmsy * (1 - BvBmsy ^ phi / (phi + 1)) - g *
              BvBmsy * FvFmsy
          )
        ))

      fvfmsy_pre_projection <- RecentStockData$FvFmsy

      profits_pre_projection <-
        RecentStockData$Price * RecentStockData$Catch - cost * (RecentStockData$g *
                                                                  RecentStockData$FvFmsy) ^ beta

      Projection <-
        Sim_Forward(
          Policy = Policies[p],
          fpolicy = Policy,
          IsCatchShare = IsCatchShare,
          bvec = bvec,
          b0 = bvbmsy_start_projection,
          pre_f = fvfmsy_pre_projection,
          pre_profits = profits_pre_projection,
          Time = ProjectionTime,
          p = Price,
          MSY = MSY,
          c = cost,
          g = g,
          phi = phi,
          beta = beta
        )

      PolicyMatrix <-
        as.data.frame(matrix(NA, nrow = ProjectionTime, ncol = dim(TempMat)[2]))

      PolicyMatrix[, 1:dim(RecentStockData)[2]] <- RecentStockData

      colnames(PolicyMatrix) <-
        c(colnames(RecentStockData), 'Policy', 'Profits')

      PolicyMatrix$Catch <- Projection$Yields

      PolicyMatrix$Price <- Projection$Price

      PolicyMatrix$MarginalCost <- Projection$MarginalCost

      PolicyMatrix$BvBmsy <- Projection$BvBmsy

      PolicyMatrix$FvFmsy <- Projection$FvFmsy

      PolicyMatrix$Year <- RecentStockData$Year + (1:ProjectionTime)

      PolicyMatrix$Profits <- Projection$Profits

      PolicyMatrix$Policy <- Policies[p]

      TempMat[counter:(counter+-1 + (dim(PolicyMatrix)[1])), ] <-
        I(PolicyMatrix)
      #
      counter <- (counter + (dim(PolicyMatrix)[1]))


    } # close policies loop
    #
    if (any(is.na(TempMat$Bmsy)))
    {
      #     TempMat$Bmsy<- TempMat$k * TempMat$BtoKRatio

      TempMat$k <- ((TempMat$MSY / TempMat$g) * (1 / TempMat$BtoKRatio))

      TempMat$Bmsy <- (TempMat$MSY / TempMat$g)

    }

    TempMat$Biomass <- (TempMat$BvBmsy * (TempMat$Bmsy))
    return(list(TempMat = TempMat, PolicyStorage = PolicyStorage))
  } #Close function
