#' RunProjections
#'
#' \code{RunProjections} is a function for running projections based on pella tomlinson parameters
#'
#' @param Data
#' @param BaselineYear
#' @param NumCPUs
#' @param StatusQuoPolicy
#' @param Policies
#' @param Discount
#' @param CatchShareCost
#' @param CatchSharePrice
#' @param IdVar
#' @param bvec
#' @param tol
#' @param beta
#' @param ProjectionTime
#'
#' @return
#' @export
RunProjection <- function(Data,
                          BaselineYear,
                          NumCPUs,
                          StatusQuoPolicy = 'StatusQuoA',
                          Policies = c(
                            'StatusQuoOpenAccess',
                            'Opt',
                            'CatchShare',
                            'StatusQuoFForever',
                            'StatusQuoBForever',
                            'Fmsy',
                            'CloseDown'
                          ),
                          Discount = 0,
                          CatchShareCost = 0.77,
                          CatchSharePrice = 1.31,
                          IdVar = 'IdOrig',
                          bvec =  seq(0.00000001, 2.5, length.out = 30),
                          tol = 0.1,
                          beta = 1.3,
                          ProjectionTime = 30,
                          default_catch_share = 0)
{

  Data = filter(Data, is.na(FvFmsy) == F & is.na(BvBmsy) == F)

  Data$BtoKRatio = Data$b_to_k_ratio

  Data$Year = Data$year

  Data$MarginalCost <- NA

  Data$Catch = Data$catch

  Data$Bmsy = Data$MSY / Data$g

  where_catch_share <-
    Data$CatchShare == 1 & Data$Year == 2012

  Data$Price[where_catch_share] <-
    Data$Price[where_catch_share] * CatchSharePrice

  Data$BvBmsy <-
    pmin(1 / Data$BtoKRatio, Data$BvBmsy) #Note capping projection data now

  Stocks <-
    unique(Data[Data$Year == BaselineYear, IdVar])

  Data <- Data[Data$Year <= BaselineYear,]

  if (class(Data$CatchShare) == 'NULL'){

    Data$CatchShare <- default_catch_share

  }

  TempStockMatrix <-
    as.data.frame(matrix(NA, nrow = 0, ncol = dim(Data)[2] + 2))

  colnames(TempStockMatrix) <-
    c(colnames(Data), 'Policy', 'Profits')

  if (NumCPUs > 1)
  {
    if (Sys.info()[1] != 'Windows') {
      Projections <-
        (
          mclapply(
            1:(length(Stocks)),
            SnowProjections,
            mc.cores = NumCPUs,
            Data = Data,
            BaselineYear = BaselineYear,
            Stocks = Stocks,
            IdVar = IdVar,
            bvec = bvec,
            Discount = Discount,
            tol = tol,
            beta = beta,
            CatchSharePrice = CatchSharePrice,
            CatchShareCost = CatchShareCost,
            Policies = Policies,
            ProjectionTime = ProjectionTime,
            TempStockMatrix = TempStockMatrix,
            StatusQuoPolicy = StatusQuoPolicy,
            mc.cleanup = T
          )
        )
    }
    if (Sys.info()[1] == 'Windows')
    {
      sfInit(parallel = TRUE, cpus = NumCPUs)

      sfExportAll()
      sfLibrary(dplyr)

      Projections <-
        sfClusterApplyLB(
          1:(length(Stocks)),
          SnowProjections,
          Data = Data,
          BaselineYear = BaselineYear,
          Stocks = Stocks,
          IdVar = IdVar,
          bvec = bvec,
          Discount = Discount,
          tol = tol,
          beta = beta,
          CatchSharePrice = CatchSharePrice,
          CatchShareCost = CatchShareCost,
          Policies = Policies,
          ProjectionTime = ProjectionTime,
          TempStockMatrix = TempStockMatrix,
          StatusQuoPolicy = StatusQuoPolicy
        )
      sfStop()
    }

  }
  if (NumCPUs == 1)

  {
    Projections <- (
      lapply(
        1:(length(Stocks)),
        SnowProjections,
        Data = Data,
        BaselineYear = BaselineYear,
        Stocks = Stocks,
        IdVar = IdVar,
        bvec = bvec,
        Discount = Discount,
        tol = tol,
        beta = beta,
        CatchSharePrice = CatchSharePrice,
        CatchShareCost = CatchShareCost,
        Policies = Policies,
        ProjectionTime = ProjectionTime,
        TempStockMatrix = TempStockMatrix,
        StatusQuoPolicy = StatusQuoPolicy
      )
    )
  }

  PolicyStorage <-
    lapply(seq(along = Projections), function(i)
      Projections[[i]]$PolicyStorage)

  TempMat <-
    lapply(seq(along = Projections), function(i)
      Projections[[i]]$TempMat)

  TempStockMatrix <- bind_rows(TempMat)

  PolicyStorage <- bind_rows(PolicyStorage)
  Data$Policy <- NA

  Data$Profits <- NA

  Data$Policy[is.na(Data$Policy)] <- 'Historic'

  HistoricData <- Data$Policy == 'Historic'

  HistoricFData <-
    Data$Policy == 'Historic' &
    Data$HasRamFvFmsy == F &
    is.na(Data$FvFmsy) & Data$Dbase != 'RAM' #FIX THIS

  Data$FvFmsy[HistoricFData] <-
    (Data$Catch[HistoricFData] / Data$MSY[HistoricFData]) / Data$BvBmsy[HistoricFData]

  FOA <-
    ((Data$phi + 1) / Data$phi) * (1 - Data$BvBmsyOpenAccess ^ Data$phi / (Data$phi +
                                                                             1))

  c_num <-
    Data$Price * FOA * Data$BvBmsyOpenAccess * Data$MSY

  c_den = (Data$g * FOA) ^ beta

  cost = (c_num / c_den)[HistoricData]

  Data$MarginalCost[HistoricData] <- cost

  Data$MarginalCost[HistoricData == T &
                      Data$CatchShare == 1] <-
    (Data$MarginalCost * CatchShareCost)[HistoricData == T &
                                           Data$CatchShare == 1]

  Data$Profits[HistoricData] = Data$Price[HistoricData] * Data$Catch[HistoricData] - Data$MarginalCost[HistoricData] *
    (Data$FvFmsy[HistoricData] * Data$g[HistoricData]) ^ beta

  Data$Biomass <- Data$BvBmsy * Data$Bmsy

  DataPlus <- rbind((Data), (TempStockMatrix))

  return(list(DataPlus = DataPlus, PolicyStorage = PolicyStorage))

}
