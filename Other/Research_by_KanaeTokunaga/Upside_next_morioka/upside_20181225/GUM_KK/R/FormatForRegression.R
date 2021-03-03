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
FormatForRegression<- function(f, Data,Fisheries,DependentVariable,CatchVariables,CatchLags,LifeHistoryVars,IsLog,IdVar)

{

  # Format Data For Regression  --------
  #This code reformats data frames into
  # the structure for a panel regression #

  # Create Regression Data Frame --------------------------------------------
  Where<- Data[,IdVar]==Fisheries[f]

  TempFrame<- Data[Where,]

  Where<- TempFrame$IdOrig==Fisheries[f]

  LifeHistoryVars<- sort(LifeHistoryVars)

  DependentName<- DependentVariable

  DependentName<- if (IsLog==T){paste('Log',DependentVariable,sep='')}

  RegNames<- c(DependentName,CatchVariables)

  RegFrame<- as.data.frame(matrix(NA,nrow=dim(TempFrame)[1],ncol=length(RegNames)))

  colnames(RegFrame)<- RegNames

  DependentTemp<-  TempFrame[,DependentVariable]

  if (IsLog==T){DependentTemp<- log(DependentTemp)}

  DependentTemp[is.infinite(DependentTemp)]<- NA

  RegFrame[,DependentName]<-DependentTemp

  # Loop Over Fisheries -----------------------------------------------------


  SlopeWindow<- 1:6

  #   for (f in 1:length(Fisheries))
  #   {

  #     if (is.integer(f/50)){   }

  # write.table(paste(round(100*(f/length(Fisheries))),"% Done with Regression Formating",sep=''), file = 'Regression Formatting Progress.txt', append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)

  MaxCatch<- max(TempFrame$Catch,na.rm=T)

  TempCatch<- TempFrame$Catch

  ScaledCatch<-  TempCatch/MaxCatch

  RegFrame[Where,'ScaledCatch']<- ScaledCatch #Create scaled catch

  RegFrame[Where,'MaxCatch']<- MaxCatch #Maximum recorded catch

  RegFrame[Where,'MeanScaledCatch']<- mean(ScaledCatch ,na.rm=T)#Create scaled catch

  RegFrame[Where,'TimeToMaxCatch']<- which(TempCatch==MaxCatch)[1] #Create time till max catch

  RegFrame[Where,'YearsBack']<-rev(1:length(TempCatch)) #Create time till max catch


  InitialSlope<- NA

  FirstCatch<- which(is.na(ScaledCatch)==F)[1]

  if ((is.na(FirstCatch)==F))
  {
    InitialSlope<- lm(formula=ScaledCatch[FirstCatch:(FirstCatch+5)] ~  SlopeWindow,na.action='na.omit')$coefficients[2]
  }
  RegFrame[Where,'InitialScaledCatchSlope']<- InitialSlope #Create initial slope of scaled catch

  BlankCatch<- matrix(NA,nrow=length(ScaledCatch),ncol=1)

  MaxFrame<- BlankCatch

  for (c in FirstCatch:length(BlankCatch))
  {
    MaxFrame[c]<- max(TempCatch[FirstCatch:c],na.rm=T)
  }

  MaxFrame[is.infinite(MaxFrame)]<-  NA

  RegFrame[Where,'CatchToRollingMax']<- TempCatch/MaxFrame #Create rolling scaled catch

  ## Populate lagged catches ##

  for (l in 1:CatchLags)
  {

    TempLag<- BlankCatch

    LagIndex<- pmax(0,(1:length(BlankCatch))-l)

    TempLag[(1+l):length(BlankCatch)]<- ScaledCatch[LagIndex]

    WhereCol<- colnames(RegFrame)==paste('ScaledCatch',l,'Back',sep='')

    RegFrame[Where,WhereCol]<- TempLag # Create lagged scaled catches

  }

  #   }#Close fisheries loop


  RegFrame<- cbind(TempFrame,RegFrame)
  return(RegFrame)

}


