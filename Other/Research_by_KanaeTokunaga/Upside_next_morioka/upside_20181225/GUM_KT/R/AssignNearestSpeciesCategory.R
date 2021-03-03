#' AssignNearestSpeciesCategory
#'
#' \code{AssignNearestSpeciesCategory} assigns the nearest species category available from regression
#' @param Data the raw data
#' @param AvailableCategories the categories present in the regression model
#' @param AllCategories all possible species categories
#'
#' @return nearest neighbot species category
#' @export
AssignNearestSpeciesCategory<- function(Data,AvailableCategories,AllCategories)
{

  PossibleNumbers<- (AllCategories$SpeciesCatName  %in% AvailableCategories)

  PossibleCats<- AllCategories[PossibleNumbers,]

  AllCats<- unique(Data$SpeciesCatName)

  OriginalSpeciesCatName<- Data$SpeciesCatName

  for (s in 1:length(AllCats))
  {

    IsAvailable<- any(AllCats[s] %in% PossibleCats$SpeciesCatName)

    if (IsAvailable==F)
    {

      Where<- Data$SpeciesCatName==AllCats[s] & is.na(Data$SpeciesCatName)==F

      Group<- floor(Data$SpeciesCat[Where][1]/10) #Find taxonomic group that it is in

      Possible<- floor(PossibleCats$SpeciesCat/10)

      if (any(Possible==Group))
      {

        SpeciesCat<- Data$SpeciesCat[Where][1]

        GroupDistance<- (abs(PossibleCats$SpeciesCat-SpeciesCat)) # Find the closest match within the group

        ClosestGroupNumber<- PossibleCats$SpeciesCat[GroupDistance==min(GroupDistance)[1]][1]

        ClosestGroup<- PossibleCats$SpeciesCatName[PossibleCats$SpeciesCat==ClosestGroupNumber]

        Data$SpeciesCatName[Where]<- ClosestGroup

      } #Close if
      else
      {

        GroupDistance<- ((abs(Possible-Group)))

        ClosestCategories<- PossibleCats[GroupDistance==min(GroupDistance)[1],]

        ClosestGroup<- ClosestCategories[ClosestCategories$SpeciesCat==max(ClosestCategories$SpeciesCat),]

        Data$SpeciesCatName[Where]<- ClosestGroup$SpeciesCatName

      } #Close else loop

    } #Close if IsAvailable statement
  } #Close species category loop


  return(list(Data=Data,OriginalSpeciesCatName=OriginalSpeciesCatName))

}# Close function
