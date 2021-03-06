#' AssignNearestSpeciesCategory
#'
#' \code{AssignNearestSpeciesCategory} assigns the nearest species category available from regression #AllCategoriesの方のカテゴリーにAvailableCategoriesのカテゴリーを最も近くなるように合わせる 
#' @param Data the raw data
#' @param AvailableCategories the categories present in the regression model #回帰モデル(reg)に存在するカテゴリー
#' @param AllCategories all possible species categories #isscaapのカテゴリー(現存する最大のカテゴリー)
#'
#' @return nearest neighbot species category
#' @export

#種のカテゴリー名のみが変わる。

AssignNearestSpeciesCategory_Kei <- function(Data,AvailableCategories,AllCategories)
{
　
  #全カテゴリー(isscaap)の中で、今回の回帰モデルに含まれるカテゴリーの場所を抽出。(魚のデータのみなので30番台のみ)
  PossibleNumbers <- (AllCategories$SpeciesCatName  %in% AvailableCategories)

  #実際に回帰モデルに含まれるカテゴリーを抜き出す
  PossibleCats<- AllCategories[PossibleNumbers,] 

  #Dataに含まれる種のカテゴリー名を抽出
  AllCats<- unique(Data$SpeciesCatName)

  #種のカテゴリー名のみをOriginalSpeciesCatNameに格納
  OriginalSpeciesCatName<- Data$SpeciesCatName

  #Dataに含まれる魚種カテゴリー1種類ごとに、regの魚種カテゴリーと対応しているかチェック
  for (s in 1:length(AllCats))
  {
    
    #カテゴリー名が対応しているかチェック
    IsAvailable<- any(AllCats[s] %in% PossibleCats$SpeciesCatName)
    
    #もし、カテゴリー名が対応していない場合
    if (IsAvailable==F)
    {
　　　#Whereにデータ内のNAではないその魚種の場所を抜き出す
      Where<- Data$SpeciesCatName==AllCats[s] & is.na(Data$SpeciesCatName)==F

      #データ内の当魚種のカテゴリー番号を1/10にして、floor関数で小数点以下を切り捨てる(10の位にする)
      Group<- floor(Data$SpeciesCat[Where][1]/10) #Find taxonomic group that it is in

      #regに含まれるカテゴリー番号を10で割る。この割り算が意味するのは、10の位が同じ種は同系統で、floorを伴った1/10は各カテゴリー番号を10の位だけにする。
      #regに含まれる魚種は魚のみなのでイカやカニは対応しない
      Possible<- floor(PossibleCats$SpeciesCat/10)

      #データのカテゴリー番号とreg(回帰モデル)に含まれるカテゴリー番号の10の位が一致する場合、(一致しない場合にこの手法を適応すると10の位ごとの同系統さが失われてしまう)
      if (any(Possible==Group))
      {

        #Dataのカテゴリー番号を抜き出し、SpeciesCatに格納。
        SpeciesCat <- Data$SpeciesCat[Where][1]

        #regに含まれている種のカテゴリー番号からデータに含まれている種のカテゴリー番号を引く：regに含まれるカテゴリーと最も距離の近いカテゴリーを探す？
        GroupDistance<- (abs(PossibleCats$SpeciesCat-SpeciesCat)) # Find the closest match within the group

        #Dataに含まれているfor loop対象のカテゴリーに最も近いregに含まれているカテゴリーをClosestGroupNumberに格納。
        ClosestGroupNumber<- PossibleCats$SpeciesCat[GroupDistance==min(GroupDistance)[1]][1]

        #ClosestGroupNumberのカテゴリー名をClosestGroupに格納。
        ClosestGroup<- PossibleCats$SpeciesCatName[PossibleCats$SpeciesCat==ClosestGroupNumber]

        #
        Data$SpeciesCatName[Where]<- ClosestGroup

      } #Close if
      else #10の位が一致しなかった場合、つまり、10の位を跨ぐ場合は、10のくらいの距離が小さい中で番号が大きい方に合わせる。
      {

        GroupDistance<- ((abs(Possible-Group))) #10の位での引き算を行う

        ClosestCategories<- PossibleCats[GroupDistance==min(GroupDistance)[1],] #回帰モデルに含まれる10の位の距離が最小の魚種カテゴリーを選択(regsに含まれる魚種カテゴリーは全て30番台なので必然的に回帰モデルに含まれる魚種カテゴリー全てが対象になる)
        
        ClosestGroup<- ClosestCategories[ClosestCategories$SpeciesCat==max(ClosestCategories$SpeciesCat),] #回帰モデルに含まれる魚種カテゴリーの中で最大の番号を持つ魚種カテゴリーを付与する。従って、基本的にsharks,rays,chimaerasがつけられる。

        Data$SpeciesCatName[Where] <- ClosestGroup$SpeciesCatName #魚種カテゴリーを付与する。

      } #Close else loop

    } #Close if IsAvailable statement
  } #Close species category loop


  return(list(Data=Data,OriginalSpeciesCatName=OriginalSpeciesCatName))

}# Close function
