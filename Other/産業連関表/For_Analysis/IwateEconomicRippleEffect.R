#岩手県の経済波及効果算出関数
#岩手県の分析ツールを参考に作成した。

IwateEconomicRippleEffect <- function(Demand_million_yen,TaisyoSangyo){
  
#小数点以下を指数表記させない
options(scipen=100)

#小数点以下の数字の有効数字を調節するfunctionを作成(cutcut)
cutcut <- function(x,digits){
  return (round(x * 10^digits) /10^digits)
}

#パッケージの読み込み
library(tidyverse)

#パスを指定
setwd("/Volumes/GoogleDrive/Shared drives/gakuLab_Research_Upside/Upside-prefecture/Other/産業連関表/For_Analysis")

#生産者価格表
SeisansyaKakaku <- read_csv("SeisansyaKakaku_Iwate2013.csv")

#投入係数表
TounyuKeisu <- read_csv("TounyuKeisu_Iwate2013.csv")

#逆行列係数表([I-(I-M)A]^-1型を使用する)
GyakuGyouretsu <- read_csv("Gyakugyouretsu_Iwate2013.csv")

#雇用表
Koyou <- read_csv("Koyou_Iwate2013.csv")

#単位：金額は100万円とする。人(ヒト)の単位は人(ニン)とする。

#枠組み作り
#雇用表は部門のみで構成されているので、枠組みに適している。
table <- Koyou[,1] %>%
  mutate(需要増加額 = 0)

#末行は合計なので取り除く。
table <- table[1:nrow(table)-1,]

#枠組みに合わせて生産者価格表・投入係数表・逆行列係数表も整形する
SeisansyaKakaku2 <- filter(SeisansyaKakaku, SeisansyaKakaku$部門名 %in% table$部門名)
TounyuKeisu2 <- filter(TounyuKeisu, TounyuKeisu$部門名 %in% table$部門名)
GyakuGyouretsu2 <- filter(GyakuGyouretsu, GyakuGyouretsu$部門名 %in% table$部門名)
GyakuGyouretsu2 <- select(GyakuGyouretsu2, table$部門名)

#対象産業と需要増加額の設定
#TaisyoSangyo <- "水産食料品"
ZoukaGaku <- Demand_million_yen#万円
SyouhiTenkan <- 0.635208 #2015年の消費転換係数を入れてある

#対象産業に需要増加額を追加する
table$需要増加額 <- ifelse(table$部門名 == TaisyoSangyo, ZoukaGaku, table$需要増加額)

#県内自給率を追加する (1-(移輸入/県内需要合計)) #需要に対して県内産業がそれぞれ応じる割合
table$県内自給率 <- (1 - ((SeisansyaKakaku2$`（控除）移輸入`*-1)/SeisansyaKakaku2$県内需要合計))
table$県内自給率 <- cutcut(table$県内自給率,6)

#研究用のオプション：県内需要に対して、全て県内産業が応じる = 対象産業の自給率100%
#こうすると、県内生産に対する後方連関効果の推定に置き換えられる
#現実的には岩手県の場合は、多く移出し、多くを移入しており、数量としてはほぼ釣り合っている
table$県内自給率 <- ifelse(table$部門名 == TaisyoSangyo, 1, table$県内自給率)

#####    直接効果    #####

#県内需要増加額を追加
table$県内需要増加額 <- table$需要増加額 * table$県内自給率

#誘発される雇用者所得や営業余剰といった粗付加価値を算出する (県内需要増加額 * 粗付加価値率)
#粗付加価値率を投入係数表から抜き出す。県ごとに名称が異なることに応じるべく"粗付加価値"でマッチするように設定。また、行なので列に入れ替え、先頭が文字列のため全体も文字列になっているため数値化した。
ArafukakatiRitsu <- as.numeric(t(filter(TounyuKeisu,grepl("粗付加価値",部門名))))[-1]

table$粗付加価値誘発額 <- table$県内需要増加額 * ArafukakatiRitsu

#誘発される粗付加価値の中でも雇用者所得を算出する
#投入係数表から雇用者所得率を抜き出す。
KoyousyaSyotokuRitsu <- as.numeric(t(filter(TounyuKeisu,grepl("雇用者所得",部門名))))[-1]

#県内需要増加額に雇用者所得率をかけて雇用者所得誘発額を算出する
table$雇用者所得誘発額 <- table$県内需要増加額 * KoyousyaSyotokuRitsu


#####   第一次波及効果    #####

#原材料費を算出する (県内需要増加額 * 対象産業の投入係数)
table$原材料費 <- as.numeric(filter(table,table$部門名 == TaisyoSangyo)[,4]) * TounyuKeisu2[,TaisyoSangyo]

#原材料にも自給率をかける

#原材料に関しては、100%オプションは不要。自給率100%オプションは需要を生産に置き換えるために必要なだけだからである。
#自給率100%オプションの解除
table$県内自給率 <- (1 - ((SeisansyaKakaku2$`（控除）移輸入`*-1)/SeisansyaKakaku2$県内需要合計))
table$県内自給率 <- cutcut(table$県内自給率,6)


table$原材料費の県内需要額 <- table$県内自給率 * table$原材料費

#原材料費による波及効果(生産誘発額)を推定
table$第一次生産誘発額 <- as.matrix(GyakuGyouretsu2) %*% as.matrix(table$原材料費の県内需要額)

#第一次生産誘発額による粗付加価値誘発額
table$第一次生産誘発額_粗付加価値誘発額 <- table$第一次生産誘発額 * ArafukakatiRitsu

#第一次生産誘発額による雇用者所得誘発額
table$第一次生産誘発額_雇用者所得誘発額 <- table$第一次生産誘発額 * KoyousyaSyotokuRitsu

#雇用表から就業係数(人/100万円)を抜き出す(末行の合計を除く)
Syugyousya <- Koyou[,"就業係数"][1:nrow(Koyou)-1,]
table$就業者誘発量 <- round(((table$県内需要増加額 + table$第一次生産誘発額) * Syugyousya)) # / 100

#雇用表から雇用係数(人/100万円)を抜き出す(末行の合計を除く)
KoyouKeisu <- Koyou[,"雇用係数"][1:nrow(Koyou)-1,]
table$雇用者誘発量 <- round(((table$県内需要増加額 + table$第一次生産誘発額) * KoyouKeisu)) # / 100

#直接＋間接(第一次)合わせた雇用者所得誘発額を算出
table$直接_第一次雇用者所得誘発額 <- table$雇用者所得誘発額 + table$第一次生産誘発額_雇用者所得誘発額

#家計消費支出額の算出(雇用所得のうち家計消費に回る額)
table$家計消費支出額 <- sum(table$直接_第一次雇用者所得誘発額) * SyouhiTenkan

#家計消費支出額に民間消費支出構成比をかけ、さらに波及効果を求める。(末行の合計を除く)
MinkanSyouhi <- na.omit(SeisansyaKakaku$民間消費支出構成比)[1:nrow(SeisansyaKakaku2)]
table$消費需要増加額 <- table$家計消費支出額 * MinkanSyouhi

#県内消費需要増加額を算出
table$県内消費需要増加額 <- table$消費需要増加額 * table$県内自給率 


#####   第二次波及効果    #####

#第二次波及効果
table$第二次生産誘発額 <- as.matrix(GyakuGyouretsu2) %*% as.matrix(table$県内消費需要増加額)

#第二次生産誘発額による粗付加価値誘発額
table$第二次生産誘発額_粗付加価値誘発額 <- table$第二次生産誘発額 * ArafukakatiRitsu

#第一次生産誘発額による雇用者所得誘発額
table$第二次生産誘発額_雇用者所得誘発額 <- table$第二次生産誘発額 * KoyousyaSyotokuRitsu

#雇用表から就業係数(人/100万円)を抜き出す(末行の合計を除く)
table$第二次就業者誘発量 <- round((table$第二次生産誘発額 * Syugyousya)) #/ 100

#雇用表から雇用係数(人/100万円)を抜き出す(末行の合計を除く)
table$第二次雇用者誘発量 <- round((table$第二次生産誘発額 * KoyouKeisu)) #/ 100



#####    総効果    #####

table$総効果生産誘発額 <- table$需要増加額 + table$第一次生産誘発額 + table$第二次生産誘発額

table$総効果_粗付加価値誘発額 <- table$粗付加価値誘発額 + table$第一次生産誘発額_粗付加価値誘発額 + table$第二次生産誘発額_粗付加価値誘発額

table$総効果_雇用者所得誘発額 <- table$雇用者所得誘発額 + table$第一次生産誘発額_雇用者所得誘発額 + table$第二次生産誘発額_雇用者所得誘発額

table$総効果_就業者誘発量 <- table$就業者誘発量 + table$第二次就業者誘発量

table$総効果_雇用者誘発量 <- table$雇用者誘発量 + table$第二次雇用者誘発量

return(table)

}

