#Model(regs)に含まれる魚種グループを表示するコード　======
a_1 =unique(regs[["M1"]][["model"]]$SpeciesCatName)
a_2 =unique(regs[["M2"]][["model"]]$SpeciesCatName)
a_3 =unique(regs[["M3"]][["model"]]$SpeciesCatName)
a_4 =unique(regs[["M4"]][["model"]]$SpeciesCatName)
a_6 =unique(regs[["M6"]][["model"]]$SpeciesCatName)

Model1 <- a_1[order(a_1)]
Model2 <- a_2[order(a_2)]
Model3 <- a_3[order(a_3)]
Model4 <- a_4[order(a_4)]
Model6 <- a_6[order(a_6)]

n <- 11
length(Model1)=n                    
length(Model2)=n
length(Model3)=n   
length(Model4)=n

regs_fish = cbind(Model1, Model2, Model3, Model4, Model6)

#===========================


#正規分布の区間内を色塗りするコード ============

#塗りつぶす範囲
min <- qnorm(0.45, bio[1], bioerror[1])
max <- qnorm(0.55, bio[1], bioerror[1])
i <- 200 #分割数(細かくするほどきれいになる)

#まずは正規分布(μ=mu、σ=sd)の確率密度関数
mu <- bio[1]
sd <- bioerror[1]
curve(dnorm(x, mu, sd), 0, 1, 
      col="black", xlab="Bmsy/K", ylab="Probability density")

xx <- seq(min, max, length=i) #min～maxまでをi等分したデータを作成
yy <- dnorm(xx, mu, sd) #変数にベクトルを入れると関数の結果もベクトルで出力される

#下限・上限を付加
xx<- c(min, xx, max, min)
yy<- c(0, yy, 0, 0)

#Polygonで作図
polygon(xx, yy, col="gray")

#積分範囲を示す線
lines(c(min, min), c(0, dnorm(min, mu, sd)), col="red")
lines(c(max, max), c(0, dnorm(max, mu, sd)), col="red")

text(min,dnorm(min,mu,sd)+0.015,paste("45%"))
text(max,dnorm(max,mu,sd)+0.015,paste("55%"))
#積分範囲を示す文字(srt=90:90度回転、adj=0:アライメント左揃え(0.5中央揃え、1:右揃え))
text(min, dnorm(min, mu, sd)+0.05, paste(min), srt=-90, adj=0)
text(max, dnorm(max, mu, sd)+0.05, paste(max), srt=-90, adj=0)

#==========================================

###### 回帰直線的なことをやりたい ######

lowlist <- NA
highlist <- NA

for (i in 1:length(bio)) {
  
low <- qnorm(0.45,bio[i],bioerror[i])
high <- qnorm(0.55,bio[i],bioerror[i])


lowlist[i] <- low
highlist[i] <- high
df <- data.frame(yr,lowlist,highlist,bio)
ggplot(df,aes(x=yr))+
  geom_line(aes(y=lowlist),col="black",linetype="solid")+
  geom_line(aes(y=highlist),col = "black",linetype="solid")+
  geom_line(aes(y=bio),col = "black",linetype="dashed")+
  labs(x="Year",y="Bmsy/K",title=dat$CommName)
}
