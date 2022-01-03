# Japan Run File (TS >= 1964, Queen crab stock revised)
## Kanae Tokunaga
## 1/18/2019 
## Important Updat made by Kristin Kleisner on 3/9/2018 (See KK for Kristin's notes)


rm(list = ls(all=TRUE))

## Set working directory
setwd("/Volumes/GoogleDrive/Shared drives/gakuLab_Research_Upside/Upside-U.Iwate/Data_Working")

## Read in data input file
dataInput = read.csv("4Inputs_for_proj_disc5per_200917.csv", header=TRUE,stringsAsFactors=FALSE)

setwd("/Volumes/GoogleDrive/Shared drives/gakuLab_Research_Upside/Upside-U.Iwate/Code")
## Read in functions file
source("2FunctionsJapan_update.R")

## Read in libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(pander)
library(tidyr)

################################################
#### Set up model run
## Enter management scenarios to loop over - select 1 through 5
scenarios = c(1,2,3,4,5,6)
## Enter whether or not to loop over catch share cost and price scalars - put "yes" or "no"
catchShareLoop = "no"
## Enter whether or not to loop over eliminating illegal fishing - put "yes" or "no"
illegalLoop = "no"

################################################
################################################
### Implementation vector
# delayVec <- c(2:20)
delayVec <- c(2)

################################################################################################
### How to divide fishing mortality rate between the two legal fleets (ratio given to fleet 1)
# split <- 1

## Loop over all fisheries
for (i in 1:nrow(dataInput))
{
    outputs = projectionModel(dataInput[i,],scenarios,catchShareLoop,illegalLoop)

    masterOutputi = cbind(rep(dataInput[i,]$Fishery,nrow(melt(outputs$BProjections))),
                          rep(dataInput[i,]$Species,nrow(melt(outputs$BProjections))),
                          melt(outputs$BProjections),
                          melt(outputs$HInt1Projections)$value,
                          melt(outputs$HInt2Projections)$value,
                          melt(outputs$HNonIntProjections)$value,
                          melt(outputs$profit1Projections)$value,
                          melt(outputs$profit2Projections)$value,
                          melt(outputs$bProjections)$value,
                          melt(outputs$fInt1Projections)$value,
                          melt(outputs$fInt2Projections)$value,
                          melt(outputs$fNonIntProjections)$value,
                          melt(outputs$fTotalProjections)$value)

    colnames(masterOutputi) = c("fishery","species","management","MC", "catchShare","illegalFishing","implementYear", "time","biomass",
                                "harvest1", "harvest2", "harvest_ill_for","profit1","profit2", "BvBMSY","FvFMSY1",
                                "FvFMSY2", "FvFMSYill", "FvFMSYtotal")

    if (i == 1) {
        masterOutput = masterOutputi
    } else {
        masterOutput = rbind(masterOutput,masterOutputi)
    }

    masterOutput$management[masterOutput$management == 1] = "SQ"
    masterOutput$management[masterOutput$management == 2] = "FMSY"
    masterOutput$management[masterOutput$management == 3] = "minRec"
    masterOutput$management[masterOutput$management == 4] = "econOpt"
    masterOutput$management[masterOutput$management == 5] = "close"
    masterOutput$management[masterOutput$management == 6] = "openA"
    masterOutput$catchShare[masterOutput$catchShare == 1] = "no_CS"
    masterOutput$catchShare[masterOutput$catchShare == 2] = "CS"
    masterOutput$illegalFishing[masterOutput$illegalFishing == 1] = "illegal_fishing"
    masterOutput$illegalFishing[masterOutput$illegalFishing == 2] = "no_illegal_fishing"

    recoveryOutputi = cbind(rep(dataInput[i,]$Fishery,nrow(melt(outputs$timeToRecovery))),
                            rep(dataInput[i,]$Species,nrow(melt(outputs$timeToRecovery))),
                            melt(outputs$timeToRecovery))

    colnames(recoveryOutputi) = c("fishery","species","management", "MC", "catchShare","illegalFishing","implementYear", "recTime")

    if (i == 1) {
      recoveryOutput = recoveryOutputi
    } else {
      recoveryOutput = rbind(recoveryOutput,recoveryOutputi)
    }

    recoveryOutput$management[recoveryOutput$management == 1] = "SQ"
    recoveryOutput$management[recoveryOutput$management == 2] = "FMSY"
    recoveryOutput$management[recoveryOutput$management == 3] = "minRec"
    recoveryOutput$management[recoveryOutput$management == 4] = "econOpt"
    recoveryOutput$management[recoveryOutput$management == 5] = "close"
    recoveryOutput$management[recoveryOutput$management == 6] = "openA"
    recoveryOutput$catchShare[recoveryOutput$catchShare == 1] = "no_CS"
    recoveryOutput$catchShare[recoveryOutput$catchShare == 2] = "CS"
    recoveryOutput$illegalFishing[recoveryOutput$illegalFishing == 1] = "illegal_fishing"
    recoveryOutput$illegalFishing[recoveryOutput$illegalFishing == 2] = "no_illegal_fishing"

    npvOutputi = cbind(rep(dataInput[i,]$Fishery,nrow(melt(outputs$npv1))),
                       rep(dataInput[i,]$Species,nrow(melt(outputs$npv1))),
                            melt(outputs$npv1),
                            melt(outputs$npv2)$value)

    colnames(npvOutputi) = c("fishery", "species","management", "MC", "catchShare","illegalFishing","implementYear", "npv1", "npv2")

    if (i == 1) {
      npvOutput = npvOutputi
    } else {
      npvOutput = rbind(npvOutput,npvOutputi)
    }

    npvOutput$management[npvOutput$management == 1] = "SQ"
    npvOutput$management[npvOutput$management == 2] = "FMSY"
    npvOutput$management[npvOutput$management == 3] = "minRec"
    npvOutput$management[npvOutput$management == 4] = "econOpt"
    npvOutput$management[npvOutput$management == 5] = "close"
    npvOutput$management[npvOutput$management == 6] = "openA"
    npvOutput$catchShare[npvOutput$catchShare == 1] = "no_CS"
    npvOutput$catchShare[npvOutput$catchShare == 2] = "CS"
    npvOutput$illegalFishing[npvOutput$illegalFishing == 1] = "illegal_fishing"
    npvOutput$illegalFishing[npvOutput$illegalFishing == 2] = "no_illegal_fishing"

}


masterOutput$implementYear = (masterOutput$implementYear + 1)
#write.csv(masterOutput,file="output/japan_20190118_mo_opt.csv")


recoveryOutput$implementYear = (recoveryOutput$implementYear + 1)
#write.csv(recoveryOutput,file="output/japan_20190118_recovery_opt.csv")

npvOutput$implementYear = (npvOutput$implementYear + 1)
#write.csv(npvOutput,file="output/japan_20190118_npv_opt.csv")

## opt at the end of csv names indicates results from model with policy that is optimized once at beginning


##########################PLOTTING!!!
#masterOutput<-read.csv("output/japan_20190118_mo_opt.csv", header=T, stringsAsFactors = F)
#recoveryOutput<-read.csv("output/japan_20190118_recovery_opt.csv", header=T, stringsAsFactors = F)
#npvOutput<-read.csv("output/japan_20190118_npv_opt.csv", header=T, stringsAsFactors = F)

SnamesAll = c("BAU","Fmsy","Econ Optimal")
legendLabels = SnamesAll[scenarios]
legendName = "Management Scenario"
# legendName1 = "Fishery"
# legendName2 = "Fishery (Coastal sp.)"
# legendName3 = "Fishery (Other sp.)"

###KK: problem here was that the variable is "fishery" not species that you want to aggregate on:
masterP<-data.frame(masterOutput)
#Fleet 1 = state (gets SQ for new BAU scenario):
head(masterP)
masterPLongH1<- dcast(masterP, fishery+species+catchShare+illegalFishing+time~management,
                      value.var = "harvest1")
masterPLongH1$fleet<-"State"
names(masterPLongH1)<-c("fishery", "species", "catchShare","illegalFishing","time","close1","econOpt1","FMSY1","minRec1","openA1","SQ1", "fleet")
#Fleet 2 = private (gets OA for new BAU scenario):
masterPLongH2<- dcast(masterP, fishery+species+catchShare+illegalFishing+time~management,
                      value.var = "harvest2")
masterPLongH2$fleet<-"Private"
names(masterPLongH2)<-c("fishery", "species","catchShare","illegalFishing","time","close2","econOpt2","FMSY2","minRec2","openA2","SQ2", "fleet")
#Fleet 3 = illegal (gets OA for new BAU scenario):
masterPLongHill<- dcast(masterP, fishery+species+catchShare+illegalFishing+time~management,
                      value.var = "harvest_ill_for")
masterPLongHill$fleet<-"Illegal"
names(masterPLongHill)<-c("fishery", "species","catchShare","illegalFishing","time","close3","econOpt3","FMSY3","minRec3","openA3","SQ3", "fleet")

masterPLong<-merge(masterPLongH1, masterPLongH2, by=c("fishery", "species","catchShare","illegalFishing","time"))
masterPLong<-merge(masterPLong, masterPLongHill, by=c("fishery", "species","catchShare","illegalFishing","time"))
masterPLong$BAU<-masterPLong$SQ1+masterPLong$SQ2+masterPLong$SQ3
masterPLong$FMSY<-masterPLong$FMSY1+masterPLong$FMSY2+masterPLong$FMSY3
masterPLong$econOpt<-masterPLong$econOpt1+masterPLong$econOpt2+masterPLong$econOpt3
names(masterPLong)
masterPLong<-masterPLong[,c(-6:-25)]
masterPWide_H<-melt(masterPLong, measure.vars = c("BAU", "FMSY", "econOpt"),
                    variable.name="management", value.name = "harvest")


mPH1<-masterPLongH1[,c(1:5, 7:8, 11:12)]
names(mPH1)<-c("fishery", "species","catchShare","illegalFishing","time","econOpt","FMSY","BAU", "fleet")
mPH2<-masterPLongH2[,c(1:5, 7:8, 11:12)]
names(mPH2)<-c("fishery", "species","catchShare","illegalFishing","time","econOpt","FMSY","BAU", "fleet")
mPH3<-masterPLongHill[,c(1:5, 7:8, 11:12)]
names(mPH3)<-c("fishery", "species","catchShare","illegalFishing","time","econOpt","FMSY","BAU", "fleet")
mPHx<-rbind(mPH1, mPH2, mPH3)

mPHxx<-melt(mPHx, measure.vars = c("BAU", "FMSY", "econOpt"),
            variable.name="management", value.name = "harvest")
mPHxx$Situation<-paste(mPHxx$catchShare,"_", mPHxx$illegalFishing, sep="")
mPHxx$Situation2<-ifelse(mPHxx$Situation=="no_CS_illegal_fishing", "Worst", ifelse(mPHxx$Situation=="CS_no_illegal_fishing", "Best", "Blah"))
mPHxx<-mPHxx[which(mPHxx$Situation2!="Blah"),]
mPHxx$ill<-ifelse(mPHxx$illegalFishing=="no_illegal_fishing", "No illegal", "Illegal")
mPHxx$CS<-ifelse(mPHxx$catchShare=="no_CS", "No CS", "CS")

#Fleet 1 = state:
masterPLongP1<- dcast(masterP, fishery+species+catchShare+illegalFishing+time~management,
                      value.var = "profit1")
masterPLongP1$fleet<-"State"
names(masterPLongP1)<-c("fishery", "species","catchShare","illegalFishing","time","close1","econOpt1","FMSY1","minRec1","openA1","SQ1", "fleet")
#Fleet 2 = private (gets OA for new BAU scenario):
masterPLongP2<- dcast(masterP, fishery+species+catchShare+illegalFishing+time~management,
                      value.var = "profit2")
masterPLongP2$fleet<-"Private"
names(masterPLongP2)<-c("fishery", "species","catchShare","illegalFishing","time","close2","econOpt2","FMSY2","minRec2","openA2","SQ2", "fleet")

masterPLong_P<-merge(masterPLongP1, masterPLongP2, by=c("fishery", "species","catchShare","illegalFishing","time"))
masterPLong_P$BAU<-masterPLong_P$SQ1+masterPLong_P$SQ2
masterPLong_P$FMSY<-masterPLong_P$FMSY1+masterPLong_P$FMSY2
masterPLong_P$econOpt<-masterPLong_P$econOpt1+masterPLong_P$econOpt2
names(masterPLong_P)
masterPLong_P<-masterPLong_P[,c(-6:-19)]
masterPWide_P<-melt(masterPLong_P, measure.vars = c("BAU", "FMSY", "econOpt"),
                    variable.name="management", value.name = "profit")
names(masterPLongP1)
mPP1<-masterPLongP1[,c(1:5, 7:8, 11:12)]
names(mPP1)<-c("fishery", "species","catchShare","illegalFishing","time","econOpt","FMSY","BAU", "fleet")
mPP2<-masterPLongP2[,c(1:5, 7:8, 11:12)]
names(mPP2)<-c("fishery", "species","catchShare","illegalFishing","time","econOpt","FMSY","BAU", "fleet")
mPPx<-rbind(mPP1, mPP2)
mPPxx<-melt(mPPx, measure.vars = c("BAU", "FMSY", "econOpt"),
            variable.name="management", value.name = "profit")
mPPxx$Situation<-paste(mPPxx$catchShare,"_", mPPxx$illegalFishing, sep="")
mPPxx$Situation2<-ifelse(mPPxx$Situation=="no_CS_illegal_fishing", "Worst", ifelse(mPPxx$Situation=="CS_no_illegal_fishing", "Best", "Blah"))
mPPxx<-mPPxx[which(mPPxx$Situation2!="Blah"),]
mPPxx$ill<-ifelse(mPPxx$illegalFishing=="no_illegal_fishing", "No illegal", "Illegal")
mPPxx$CS<-ifelse(mPPxx$catchShare=="no_CS", "No CS", "CS")


masterPsub<-masterP[masterP$management %in% c("SQ", "FMSY", "econOpt"),]
masterPsub$management<-ifelse(masterPsub$management=="SQ", "BAU", masterPsub$management)
masterPsub<-merge(masterPsub, masterPWide_H, by=c("fishery", "species","catchShare","illegalFishing","time", "management"))
masterPsub<-merge(masterPsub, masterPWide_P, by=c("fishery", "species","catchShare","illegalFishing","time", "management"))

masterPsub$Situation<-paste(masterPsub$catchShare,"_", masterPsub$illegalFishing, sep="")
masterPsub$Situation2<-ifelse(masterPsub$Situation=="no_CS_illegal_fishing", "Worst", ifelse(masterPsub$Situation=="CS_no_illegal_fishing", "Best", "Blah"))
masterPsub<-masterPsub[which(masterPsub$Situation2!="Blah"),]
masterPsub$ill<-ifelse(masterPsub$illegalFishing=="no_illegal_fishing", "No illegal", "Illegal")
masterPsub$CS<-ifelse(masterPsub$catchShare=="no_CS", "No CS", "CS")

#追加
#write.csv(masterPsub,file = "ResultUpsideJPN20200917.csv")

npvP<-data.frame(npvOutput)
#Fleet 1 = state (gets SQ for new BAU scenario):
npvP1<- dcast(npvP, fishery+species+catchShare+illegalFishing~management,
                      value.var = "npv1")
names(npvP1)<-c("fishery", "species","catchShare","illegalFishing","close1","econOpt1","FMSY1","minRec1","openA1","SQ1")
#Fleet 2 = private (gets OA for new BAU scenario):
npvP2<- dcast(npvP, fishery+species+catchShare+illegalFishing~management,
                      value.var = "npv2")
names(npvP2)<-c("fishery", "species","catchShare","illegalFishing","close2","econOpt2","FMSY2","minRec2","openA2","SQ2")

npvP3<-merge(npvP1, npvP2, by=c("fishery", "species","catchShare","illegalFishing"))
npvP3$BAU<-npvP3$SQ1+npvP3$SQ2
npvP3$FMSY<-npvP3$FMSY1+npvP3$FMSY2
npvP3$econOpt<-npvP3$econOpt1+npvP3$econOpt2
names(npvP3)
npvP3<-npvP3[,c(-5:-16)]
npvP3Wide<-melt(npvP3, measure.vars = c("BAU", "FMSY", "econOpt"),
                    variable.name="management", value.name = "npv")

npvP3Wide$Situation<-paste(npvP3Wide$catchShare,"_", npvP3Wide$illegalFishing, sep="")
npvP3Wide$Situation2<-ifelse(npvP3Wide$Situation=="no_CS_illegal_fishing", "Worst", ifelse(npvP3Wide$Situation=="CS_no_illegal_fishing", "Best", "Blah"))
npvP3Wide<-npvP3Wide[which(npvP3Wide$Situation2!="Blah"),]



##########KK: PLOTTING CODE:
library("RColorBrewer"); library(directlabels); library(ggrepel)
names(masterPsub)

Totals<-masterPsub%>%
  group_by(time, management)%>%
  summarise(biomass=sum(biomass), harvest=sum(harvest), profit=sum(profit))%>%
  mutate(manag = if_else(management=="econOpt", "Econ. opt.", management))

setwd("~/Dropbox/EDFUPSIDE2018/JapanUpside_2018")
write.csv(Totals, file = "output/Results_FishDisc0727/TotalsBAU.csv", row.names=FALSE)

##KK: the following code makes the Costello plot--I simplified so that it does't 
pdf("output/Results_FishDisc0727(phi=0.188)_Final/CostelloPlot_Disc0727(phi=0.188).pdf",width=6,height=4,onefile=T) 
  Totals1<-Totals
  check<-ggplot(Totals1, aes(time, biomass, size=harvest, fill=profit, alpha=0.2), colour="black") + 
    geom_point(shape=21) + 
    scale_fill_gradient(name="Profit [USD]", 
                        low = "red", high = "blue",
                        na.value = "grey50", guide = "colourbar")+
    scale_size(name="Harvest [MT]")+
    scale_alpha(guide=F) +
    geom_dl(aes(label = manag), method = "last.qp", cex = 0.9) +
    xlim(c(0,60)) +
    xlab("Time") +
    ylab("Biomass [MT]") +
    ggtitle("") +
    theme_bw()
  print(check)
  dev.off()

##KK: don't really need the "next"CostelloPlot_combo" plot as this is used only if you have different scenarios you are comparing.  
##KK: I deleted the CostelloPlot_combo code.

fisheryNames <- unique(as.factor(masterOutput$fishery))
#fisheryNames<-rep(fisheryNames, 2)
fisheryNames<-fisheryNames[order(fisheryNames)]
lineSize <- 1.5
## Grab legend from ggplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}


i=NULL
TIME=50
 i= fisheryNames[16]
unique(masterPsub$fishery)

for (i in fisheryNames[9:95]){
    datadf <- masterPsub %>%
      filter(fishery == i)
    FishTitle<-datadf$fishery[1]
    ###Make Kobe plot with density and arrows from BaseYear to end year by management scenario:
    data1<-subset(datadf, subset=time==1)
    data2<-subset(datadf, subset=time==TIME)
    data1$FvFMSYtotal[data1$FvFMSYtotal>4]<- 4
    data1$BvBMSY[data1$BvBMSY>2.5]<- 2.5
    data2$FvFMSYtotal[data2$FvFMSYtotal>4]<- 4
    data2$BvBMSY[data2$BvBMSY>2.5]<- 2.5
    data1<-data1[c(-1:-2),]
    BaseEnd<-rbind(data1, data2)
    BaseEnd<-data.frame(BaseEnd)
    BaseEnd$management<-ifelse(BaseEnd$time==1, "BaseYear", BaseEnd$management)
    ENDS<-NULL
    for (k in 2:4){
      EndVal<-ifelse(BaseEnd[1,16]>BaseEnd[k, 16], "first", "last")
      ENDS<-rbind(ENDS, EndVal)
    }
    #"#D55E00"= red (BAU),  "#E69F00" = gold (econOpt), "#0072B2"= blue (FMSY)
    kobe<-ggplot(BaseEnd, aes(BvBMSY, FvFMSYtotal), col=management, shape=Year) +
      stat_density_2d(aes(fill = ..density..), geom = "tile", n = 100, alpha = 1, contour = F) +
      scale_fill_gradient2(guide = F, low = "lightskyblue",mid = "white", high = "gold", midpoint = 0.5) +
      geom_point(shape=c(1, 24, 24, 24), size = 6, alpha=0.75) +
      geom_point(data=BaseEnd,shape = c(1, 24, 24, 24), size = 6, fill = c("black", "#D55E00", "#E69F00",  "#0072B2" ), alpha = 0.75) +
      geom_line(data=BaseEnd[c(1, 2),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[1], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
      geom_line(data=BaseEnd[c(1, 3),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[2], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
      geom_line(data=BaseEnd[c(1, 4),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[3], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
      scale_x_continuous(limits = c(-1, 4), breaks = seq(-1,4, by = 0.5), labels = c(seq(-1, 2, by = 0.5), expression(phantom(x) >= 2.5), seq(3,4, by = 0.5))) +
      scale_y_continuous(limits = c(-1,6), breaks = seq(-1, 6, by = 0.5), labels = c(seq(-1,3.5, by = 0.5), expression(phantom(x) >= 4), seq(4.5,6, by = 0.5))) +
      coord_cartesian(xlim = c(0, 2.5), ylim = c(0,4)) +
      geom_hline(aes(yintercept = 1), linetype = "longdash") +
      geom_vline(aes(xintercept = 1), linetype = "longdash") +
      labs(x=expression(B/B[msy]), y=expression(F/F[msy]))+
      theme(legend.position="none")+
      ggtitle(FishTitle)
    
    ########Make biomass, harvest, profit time series plots:
    datadf2 <- datadf %>%
      group_by(management, time) %>%
      summarise(meanb = mean(biomass),
                sdb = sd(biomass),
                meanh = mean(harvest),
                sdh = sd(harvest),
                meanp = mean(profit),
                sdp = sd(profit),
                meanBvBMSY = mean(BvBMSY),
                sdBvBMSY = sd(BvBMSY),
                meanFvFMSYtotal = mean(FvFMSYtotal),
                sdFvFMSYtotal = sd(FvFMSYtotal))
    ### Biomass plot:
    #"#D55E00"= red (BAU),  "#E69F00" = gold (econOpt), "#0072B2"= blue (FMSY)
    bplot = ggplot(datadf2, aes(x = time, y = meanb, group = management, colour = management, fill = management)) +
      # geom_ribbon(aes(ymin = meanb - 2 * sdb, ymax = meanb + 2 * sdb)) +
      geom_line(size = lineSize) +
      geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
      xlab("Time") +
      ylab("Biomass [MT]") +
      scale_colour_manual(name=legendName, 
                          labels = c("BAU" = "Business As Usual",
                                     "FMSY" = "FMSY", 
                                     "econOpt" = "Economically Optimal"),
                          values = c("BAU" = "#D55E00",
                                     "FMSY" = "#0072B2",
                                     "econOpt" = "#E69F00")) +
      theme_bw() +
      theme(legend.position="none")
    
    
    ### Harvest plot:
    hplot1 = ggplot(datadf2, aes(x = time, y = meanh, group = management, colour = management, fill = management)) +
      # geom_ribbon(aes(ymin = meanb - 2 * sdb, ymax = meanb + 2 * sdb)) +
      geom_line(size = lineSize) +
      geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
      xlab("Time") +
      ylab("Harvest [MT]") +
      scale_colour_manual(name=legendName, 
                          labels = c("BAU" = "Business As Usual",
                                     "FMSY" = "FMSY", 
                                     "econOpt" = "Economically Optimal"),
                          values = c("BAU" = "#D55E00",
                                     "FMSY" = "#0072B2",
                                     "econOpt" = "#E69F00")) +
      theme_bw() +
      theme(legend.position="none")
    
    
    
    ### Profit plot:
    pplot1 = ggplot(datadf2, aes(x = time, y = meanp, group = management, colour = management, fill = management)) +
      # geom_ribbon(aes(ymin = meanb - 2 * sdb, ymax = meanb + 2 * sdb)) +
      geom_line(size = lineSize) +
      geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
      xlab("Time") +
      ylab("Profit [JPY]") +
      scale_colour_manual(name=legendName, 
                          labels = c("BAU" = "Business As Usual",
                                     "FMSY" = "FMSY", 
                                     "econOpt" = "Economically Optimal"),
                          values = c("BAU" = "#D55E00",
                                     "FMSY" = "#0072B2",
                                     "econOpt" = "#E69F00")) +
      theme_bw() +
      theme(legend.position="none")
    
    
    ### NPV plot
    npvdf <- npvP3Wide %>%
      filter(fishery == i) 
      
    npvplot = ggplot(npvdf, aes(x = reorder(management, npv), y = npv, fill = management)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
      xlab("Management Scenario") +
      ylab("Total NPV [JPY]") +
      scale_fill_manual(name="", 
                        labels = c("FMSY" = "FMSY",
                                   "BAU" = "Business As Usual", 
                                   "econOpt" = "Economically Optimal" 
                        ),
                        values = c("BAU" = "#D55E00",
                                   "FMSY" = "#0072B2",
                                   "econOpt" = "#E69F00")) +
      theme_bw() +
      theme(legend.position="bottom", legend.text = element_text(size = 7)) + guides(fill = guide_legend(nrow = 2, byrow = T))
    
    
    colorLineLegend = ggplot(datadf, aes(x = time, y = harvest, colour = management, fill = management, lty = fleet)) +
      # geom_ribbon(aes(ymin = meanh - 2 * sdh, ymax = meanh + 2 * sdh)) +
      geom_line(size = lineSize) +
      geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
      xlab("Time") +
      ylab("Harvest [MT]") +
      # scale_fill_manual(guide = "none") +
      scale_colour_manual(guide = F, name=legendName, 
                          labels = c("FMSY" = "FMSY",
                                     "BAU" = "Business As Usual", 
                                     "econOpt" = "Economically Optimal" 
                          ),
                          values = c("BAU" = "#D55E00",
                                     "FMSY" = "#0072B2",
                                     "econOpt" = "#E69F00")) +
      scale_linetype_manual(values = c("State" = 1, 
                                       "Private" = 2, 
                                       "Illegal" = 3),
                            labels = c("State" = "State",
                                       "Private" = "Private",
                                       "Illegal" = "Illegal")) +
      theme_bw() +
      theme(legend.key = element_rect(colour = "transparent", fill = "white"),
            legend.key.width = unit(5, "line"))
    
    
    leg<-g_legend(colorLineLegend)

    gs<-list(kobe,
             npvplot,
             bplot,
             hplot1,
             pplot1)
    lay <- rbind(c(1,1,2),
                 c(3,4,5))
    GL<-grid.arrange(grobs = gs, layout_matrix = lay)

    ggsave(file=paste("output/Results_FishDisc0727/", FishTitle, "Disc0727.pdf"), GL, height=7,width=12)
    print(GL)
}
dev.off()

###############################Management scenarios:
## NOTE: these are set up for the 9 species run that I did. They are too messy for more species than that.
# setwd('C:/Users/kkleisner/Documents/ContractWork/EDF/Upside/Cuba/Feb2018_Romina/Output/Results_MangDisc10_Rom')
# 
# hcrNames <- as.vector(unique(masterPsub$management))
# phi <- 0.188
# maxF <- (phi + 1) / phi
# maxb <- (phi + 1) ^ (1 / phi)
# q="econOpt"
# j="best"
# 
# for (q in hcrNames){
#   for (j in Sit) {
#     
#     hcrdatadf <- masterPsub %>%
#       filter(management == q,
#              Situation2 == j)
#     
#     hcrdatadf2<-hcrdatadf%>%
#       group_by(species,catchShare,illegalFishing,management,fishery )%>%
#       mutate(relbio = biomass/max(biomass))
#     MangTitle<-hcrdatadf$management[1]
#     CS<-hcrdatadf$CS[1]
#     CS<-ifelse(CS=="CS", "Economic incentives", "No economic incentives")
#     ill<-hcrdatadf$ill[1]
#     ill<-ifelse(ill=="No illegal", "no illegal fishing", "with illegal fishing")
#     Situ<-hcrdatadf$Situation2[1]
# 
#     bplotHCR = ggplot(hcrdatadf2, aes(x = time, y = relbio, group = fishery, colour=fishery, fill=fishery), size=lineSize) +
#       geom_line(size = lineSize) +
#       geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
#       geom_hline(yintercept=0.8, lty=2, colour="black", size=0.5, alpha=0.8) +
#       xlab("Time") +
#       ylab("Relative Biomass [MT]") +
#       scale_colour_manual(name="Fishery",
#                           labels = c("Biajaiba Zona-A" = "Biajaiba",
#                                      "Chicharro Zona-A" = "Chicharro",
#                                      "Liseta Zona-A" = "Liseta",
#                                      "Machuelo Zona-A" = "Machuelo",
#                                      "Mojarra Zona-A" = "Mojarra",
#                                      "Pargo Zona-A" = "Pargo",
#                                      "Ronco Zona-A" = "Ronco",
#                                      "Sabalo Zona-A" = "Sabalo",
#                                      "Sierra Zona-A" = "Sierra"),
#                           values = c("Biajaiba Zona-A" = "#E69F00",
#                                      "Chicharro Zona-A" = "#CC79A7",
#                                      "Liseta Zona-A" = "#669999",
#                                      "Machuelo Zona-A" = "#009E73",
#                                      "Mojarra Zona-A" = "#99ffcc",
#                                      "Pargo Zona-A" = "#cc0066",
#                                      "Ronco Zona-A" = "#cc33ff",
#                                      "Sabalo Zona-A" = "#56B4E9",
#                                      "Sierra Zona-A" = "#996600")) +
#       theme_bw() +
#       theme(legend.position="none")
#     
#     ### NPV plot
#     
#     hcrnpvdf <- npvP3Wide %>%
#       filter(management == q,
#              Situation2 == j) 
#     
#     npvplothcr = ggplot(hcrnpvdf, (aes(x = reorder(species, npv), y = npv, fill = species))) +
#       geom_bar(stat = "identity") +
#       geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
#       xlab("Fishery") +
#       ylab("Total NPV [USD]") +
#       scale_fill_manual(name="Fishery", 
#                         values = c("Biajaiba" = "#E69F00",
#                                    "Chicharro" = "#CC79A7",
#                                    "Liseta" = "#669999",
#                                    "Machuelo" = "#009E73",
#                                    "Mojarra" = "#99ffcc",
#                                    "Pargo" = "#cc0066",
#                                    "Ronco" = "#cc33ff",
#                                    "Sabalo" = "#56B4E9",
#                                    "Sierra" = "#996600")) +
#       theme(legend.position="bottom",
#             axis.text.x = element_text(angle = 45, hjust = 1),
#             axis.title.x=element_blank(),
#             legend.text = element_text(size = 7)) + guides(fill = guide_legend(nrow = 3, byrow = T))
#     
#     
#     ## Profit plot
#     
#     hcrdatadf2<-hcrdatadf%>%
#       group_by(species,catchShare,illegalFishing,management,fishery )%>%
#       mutate(relprof = profit/max(profit))
#     
#     ## 1
#     pplot1hcr = ggplot(hcrdatadf2, aes(x = time, y = relprof, colour = fishery, fill = fishery)
#                        , size = lineSize) +
#       geom_line(size = lineSize) +
#       # geom_ribbon(aes(ymin = meanp - 2 * sdp, ymax = meanp + 2 * sdp)) +
#       geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
#       xlab("Time") +
#       ylab(" Relative Profit [USD]") +
#       scale_colour_manual(name=legendName1,
#                           labels = c("Biajaiba Zona-A" = "Biajaiba",
#                                      "Chicharro Zona-A" = "Chicharro",
#                                      "Liseta Zona-A" = "Liseta",
#                                      "Machuelo Zona-A" = "Machuelo",
#                                      "Mojarra Zona-A" = "Mojarra",
#                                      "Pargo Zona-A" = "Pargo",
#                                      "Ronco Zona-A" = "Ronco",
#                                      "Sabalo Zona-A" = "Sabalo",
#                                      "Sierra Zona-A" = "Sierra"),
#                           values = c("Biajaiba Zona-A" = "#E69F00",
#                                      "Chicharro Zona-A" = "#CC79A7",
#                                      "Liseta Zona-A" = "#669999",
#                                      "Machuelo Zona-A" = "#009E73",
#                                      "Mojarra Zona-A" = "#99ffcc",
#                                      "Pargo Zona-A" = "#cc0066",
#                                      "Ronco Zona-A" = "#cc33ff",
#                                      "Sabalo Zona-A" = "#56B4E9",
#                                      "Sierra Zona-A" = "#996600")) +
#       theme_bw() +
#       theme(legend.position="none")
#     
#     
#     ### Harvest plot
#     
#     hcrdatadf2<-hcrdatadf%>%
#       group_by(species,catchShare,illegalFishing,management,fishery )%>%
#       mutate(relharv = harvest/max(harvest))
#     
#     
#     hplot1hcr =  ggplot(hcrdatadf2, aes(x = time, y = relharv, colour = fishery, fill = fishery), size = lineSize) +
#       geom_line(size = lineSize) +
#       geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
#       xlab("Time") +
#       ylab("Relative Harvest [MT]") +
#       theme_bw() +
#       scale_colour_manual(name=legendName1,
#                           labels = c("Biajaiba Zona-A" = "Biajaiba",
#                                      "Chicharro Zona-A" = "Chicharro",
#                                      "Liseta Zona-A" = "Liseta",
#                                      "Machuelo Zona-A" = "Machuelo",
#                                      "Mojarra Zona-A" = "Mojarra",
#                                      "Pargo Zona-A" = "Pargo",
#                                      "Ronco Zona-A" = "Ronco",
#                                      "Sabalo Zona-A" = "Sabalo",
#                                      "Sierra Zona-A" = "Sierra"),
#                           values = c("Biajaiba Zona-A" = "#E69F00",
#                                      "Chicharro Zona-A" = "#CC79A7",
#                                      "Liseta Zona-A" = "#669999",
#                                      "Machuelo Zona-A" = "#009E73",
#                                      "Mojarra Zona-A" = "#99ffcc",
#                                      "Pargo Zona-A" = "#cc0066",
#                                      "Ronco Zona-A" = "#cc33ff",
#                                      "Sabalo Zona-A" = "#56B4E9",
#                                      "Sierra Zona-A" = "#996600")) +
#       theme(legend.position="none")
#     
#     
#     ###Make Kobe plot with density and arrows from BaseYear to end year by management scenario:
#     data1<-subset(hcrdatadf, subset=time==1)
#     data2<-subset(hcrdatadf, subset=time==TIME)
#     data1$FvFMSYtotal[data1$FvFMSYtotal>4]<- 4
#     data1$BvBMSY[data1$BvBMSY>2.5]<- 2.5
#     data2$FvFMSYtotal[data2$FvFMSYtotal>4]<- 4
#     data2$BvBMSY[data2$BvBMSY>2.5]<- 2.5
#     #data1<-data1[c(-1:-8),]
#     BaseEnd<-rbind(data1, data2)
#     BaseEnd<-data.frame(BaseEnd)
#     ENDS<-NULL
#     FISH<-unique(fisheryNames)
#     k="Biajaiba Zona-A"
#     for (k in FISH){
#       EndVal<-BaseEnd[which(BaseEnd$fishery==k),]
#       EndVal2<-ifelse(EndVal[1,16]>EndVal[2, 16], "first", "last")
#       ENDS<-rbind(ENDS, EndVal2)
#     }
#     kobeHCR<-ggplot(BaseEnd, aes(BvBMSY, FvFMSYtotal), col=fishery, shape=as.factor(time)) +
#       geom_point()+
#       stat_density_2d(aes(fill = ..density..), geom = "tile", n = 100, alpha = 1, contour = F) +
#       scale_fill_gradient2(guide = F, low = "lightskyblue",mid = "white", high = "gold", midpoint = 0.5) +
#       
#       geom_point(data=BaseEnd,shape = c(rep(21,9), rep(24, 9)), size = 6, 
#                  fill = c("#E69F00","#CC79A7", "#669999", "#009E73", "#99ffcc", "#cc0066","#cc33ff", "#56B4E9", "#996600", "#E69F00","#CC79A7", "#669999","#009E73", "#99ffcc", "#cc0066", "#cc33ff", "#56B4E9", "#996600"), alpha = 0.75) +
#       geom_line(data=BaseEnd[c(1, 10),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[1], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
#       geom_line(data=BaseEnd[c(2, 11),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[2], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
#       geom_line(data=BaseEnd[c(3, 12),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[3], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
#       geom_line(data=BaseEnd[c(4, 13),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[4], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
#       geom_line(data=BaseEnd[c(5, 14),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[5], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
#       geom_line(data=BaseEnd[c(6, 15),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[6], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
#       geom_line(data=BaseEnd[c(7, 16),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[7], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
#       geom_line(data=BaseEnd[c(8, 17),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[8], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
#       geom_line(data=BaseEnd[c(9, 18),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[9], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
#       scale_x_continuous(limits = c(-1, 4), breaks = seq(-1,4, by = 0.5), labels = c(seq(-1, 2, by = 0.5), expression(phantom(x) >= 2.5), seq(3,4, by = 0.5))) +
#       scale_y_continuous(limits = c(-1,6), breaks = seq(-1, 6, by = 0.5), labels = c(seq(-1,3.5, by = 0.5), expression(phantom(x) >= 4), seq(4.5,6, by = 0.5))) +
#       coord_cartesian(xlim = c(0, 2.5), ylim = c(0,4)) +
#       geom_hline(aes(yintercept = 1), linetype = "longdash") +
#       geom_vline(aes(xintercept = 1), linetype = "longdash") +
#       labs(x=expression(B/B[msy]), y=expression(F/F[msy]))+
#       theme(legend.position="none")+
#       ggtitle(paste(MangTitle, ": ", CS, " & ", ill, sep=""))
#     
#     
#     gs<-list(kobeHCR,
#              npvplothcr,
#              bplotHCR,
#              hplot1hcr,
#              pplot1hcr)
#     lay <- rbind(c(1,1,2),
#                  c(3,4,5))
#     GL<-grid.arrange(grobs = gs, layout_matrix = lay)
#     
#     PDFname<-paste(MangTitle, "_", Situ, sep="")
#     ggsave(file=paste(PDFname, "025.pdf"), GL, height=7,width=12)
#     print(GL)
#   }
#   
# }
# dev.off()