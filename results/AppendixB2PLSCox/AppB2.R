{
  ###
library(survival)
library(survAUC)
library(pls)

source("../../functions/predict.pls.cox.R")
source("../../functions/model.pls.cv.cox.R")
source("../../functions/model.cox.by.var.R")

## time vs DR

####  2PLOTS
source("../plot3.auc.pe.R")

####  FUNCTION
source("AppB2.loop.fit3.R")


I <-5

}

load("../../datasets/gene_test.RData")

{
#0.2cens
##G1
DT <- cbind.data.frame(ydata.2,gene2[,1:300])
times <- 1:max(DT$years)
max.comp <-8 #round(nrow(DT)/25)
LOOP3(file.name = "Comp0_G1_0.2.RData",I=I, max.comp = max.comp, times=times)
p1 <- plot3.auc.pe("Comp0_G1_0.2.RData", times = times, data.name = "Gene1 with 17% censoring")

##G2

DT <- cbind.data.frame(ydata.2,gene2)
times <- 1:max(DT$years)
max.comp <-5 #round(nrow(DT)/25)
LOOP3(file.name = "Comp0_G2_0.2.RData",I=I, max.comp = max.comp, times=times)
p2 <- plot3.auc.pe("Comp0_G2_0.2.RData", times = times, data.name = "Gene2 with 17% censoring")

#0.5cens
##G1
DT <- cbind.data.frame(ydata.5,gene2[,1:300])
times <- 1:max(DT$years)
max.comp <-8 #round(nrow(DT)/25)
LOOP3(file.name = "Comp0_G1_0.5.RData",I=I, max.comp = max.comp, times=times)
p3 <- plot3.auc.pe("Comp0_G1_0.5.RData", times = times, data.name = "Gene1 with 48% censoring")

##G2

DT <- cbind.data.frame(ydata.5,gene2)
times <- 1:max(DT$years)
max.comp <-5 #round(nrow(DT)/25)
LOOP3(file.name = "Comp0_G2_0.5.RData",I=I, max.comp = max.comp, times=times)
p4 <- plot3.auc.pe("Comp0_G2_0.5.RData", times = times, data.name = "Gene2 with 48% censoring")

#0.7cens
##G1

DT <- cbind.data.frame(ydata.7,gene2[,1:300])
times <- 1:max(DT$years)
max.comp <-8 #round(nrow(DT)/25)
LOOP3(file.name = "Comp0_G1_0.7.RData",I=I, max.comp = max.comp, times=times)
p5 <- plot3.auc.pe("Comp0_G1_0.7.RData", times = times, data.name = "Gene1 with 67% censoring")

##G2

DT <- cbind.data.frame(ydata.7,gene2)
times <- 1:max(DT$years)
max.comp <-5 #round(nrow(DT)/25)
LOOP3(file.name = "Comp0_G2_0.7.RData",I=I, max.comp = max.comp, times=times)
p6 <- plot3.auc.pe("Comp0_G2_0.7.RData", times = times, data.name = "Gene2 with 67% censoring")
}

{
  ## DLBCL
  load("../../datasets/1DLBCL.RData")
  DT<- DLBCL
  times <- 1:max(DT$years)
  max.comp <- 7 #round(nrow(DT)/25)
  LOOP3(file.name = "Comp0_DLBCL.RData", I=I, max.comp = max.comp, times=times)
  p7 <- plot3.auc.pe("Comp0_DLBCL.RData", times = times,  data.name = "DLBCL with 41% censoring")

  ## LUNG
  load("../../datasets/1LUNG.RData")
  DT<- LUNG
  times <- 1:max(DT$years)
  max.comp <- 5 #round(nrow(DT)/25)
  LOOP3(file.name = "Comp0_LUNG.RData", I=I, max.comp = max.comp, times=times)
  p8 <- plot3.auc.pe("Comp0_LUNG.RData", times = times,  data.name = "LUNG with 42% censoring")
}
  
{
  ##NKI
  load("../../datasets/2NKI.RData")
  DT <- NKI
  times <- 1:max(DT$years)
  max.comp <- 13 #round(nrow(DT)/25)
  LOOP3(file.name = "Comp0_NKI.RData",I=I, max.comp = max.comp, times=times)
  p9 <- plot3.auc.pe("Comp0_NKI.RData", times = times, data.name = "NKI with 66% censoring")
  
}
