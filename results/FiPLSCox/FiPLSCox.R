{
library(survival)
library(survAUC)
library(pls)


source("../../functions/predict.pls.cox.R")
source("../../functions/model.pls.cv.cox.R")
source("../../functions/model.fi.pls.cox.R")



#### 4PLOT
source("../plot5.auc.pe.R")

#### FUNCTIONS

source("FiPLSCox.loop.fit5.R")

I<-5
}

#0.2cens
##G1
load("../../datasets/gene_test.RData")
DT <- cbind.data.frame(ydata.2,gene2[,1:300])
times <- 1:max(DT$years)
max.comp1 <- 8
max.comp  <- 8
LOOP5(file.name = "Comp3_G1_0.2.RData",I=I, max.comp1=max.comp1, max.comp = max.comp, times=times, ncut.var = 30)
p1 <- plot5.auc.pe("Comp3_G1_0.2.RData", times = times, data.name = "Gene1 with 20% censoring")


#G2
DT <- cbind.data.frame(ydata.2,gene2)
times <- 1:max(DT$years)
max.comp1 <- 5
max.comp  <- 5
LOOP5(file.name = "Comp3_G2_0.2.RData",I=I, max.comp1=max.comp1,max.comp = max.comp, times=times, ncut.var = 60)
p2 <- plot5.auc.pe("Comp3_G2_0.2.RData", times = times, data.name = "Gene2 with 20% censoring")

#0.5cens
##G1

DT <- cbind.data.frame(ydata.5,gene2[,1:300])
times <- 1:max(DT$years)
max.comp1 <- 8
max.comp  <- 8
LOOP5(file.name = "Comp3_G1_0.5.RData",I=I, max.comp1=max.comp1,max.comp = max.comp, times=times, ncut.var = 30)
p3 <- plot5.auc.pe("Comp3_G1_0.5.RData", times = times, data.name = "Gene1 with 50% censoring")

#G2

DT <- cbind.data.frame(ydata.5,gene2)
times <- 1:max(DT$years)
max.comp1 <- 5
max.comp  <- 5
LOOP5(file.name = "Comp3_G2_0.5.RData",I=I, max.comp1=max.comp1,max.comp = max.comp, times=times, ncut.var = 60)
p4 <- plot5.auc.pe("Comp3_G2_0.5.RData", times = times, data.name = "Gene2 with 50% censoring")

#0.7cens
##G1

DT <- cbind.data.frame(ydata.7,gene2[,1:300])
times <- 1:max(DT$years)
max.comp1 <- 8
max.comp  <- 8
LOOP5(file.name = "Comp3_G1_0.7.RData",I=I, max.comp1=max.comp1,max.comp = max.comp, times=times, ncut.var = 30)
p5 <- plot5.auc.pe("Comp3_G1_0.7.RData", times = times, data.name = "Gene1 with 70% censoring")

#G2

DT <- cbind.data.frame(ydata.7,gene2)
times <- 1:max(DT$years)
max.comp1 <- 5
max.comp  <- 5
LOOP5(file.name = "Comp3_G2_0.7.RData",I=I, max.comp1=max.comp1,max.comp = max.comp, times=times, ncut.var = 60)
p6 <- plot5.auc.pe("Comp3_G2_0.7.RData", times = times, data.name = "Gene2 with 70% censoring")




### real data

## DLBCL
load("../../Datasets/1DLBCL.RData")
DT<- DLBCL
#times <- 1:max(DT$years)
times <- 1:max(DT$years)
max.comp1 <- 7 #round(nrow(DT)/25)
max.comp <- 7
LOOP5(file.name = "Comp3_DLBCL.RData", I=I, max.comp1=max.comp1,max.comp = max.comp, times=times, ncut.var = 50)
p7 <- plot5.auc.pe("Comp3_DLBCL.RData", times = times,  data.name = "DLBCL with 41% censoring")
ggsave("C:/Users/ayame/Dropbox/All/Comp3_DLBCL.png",p7)

## LUNG
load("../../Datasets/1LUNG.RData")
DT<- LUNG
times <- 1:max(DT$years)
max.comp1<-5
max.comp <- 5 #round(nrow(DT)/25)
LOOP5(file.name = "Comp3_LUNG.RData", I=I, max.comp1=max.comp1,max.comp = max.comp, times=times, ncut.var = 100)
p8 <- plot5.auc.pe("Comp3_LUNG.RData", times = times,  data.name = "LUNG with 42% censoring")
ggsave("C:/Users/ayame/Dropbox/All/Comp3_LUNG.png",p8)






##NKI
load("../../Datasets/2NKI.RData")
DT <- NKI
times <- 1:max(DT$years)
max.comp1 <- 13 #round(nrow(DT)/25)
max.comp<- 13
LOOP5(file.name = "Comp3_NKI50.RData",I=I, max.comp1=max.comp1,max.comp = max.comp, times=times, ncut.var = 50)
p9 <- plot5.auc.pe("Comp3_NKI50.RData", times = times, data.name = "NKI with 66% censoring")

# ##NKI
# load("../../Datasets/2NKI.RData")
# DT <- NKI
# times <- 1:max(DT$years)
# max.comp1 <- 13 #round(nrow(DT)/25)
# max.comp<- 13
# LOOP5(file.name = "Comp3_NKI100.RData",I=I, max.comp1=max.comp1,max.comp = max.comp, times=times, ncut.var = 100)
# p9 <- plot5.auc.pe("Comp3_NKI100.RData",  times = times, data.name = "NKI with 66% censoring")

