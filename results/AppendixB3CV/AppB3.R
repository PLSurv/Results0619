{
library(survival)
library(survAUC)
library(pls)

source("../../functions/predict.pls.cox.R")
source("../../functions/model.pls.cv.cox.R")
source("../../functions/model.pls.cox.cv.R")

#### 4PLOT
source("../plot5.auc.pe.R")

#### FUNCTIONS

source("AppB3.loop.fit5.R")

I<-5
}


#0.2cens
##G1
load("../../datasets/gene_test.RData")
DT <- cbind.data.frame(ydata.2,gene2[,1:300])
times <- 1:max(DT$years)
max.comp <-8
LOOP5(file.name = "Comp1_G1_0.2.RData",I=I, max.comp = max.comp, times=times)
p2 <- plot5.auc.pe("Comp1_G1_0.2.RData",times, data.name = "Gene1 with 17% censoring")

##G2
DT <- cbind.data.frame(ydata.2,gene2)
times <-1:max(DT$years)
max.comp <- 5
LOOP5(file.name = "Comp1_G2_0.2.RData",I=I, max.comp = max.comp, times=times)
p3 <- plot5.auc.pe("Comp1_G2_0.2.RData",times, data.name = "Gene2 with 17% censoring")

#0.5cens
##G1
DT <- cbind.data.frame(ydata.5,gene2[,1:300])
times <- 1:max(DT$years)
max.comp <- 8
LOOP5(file.name = "Comp1_G1_0.5.RData",I=I, max.comp = max.comp, times=times)
p4 <- plot5.auc.pe("Comp1_G1_0.5.RData",times, data.name = "Gene1 with 48% censoring")

##G2

DT <- cbind.data.frame(ydata.5,gene2)
times <- 1:max(DT$years)
max.comp <- 5
LOOP5(file.name = "Comp1_G2_0.5.RData",I=I, max.comp = max.comp, times=times)
p5 <- plot5.auc.pe("Comp1_G2_0.5.RData",times, data.name = "Gene2 with 48% censoring")

#0.7cens
##G1

DT <- cbind.data.frame(ydata.7,gene2[,1:300])
times <- 1:max(DT$years)
max.comp <- 8
LOOP5(file.name = "Comp1_G1_0.7.RData",I=I, max.comp = max.comp, times=times)
p6 <- plot5.auc.pe("Comp1_G1_0.7.RData",times, data.name = "Gene1 with 67% censoring")

##G2

DT <- cbind.data.frame(ydata.7,gene2)
times <- 1:max(DT$years)
max.comp <- 5
LOOP5(file.name = "Comp1_G2_0.7.RData",I=I, max.comp = max.comp, times=times)
p7 <- plot5.auc.pe("Comp1_G2_0.7.RData",times, data.name = "Gene2 with 67% censoring")

