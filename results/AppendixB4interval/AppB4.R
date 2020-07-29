{
  library(survival)
  library(survAUC)
  library(pls)
  #library(doParallel)
  #l#ibrary(parallel)
  source("../../functions/predict.pls.cox.R")
  source("../../functions/model.pls.cv.cox.R")
  source("../../functions/model.fi.pls.cox.R")
  
  #### 4PLOT
  source("../plot4.auc.pe.R")
  
  #### FUNCTIONS
  
  source("AppB4.loop.fit4.R")
  
  I<-5}

#0.2cens
#G2
load("../../datasets/gene_test.RData")
DT <- cbind.data.frame(ydata.2,gene2)
times <- 1:max(DT$years)
max.comp1 <- 5
max.comp  <- 5
LOOP4(file.name = "Comp3_G2_0.2.RData",I=I,max.comp = max.comp, times=times, ncut.var1 = 10, ncut.var2 = 60)
p2 <- plot4.auc.pe("Comp3_G2_0.2.RData", times = times, data.name = "Gene2 with 17% censoring")

#0.5cens
#G2

DT <- cbind.data.frame(ydata.5,gene2)
times <- 1:max(DT$years)
max.comp1 <- 5
max.comp  <- 5
LOOP4(file.name = "Comp3_G2_0.5.RData",I=I,max.comp = max.comp, times=times, ncut.var1 = 10, ncut.var2 = 60)
p4 <- plot4.auc.pe("Comp3_G2_0.5.RData", times = times, data.name = "Gene2 with 48% censoring")

#0.7cens
#G2

DT <- cbind.data.frame(ydata.7,gene2)
times <- 1:max(DT$years)
max.comp1 <- 5
max.comp  <- 5
LOOP4(file.name = "Comp3_G2_0.7.RData",I=I,max.comp = max.comp, times=times, ncut.var1 = 10, ncut.var2 = 60)
p6 <- plot4.auc.pe("Comp3_G2_0.7.RData", times = times, data.name = "Gene2 with 67% censoring")

