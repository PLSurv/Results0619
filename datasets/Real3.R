#if (!requireNamespace("BiocManager", quietly = TRUE)) {install.packages("BiocManager")}
#BiocManager::install("Biobase")

library(Biobase)
library(dplyr)

##----------1. DLBCL, 188*2296
#BiocManager::install("DLBCL")
data(exprLym, package="DLBCL")
{
  ydata <- pData(exprLym)[,3:4]
  ydata$years <- ydata$FollowUpYears  
  ydata$event <- ifelse(ydata$StatusAtFollowUp=="Alive", 0, 1)
  ydata <- ydata[,c("years", "event")]
  
  xdata <- t(exprs(exprLym))
  xdata <- xdata %>%
    { (apply(., 2, sd) != 0) } %>% 
    { xdata[, .] } %>% 
    scale  
  xdata <- subset(xdata, select = !is.na(colMeans(xdata))) #remove missing vars
  colnames(xdata)<- paste0("dl", 1:ncol(xdata))
  
  xydata <- cbind.data.frame(ydata, xdata)
  xydata <- xydata[xydata$years>0 &!is.na(xydata$years),]
  
  DLBCL <- xydata
}
save(DLBCL, file = "DLBCL.RData") 

##----------2 lungExpression, 125*3171
#BiocManager::install("lungExpression")

{
  data(harvard, package="lungExpression")
  
  ydata <- pData(harvard)[,c("surv.time", "dead")]
  ydata$years <- ydata$surv.time/12
  ydata$event <- ydata$dead
  ydata <- ydata[,c("years", "event")]
  
  xdata <- t(exprs(harvard))
  xdata <- xdata %>%
    { (apply(., 2, sd) != 0) } %>% 
    { xdata[, .] } %>% 
    scale  
  xdata <- subset(xdata, select = !is.na(colMeans(xdata))) #remove missing vars
  colnames(xdata)<- paste0("hvd", 1:ncol(xdata))
  
  xydata <- cbind.data.frame(ydata, xdata)
  xydata <- xydata[xydata$years>0 &!is.na(xydata$years),]
  
  LUNG <- xydata
}

save(LUNG, file = "LUNG.RData") 

##---------- medium data ----------##

##----------3. NKI, 319*14318
#BiocManager::install("breastCancerNKI")

data(nki, package = "breastCancerNKI")

{
  ydata <- pData(nki)[,c("t.dmfs", "e.dmfs")]
  ydata$years <- ydata$t.dmfs/365.5
  ydata$event <- ydata$e.dmfs
  ydata <- ydata[,c("years", "event")]
  
  xdata <-t(exprs(nki))
  xdata <- xdata %>%
    { (apply(., 2, sd) != 0) } %>% 
    { xdata[, .] } %>% 
    scale
  xdata <- subset(xdata, select = !is.na(colMeans(xdata))) #remove missing vars
  
  colnames(xdata)<- paste0("nki", 1:ncol(xdata))
  
  xydata <- cbind.data.frame(ydata,xdata)
  xydata <- xydata[xydata$years>0 &!is.na(xydata$years),]
  
  NKI <- xydata
}

save(NKI, file = "NKI.RData") 
