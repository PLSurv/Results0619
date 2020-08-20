## PLS-Cox model
## with/or without PLS-RMSE-CV optimal component

cox.by.var <- function(
  X,      
  time,   
  event,
  newX
){
  data <- data.frame(time=time, event=event, X)
  
  LP<- sapply(1:ncol(X), function(i){
    
    fm <- as.formula(paste0("Surv(time, event)~", names(X)[i]))
    fit <- coxph(fm, data=data)
    c(diff(fit$loglik), predict(fit), predict(fit, newdata=newX))
    
  })
  
  LP <- list(logLike = LP[1,],
             lp = LP[2:(nrow(X)+1), which.max(LP[1,])],#
             lpnew=LP[-c(1:(nrow(X)+1)), which.max(LP[1,])])#
  
  return(LP)
  
}

