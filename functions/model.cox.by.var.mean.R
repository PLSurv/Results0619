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
    c(predict(fit), predict(fit, newdata=newX))
    
  })
  
  LP <- list(
             lp.mean = apply(LP[1:nrow(X), ],1,mean),
             lpnew.mean = apply(LP[-c(1:nrow(X)), ],1,mean)
  )
  
LP
  
}

