
######
predict.pls.cox <- function(
  object,
  newX){
  
  if (!inherits(object, "pls.cox"))  { stop("This function only works for models from function 'pls.cox' class") }
  
  if (missing(newX)) lp <- object$Cox$linear.predictors
  
  
  else{
    if (FALSE %in% (names(object[["PLS"]][["scale"]]) %in% colnames(newX))) stop("The variable names in newX and X do not match")
    newdata <- as.data.frame(newX)
    
    LVnew <- as.data.frame(predict(object$PLS, newdata=newdata, type="scores"))
    colnames(LVnew) <- paste0("Comp", 1:ncol(LVnew))
    lp <- predict(object$Cox, newdata=LVnew)
    
  }
}


######
predict.i.pls.cox <- function(
  object, 
  newX) {
  
  if (!inherits(object, "i.pls.cox"))  { stop('This function only works for objects of class "i.pls.cox"') }
  
  k <- length(object)
  score.seq <- NULL
  for (i in 1:k){
    
    score.seq[i] <- object[[i]][["Cox"]][["score"]]
  }
  
  op.id <- which.max(score.seq)
  lp <- NULL
  
  if (missing(newX)){
    
    for (i in 1:k){
      if (missing(newX)) lp <- cbind(lp, predict(object[[i]]))
    }
    
    opt.lp <- predict(object[[op.id]])
  }
  
  else {
    for (i in 1:k){
      lp <- cbind(lp, predict(object[[i]], newX))
    }
    
    opt.lp <- predict(object[[op.id]], newX=newX)
  }
  list(lp.mean=apply(lp,1,mean),
       lp.opt=opt.lp,
       opt.interval.id=op.id,
       socre=score.seq
  )
  
}

######
surv.cv <- function(
  train.data, 
  test.data, 
  lp,
  lpnew, 
  seq.times, 
  cv.type=c("auc", "ibs", "c")
){
  
  Surv.rsp <- with(train.data, Surv(time, event))
  Surv.rsp.new <- with(test.data, Surv(time, event))
  
  switch(
    cv.type, 
    "auc" = AUC.uno(Surv.rsp, Surv.rsp.new, lpnew, seq.times)$iauc, 
    "ibs" = predErr(Surv.rsp, Surv.rsp.new, lp, lpnew, seq.times)$ierror,
    "c"   = GHCI(lpnew)
  ) 
}

######
surv.y <- function(time, event, Y.type){
  
  switch(
    Y.type,
    "DR" = residuals(coxph(Surv(time, event)~1, data=data.frame(time=time, event=event),ties="breslow", model=FALSE, x=FALSE, y=FALSE),  
                     type="deviance"),
    "time"= time,
    "log"=log(time)
  )
  
}