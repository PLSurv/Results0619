
pls.cox.cv <- function(
  X,
  time,
  event,
  Y.type=c("DR","log", "time"),
  max.comp,
  nfold.cv,
  cv.type=c("auc", "ibs", "c", "concordance"),
  seq.times
  ) { 
  
  Y.type <- match.arg(Y.type)
  cv.type <- match.arg(cv.type)
  
  
  nfold.cv <- round(nfold.cv)
  n.row <- nrow(X)
  n.col <- ncol(X)
  
  if (max.comp > min(floor(n.row/nfold.cv*(nfold.cv-1)), n.col)) max.comp <- min(floor(n.row/nfold.cv*(nfold.cv-1)), n.col)
  max.comp <- round(max.comp)
  
  index <- sample(rep(seq(nfold.cv), length = n.row))
  ## data
  
  data <- cbind.data.frame(
    i=index,
    time=time, event=event,
    X)
  
  ## 1. No CV, use full data
  if (nfold.cv==1){
    message("No CV process, PLS-Cox without cross-validation")
    
    fit <- pls.cv.cox(X=X, time = time, event = event, Y.type=Y.type, 
                      max.comp=max.comp, plsr.validation = "none")
    fit
    
  }
  
  ## 2. rules to choose opt.a
  else {

    list.data <- vector("list", nfold.cv)
    
    for (n in 1:nfold.cv){
      list.data[[n]] <- list(
        test  = data[data$i==n,], #test set
        train = data[!data$i==n,]#training set
      )
    }
    
    if (cv.type=="concordance"){
      criteria <-sapply(1:max.comp, function(max.comp){
        
        sapply(1:nfold.cv, function(n){
          fit <- pls.cv.cox(X=list.data[[n]]$train[,-c(1:3)], time = list.data[[n]]$train$time, event = list.data[[n]]$train$event, Y.type=Y.type, 
                            max.comp=max.comp, plsr.validation = "none")
          fit$Cox$concordance[6]
        })
      })
    }
    
    else{
      criteria <-sapply(1:max.comp, function(max.comp){
        
        sapply(1:nfold.cv, function(n){
          fit <- pls.cv.cox(X=list.data[[n]]$train[,-c(1:3)], time = list.data[[n]]$train$time, event = list.data[[n]]$train$event, Y.type=Y.type, 
                            max.comp=max.comp, plsr.validation = "none")
          
          ## predict
          lp <- predict(fit)
          lpnew <- predict(fit, newX=list.data[[n]]$test)
          
          ## auc/ibs index vector
          surv.cv(train.data = list.data[[n]]$train, test.data = list.data[[n]]$test, lp=lp, lpnew = lpnew, 
                  seq.times =seq.times, cv.type = cv.type )
        })
      })
      
     
    }
    
    criteria <- colMeans(criteria)
  
    opt.a <- switch(cv.type,
                    "concordance"=min(which(criteria>0.9)),
                    "auc"  =which.max(criteria),
                    "ibs"  =which.min(criteria),
                    "c"    =which.max(criteria)
                    )
    #opt.a
    
    if(length(opt.a)>0) fit <- pls.cv.cox(X=X, time = time, event = event,Y.type=Y.type, max.comp=opt.a, plsr.validation = "none")
    else stop("Reset 'seq.times' into a shorter duration")

    fit$criteria <- criteria
    class(fit) <- "pls.cox"
    fit
    
  }
  
}
