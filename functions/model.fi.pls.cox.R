
fi.pls.cox <- function(
  X,
  time,
  event, 
  Y.type=c("DR", "log", "time"),
  max.comp,
  ncut.var,
  plsr.validation=c("CV", "none", "LOO"),
  random.cut=FALSE,
  show=c("lps, models"),
  newX,
  hide.message=TRUE
)
{
  
  if (missing(X)) stop("Please specify gene expression data")
  if (missing(time)) stop("Please specify time data")
  if (missing(event)) stop("Please specify censoring data")
  
  show <- match.arg(show)
  
  ncut.var <- round(ncut.var)
  if (ncut.var<2) {
    
    if (!hide.message) {message("No split in the variables, do 'pls.cv.cox'")}
    
    if (show=="lps"){
      fit <- pls.cv.cox(X=X, time = time, event = event, Y.type = Y.type,
                        max.comp =max.comp, plsr.validation = plsr.validation)
      
      LP<- list(logLike = diff(fit$Cox$loglik),lp = predict(fit),lpnew = predict(fit, newX))
      return(LP)
      
    }
    else{
      fit <- pls.cv.cox(X=X, time = time, event = event, Y.type = Y.type,
                        max.comp =max.comp, plsr.validation = plsr.validation)
      return(fit)
      
    }
  }
  
  max.comp <- round(max.comp)
  
  pred.id <- pred <- model <-vector("list", ncut.var)
  
  if (random.cut) {interval.index <-sample(rep(seq(ncut.var), length = ncol(X)))}
  else {interval.index <- sort(rep(seq(ncut.var), length = ncol(X)))}
  
  for (i in 1L:ncut.var) pred.id[[i]] <- which(interval.index %in% c(i))
  
  score <- sapply(1:ncut.var, function(i){
    
    fit <- pls.cv.cox(X=X[,pred.id[[i]],drop=FALSE], time = time, event = event, Y.type = Y.type,
                      max.comp =max.comp, plsr.validation = "CV")
    #fit$Cox$score
    diff(fit$Cox$loglik)
    #-2*fit$Cox$loglik[2]+2*fit$PLS$ncomp
    
  })
  
  order.scores <- order(score, decreasing = TRUE)
  
  #pred  <- vector("list", ncut.var)
  pred[[1]] <- unique(pred.id[[order.scores[[1]]]])
  
  for (i in 1:(ncut.var-1)) pred[[i+1]] <- unique(c(pred[[i]],pred.id[[order.scores[[i+1]]]]))
  
  if (show=="lps"){
    LP <- sapply(1:(ncut.var-1), function(i){
      fit <- pls.cv.cox(X=X[,pred[[i]], drop=FALSE], time = time, event = event, Y.type = Y.type,
                        max.comp =max.comp, plsr.validation = plsr.validation)
      c(diff(fit$Cox$loglik), predict(fit), predict(fit, newX))
      
    })
    
    LP <- list(logLike = LP[1,],
               lp = LP[2:(nrow(X)+1), which.max(LP[1,])],
               lpnew=LP[-c(1:(nrow(X)+1)), which.max(LP[1,])])

    return(LP)
    
    
  }
  else {
    model <- lapply(1:(ncut.var), function(i){
      pls.cv.cox(X=X[,pred[[i]], drop=FALSE], time = time, event = event, Y.type = Y.type,
                 max.comp =max.comp, plsr.validation = plsr.validation)
      
    })
    class(model) <- "i.pls.cox"
    
    return(model)
    
  }
  
}
