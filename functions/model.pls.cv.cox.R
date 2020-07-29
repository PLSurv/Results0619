## PLS-Cox model
## with/or without PLS-RMSE-CV optimal component

pls.cv.cox <- function(
  X,      
  time,   
  event,  
  Y.type=c("DR","time","log"), 
  max.comp, 
  plsr.validation = c("CV", "none", "LOO")
){
  
  Y.type <- match.arg(Y.type)
  plsr.validation <- match.arg(plsr.validation)
  
  ## choose different y in pls
  y <- surv.y(time=time, event = event, Y.type = Y.type)
  
  ## data with y
  #if (is.null(dimnames(X)[[1]])) dimnames(X)[[1]] <- 1:nrow(X)
  if (is.null(colnames(X))) stop("Please assign variables names")
  pls.data <- cbind.data.frame(y=y, X)
  
  if (max.comp > min(dim(X))) max.comp <- min(dim(X))
  max.comp <- round(max.comp)
  
  
  ## pls regression
  pls.r <- plsr(
    y ~ .,
    data = pls.data,
    ncomp = max.comp,
    scale = TRUE,
    center = TRUE,
    method = "simpls",
    validation = plsr.validation,
    model=FALSE
  )
  
  ## extract latent variables
  LV <- as.data.frame(predict(pls.r, type="scores"))
  colnames(LV) <- paste0("Comp", 1:pls.r$ncomp)
  
  if(pls.r$ncomp >= nrow(LV)) stop("Set 'max.comp' << than the number of rows of X")
  
  cox.data <- cbind.data.frame(time=time, event=event, LV)
  cox <- coxph(Surv(time, event)~., data = cox.data, 
               ties="breslow", model=FALSE, x=FALSE, y=FALSE)
  
  pls.r$Xvar     <- pls.r$Xtotvar  <- pls.r$loadings <- NULL
  pls.r$call     <- pls.r$fit.time <- pls.r$method   <- NULL
  pls.r$Yscores  <- pls.r$Yloadings<- pls.r$residuals<- NULL
  
  #cox$concordance <- cox$call <- cox$formula <-cox$loglik <- cox$wald.test<- NULL 
  cox$var <- cox$iter <- cox$assign  <-NULL
  
  ## model
  model <- list(PLS = pls.r, Cox = cox)
  class(model) <- "pls.cox"
  model
}

