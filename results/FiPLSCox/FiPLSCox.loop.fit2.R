LOOP5 <- function(file.name, I, max.comp1, max.comp, times, ncut.var){
  
  N <- nrow(DT)
  
  AUC1 <- AUC2<- 
  PE1 <- PE2 <-matrix(
    0, nrow = length(times), ncol = I, 
    dimnames = list(c(paste0("year.",times)),
                    c(paste0("repeat.", 1:I))))
  
  iauc1 <- iauc2<- ipe1 <- ipe2<- NULL
  
  ## Run
  for (i in 1:I) {
    
    E <- DT[DT$event==1,]
    C <- DT[DT$event==0,]
    id.e <- sample(nrow(E),nrow(E)/2)
    id.c <- sample(nrow(C), nrow(C)/2)
    
    E.train<- DT[-id.e,]
    C.train<- DT[-id.c,]
    
    E.test<- DT[id.e,]
    C.test<- DT[id.c,]
    
    train <- rbind(E.train,C.train)
    test <- rbind(E.test,C.test)
    
    # 
    fit1 <- pls.cv.cox(X=train[,-c(1:2)], time=train$years, event = train$event, max.comp = max.comp1,Y.type = "DR", 
                       plsr.validation = "CV")
    lp1 <- predict(fit1)
    lpnew1 <- predict(fit1, newX=test) 
    
    # 
    fit2 <- fi.pls.cox(X=train[,-c(1:2)], time=train$years, event = train$event, max.comp = max.comp,Y.type = "DR",
                          ncut.var =ncut.var,
                          plsr.validation = "CV", random.cut = FALSE,
                          show = "lps", newX = test)
    lp2 <- fit2$lp
    lpnew2 <- fit2$lpnew
    
    ## AUC
    
    Surv.rsp <- with(train, Surv(years, event))
    Surv.rsp.new <- with(test, Surv(years, event))
    
    #times <- 1:min(max(train$years), max(test$years))
    
    auc1<-AUC.uno(Surv.rsp, Surv.rsp.new , lpnew1, times)
    auc2<-AUC.uno(Surv.rsp, Surv.rsp.new , lpnew2, times)
    

    iauc1 <- c(iauc1, auc1$iauc)
    iauc2 <- c(iauc2, auc2$iauc)
    
    
    
    AUC1[,i] <- auc1$auc
    AUC2[,i] <- auc2$auc
   
    
    
    #PE
    pe1<-predErr(Surv.rsp, Surv.rsp.new , lp1, lpnew1, times, type="robust", int.type = "weighted")
    pe2<-predErr(Surv.rsp, Surv.rsp.new , lp2, lpnew2, times, type="robust",int.type = "weighted")
   
    
    ipe1 <- c(ipe1, pe1$ierror)
    ipe2 <- c(ipe2, pe2$ierror)
   
    
    PE1[,i] <- pe1$error
    PE2[,i] <- pe2$error
   
    

    
  }
  
  save(
    I,
    AUC1, AUC2, 
    PE1, PE2, 
    iauc1, iauc2, 
    ipe1, ipe2, 
    file = file.name)
}
