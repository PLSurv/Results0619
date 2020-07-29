LOOP5 <- function(file.name, I, max.comp, times){
  N <- nrow(DT)
  
  AUC1 <- AUC2<- AUC3 <- AUC4 <- AUC5 <- PE1 <- PE2 <- PE3<- PE4<- PE5<-matrix(
    0, nrow = length(times), ncol = I, 
    dimnames = list(c(paste0("year.",times)),
                    c(paste0("repeat.", 1:I))))
  
  iauc1 <- iauc2 <- iauc3<- iauc4<- iauc5 <- ipe1 <- ipe2 <- ipe3 <- ipe4 <- ipe5<- NULL
  
  ## Run
  for (i in 1:I){
    
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
    fit1 <- pls.cv.cox(X=train[,-c(1:2)], time=train$years, event = train$event, max.comp = max.comp,Y.type = "DR", 
                       plsr.validation = "CV")
    lp1 <- predict(fit1)
    lpnew1 <- predict(fit1, newX=test) 
    
    # 
    fit2 <- pls.cox.cv(X=train[,-c(1:2)], time=train$years, event = train$event, max.comp = max.comp,Y.type = "time", 
                       seq.times=1:quantile(train$years)[4], cv.type ="auc", nfold.cv = 10)
    lp2 <- predict(fit2)
    lpnew2 <- predict(fit2, newX=test) 
    
    #
    fit3 <- pls.cox.cv(X=train[,-c(1:2)], time=train$years, event = train$event, max.comp = max.comp,Y.type = "DR", 
                       seq.times=1:quantile(train$years)[4], cv.type ="auc", nfold.cv = 10)
    lp3 <- predict(fit3)
    lpnew3 <- predict(fit3, newX=test)
    
    #
    fit4 <-pls.cox.cv(X=train[,-c(1:2)], time=train$years, event = train$event, max.comp = max.comp,Y.type = "time", 
                      seq.times = 1:quantile(train$years)[4], cv.type ="ibs", nfold.cv = 10)
    lp4 <- predict(fit4)
    lpnew4 <- predict(fit4, newX=test)
    
    fit5 <-pls.cox.cv(X=train[,-c(1:2)], time=train$years, event = train$event, max.comp = max.comp,Y.type = "DR", 
                      seq.times = 1:quantile(train$years)[4], cv.type ="ibs", nfold.cv = 10)
    lp5 <- predict(fit5)
    lpnew5 <- predict(fit5, newX=test)
    
    ## AUC
    
    Surv.rsp <- with(train, Surv(years, event))
    Surv.rsp.new <- with(test, Surv(years, event))

    auc1<-AUC.uno(Surv.rsp, Surv.rsp.new , lpnew1, times)
    auc2<-AUC.uno(Surv.rsp, Surv.rsp.new , lpnew2, times)
    auc3<-AUC.uno(Surv.rsp, Surv.rsp.new , lpnew3, times)
    auc4<-AUC.uno(Surv.rsp, Surv.rsp.new , lpnew4, times)
    auc5<-AUC.uno(Surv.rsp, Surv.rsp.new , lpnew5, times)
    
    
    iauc1 <- c(iauc1, auc1$iauc)
    iauc2 <- c(iauc2, auc2$iauc)
    iauc3 <- c(iauc3, auc3$iauc)
    iauc4 <- c(iauc4, auc4$iauc)
    iauc5 <- c(iauc5, auc5$iauc)
    
    AUC1[,i] <- auc1$auc
    AUC2[,i] <- auc2$auc
    AUC3[,i] <- auc3$auc
    AUC4[,i] <- auc4$auc
    AUC5[,i] <- auc5$auc
    #PE
    pe1<-predErr(Surv.rsp, Surv.rsp.new , lp1, lpnew1, times, type="robust", int.type = "weighted")
    pe2<-predErr(Surv.rsp, Surv.rsp.new , lp2, lpnew2, times, type="robust", int.type = "weighted")
    pe3<-predErr(Surv.rsp, Surv.rsp.new , lp3, lpnew3, times, type="robust", int.type = "weighted")
    pe4<-predErr(Surv.rsp, Surv.rsp.new , lp4, lpnew4, times, type="robust", int.type = "weighted")
    pe5<-predErr(Surv.rsp, Surv.rsp.new , lp5, lpnew5, times, type="robust", int.type = "weighted")
    
    ipe1 <- c(ipe1, pe1$ierror)
    ipe2 <- c(ipe2, pe2$ierror)
    ipe3 <- c(ipe3, pe3$ierror)
    ipe4 <- c(ipe4, pe4$ierror)
    ipe5 <- c(ipe5, pe5$ierror)
    
    PE1[,i] <- pe1$error
    PE2[,i] <- pe2$error
    PE3[,i] <- pe3$error
    PE4[,i] <- pe4$error
    PE5[,i] <- pe5$error
  }
  
  save(I,
    AUC1, AUC2, AUC3, AUC4, AUC5,
    PE1, PE2, PE3, PE4, PE5,
    iauc1, iauc2, iauc3, iauc4,iauc5,
    ipe1, ipe2, ipe3, ipe4, ipe5,
    file = file.name)
}
