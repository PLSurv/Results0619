##----------simulation data 
##
## gene1: simple mode
## gene2: complex mode
##
##----------

{ # set n and p
  #j-,n-th patient, n, i-th gene
  n <- j <- 200 
  i <- 3000  
  # gene1 and gene2
  X2<- matrix(0,nrow=i, ncol=j) 
}



{# reproducible random number creation
  set.seed(1); e1 <- rnorm(50*j/2)
  set.seed(2); e2 <- rnorm(50*j/2)
  set.seed(3); e3 <- rnorm((i-50)*j)
  
  set.seed(1); u1 <- runif(j, 0, 1)
  set.seed(2); u2 <- runif(j, 0, 1)
  set.seed(3); u3 <- runif(j, 0, 1)
  
  set.seed(4); e4 <- rnorm(50*j)
  set.seed(5); e5 <- rnorm(100*j)
  set.seed(6); e6 <- rnorm(100*j)
  set.seed(7); e7 <- rnorm((i-300)*j)
  
  set.seed(8); e8 <- rnorm(j,0,1.5)
}

{ ##---------- II. gene2 data
  ## link part, same as X1
  X2[1:50, 1:(j/2)]   <- X1[1:50, 1:(j/2)]
  X2[1:50, (j/2+1):j] <- X1[1:50, (j/2+1):j]
  
  U1 <- matrix(rep(1*(u1<0.4), 50), 50, j, byrow = TRUE)
  U2 <- matrix(rep(1*(u2<0.7), 100), 100, j, byrow = TRUE)
  U3 <- matrix(rep(1*(u3<0.3), 100), 100, j, byrow = TRUE)
  
  X2[51:100,]  <- 3.5 + 1.5*U1 +matrix(e4, nrow=50, ncol=j)
  X2[101:200,] <- 3.5 + 0.5*U2 +matrix(e5, nrow=100, ncol=j)
  X2[201:300,] <- 3.5 - 1.5*U3 +matrix(e6, nrow=100, ncol=j)
  X2[-(1:300),]<- 3.5 + matrix(e7, nrow=i-300, ncol=j)
  
  ## trans and center X2 
  gene2 <- as.data.frame(scale(t(X2), center = TRUE, scale = FALSE))
  dimnames(gene2) <- list(c(1:j), paste0("g2.", c(1:i)))
}

## relation to genes
y <- colSums(X1[1:50,])/25+ e8 ## n=j


{
  set.seed(9); v <- runif(n)
  ## censored data is distributed as U[0,10]
  set.seed(10); centime <- runif(n, 1/365.5, 25)
}

{##---------- 1. censor=20 % 
  
  # expoential latent event times
  fulltime <- (- log(v) / (0.00073 * exp(y)))  #^(1 / rho=1)  ## 18%
  
  # follow-up times and event indicators
  time <- pmin(fulltime, centime)
  event <- as.numeric(fulltime <= centime)
  
  ## survival data
  ydata.2 <- data.frame(years=time, event=event)
}


{## check censoring %
  summary(fulltime)
  summary(time)
  1-sum(event)/n
}


{##---------- 2. censor=50%
  
  # expoential latent event times
  fulltime <- (- log(v) / (0.000073 * exp(y)))  #^(1 / rho=1)  
  
  # follow-up times and event indicators
  time <- pmin(fulltime, centime)
  event <- as.numeric(fulltime <= centime)
  
  ## survival data
  ydata.5 <- data.frame(years=time, event=event)
}


{## check censoring %
  summary(fulltime)
  summary(time)
  1-sum(event)/n
}

{## 3.censor = 70%
  
  # expoential latent event times
  fulltime <- (- log(v) / (0.000017 * exp(y)))  #^(1 / rho=1)  
  
  # follow-up times and event indicators
  time <- pmin(fulltime, centime)
  event <- as.numeric(fulltime <= centime)
  
  ## survival data
  ydata.7 <- data.frame(years=time, event=event)
}

{## check censoring %
  summary(fulltime)
  summary(time)
  1-sum(event)/n
}

##---------- same censor% 

save(ydata.2, ydata.5, ydata.7, gene2, file = "gene_test.RData")


{##----------##---------- data descriptives ----------##----------
  library(survival)
  f1<- survfit(Surv(years, event)~1, data = ydata.2)
  f2<- survfit(Surv(years, event)~1, data = ydata.5)
  f3<- survfit(Surv(years, event)~1, data = ydata.7)
  
  plot(f1, mark.time = TRUE)
  plot(f2, mark.time = TRUE)
  plot(f3, mark.time = TRUE)
}
{
  coxph(formula = Surv(years, event) ~ ., data = ydata.2)
  coxph(formula = Surv(years, event) ~ ., data = ydata.5)
  coxph(formula = Surv(years, event) ~ ., data = ydata.7)
}
##----------

#library(spls)
#spls(gene1, ydata.5, 2, 0.9, scale.x=TRUE)
#gene1[50:55,50:55]

plot(apply(gene1,2,mean))
plot(apply(gene1,2,var))

library(gplots)
heatmap.2(as.matrix(gene1)[100:200,1:100], 
          dendrogram="both",
          scale="none",
          col=redgreen(256))
