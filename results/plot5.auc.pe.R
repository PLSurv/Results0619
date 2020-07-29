library(ggthemes)
library(ggplot2)
library(grid)
library(gridExtra)

plot5.auc.pe <- function(file.name, times, data.name){
  load(file.name)
  pd <- position_dodge(0.5)
  
  
  ## AUC plot
  pauc1 <- data.frame(time=paste0(times,"y"),
                      auc=apply(AUC1, 1, mean),
                      n=rep(I,length(times)),
                      sd = apply(AUC1, 1, sd))
  pauc2 <- data.frame(time=paste0(times,"y"),
                      auc=apply(AUC2, 1, mean),
                      n=rep(I,length(times)),
                      sd = apply(AUC2, 1, sd))
  pauc3 <- data.frame(time=paste0(times,"y"),
                      auc=apply(AUC3, 1, mean),
                      n=rep(I,length(times)),
                      sd = apply(AUC3, 1, sd))
  pauc4 <- data.frame(time=paste0(times,"y"),
                      auc=apply(AUC4, 1, mean),
                      n=rep(I,length(times)),
                      sd = apply(AUC4, 1, sd))
  pauc5 <- data.frame(time=paste0(times,"y"),
                      auc=apply(AUC5, 1, mean),
                      n=rep(I,length(times)),
                      sd = apply(AUC5, 1, sd))
  
  
  pauc <- rbind(pauc1, pauc2, pauc3, pauc4, pauc5)
  pauc$auc.grp <- c(rep("AUC1",length(times)),
                    rep("AUC2",length(times)),
                    rep("AUC3",length(times)),
                    rep("AUC4",length(times)),
                    rep("AUC5",length(times)))
  pauc$time <- factor(pauc$time, levels = paste0(times,"y"))
  
  p1<-ggplot(pauc, aes(x=time, y=auc, color=auc.grp, group=auc.grp))+
    geom_errorbar(aes(ymin=pmax(0.5,auc-sd), ymax=pmin(auc+sd,1),color=auc.grp),
                  width=0.2, size=0.1, position = pd)+
    geom_line(position = pd, linetype = "dotted", size=0.25)+
    geom_point(size=1,position = pd, shape=3) +
    ylim(0.5,1)+
    xlab("")+ylab("Mean AUC")+
    geom_rug(size=0.2,position = pd)+
    theme_tufte()+
    theme(plot.title = element_text(size = 10))+
    labs(color="")+
    #scale_color_d3()+
    #scale_color_manual(values =cl)+
    ggtitle(sprintf("Mean.iAUC1, 2, 3, 4, 5 =  %.3f, %.3f, %.3f, %.3f, %.3f", 
                    mean(iauc1), mean(iauc2), mean(iauc3), mean(iauc4), mean(iauc5)))
  
  ## PE plot
  ppe1 <- data.frame(time=paste0(times,"y"),
                     pe=apply(PE1, 1, mean),
                     n=rep(I,length(times)),
                     sd = apply(PE1, 1, sd))
  ppe2 <- data.frame(time=paste0(times,"y"),
                     pe=apply(PE2, 1, mean),
                     n=rep(I,length(times)),
                     sd = apply(PE2, 1, sd))
  ppe3 <- data.frame(time=paste0(times,"y"),
                     pe=apply(PE3, 1, mean),
                     n=rep(I,length(times)),
                     sd = apply(PE3, 1, sd))
  ppe4 <- data.frame(time=paste0(times,"y"),
                     pe=apply(PE4, 1, mean),
                     n=rep(I,length(times)),
                     sd = apply(PE4, 1, sd))
  ppe5 <- data.frame(time=paste0(times,"y"),
                     pe=apply(PE5, 1, mean),
                     n=rep(I,length(times)),
                     sd = apply(PE5, 1, sd))
  
  
  ppe <- rbind(ppe1, ppe2, ppe3,ppe4,ppe5)
  ppe$pe.grp <- c(rep("PE1",length(times)),
                  rep("PE2",length(times)),
                  rep("PE3",length(times)),
                  rep("PE4",length(times)),
                  rep("PE5",length(times)))
  ppe$time <- factor(ppe$time, levels = paste0(times,"y"))
  
  p2 <-ggplot(ppe, aes(x=time, y=pe, color=pe.grp, group=pe.grp))+
    geom_errorbar(aes(ymin=pmax(0,pe-sd), ymax=pmin(pe+sd,0.5),color=pe.grp),
                  width=0.2, size=0.1, position = pd)+
    geom_line(position = pd, linetype = "dotted", size=0.25)+
    geom_point(size=1,position = pd, shape=3) +
    ylim(0,0.5)+
    xlab("")+ylab("Mean PE")+
    geom_rug(size=0.2,position = pd)+
    theme_tufte()+
    theme(plot.title = element_text(size = 10))+
    labs(color="")+
    #scale_color_d3()+
    #scale_color_manual(values =cl)+
    ggtitle(sprintf("Mean.iPE1, 2, 3, 4, 5 =  %.3f, %.3f, %.3f, %.3f, %.3f", 
                    mean(ipe1), mean(ipe2), mean(ipe3), mean(ipe4), mean(ipe5)))
  
  gridExtra::grid.arrange(p1, p2, nrow = 2,
                          top = textGrob(data.name, just="right", 
                                         gp=gpar(fontsize=12, fontfamily="Times")))
}
