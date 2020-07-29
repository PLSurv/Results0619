library(ggthemes)
library(ggplot2)
library(grid)
library(gridExtra)

plot2.auc.pe <- function(file.name, times=1:10, I=5, data.name){
  load(file.name)
  pd <- position_dodge(0.3)
  
  ## AUC plot
  pauc1 <- data.frame(time=paste0(times,"y"),
                      auc=apply(AUC1, 1, mean),
                      n=rep(I,length(times)),
                      sd = apply(AUC1, 1, sd))
  pauc2 <- data.frame(time=paste0(times,"y"),
                      auc=apply(AUC2, 1, mean),
                      n=rep(I,length(times)),
                      sd = apply(AUC2, 1, sd))
  
  pauc <- rbind(pauc1, pauc2)
  pauc$auc.grp <- c(rep("AUC1",length(times)),
                    rep("AUC2",length(times))
  )
  pauc$time <- factor(pauc$time, levels = paste0(times,"y"))
  
  p1<-ggplot(pauc, aes(x=time, y=auc, color=auc.grp, group=auc.grp))+
    geom_errorbar(aes(ymin=pmax(0.5,auc-sd), ymax=pmin(auc+sd,1),color=auc.grp),
                  width=0.2, size=0.25, position = pd)+
    geom_line(position = pd, linetype = "dotted", size=0.25)+
    geom_point(size=1,position = pd) +
    ylim(0.5,1)+
    xlab("")+ylab("mean AUC")+
    geom_rug(size=0.2,position = pd)+
    theme_tufte()+
    theme(plot.title = element_text(size = 10))+
    labs(color="")+
    #scale_color_d3()+
    #scale_color_manual(values ="Dark2")+
    ggtitle(sprintf("Mean.iAUC1, 2 =  %.3f, %.3f", 
                    mean(iauc1), mean(iauc2)))
  
  ## PE plot
  ppe1 <- data.frame(time=paste0(times,"y"),
                     pe=apply(PE1, 1, mean),
                     n=rep(I,length(times)),
                     sd = apply(PE1, 1, sd))
  ppe2 <- data.frame(time=paste0(times,"y"),
                     pe=apply(PE2, 1, mean),
                     n=rep(I,length(times)),
                     sd = apply(PE2, 1, sd))
  
  ppe <- rbind(ppe1, ppe2)
  ppe$pe.grp <- c(rep("PE1",length(times)),
                  rep("PE2",length(times)))
  ppe$time <- factor(ppe$time, levels = paste0(times,"y"))
  
  p2 <-ggplot(ppe, aes(x=time, y=pe, color=pe.grp, group=pe.grp))+
    geom_errorbar(aes(ymin=pmax(0,pe-sd), ymax=pmin(pe+sd,0.5),color=pe.grp),
                  width=0.2, size=0.25, position = pd)+
    geom_line(position = pd,linetype = "dotted", size=0.25)+
    geom_point(size=1,position = pd) +
    ylim(0,0.5)+
    xlab("")+ylab("Mean PE")+
    geom_rug(size=0.2,position = pd)+
    theme_tufte()+
    theme(plot.title = element_text(size = 10))+
    labs(color="")+
    #scale_color_d3()+
    #scale_color_manual(palette = "Dark2")+
    ggtitle(sprintf("Mean.iPE1, 2 =  %.3f, %.3f", 
                    mean(ipe1), mean(ipe2)))
  
  gridExtra::grid.arrange(p1, p2, nrow = 2,
                          top = textGrob(data.name, just="right", 
                                         gp=gpar(fontsize=12, fontfamily="Times New Roman")))
}