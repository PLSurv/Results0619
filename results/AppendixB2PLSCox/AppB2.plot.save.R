source("../plot3.auc.pe.R")


#0.2cens
load("../../datasets/gene_test.RData")
DT <- cbind.data.frame(ydata.2,gene2)
times <- 1:max(DT$years)
p1 <- plot3.auc.pe("Comp0_G1_0.2.RData",  times = times, data.name = "Gene1 with 17% censoring")
p2 <- plot3.auc.pe("Comp0_G2_0.2.RData",  times = times, data.name = "Gene2 with 17% censoring")
ggsave("PNG/Comp0_G1_0.2.png", p1)
ggsave("PNG/Comp0_G2_0.2.png", p2)

#0.5cens
DT <- cbind.data.frame(ydata.5,gene2)
times <- 1:max(DT$years)
p3 <- plot3.auc.pe("Comp0_G1_0.5.RData",  times = times, data.name = "Gene1 with 48% censoring")
p4 <- plot3.auc.pe("Comp0_G2_0.5.RData",  times = times, data.name = "Gene2 with 48% censoring")
ggsave("PNG/Comp0_G1_0.5.png", p3)
ggsave("PNG/Comp0_G2_0.5.png", p4)


#0.7cens
DT <- cbind.data.frame(ydata.7,gene2)
times <- 1:max(DT$years)
p5 <- plot3.auc.pe("Comp0_G1_0.7.RData",  times = times, data.name = "Gene1 with 67% censoring")
p6 <- plot3.auc.pe("Comp0_G2_0.7.RData",  times = times, data.name = "Gene2 with 67% censoring")
ggsave("PNG/Comp0_G1_0.7.png", p5)
ggsave("PNG/Comp0_G2_0.7.png", p6)



P1 <- gridExtra::grid.arrange(p1, p3, p5, nrow = 3)
P2 <- gridExtra::grid.arrange(p2, p4, p6, nrow = 3)

#ggsave("~/Documents/Git_yiapr/Thesis/thesis/X_appendix/figB/comp1.png", P1, width = 6, height = 9)
#ggsave("~/Documents/Git_yiapr/Thesis/thesis/X_appendix/figB/comp2.png", P2, width = 6, height = 9)



### real data

## DLBCL
load("../../datasets/1DLBCL.RData")
DT<- DLBCL
times <- 1:max(DT$years)
p7 <- plot3.auc.pe("Comp0_DLBCL.RData",  times = times,  data.name = "DLBCL with 41% censoring")

## LUNG
load("../../datasets/1LUNG.RData")
DT<- LUNG
times <- 1:max(DT$years)
p8 <- plot3.auc.pe("Comp0_LUNG.RData",  times = times,  data.name = "LUNG with 42% censoring")

##NKI
load("../../Datasets/2NKI.RData")
DT <- NKI
times <- 1:max(DT$years)
p9 <- plot3.auc.pe("Comp0_NKI.RData",  times = times, data.name = "NKI with 66% censoring")

ggsave("PNG/Comp0_DLBCL.png", p7)
ggsave("PNG/Comp0_LUNG.png", p8)
ggsave("PNG/Comp0_NKI.png", p9)

P3 <- gridExtra::grid.arrange(p7, p8,p9, nrow = 3)
#ggsave("~/Documents/Git_yiapr/Thesis/thesis/X_appendix/figB/comp3.png", P3, width = 6, height = 9)

