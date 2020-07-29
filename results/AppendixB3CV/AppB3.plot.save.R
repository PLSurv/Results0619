
source("../plot5.auc.pe.R")

#0.2cens
##G1
load("../../datasets/gene_test.RData")
DT <- cbind.data.frame(ydata.2,gene2[,1:300])
times <- 1:max(DT$years)
p1 <- plot5.auc.pe("Comp1_G1_0.2.RData",times, data.name = "Gene1 with 17% censoring")
p2 <- plot5.auc.pe("Comp1_G2_0.2.RData",times, data.name = "Gene2 with 17% censoring")
ggsave("PNG/Comp1_G1_0.2.png", p1)
ggsave("PNG/Comp1_G2_0.2.png", p2)


#0.5cens
DT <- cbind.data.frame(ydata.5,gene2[,1:300])
times <- 1:max(DT$years)
p3 <- plot5.auc.pe("Comp1_G1_0.5.RData",times, data.name = "Gene1 with 48% censoring")
p4 <- plot5.auc.pe("Comp1_G2_0.5.RData",times, data.name = "Gene2 with 48% censoring")
ggsave("PNG/Comp1_G1_0.5.png", p3)
ggsave("PNG/Comp1_G2_0.5.png", p4)


#0.7cens
DT <- cbind.data.frame(ydata.7,gene2[,1:300])
times <- 1:max(DT$years)
p5 <- plot5.auc.pe("Comp1_G1_0.7.RData",times, data.name = "Gene1 with 67% censoring")
p6 <- plot5.auc.pe("Comp1_G2_0.7.RData",times, data.name = "Gene2 with 67% censoring")
ggsave("PNG/Comp1_G1_0.7.png", p5)
ggsave("PNG/Comp1_G2_0.7.png", p6)



P1 <- gridExtra::grid.arrange(p1, p3, p5, nrow = 3)
P2 <- gridExtra::grid.arrange(p2, p4, p6, nrow = 3)

#ggsave("~/Documents/Git_yiapr/Thesis/thesis/X_appendix/figB/CV1.png", P1, width = 6, height = 9)
#ggsave("~/Documents/Git_yiapr/Thesis/thesis/X_appendix/figB/CV2.png", P2, width = 6, height = 9)

