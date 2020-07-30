#cut

source("../plot2.auc.pe.R")

#0.2cens
#G2
load("../../datasets/gene_test.RData")
DT <- cbind.data.frame(ydata.2,gene2)
times <- 1:max(DT$years)
p2 <- plot2.auc.pe("Comp3_G2_0.2.RData", times = times, data.name = "Gene2 with 17% censoring")
ggsave("PNG/Comp3_G2_0.2.png",p2)
#0.5cens
#G2

DT <- cbind.data.frame(ydata.5,gene2)
times <- 1:max(DT$years)
p4 <- plot2.auc.pe("Comp3_G2_0.5.RData", times = times, data.name = "Gene2 with 48% censoring")
ggsave("PNG/Comp3_G2_0.5.png",p4)
#0.7cens
#G2

DT <- cbind.data.frame(ydata.7,gene2)
times <- 1:max(DT$years)
p6 <- plot2.auc.pe("Comp3_G2_0.7.RData", times = times, data.name = "Gene2 with 67% censoring")
ggsave("PNG/Comp3_G2_0.7.png",p6)

P2 <- gridExtra::grid.arrange(p2, p4, p6, nrow = 3)
#ggsave("~/Documents/Git_yiapr/Thesis/thesis07/X_appendix/figB/cut.png", P2, width = 6, height = 9)
