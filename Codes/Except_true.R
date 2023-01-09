data_visualization1w <- data_frame(Fold=c(1,2,3,4,5,6),
                                   gam=c(168,163,169,149,172,168),
                                   Linear=c(170.697,	168.6241,	142.6296,	221.5714,	131.4606,	158.8727),
                                   BRF=c(121.9160, 
                                         124.0717, 
                                         135.9551,
                                         125.5093, 
                                         127.0380, 
                                         114.0638))%>% as.data.table()

Repeat1_True_vs_gamw <- data_visualization1w$Fold <- as.factor(data_visualization1w$Fold)
data_visualization1w <- melt(data_visualization1w)
plot(as.numeric(data_visualization1w$Fold),data_visualization1w$value)

r1w <- ggplot(data_visualization1w, aes(x=Fold,y=value, colour = variable)) + geom_point(position = position_dodge(width = .3))
#################################################################################

data_visualization2w <- data_frame(Fold=c(1,2,3,4,5,6), 
                                   gam=c(169,149,163,172,168,168),
                                   Linear=c(142.6296,	221.5714,	168.6241,	131.4606,	158.8727,	170.697),
                                   BRF=c(124.9218, 
                                         129.5126, 
                                         109.8994, 
                                         119.0610, 
                                         112.0528, 
                                         126.2424))%>% as.data.table()


Repeat2_True_vs_gamw <- data_visualization2w$Fold <- as.factor(data_visualization2w$Fold)
data_visualization2w <- melt(data_visualization2w)

r2w <- ggplot(data_visualization2w, aes(x=Fold,y=value, colour = variable)) + geom_point(position = position_dodge(width = .3))

#################################################################################
data_visualization3w <- data_frame(Fold=c(1,2,3,4,5,6),
                                   gam=c(164,169,164,167,155,174),
                                   Linear=c(218.7514, 
                                            135.5495, 
                                            156.6744, 
                                            170.0255, 
                                            206.6103, 
                                            147.6641),
                                   BRF=c(115.0424, 
                                         114.9593, 
                                         117.9030, 
                                         130.4493,
                                         123.7308, 
                                         124.3256))%>%as.data.table()

Repeat3_True_vs_gamw <- data_visualization3w$Fold <- as.factor(data_visualization3w$Fold)
data_visualization3w <- melt(data_visualization3w)

r3w <- ggplot(data_visualization3w, aes(x=Fold,y=value, colour = variable)) + geom_point(position = position_dodge(width = .3))



#################################################################################
data_visualization4w <- data_frame(Fold=c(1,2,3,4,5,6), 
                                   gam=c(167,156,174,160,168,168), Linear=c(132.0992, 
                                                                            209.1405, 
                                                                            151.1542, 
                                                                            200.2647, 
                                                                            148.0210,
                                                                            167.2167),
                                   BRF=c(125.2462, 
                                         114.8074, 
                                         129.0142, 
                                         123.4694, 
                                         112.6788, 
                                         117.0909))%>%as.data.table()

Repeat4_True_vs_gamw <- data_visualization4w$Fold <- as.factor(data_visualization4w$Fold)
data_visualization4w <- melt(data_visualization4w)

r4w <- ggplot(data_visualization4w, aes(x=Fold,y=value, colour = variable)) + geom_point(position = position_dodge(width = .3))
#################################################################################
data_visualization5w <- data_frame(Fold=c(1,2,3,4,5,6), 
                                   gam=c(172,	168,	168,	163, 149,	169),
                                   Linear=c(131.4606, 
                                            170.6970, 
                                            158.8727, 
                                            168.6241, 
                                            221.5714, 
                                            142.6296 ),
                                   BRF=c(123.5394, 
                                         138.1111, 
                                         127.1454, 
                                         122.8106,
                                         129.6000, 
                                         113.7755))%>%as.data.table()

Repeat5_True_vs_gamw <- data_visualization5w$Fold <- as.factor(data_visualization5w$Fold)
data_visualization5w <- melt(data_visualization5w)

r5w <- ggplot(data_visualization5w, aes(x=Fold,y=value, colour = variable)) + geom_point(position = position_dodge(width = .3))
#################################################################################
data_visualization6w <- data_frame(Fold=c(1,2,3,4,5,6), 
                                   gam=c(174,	167,	165,	163,	169,	153),
                                   Linear=c(149.4847, 
                                            170.1434, 
                                            157.7807, 
                                            220.9213, 
                                            135.5461, 
                                            210.4565),
                                   BRF=c(118.0528, 
                                         110.1522, 
                                         122.7553, 
                                         125.3664, 
                                         120.1910, 
                                         127.3569))%>%as.data.table()

Repeat6_True_vs_gamw <- data_visualization6w$Fold <- as.factor(data_visualization6w$Fold)
data_visualization6w <- melt(data_visualization6w)

r6w <- ggplot(data_visualization6w, aes(x=Fold,y=value, colour = variable)) + geom_point(position = position_dodge(width = .3))
#################################################################################
data_visualization7w <- data_frame(Fold=c(1,2,3,4,5,6), 
                                   gam=c(144,
                                         165,
                                         167,
                                         174,
                                         169,
                                         165),
                                   Linear=c(222.7222, 
                                            145.3886, 
                                            172.5797, 
                                            141.5372,
                                            132.4000, 
                                            167.8797),
                                   BRF=c(131.1510, 
                                         119.6732, 
                                         122.6067, 
                                         120.8167, 
                                         114.9918,
                                         127.1818))%>%as.data.table()

Repeat7_True_vs_gamw <- data_visualization7w$Fold <- as.factor(data_visualization7w$Fold)
data_visualization7w <- melt(data_visualization7w)

r7w <- ggplot(data_visualization7w, aes(x=Fold,y=value, colour = variable)) + geom_point(position = position_dodge(width = .3))
#################################################################################
data_visualization8w <- data_frame(Fold=c(1,2,3,4,5,6), 
                                   gam=c(172,	152,	170,	167,	168,	163),
                                   Linear=c(128.5130, 
                                            215.3353, 
                                            152.8892,
                                            173.0881, 
                                            157.3472, 
                                            177.8860),
                                   BRF=c(125.8475, 
                                         126.2649, 
                                         133.2291, 
                                         122.5379,
                                         113.1987, 
                                         124.2353))%>%as.data.table()

Repeat8_True_vs_gamw <- data_visualization8w$Fold <- as.factor(data_visualization8w$Fold)
data_visualization8w <- melt(data_visualization8w)

r8w <- ggplot(data_visualization8w, aes(x=Fold,y=value, colour = variable)) + geom_point(position = position_dodge(width = .3))
#################################################################################
data_visualization9w <- data_frame(Fold=c(1,2,3,4,5,6), 
                                   gam=c(165,	169,	168,	174,	166,	138),
                                   Linear=c(174.7683, 
                                            136.1395, 
                                            170.5305, 
                                            153.7955, 
                                            160.6489, 
                                            231.4632),
                                   BRF=c(123.9515, 
                                         115.3554, 
                                         121.0232, 
                                         125.3844, 
                                         137.2556, 
                                         129.2000))%>%as.data.table()

Repeat9_True_vs_gamw <- data_visualization9w$Fold <- as.factor(data_visualization9w$Fold)
data_visualization9w <- melt(data_visualization9w)

r9w <- ggplot(data_visualization9w, aes(x=Fold,y=value, colour = variable)) + geom_point(position = position_dodge(width = .3))
#################################################################################
data_visualization10w <- data_frame(Fold=c(1,2,3,4,5,6), 
                                    gam=c(174,	169,	169,	168,	151,	159),
                                    Linear=c(155.8165, 
                                             151.2541, 
                                             173.7566, 
                                             134.4124, 
                                             222.2537, 
                                             178.4749),
                                    BRF=c(114.1020, 
                                          138.4778, 
                                          124.1394, 
                                          122.1780, 
                                          129.8000, 
                                          127.8298))%>%as.data.table()

Repeat10_True_vs_gamw <- data_visualization10w$Fold <- as.factor(data_visualization10w$Fold)
data_visualization10w <- melt(data_visualization10w)

r10w <- ggplot(data_visualization10w, aes(x=Fold,y=value, colour = variable)) + geom_point(position = position_dodge(width = .3))
#################################################################################
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
install.packages("cowplot")
library(cowplot)

plot_grid(r1w, r2w, r3w,r4w, r5w, r6w, r7w, r8w, r9w, r10w + rremove("x.text"), 
          labels = c("R1", "R2", "R3","R4", "R5", "R6", "R7", "R8", "R9", "R10"), label_size = 9,
          
          ncol = 3, nrow = 5)

#################################################################################




