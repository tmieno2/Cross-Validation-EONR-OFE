
####################### True_vs_gam ############################################
################################################################################
f1 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(157.7012, 126.6715,
                                                             183.5337, 119.9725, 102.4647,115.3339, 199.6354, 
                                                             103.2737, 159.4253, 114.0839),
                 gam=c(168, 169, 164, 167, 172, 174, 
                       144, 172, 165, 174))%>%as.data.table()



fold_1_True_vs_gam <- f1$Repeat <- as.factor(f1$Repeat)
f1 <- melt(f1)
plot(as.numeric(f1$Repeat),f1$value)

f1_plot <- ggplot(f1, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################

f2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(163.9320, 199.3947, 
                                                             119.8310, 198.2008,
                                                             157.7012, 157.5470, 
                                                             139.5351,
                                                             192.7260, 123.0092, 
                                                             129.9887 ),
                 gam=c(163, 149, 169, 156, 168, 167, 165, 152, 
                       169, 169))%>%as.data.table()



fold_2_True_vs_gam <- f2$Repeat <- as.factor(f2$Repeat)
f2 <- melt(f2)
plot(as.numeric(f2$Repeat),f2$value)

f2_plot <- ggplot(f2, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


#################################################################################
f3 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6715, 163.9320, 
                                                             153.3165, 115.4788,
                                                             126.6989, 154.3108, 
                                                             158.9398,
                                                             132.5109, 168.3922, 
                                                             159.0119 ),
                 gam=c(169, 163, 164, 174, 168, 165, 167, 170, 168, 169))%>%as.data.table()



fold_3_True_vs_gam <- f3$Repeat <- as.factor(f3$Repeat)
f3 <- melt(f3)
plot(as.numeric(f3$Repeat),f3$value)

f3_plot <- ggplot(f3, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f4 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(199.3947, 102.4647, 
                                                             158.0301,
                                                             187.3374, 163.9320, 
                                                             186.9898, 114.8227,
                                                             172.9661, 115.0818, 
                                                             127.9556),
                 gam=c(149, 172, 167, 160, 163, 163, 174, 167, 174, 168))%>%as.data.table()



fold_4_True_vs_gam <- f4$Repeat <- as.factor(f4$Repeat)
f4 <- melt(f4)
plot(as.numeric(f4$Repeat),f4$value)

f4_plot <- ggplot(f4, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f5 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(102.4647, 126.6989, 
                                                             204.0006, 134.6703,
                                                             199.3947, 121.1657, 
                                                             107.9045,
                                                             127.5094, 154.3219, 
                                                             196.9187),
                 gam=c(172, 168, 155, 168, 149, 169, 169, 168, 166, 151))%>%as.data.table()



fold_5_True_vs_gam <- f5$Repeat <- as.factor(f5$Repeat)
f5 <- melt(f5)
plot(as.numeric(f5$Repeat),f5$value)

f5_plot <- ggplot(f5, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f6 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6989, 157.7012, 
                                                             115.5513, 154.7775,
                                                             126.6715, 204.1703,
                                                             159.5956, 165.5157, 
                                                             200.9931, 186.7654),
                 gam=c(168, 168, 174, 168, 169, 153, 165, 163, 138, 159))%>%as.data.table()



fold_6_True_vs_gam <- f6$Repeat <- as.factor(f6$Repeat)
f6 <- melt(f6)
plot(as.numeric(f6$Repeat),f6$value)

f6_plot <- ggplot(f6, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

################################################################################
################################################################################

#True_vs_gam

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
install.packages("cowplot")
library(cowplot)

true_vs_gam_fold_by_fold <- plot_grid(f1_plot, f2_plot, f3_plot, f4_plot, f5_plot, 
                                      f6_plot + rremove("x.text"), 
                                      labels = c("f1", "f2", "f3","f4", "f5", "f6"), 
                                      label_size = 9,
                                      
                                      ncol = 2, nrow = 3)


################################################################################












