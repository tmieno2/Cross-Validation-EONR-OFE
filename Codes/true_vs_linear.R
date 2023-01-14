#######################  true_vs_linear #######################################
################################################################################
f1_lt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(157.7012, 126.6715,
                                                                183.5337, 119.9725, 102.4647,115.3339, 199.6354, 
                                                                103.2737, 159.4253, 114.0839),
                    linear=c(170.6970, 142.6296, 218.7514, 132.0992, 131.4606, 149.4847, 222.7222,
                             128.5130, 174.7683, 155.8165))%>%as.data.table()



fold_1_True_vs_linear<- f1_lt$Repeat <- as.factor(f1_lt$Repeat)
f1_lt <- melt(f1_lt)
plot(as.numeric(f1_lt$Repeat),f1_lt$value)

f1_lt_plot <- ggplot(f1_lt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################

f2_lt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(163.9320, 199.3947, 
                                                                119.8310, 198.2008,
                                                                157.7012, 157.5470, 
                                                                139.5351,
                                                                192.7260, 123.0092, 
                                                                129.9887),
                    linear=c(168.6241, 221.5714, 135.5495, 209.1405, 170.6970, 170.1434, 145.3886,
                             215.3353, 136.1395, 151.2541))%>%as.data.table()



fold_2_True_vs_linear<- f2_lt$Repeat <- as.factor(f2_lt$Repeat)
f2_lt <- melt(f2_lt)
plot(as.numeric(f2_lt$Repeat),f2_lt$value)

f2_lt_plot <- ggplot(f2_lt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


#################################################################################
f3_lt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6715, 163.9320, 
                                                                153.3165, 115.4788,
                                                                126.6989, 154.3108, 
                                                                158.9398,
                                                                132.5109, 168.3922, 
                                                                159.0119 ),
                    linear=c(142.6296, 168.6241, 156.6744, 151.1542, 158.8727, 157.7807, 172.5797,
                             152.8892, 170.5305, 173.7566))%>%as.data.table()



fold_3_True_vs_linear <- f3_lt$Repeat <- as.factor(f3_lt$Repeat)
f3_lt <- melt(f3_lt)
plot(as.numeric(f3_lt$Repeat),f3_lt$value)

f3_lt_plot <- ggplot(f3_lt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f4_lt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(199.3947, 102.4647, 
                                                                158.0301,
                                                                187.3374, 163.9320, 
                                                                186.9898, 114.8227,
                                                                172.9661, 115.0818, 
                                                                127.9556),
                    linear=c(221.5714, 131.4606, 170.0255, 200.2647, 168.6241, 220.9213, 141.5372,
                             173.0881, 153.7955, 134.4124 ))%>%as.data.table()



fold_4_True_vs_linear <- f4_lt$Repeat <- as.factor(f4_lt$Repeat)
f4_lt <- melt(f4_lt)
plot(as.numeric(f4_lt$Repeat),f4_lt$value)

f4_lt_plot <- ggplot(f4_lt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f5_lt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(102.4647, 126.6989, 
                                                                204.0006, 134.6703,
                                                                199.3947, 121.1657, 
                                                                107.9045,
                                                                127.5094, 154.3219, 
                                                                196.9187),
                    linear=c(131.4606, 158.8727, 206.6103, 148.0210, 221.5714, 135.5461, 132.4000,
                             157.3472, 160.6489, 222.2537))%>%as.data.table()



fold_5_True_vs_linear <- f5_lt$Repeat <- as.factor(f5_lt$Repeat)
f5_lt <- melt(f5_lt)
plot(as.numeric(f5_lt$Repeat),f5_lt$value)

f5_lt_plot <- ggplot(f5_lt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f6_lt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6989, 157.7012, 
                                                                115.5513, 154.7775,
                                                                126.6715, 204.1703,
                                                                159.5956, 165.5157, 
                                                                200.9931, 186.7654),
                    linear=c(158.8727, 170.6970, 147.6641, 167.2167, 142.6296, 210.4565, 167.8797,
                             177.8860, 231.4632, 178.4749 ))%>%as.data.table()



fold_6_True_vs_linear <- f6_lt$Repeat <- as.factor(f6_lt$Repeat)
f6_lt <- melt(f6_lt)
plot(as.numeric(f6_lt$Repeat),f6_lt$value)

f6_lt_plot <- ggplot(f6_lt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
################################################################################
#True_vs_linear

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
install.packages("cowplot")
library(cowplot)

true_vs_linear_fold_by_fold <- plot_grid(f1_lt_plot, f2_lt_plot, f3_lt_plot, f4_lt_plot,
                                         f5_lt_plot, f6_lt_plot + rremove("x.text"), 
                                         labels = c("f1", "f2", "f3","f4", "f5", "f6"), 
                                         label_size = 9,
                                         
                                         ncol = 2, nrow = 3)


################################################################################






