#######################  true_VS_BRF #######################################
################################################################################
f1_bt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(157.7012, 126.6715,
                                                                183.5337, 119.9725, 102.4647,115.3339, 199.6354, 
                                                                103.2737, 159.4253, 114.0839),
                    BRF=c(121.9160, 124.9218, 115.0424, 125.2462, 123.5394, 118.0528, 131.1510,
                          125.8475, 123.9515, 114.1020))%>%as.data.table()



fold_1_True_vs_BRF<- f1_bt$Repeat <- as.factor(f1_bt$Repeat)
f1_bt <- melt(f1_bt)
plot(as.numeric(f1_bt$Repeat),f1_bt$value)

f1_bt_plot <- ggplot(f1_bt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################

f2_bt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(163.9320, 199.3947, 
                                                                119.8310, 198.2008,
                                                                157.7012, 157.5470, 
                                                                139.5351,
                                                                192.7260, 123.0092, 
                                                                129.9887),
                    BRF=c(124.0717, 129.5126, 114.9593, 114.8074, 138.1111, 110.1522, 119.6732,
                          126.2649, 115.3554, 138.4778 ))%>%as.data.table()



fold_2_True_vs_BRF<- f2_bt$Repeat <- as.factor(f2_bt$Repeat)
f2_bt <- melt(f2_bt)
plot(as.numeric(f2_bt$Repeat),f2_bt$value)

f2_bt_plot <- ggplot(f2_bt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


#################################################################################
f3_bt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6715, 163.9320, 
                                                                153.3165, 115.4788,
                                                                126.6989, 154.3108, 
                                                                158.9398,
                                                                132.5109, 168.3922, 
                                                                159.0119 ),
                    BRF=c(135.9551, 109.8994, 117.9030, 129.0142, 127.1454, 122.7553, 122.6067,
                          133.2291, 121.0232, 124.1394))%>%as.data.table()



fold_3_True_vs_BRF <- f3_bt$Repeat <- as.factor(f3_bt$Repeat)
f3_bt <- melt(f3_bt)
plot(as.numeric(f3_bt$Repeat),f3_bt$value)

f3_bt_plot <- ggplot(f3_bt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f4_bt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(199.3947, 102.4647, 
                                                                158.0301,
                                                                187.3374, 163.9320, 
                                                                186.9898, 114.8227,
                                                                172.9661, 115.0818, 
                                                                127.9556),
                    BRF=c(125.5093, 119.0610, 130.4493, 123.4694, 122.8106, 125.3664, 120.8167,
                          122.5379, 125.3844, 122.1780))%>%as.data.table()



fold_4_True_vs_BRF <- f4_bt$Repeat <- as.factor(f4_bt$Repeat)
f4_bt <- melt(f4_bt)
plot(as.numeric(f4_bt$Repeat),f4_bt$value)

f4_bt_plot <- ggplot(f4_bt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f5_bt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(102.4647, 126.6989, 
                                                                204.0006, 134.6703,
                                                                199.3947, 121.1657, 
                                                                107.9045,
                                                                127.5094, 154.3219, 
                                                                196.9187),
                    BRF=c(127.0380, 112.0528, 123.7308, 112.6788, 129.6000, 120.1910, 114.9918,
                          113.1987, 137.2556, 129.8000))%>%as.data.table()



fold_5_True_vs_BRF <- f5_bt$Repeat <- as.factor(f5_bt$Repeat)
f5_bt <- melt(f5_bt)
plot(as.numeric(f5_bt$Repeat),f5_bt$value)

f5_bt_plot <- ggplot(f5_bt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f6_bt <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6989, 157.7012, 
                                                                115.5513, 154.7775,
                                                                126.6715, 204.1703,
                                                                159.5956, 165.5157, 
                                                                200.9931, 186.7654),
                    BRF=c(114.0638, 126.2424, 124.3256, 117.0909, 113.7755, 127.3569, 127.1818,
                          124.2353, 129.2000, 127.8298))%>%as.data.table()



fold_6_True_vs_BRF <- f6_bt$Repeat <- as.factor(f6_bt$Repeat)
f6_bt <- melt(f6_bt)
plot(as.numeric(f6_bt$Repeat),f6_bt$value)

f6_bt_plot <- ggplot(f6_bt, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

################################################################################
################################################################################
#True_vs_BRF

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
install.packages("cowplot")
library(cowplot)

true_vs_BRF_fold_by_fold <- plot_grid(f1_bt_plot, f2_bt_plot, f3_bt_plot, f4_bt_plot,
                                      f5_bt_plot, f6_bt_plot + rremove("x.text"), 
                                      labels = c("f1", "f2", "f3","f4", "f5", "f6"), 
                                      label_size = 9,
                                      
                                      ncol = 2, nrow = 3)


################################################################################






