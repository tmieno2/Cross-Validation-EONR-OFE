####################### gam_VS_BRF  #######################################
################################################################################
f1_gb <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(168, 169, 164, 167, 172, 174, 
                                                         144, 172, 165, 174),
                    BRF=c(121.9160, 124.9218, 115.0424, 125.2462, 123.5394, 118.0528, 131.1510,
                          125.8475, 123.9515, 114.1020))%>%as.data.table()



fold_1_gam_vs_BRF<- f1_gb$Repeat <- as.factor(f1_gb$Repeat)
f1_gb <- melt(f1_gb)
plot(as.numeric(f1_gb$Repeat),f1_gb$value)

f1_gb_plot <- ggplot(f1_gb, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################

f2_gb <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(163, 149, 169, 156, 168, 167, 165, 152, 
                                                         169, 169),
                    BRF=c(124.0717, 129.5126, 114.9593, 114.8074, 138.1111, 110.1522, 119.6732,
                          126.2649, 115.3554, 138.4778))%>%as.data.table()



fold_2_gam_vs_BRF<- f2_gb$Repeat <- as.factor(f2_gb$Repeat)
f2_gb <- melt(f2_gb)
plot(as.numeric(f2_gb$Repeat),f2_gb$value)

f2_gb_plot <- ggplot(f2_gb, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


#################################################################################

f3_gb <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(169, 163, 164, 174, 168, 165, 167, 170, 168, 169),
                    
                    BRF=c(135.9551, 109.8994, 117.9030, 129.0142, 127.1454, 122.7553, 122.6067,
                          133.2291, 121.0232, 124.1394))%>%as.data.table()



fold_3_gam_vs_BRF<- f3_gb$Repeat <- as.factor(f3_gb$Repeat)
f3_gb <- melt(f3_gb)
plot(as.numeric(f3_gb$Repeat),f3_gb$value)

f3_gb_plot <- ggplot(f3_gb, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


#################################################################################
f4_gb <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(149, 172, 167, 160, 163, 163, 174, 167, 174, 168),
                    
                    BRF=c(125.5093, 119.0610, 130.4493, 123.4694, 122.8106, 125.3664, 120.8167,
                          122.5379, 125.3844, 122.1780))%>%as.data.table()



fold_4_gam_vs_BRF<- f4_gb$Repeat <- as.factor(f4_gb$Repeat)
f4_gb <- melt(f4_gb)
plot(as.numeric(f4_gb$Repeat),f4_gb$value)

f4_gb_plot <- ggplot(f4_gb, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f5_gb <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(172, 168, 155, 168, 149, 169, 169, 168, 166, 151),
                    
                    BRF=c(127.0380, 112.0528, 123.7308, 112.6788, 129.6000, 120.1910, 114.9918,
                          113.1987, 137.2556, 129.8000))%>%as.data.table()



fold_5_gam_vs_BRF<- f5_gb$Repeat <- as.factor(f5_gb$Repeat)
f5_gb <- melt(f5_gb)
plot(as.numeric(f5_gb$Repeat),f5_gb$value)

f5_gb_plot <- ggplot(f5_gb, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


#################################################################################
f6_gb <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(168, 168, 174, 168, 169, 153, 165, 163, 138, 159),
                    
                    BRF=c(114.0638, 126.2424, 124.3256, 117.0909, 113.7755, 127.3569, 127.1818,
                          124.2353, 129.2000, 127.8298))%>%as.data.table()



fold_6_gam_vs_BRF<- f6_gb$Repeat <- as.factor(f6_gb$Repeat)
f6_gb <- melt(f6_gb)
plot(as.numeric(f6_gb$Repeat),f6_gb$value)

f6_gb_plot <- ggplot(f6_gb, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


################################################################################
################################################################################
#gam_vs_BRF

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
install.packages("cowplot")
library(cowplot)

gam_vs_BRF_fold_by_fold <- plot_grid(f1_gb_plot, f2_gb_plot, f3_gb_plot, f4_gb_plot,
                                     f5_gb_plot, f6_gb_plot + rremove("x.text"), 
                                     labels = c("f1", "f2", "f3","f4", "f5", "f6"), 
                                     label_size = 9,
                                     
                                     ncol = 2, nrow = 3)


################################################################################






