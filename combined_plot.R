
########################### True_vs_gam ########################################
f1_combined_true_gam <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(157.7012, 126.6715,
                                                                               183.5337, 119.9725, 102.4647,115.3339, 199.6354, 
                                                                               103.2737, 159.4253, 114.0839),
                                   gam=c(168, 169, 164, 167, 172, 174, 
                                         144, 172, 165, 174))%>%as.data.table()



f2_combined_true_gam <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(163.9320, 199.3947, 
                                                                               119.8310, 198.2008,
                                                                               157.7012, 157.5470, 
                                                                               139.5351,
                                                                               192.7260, 123.0092, 
                                                                               129.9887 ),
                                   gam=c(163, 149, 169, 156, 168, 167, 165, 152, 
                                         169, 169))%>%as.data.table()



f3_combined_true_gam <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6715, 163.9320, 
                                                                               153.3165, 115.4788,
                                                                               126.6989, 154.3108, 
                                                                               158.9398,
                                                                               132.5109, 168.3922, 
                                                                               159.0119 ),
                                   gam=c(169, 163, 164, 174, 168, 165, 167, 170, 168, 169))%>%as.data.table()



f4_combined_true_gam <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(199.3947, 102.4647, 
                                                                               158.0301,
                                                                               187.3374, 163.9320, 
                                                                               186.9898, 114.8227,
                                                                               172.9661, 115.0818, 
                                                                               127.9556),
                                   gam=c(149, 172, 167, 160, 163, 163, 174, 167, 174, 168))%>%as.data.table()



f5_combined_true_gam <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(102.4647, 126.6989, 
                                                                               204.0006, 134.6703,
                                                                               199.3947, 121.1657, 
                                                                               107.9045,
                                                                               127.5094, 154.3219, 
                                                                               196.9187),
                                   gam=c(172, 168, 155, 168, 149, 169, 169, 168, 166, 151))%>%as.data.table()



f6_combined_true_gam <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6989, 157.7012, 
                                                                               115.5513, 154.7775,
                                                                               126.6715, 204.1703,
                                                                               159.5956, 165.5157, 
                                                                               200.9931, 186.7654),
                                   gam=c(168, 168, 174, 168, 169, 153, 165, 163, 138, 159))%>%as.data.table()






true_gam_combined <- rbind(f1_combined_true_gam,f2_combined_true_gam,f3_combined_true_gam,
                           f4_combined_true_gam,f5_combined_true_gam,f6_combined_true_gam)%>%
  subset(., select = -Repeat) 




plot_true_gam_combined <- ggplot(true_gam_combined, aes(x=true_value,y=gam, colour = "red")) + 
  geom_point()

################################################################################
#True_vs_linear

f1_combined_true_linear <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(157.7012, 126.6715,
                                                                                  183.5337, 119.9725, 102.4647,115.3339, 199.6354, 
                                                                                  103.2737, 159.4253, 114.0839),
                                      linear=c(170.6970, 142.6296, 218.7514, 132.0992, 131.4606, 149.4847, 222.7222,
                                               128.5130, 174.7683, 155.8165))%>%as.data.table()

f2_combined_true_linear <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(163.9320, 199.3947, 
                                                                                  119.8310, 198.2008,
                                                                                  157.7012, 157.5470, 
                                                                                  139.5351,
                                                                                  192.7260, 123.0092, 
                                                                                  129.9887),
                                      linear=c(168.6241, 221.5714, 135.5495, 209.1405, 170.6970, 170.1434, 145.3886,
                                               215.3353, 136.1395, 151.2541))%>%as.data.table()

f3_combined_true_linear <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6715, 163.9320, 
                                                                                  153.3165, 115.4788,
                                                                                  126.6989, 154.3108, 
                                                                                  158.9398,
                                                                                  132.5109, 168.3922, 
                                                                                  159.0119 ),
                                      linear=c(142.6296, 168.6241, 156.6744, 151.1542, 158.8727, 157.7807, 172.5797,
                                               152.8892, 170.5305, 173.7566))%>%as.data.table()


f4_combined_true_linear  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(199.3947, 102.4647, 
                                                                                   158.0301,
                                                                                   187.3374, 163.9320, 
                                                                                   186.9898, 114.8227,
                                                                                   172.9661, 115.0818, 
                                                                                   127.9556),
                                       linear=c(221.5714, 131.4606, 170.0255, 200.2647, 168.6241, 220.9213, 141.5372,
                                                173.0881, 153.7955, 134.4124 ))%>%as.data.table()


f5_combined_true_linear <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(102.4647, 126.6989, 
                                                                                  204.0006, 134.6703,
                                                                                  199.3947, 121.1657, 
                                                                                  107.9045,
                                                                                  127.5094, 154.3219, 
                                                                                  196.9187),
                                      linear=c(131.4606, 158.8727, 206.6103, 148.0210, 221.5714, 135.5461, 132.4000,
                                               157.3472, 160.6489, 222.2537))%>%as.data.table()


f6_combined_true_linear <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6989, 157.7012, 
                                                                                  115.5513, 154.7775,
                                                                                  126.6715, 204.1703,
                                                                                  159.5956, 165.5157, 
                                                                                  200.9931, 186.7654),
                                      linear=c(158.8727, 170.6970, 147.6641, 167.2167, 142.6296, 210.4565, 167.8797,
                                               177.8860, 231.4632, 178.4749 ))%>%as.data.table()


true_linear_combined <- rbind(f1_combined_true_linear,f2_combined_true_linear,f3_combined_true_linear,
                              f4_combined_true_linear,f5_combined_true_linear,f6_combined_true_linear)%>%
  subset(., select = -Repeat)




plot_true_linear_combined <- ggplot(true_linear_combined, aes(x=true_value,y=linear, colour = "red")) + 
  geom_point()
################################################################################
#true_VS_BRF 

f1_combined_true_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(157.7012, 126.6715,
                                                                               183.5337, 119.9725, 102.4647,115.3339, 199.6354, 
                                                                               103.2737, 159.4253, 114.0839),
                                   BRF=c(121.9160, 124.9218, 115.0424, 125.2462, 123.5394, 118.0528, 131.1510,
                                         125.8475, 123.9515, 114.1020))%>%as.data.table()





f2_combined_true_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(163.9320, 199.3947, 
                                                                               119.8310, 198.2008,
                                                                               157.7012, 157.5470, 
                                                                               139.5351,
                                                                               192.7260, 123.0092, 
                                                                               129.9887),
                                   BRF=c(124.0717, 129.5126, 114.9593, 114.8074, 138.1111, 110.1522, 119.6732,
                                         126.2649, 115.3554, 138.4778 ))%>%as.data.table()


f3_combined_true_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6715, 163.9320, 
                                                                               153.3165, 115.4788,
                                                                               126.6989, 154.3108, 
                                                                               158.9398,
                                                                               132.5109, 168.3922, 
                                                                               159.0119 ),
                                   BRF=c(135.9551, 109.8994, 117.9030, 129.0142, 127.1454, 122.7553, 122.6067,
                                         133.2291, 121.0232, 124.1394))%>%as.data.table()

f4_combined_true_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(199.3947, 102.4647, 
                                                                               158.0301,
                                                                               187.3374, 163.9320, 
                                                                               186.9898, 114.8227,
                                                                               172.9661, 115.0818, 
                                                                               127.9556),
                                   BRF=c(125.5093, 119.0610, 130.4493, 123.4694, 122.8106, 125.3664, 120.8167,
                                         122.5379, 125.3844, 122.1780))%>%as.data.table()


f5_combined_true_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(102.4647, 126.6989, 
                                                                               204.0006, 134.6703,
                                                                               199.3947, 121.1657, 
                                                                               107.9045,
                                                                               127.5094, 154.3219, 
                                                                               196.9187),
                                   BRF=c(127.0380, 112.0528, 123.7308, 112.6788, 129.6000, 120.1910, 114.9918,
                                         113.1987, 137.2556, 129.8000))%>%as.data.table()

f6_combined_true_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),true_value=c(126.6989, 157.7012, 
                                                                               115.5513, 154.7775,
                                                                               126.6715, 204.1703,
                                                                               159.5956, 165.5157, 
                                                                               200.9931, 186.7654),
                                   BRF=c(114.0638, 126.2424, 124.3256, 117.0909, 113.7755, 127.3569, 127.1818,
                                         124.2353, 129.2000, 127.8298))%>%as.data.table()

true_BRF_combined <- rbind(f1_combined_true_BRF,f2_combined_true_BRF,f3_combined_true_BRF,
                           f4_combined_true_BRF,f5_combined_true_BRF,f6_combined_true_BRF)%>%
  subset(., select = -Repeat)




plot_true_BRF_combined <- ggplot(true_BRF_combined, aes(x=true_value,y=BRF, colour = "red")) + 
  geom_point()


################################################################################
#gam_vs_linear

f1_combined_gam_linear <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(168, 169, 164, 167, 172, 174, 
                                                                          144, 172, 165, 174),
                                     linear=c(170.6970, 142.6296, 218.7514, 132.0992, 131.4606, 149.4847, 222.7222,
                                              128.5130, 174.7683, 155.8165))%>%as.data.table()


f2_combined_gam_linear  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(163, 149, 169, 156, 168, 167, 165, 152, 
                                                                           169, 169),
                                      linear=c(168.6241, 221.5714, 135.5495, 209.1405, 170.6970, 170.1434, 145.3886,
                                               215.3353, 136.1395, 151.2541 ))%>%as.data.table()


f3_combined_gam_linear  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(169, 163, 164, 174, 168, 165, 167, 170, 168, 169),
                                      
                                      linear=c(142.6296, 168.6241, 156.6744, 151.1542, 158.8727, 157.7807, 172.5797,
                                               152.8892, 170.5305, 173.7566))%>%as.data.table()

f4_combined_gam_linear  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(149, 172, 167, 160, 163, 163, 174, 167, 174, 168),
                                      
                                      linear=c(221.5714, 131.4606, 170.0255, 200.2647, 168.6241, 220.9213, 141.5372,
                                               173.0881, 153.7955, 134.4124))%>%as.data.table()

f5_combined_gam_linear  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(172, 168, 155, 168, 149, 169, 169, 168, 166, 151),
                                      
                                      linear=c(131.4606, 158.8727, 206.6103, 148.0210, 221.5714, 135.5461, 132.4000,
                                               157.3472, 160.6489, 222.2537))%>%as.data.table()

f6_combined_gam_linear <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(168, 168, 174, 168, 169, 153, 165, 163, 138, 159),
                                     
                                     linear=c(158.8727, 170.6970, 147.6641, 167.2167, 142.6296,
                                              210.4565, 167.8797, 177.8860, 231.4632, 178.4749 ))%>%as.data.table()


gam_linear_combined <- rbind(f1_combined_gam_linear,f2_combined_gam_linear,f3_combined_gam_linear,
                             f4_combined_gam_linear,f5_combined_gam_linear,f6_combined_gam_linear)%>%
  subset(., select = -Repeat)




plot_gam_linear_combined <- ggplot(gam_linear_combined, aes(x=gam,y=linear, colour = "red")) + 
  geom_point()

################################################################################
#gam_vs_BRF

f1_combined_gam_BRF  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(168, 169, 164, 167, 172, 174, 
                                                                        144, 172, 165, 174),
                                   BRF=c(121.9160, 124.9218, 115.0424, 125.2462, 123.5394, 118.0528, 131.1510,
                                         125.8475, 123.9515, 114.1020))%>%as.data.table()



f2_combined_gam_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(163, 149, 169, 156, 168, 167, 165, 152, 
                                                                       169, 169),
                                  BRF=c(124.0717, 129.5126, 114.9593, 114.8074, 138.1111, 110.1522, 119.6732,
                                        126.2649, 115.3554, 138.4778))%>%as.data.table()



f3_combined_gam_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(169, 163, 164, 174, 168, 165, 167, 170, 168, 169),
                                  
                                  BRF=c(135.9551, 109.8994, 117.9030, 129.0142, 127.1454, 122.7553, 122.6067,
                                        133.2291, 121.0232, 124.1394))%>%as.data.table()




f4_combined_gam_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(149, 172, 167, 160, 163, 163, 174, 167, 174, 168),
                                  
                                  BRF=c(125.5093, 119.0610, 130.4493, 123.4694, 122.8106, 125.3664, 120.8167,
                                        122.5379, 125.3844, 122.1780))%>%as.data.table()



f5_combined_gam_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(172, 168, 155, 168, 149, 169, 169, 168, 166, 151),
                                  
                                  BRF=c(127.0380, 112.0528, 123.7308, 112.6788, 129.6000, 120.1910, 114.9918,
                                        113.1987, 137.2556, 129.8000))%>%as.data.table()



f6_combined_gam_BRF <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(168, 168, 174, 168, 169, 153, 165, 163, 138, 159),
                                  
                                  BRF=c(114.0638, 126.2424, 124.3256, 117.0909, 113.7755, 127.3569, 127.1818,
                                        124.2353, 129.2000, 127.8298))%>%as.data.table()



gam_BRF_combined <- rbind(f1_combined_gam_BRF,f2_combined_gam_BRF,f3_combined_gam_BRF,
                          f4_combined_gam_BRF,f5_combined_gam_BRF,f6_combined_gam_BRF)%>%
  subset(., select = -Repeat)




plot_gam_BRF_combined <- ggplot(gam_BRF_combined, aes(x=gam,y=BRF, colour = "red")) + 
  geom_point()

################################################################################


if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
install.packages("cowplot")
library(cowplot)

All_plot <- plot_grid(plot_true_gam_combined, plot_true_linear_combined, plot_true_BRF_combined, plot_gam_linear_combined, 
                      plot_gam_BRF_combined + rremove("x.text"), 
                      labels = c("true_gam", "true_linear", "true_BRF","gam_linear", "gam_BRF"), 
                      label_size = 9,
                      
                      ncol = 2, nrow = 3)


################################################################################
















