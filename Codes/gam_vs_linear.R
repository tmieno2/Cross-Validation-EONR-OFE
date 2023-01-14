####################### gam_VS_linear  #######################################
################################################################################
f1_gl <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(168, 169, 164, 167, 172, 174, 
                                                         144, 172, 165, 174),
                    linear=c(170.6970, 142.6296, 218.7514, 132.0992, 131.4606, 149.4847, 222.7222,
                             128.5130, 174.7683, 155.8165))%>%as.data.table()



fold_1_gam_vs_linear<- f1_gl$Repeat <- as.factor(f1_gl$Repeat)
f1_gl <- melt(f1_gl)
plot(as.numeric(f1_gl$Repeat),f1_gl$value)

f1_gl_plot <- ggplot(f1_gl, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################

f2_gl <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(163, 149, 169, 156, 168, 167, 165, 152, 
                                                         169, 169),
                    linear=c(168.6241, 221.5714, 135.5495, 209.1405, 170.6970, 170.1434, 145.3886,
                             215.3353, 136.1395, 151.2541 ))%>%as.data.table()



fold_2_gam_vs_linear<- f2_gl$Repeat <- as.factor(f2_gl$Repeat)
f2_gl <- melt(f2_gl)
plot(as.numeric(f2_gl$Repeat),f2_gl$value)

f2_gl_plot <- ggplot(f2_gl, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


#################################################################################

f3_gl <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(169, 163, 164, 174, 168, 165, 167, 170, 168, 169),
                    
                    linear=c(142.6296, 168.6241, 156.6744, 151.1542, 158.8727, 157.7807, 172.5797,
                             152.8892, 170.5305, 173.7566))%>%as.data.table()



fold_3_gam_vs_linear<- f3_gl$Repeat <- as.factor(f3_gl$Repeat)
f3_gl <- melt(f3_gl)
plot(as.numeric(f3_gl$Repeat),f3_gl$value)

f3_gl_plot <- ggplot(f3_gl, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


#################################################################################
f4_gl <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(149, 172, 167, 160, 163, 163, 174, 167, 174, 168),
                    
                    linear=c(221.5714, 131.4606, 170.0255, 200.2647, 168.6241, 220.9213, 141.5372,
                             173.0881, 153.7955, 134.4124))%>%as.data.table()



fold_4_gam_vs_linear<- f4_gl$Repeat <- as.factor(f4_gl$Repeat)
f4_gl <- melt(f4_gl)
plot(as.numeric(f4_gl$Repeat),f4_gl$value)

f4_gl_plot <- ggplot(f4_gl, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))

#################################################################################
f5_gl <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(172, 168, 155, 168, 149, 169, 169, 168, 166, 151),
                    
                    linear=c(131.4606, 158.8727, 206.6103, 148.0210, 221.5714, 135.5461, 132.4000,
                             157.3472, 160.6489, 222.2537))%>%as.data.table()



fold_5_gam_vs_linear<- f5_gl$Repeat <- as.factor(f5_gl$Repeat)
f5_gl <- melt(f5_gl)
plot(as.numeric(f5_gl$Repeat),f5_gl$value)

f5_gl_plot <- ggplot(f5_gl, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


#################################################################################
f6_gl <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=c(168, 168, 174, 168, 169, 153, 165, 163, 138, 159),
                    
                    linear=c(158.8727, 170.6970, 147.6641, 167.2167, 142.6296,
                             210.4565, 167.8797, 177.8860, 231.4632, 178.4749 ))%>%as.data.table()



fold_6_gam_vs_linear<- f6_gl$Repeat <- as.factor(f6_gl$Repeat)
f6_gl <- melt(f6_gl)
plot(as.numeric(f6_gl$Repeat),f6_gl$value)

f6_gl_plot <- ggplot(f6_gl, aes(x=Repeat,y=value, colour = variable)) + 
  geom_point(position = position_dodge(width = .3))


################################################################################
################################################################################
#gam_vs_linear

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
install.packages("cowplot")
library(cowplot)

gam_vs_linear_fold_by_fold <- plot_grid(f1_gl_plot, f2_gl_plot, f3_gl_plot, f4_gl_plot,
                                        f5_gl_plot, f6_gl_plot + rremove("x.text"), 
                                        labels = c("f1", "f2", "f3","f4", "f5", "f6"), 
                                        label_size = 9,
                                        
                                        ncol = 2, nrow = 3)


################################################################################






