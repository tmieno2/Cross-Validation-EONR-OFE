data <- readRDS("Shared/Data/all_sim_data.rds")

#data field #1
data <- data[3, ]$reg_data[[1]]$data[[1]]
#data <- data[3, ]$reg_data[[1]]$data[[2]]


# X = 
#   theta_b2_2  theta_b2_1 theta_b1_2 theta_b1_1
# plateau_2_1 plateau_2_2 plateau_1_1 plateau_1_2
# Nk_2_1   Nk_2_2   Nk_1_1   Nk_1_2
# T = N_tgt
# data[, unique(N_tgt)] 
# data[, N_tgt] 

################################################################################
# BRF for 10 rep, S_learner  

install.packages("parallel")
install.packages("sf")
library(data.table)
library(tidyverse)
library(sf)
library(ranger)
library(parallel)
library(grf)
library(rpart)
library(rattle)
library(wooldridge)
library(spatialsample)
library(patchwork)
library(mgcv)
library(gstat)
################################################################################
library(data.table)
library(tidyverse)
data <- readRDS("Shared/Data/all_sim_data.rds")

names(data)
data <- data[3, ]$reg_data[[1]]$data[[1]]

################################################################################
data_to_work_with <- data[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                              "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                              "plateau_1_2" ,"yield" , "X","Y" ,  "N_tgt","b1","b2","Nk"  )]
# %>% tibble::rowid_to_column(., "id")


data_sf_2 <- st_as_sf(data_to_work_with, coords = c("X", "Y"))

################################################################################
try <- function(n){
  
  skcv_folds_try <- spatial_clustering_cv(data_sf_2, v = 6) 
  
  scv_try<-
    skcv_folds_try %>% 
    rowwise() %>% 
    mutate(
      training_data = list(analysis(splits)),
      test_data = list(assessment(splits))
      
    )
  data_to_re <- scv_try
}

list_of_repeat <- lapply(1:10, try)

#################################################################################
#BRF

l <- function(n){ 
  yy <- list_of_repeat[n]%>%data.frame()
  
  brf <- lapply(1:6, function(i){
    
    train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
    
    test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()%>%.[,-c(13,15,16,17)] 
    
    # boosted regression forest train   
    
    X <-as.matrix(train_data)%>%.[,-c(13,15,16,17)]
    Y <- train_data$yield
    
    boosted.forest <- boosted_regression_forest(X, Y)
    
    # boosted forest predict 
    EONR_1 <- lapply(1:nrow(test_data), function(i){
      row=1
      times = 134
      #prediction for each cell
      test_data_cell <- test_data[i,]%>% subset(., select=-c(N_tgt))%>% 
        .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
      
      pred <- predict(boosted.forest, test_data_cell)$predictions
      
      test_data_cell$y_hat <- pred
      
      test_data_cell <- test_data_cell%>% mutate(profit=(pCorn*y_hat)-(pN*N_tgt))
      
      
      EONR <- test_data_cell [which.max(profit), ]$N_tgt  #each cell
      data_to_return <- data_frame(EONR=EONR)  #each cell
      return(data_to_return)
      
    })%>%unlist()%>% data_frame()
    
    mean_each_fold <- mean(EONR_1$.)
    data_to_r <- data_frame(ff=mean_each_fold)
    return(data_to_r)
  }
  )%>%unlist%>%data_frame()  
  
  
  data_to_re <- brf
  return(data_to_re)
}


#pCorn=0.246063
#pN=2.204624
brf_results <- mclapply(1:10, l, mc.cores = detectCores() - 1)


#################################################################################
df_rep1<-  brf_results [1]%>%data.frame()%>% setnames(.,"repeat1")
df_rep2 <- brf_results [2]%>%data.frame()%>% setnames(.,"repeat2")
df_rep3 <- brf_results [3]%>%data.frame()%>% setnames(.,"repeat3")
df_rep4 <- brf_results [4]%>%data.frame()%>% setnames(.,"repeat4")
df_rep5 <- brf_results [5]%>%data.frame()%>% setnames(.,"repeat5")
df_rep6 <- brf_results [6]%>%data.frame()%>% setnames(.,"repeat6")
df_rep7 <- brf_results [7]%>%data.frame()%>% setnames(.,"repeat7")
df_rep8 <- brf_results [8]%>%data.frame()%>% setnames(.,"repeat8")
df_rep9 <- brf_results [9]%>%data.frame()%>% setnames(.,"repeat9")
df_rep10 <-brf_results [10]%>%data.frame()%>% setnames(.,"repeat10")

BRF_folds_2 <- cbind(df_rep1,df_rep2,df_rep3,df_rep4,df_rep5,df_rep6,df_rep7,df_rep8,df_rep9,df_rep10)%>%
  mutate(mean_of_repeats=rowMeans(.))

#################################################################################

# Linear 10 rep, S_learner

linear_function <- function(n){ 
  yy <- list_of_repeat[n]%>%data.frame()
  
  linear_reg <- lapply(1:6, function(i){
    
    train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
    
    test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()
    
    # linear regression  train   
    
    linear <- lm(yield ~  theta_b2_2*N_tgt + theta_b2_1*N_tgt + 
                   theta_b1_2*N_tgt + theta_b1_1*N_tgt + Nk_2_1*N_tgt + Nk_2_2*N_tgt + Nk_1_1*N_tgt + Nk_1_2*N_tgt + plateau_2_1*N_tgt +
                   plateau_2_2*N_tgt + plateau_1_1*N_tgt + plateau_1_2*N_tgt + I(N_tgt^2),
                 data = train_data)
    
    
    
    # linear predict 
    EONR_1 <- lapply(1:nrow(test_data), function(i){
      row=1
      times = 134
      #prediction for each cell
      test_data_cell <- test_data[i,]%>% subset(., select=-c(N_tgt))%>% 
        .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
      
      predict_linear <-  linear %>% predict(test_data_cell)
      
      
      test_data_cell$y_hat <-  predict_linear
      
      test_data_cell <- test_data_cell%>% mutate(profit=(pCorn*y_hat)-(pN*N_tgt))
      
      
      EONR <- test_data_cell [which.max(profit), ]$N_tgt  #each cell
      data_to_retur <- data_frame(EONR=EONR)  #each cell
      return(data_to_retur)
      
    })%>%unlist()%>% data_frame()
    
    mean_each_fold <- mean(EONR_1$.)
    data_to_rn <- data_frame(ff=mean_each_fold)
    return(data_to_rn)
  }
  )%>%unlist%>%data_frame()  
  
  
  data_to_reu <- linear_reg
  return(data_to_reu)
}

linear_results_second_run_2 <- mclapply(1:10, linear_function, mc.cores = detectCores() - 1)



#################################################################################

lin1<- linear_results_second_run_2[1]%>%data.frame()%>% setnames(.,"repeat1")
lin2 <- linear_results_second_run_2[2]%>%data.frame()%>% setnames(.,"repeat2")
lin3 <- linear_results_second_run_2[3]%>%data.frame()%>% setnames(.,"repeat3")
lin4 <- linear_results_second_run_2[4]%>%data.frame()%>% setnames(.,"repeat4")
lin5 <- linear_results_second_run_2[5]%>%data.frame()%>% setnames(.,"repeat5")
lin6 <- linear_results_second_run_2[6]%>%data.frame()%>% setnames(.,"repeat6")
lin7 <- linear_results_second_run_2[7]%>%data.frame()%>% setnames(.,"repeat7")
lin8 <- linear_results_second_run_2[8]%>%data.frame()%>% setnames(.,"repeat8")
lin9 <- linear_results_second_run_2[9]%>%data.frame()%>% setnames(.,"repeat9")
lin10 <-linear_results_second_run_2[10]%>%data.frame()%>% setnames(.,"repeat10")

lin_folds_2 <- cbind(lin1,lin2,lin3,lin4,lin5,
                     lin6,lin7,lin8,lin9,lin10)%>%
  mutate(mean_of_repeats=rowMeans(.))

#################################################################################
#RF

rf_with_repeat <- function(n){ 
  yy <- list_of_repeat[n]%>%data.frame()
  
  rf <- lapply(1:6, function(i){
    
    train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
    
    test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()%>%.[,-c(13,15,16,17)] 
    
    # random forest train   
    
    RF <- 
      ranger(
        yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
          Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2+ N_tgt,     
        data = train_data
      )
    
    # random forest predict 
    EONR_1 <- lapply(1:nrow(test_data), function(i){
      row=1
      times = 134
      #prediction for each cell
      test_data_cell <- test_data[i,]%>% subset(., select=-c(N_tgt))%>% 
        .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
      
      pred <- predict(RF, test_data_cell)$predictions
      
      test_data_cell$y_hat <- pred
      
      test_data_cell <- test_data_cell%>% mutate(profit=(pCorn*y_hat)-(pN*N_tgt))
      
      
      EONR <- test_data_cell [which.max(profit), ]$N_tgt  #each cell
      data_to_return <- data_frame(EONR=EONR)  #each cell
      return(data_to_return)
      
    })%>%unlist()%>% data_frame()
    
    mean_each_fold <- mean(EONR_1$.)
    data_to_r <- data_frame(ff=mean_each_fold)
    return(data_to_r)
  }
  )%>%unlist%>%data_frame()  
  
  
  data_to_re <- rf
  return(data_to_re)
}


#pCorn=0.246063
#pN=2.204624
RF_with_repeat_results <- mclapply(1:10, rf_with_repeat, mc.cores = detectCores() - 1)



#################################################################################
df1_RF_with_repeat<-  RF_with_repeat_results [1]%>%data.frame()%>% setnames(.,"repeat1")
df2_RF_with_repeat <- RF_with_repeat_results [2]%>%data.frame()%>% setnames(.,"repeat2")
df3_RF_with_repeat <- RF_with_repeat_results [3]%>%data.frame()%>% setnames(.,"repeat3")
df4_RF_with_repeat <- RF_with_repeat_results [4]%>%data.frame()%>% setnames(.,"repeat4")
df5_RF_with_repeat <- RF_with_repeat_results [5]%>%data.frame()%>% setnames(.,"repeat5")
df6_RF_with_repeat <- RF_with_repeat_results [6]%>%data.frame()%>% setnames(.,"repeat6")
df7_RF_with_repeat <- RF_with_repeat_results [7]%>%data.frame()%>% setnames(.,"repeat7")
df8_RF_with_repeat <- RF_with_repeat_results [8]%>%data.frame()%>% setnames(.,"repeat8")
df9_RF_with_repeat <- RF_with_repeat_results [9]%>%data.frame()%>% setnames(.,"repeat9")
df10_RF_with_repeat <-RF_with_repeat_results [10]%>%data.frame()%>% setnames(.,"repeat10")

RF_folds_with_repeat <- cbind(df1_RF_with_repeat,df2_RF_with_repeat,df3_RF_with_repeat,
                              df4_RF_with_repeat,df5_RF_with_repeat,df6_RF_with_repeat,df7_RF_with_repeat,
                              df8_RF_with_repeat,df9_RF_with_repeat,df10_RF_with_repeat)%>%
  mutate(mean_of_repeats=rowMeans(.))

#################################################################################
#gam

gam_fun_update <- function(n){ 
  yy <- list_of_repeat[n]%>%data.frame()
  
  gam <- lapply(1:6, function(i){
    
    train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
    test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()
    
    # gam  train   
    
    gam_train <- 
      gam(yield ~ s(N_tgt,k=3), data = test_data)
    
    # gam predict 
    
    predict_gam <- predict(gam_train, test_data)%>% data_frame()
    
    
    test_data$y_hat <-  predict_gam
    
    test_data <- test_data%>% mutate(profit=(pCorn*y_hat)-(pN*N_tgt))
    
    
    EONR <- test_data[which.max(profit), ]$N_tgt 
    
    
    
    
    data_to_rn <- data_frame(ff=EONR)
    return(data_to_rn)
  }
  )%>%unlist%>%data_frame()  
  
  
  data_to_reu <- gam
  return(data_to_reu)
}

gam_results_10_repeats_update <- mclapply(1:10, gam_fun_update, mc.cores = detectCores() - 1)


#################################################################################

gam1_updated<-  gam_results_10_repeats_update[1]%>%data.frame()%>% setnames(.,"repeat1")
gam2_updated <- gam_results_10_repeats_update[2]%>%data.frame()%>% setnames(.,"repeat2")
gam3_updated <- gam_results_10_repeats_update[3]%>%data.frame()%>% setnames(.,"repeat3")
gam4_updated <- gam_results_10_repeats_update[4]%>%data.frame()%>% setnames(.,"repeat4")
gam5_updated <- gam_results_10_repeats_update[5]%>%data.frame()%>% setnames(.,"repeat5")
gam6_updated <- gam_results_10_repeats_update[6]%>%data.frame()%>% setnames(.,"repeat6")
gam7_updated <- gam_results_10_repeats_update[7]%>%data.frame()%>% setnames(.,"repeat7")
gam8_updated <- gam_results_10_repeats_update[8]%>%data.frame()%>% setnames(.,"repeat8")
gam9_updated <- gam_results_10_repeats_update[9]%>%data.frame()%>% setnames(.,"repeat9")
gam10_updated <- gam_results_10_repeats_update[10]%>%data.frame()%>% setnames(.,"repeat10")

gam_folds__updated <- cbind(gam1_updated,gam2_updated,gam3_updated,gam4_updated,gam5_updated,
                            gam6_updated,gam7_updated,gam8_updated,gam9_updated,gam10_updated)
#%>%mutate(mean_of_repeats=rowMeans(.))

#################################################################################

#True_eonr
data_to_work_with <- data_to_work_with%>%
  .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
  .[, opt_N := pmin(Nk, opt_N)] %>%
  .[, opt_N := pmax(0, opt_N)]



true_raw <- try(1)


true <- function(i) { 
  
  d <- true_raw[["test_data"]][[i]]
  true_eonr <- mean(d$opt_N)
  
  return(true_eonr)
}

true_updated <- mclapply(1:6, true, mc.cores = detectCores() - 1)%>%unlist()%>% data_frame()  

#################################################################################
# MSE ####

#mes gam & true

mix_gam_true <- cbind(gam_folds__updated,true_updated )%>% rename(.,true_EONR=.,
                                                                  gam_repeat_1=repeat1,
                                                                  gam_repeat_2=repeat2, gam_repeat_3=repeat3,
                                                                  gam_repeat_4=repeat4, gam_repeat_5=repeat5,
                                                                  gam_repeat_6=repeat6, gam_repeat_7=repeat7, gam_repeat_8=repeat8,
                                                                  gam_repeat_9=repeat9, gam_repeat_10=repeat10)

drive_diff_EONR_fold_by_fold <- mix_gam_true%>% mutate(diff_EONR_gam_true_repeat_1=(gam_repeat_1-true_EONR)^2,
                                                       diff_EONR_gam_true_repeat_2=(gam_repeat_2-true_EONR)^2,
                                                       diff_EONR_gam_true_repeat_3=(gam_repeat_3-true_EONR)^2,
                                                       diff_EONR_gam_true_repeat_4=(gam_repeat_4-true_EONR)^2,
                                                       diff_EONR_gam_true_repeat_5=(gam_repeat_5-true_EONR)^2,
                                                       diff_EONR_gam_true_repeat_6=(gam_repeat_6-true_EONR)^2,
                                                       diff_EONR_gam_true_repeat_7=(gam_repeat_7-true_EONR)^2,
                                                       diff_EONR_gam_true_repeat_8=(gam_repeat_8-true_EONR)^2,
                                                       diff_EONR_gam_true_repeat_9=(gam_repeat_9-true_EONR)^2,
                                                       diff_EONR_gam_true_repeat_10=(gam_repeat_10-true_EONR)^2)%>% .[-c(1:11) ] 




mse_gam_true<-as.data.frame(t(drive_diff_EONR_fold_by_fold))%>% 
  rename(
    fold_1 =V1 ,
    fold_2 = V2,  fold_3 = V3,  fold_4 = V4,  fold_5 = V5,  fold_6 = V6)%>% mutate(SUM=rowSums(.))

final_mse_gam_true_average_mse_across_repeat <- mean(mse_gam_true$SUM) #16693.58


#################################################################################
#mes gam & linear

mix_gam_linear <- cbind(gam_folds__updated,lin_folds_2)%>% .[-c(21) ] %>%rename(., linear_repeat_1=repeat1.1,
                                                                                linear_repeat_2=repeat2.1, linear_repeat_3=repeat3.1,linear_repeat_4=repeat4.1, linear_repeat_5=repeat5.1, linear_repeat_6=repeat6.1,
                                                                                linear_repeat_7=repeat7.1, linear_repeat_8=repeat8.1,linear_repeat_9=repeat9.1,linear_repeat_10=repeat10.1,
                                                                                gam_repeat_1=repeat1,
                                                                                gam_repeat_2=repeat2, gam_repeat_3=repeat3,
                                                                                gam_repeat_4=repeat4, gam_repeat_5=repeat5,
                                                                                gam_repeat_6=repeat6, gam_repeat_7=repeat7, gam_repeat_8=repeat8,
                                                                                gam_repeat_9=repeat9, gam_repeat_10=repeat10)

drive_diff_EONR_fold_by_fold_linear_gam <- mix_gam_linear %>% mutate(diff_EONR_gam_linear_repeat_1=(gam_repeat_1-linear_repeat_1)^2,
                                                                     diff_EONR_gam_linear_repeat_2=(gam_repeat_2-linear_repeat_2)^2,
                                                                     diff_EONR_gam_linear_repeat_3=(gam_repeat_3-linear_repeat_3)^2,
                                                                     diff_EONR_gam_linear_repeat_4=(gam_repeat_4-linear_repeat_4)^2,
                                                                     diff_EONR_gam_linear_repeat_5=(gam_repeat_5-linear_repeat_5)^2,
                                                                     diff_EONR_gam_linear_repeat_6=(gam_repeat_6-linear_repeat_6)^2,
                                                                     diff_EONR_gam_linear_repeat_7=(gam_repeat_7-linear_repeat_7)^2,
                                                                     diff_EONR_gam_linear_repeat_8=(gam_repeat_8-linear_repeat_8)^2,
                                                                     diff_EONR_gam_linear_repeat_9=(gam_repeat_9-linear_repeat_9)^2,
                                                                     diff_EONR_gam_linear_repeat_10=(gam_repeat_10-linear_repeat_10)^2)%>% .[-c(1:20) ] 




mse_gam_linear<-as.data.frame(t(drive_diff_EONR_fold_by_fold_linear_gam))%>% 
  rename(
    fold_1 =V1 ,
    fold_2 = V2,  fold_3 = V3,  fold_4 = V4,  fold_5 = V5,  fold_6 = V6)%>% mutate(SUM=rowSums(.))

final_mse_gam_linear_average_mse_across_repeat <- mean(mse_gam_linear$SUM) #16790.97


#################################################################################
#mes gam & BRF

mix_gam_BRF <- cbind(gam_folds__updated,BRF_folds_2)%>% .[-c(21) ] %>%rename(., BRF_repeat_1=repeat1.1,
                                                                             BRF_repeat_2=repeat2.1, BRF_repeat_3=repeat3.1,BRF_repeat_4=repeat4.1, BRF_repeat_5=repeat5.1, BRF_repeat_6=repeat6.1,
                                                                             BRF_repeat_7=repeat7.1, BRF_repeat_8=repeat8.1,BRF_repeat_9=repeat9.1,BRF_repeat_10=repeat10.1,
                                                                             gam_repeat_1=repeat1,
                                                                             gam_repeat_2=repeat2, gam_repeat_3=repeat3,
                                                                             gam_repeat_4=repeat4, gam_repeat_5=repeat5,
                                                                             gam_repeat_6=repeat6, gam_repeat_7=repeat7, gam_repeat_8=repeat8,
                                                                             gam_repeat_9=repeat9, gam_repeat_10=repeat10)

drive_diff_EONR_fold_by_fold_BRF_gam <- mix_gam_BRF %>% mutate(diff_EONR_gam_BRF_repeat_1=(gam_repeat_1-BRF_repeat_1)^2,
                                                               diff_EONR_gam_BRF_repeat_2=(gam_repeat_2-BRF_repeat_2)^2,
                                                               diff_EONR_gam_BRF_repeat_3=(gam_repeat_3-BRF_repeat_3)^2,
                                                               diff_EONR_gam_BRF_repeat_4=(gam_repeat_4-BRF_repeat_4)^2,
                                                               diff_EONR_gam_BRF_repeat_5=(gam_repeat_5-BRF_repeat_5)^2,
                                                               diff_EONR_gam_BRF_repeat_6=(gam_repeat_6-BRF_repeat_6)^2,
                                                               diff_EONR_gam_BRF_repeat_7=(gam_repeat_7-BRF_repeat_7)^2,
                                                               diff_EONR_gam_BRF_repeat_8=(gam_repeat_8-BRF_repeat_8)^2,
                                                               diff_EONR_gam_BRF_repeat_9=(gam_repeat_9-BRF_repeat_9)^2,
                                                               diff_EONR_gam_BRF_repeat_10=(gam_repeat_10-BRF_repeat_10)^2)%>% .[-c(1:20) ] 




mse_gam_BRF<-as.data.frame(t(drive_diff_EONR_fold_by_fold_BRF_gam ))%>% 
  rename(
    fold_1 =V1 ,
    fold_2 = V2,  fold_3 = V3,  fold_4 = V4,  fold_5 = V5,  fold_6 = V6)%>% mutate(SUM=rowSums(.))

final_mse_gam_BRF_average_mse_across_repeat <- mean(mse_gam_BRF$SUM) #13627.29


#################################################################################
#mes gam & RF

mix_gam_RF <- cbind(gam_folds__updated,RF_folds_with_repeat)%>% .[-c(21) ] %>%rename(., RF_repeat_1=repeat1.1,
                                                                                     RF_repeat_2=repeat2.1, RF_repeat_3=repeat3.1,RF_repeat_4=repeat4.1, RF_repeat_5=repeat5.1, RF_repeat_6=repeat6.1,
                                                                                     RF_repeat_7=repeat7.1, RF_repeat_8=repeat8.1,RF_repeat_9=repeat9.1,RF_repeat_10=repeat10.1,
                                                                                     gam_repeat_1=repeat1,gam_repeat_2=repeat2, gam_repeat_3=repeat3,gam_repeat_4=repeat4, gam_repeat_5=repeat5,
                                                                                     gam_repeat_6=repeat6, gam_repeat_7=repeat7, gam_repeat_8=repeat8,
                                                                                     gam_repeat_9=repeat9, gam_repeat_10=repeat10)

drive_diff_EONR_fold_by_fold_RF_gam <- mix_gam_RF %>% mutate(diff_EONR_gam_RF_repeat_1=(gam_repeat_1-RF_repeat_1)^2,
                                                             diff_EONR_gam_RF_repeat_2=(gam_repeat_2-RF_repeat_2)^2,
                                                             diff_EONR_gam_RF_repeat_3=(gam_repeat_3-RF_repeat_3)^2,
                                                             diff_EONR_gam_RF_repeat_4=(gam_repeat_4-RF_repeat_4)^2,
                                                             diff_EONR_gam_RF_repeat_5=(gam_repeat_5-RF_repeat_5)^2,
                                                             diff_EONR_gam_RF_repeat_6=(gam_repeat_6-RF_repeat_6)^2,
                                                             diff_EONR_gam_RF_repeat_7=(gam_repeat_7-RF_repeat_7)^2,
                                                             diff_EONR_gam_RF_repeat_8=(gam_repeat_8-RF_repeat_8)^2,
                                                             diff_EONR_gam_RF_repeat_9=(gam_repeat_9-RF_repeat_9)^2,
                                                             diff_EONR_gam_RF_repeat_10=(gam_repeat_10-RF_repeat_10)^2)%>% .[-c(1:20) ] 




mse_gam_RF<-as.data.frame(t(drive_diff_EONR_fold_by_fold_RF_gam))%>% 
  rename(
    fold_1 =V1 ,
    fold_2 = V2,  fold_3 = V3,  fold_4 = V4,  fold_5 = V5,  fold_6 = V6)%>% mutate(SUM=rowSums(.))

final_mse_gam_RF_average_mse_across_repeat <- mean(mse_gam_RF$SUM) #12387.29


#################################################################################
#mes linear & true

mix_linear_true <- cbind(lin_folds_2,true_updated )%>% rename(.,true_EONR=.,
                                                              linear_repeat_1=repeat1,
                                                              linear_repeat_2=repeat2, linear_repeat_3=repeat3,
                                                              linear_repeat_4=repeat4, linear_repeat_5=repeat5,
                                                              linear_repeat_6=repeat6, linear_repeat_7=repeat7, linear_repeat_8=repeat8,
                                                              linear_repeat_9=repeat9, linear_repeat_10=repeat10)

drive_diff_EONR_fold_by_fold_linear_true <- mix_linear_true%>% mutate(diff_EONR_linear_true_repeat_1=(linear_repeat_1-true_EONR)^2,
                                                                      diff_EONR_linear_true_repeat_2=(linear_repeat_2-true_EONR)^2,
                                                                      diff_EONR_linear_true_repeat_3=(linear_repeat_3-true_EONR)^2,
                                                                      diff_EONR_linear_true_repeat_4=(linear_repeat_4-true_EONR)^2,
                                                                      diff_EONR_linear_true_repeat_5=(linear_repeat_5-true_EONR)^2,
                                                                      diff_EONR_linear_true_repeat_6=(linear_repeat_6-true_EONR)^2,
                                                                      diff_EONR_linear_true_repeat_7=(linear_repeat_7-true_EONR)^2,
                                                                      diff_EONR_linear_true_repeat_8=(linear_repeat_8-true_EONR)^2,
                                                                      diff_EONR_linear_true_repeat_9=(linear_repeat_9-true_EONR)^2,
                                                                      diff_EONR_linear_true_repeat_10=(linear_repeat_10-true_EONR)^2)%>% .[-c(1:12) ] 




mse_linear_true<-as.data.frame(t(drive_diff_EONR_fold_by_fold_linear_true))%>% 
  rename(
    fold_1 =V1 ,
    fold_2 = V2,  fold_3 = V3,  fold_4 = V4,  fold_5 = V5,  fold_6 = V6)%>% mutate(SUM=rowSums(.))

final_mse_linear_true_average_mse_across_repeat <- mean(mse_linear_true$SUM) #11287.02


#################################################################################
#mes BRF & true

mix_BRF_true <- cbind(BRF_folds_2,true_updated )%>% rename(.,true_EONR=.,
                                                           BRF_repeat_1=repeat1,
                                                           BRF_repeat_2=repeat2, BRF_repeat_3=repeat3,
                                                           BRF_repeat_4=repeat4, BRF_repeat_5=repeat5,
                                                           BRF_repeat_6=repeat6, BRF_repeat_7=repeat7, BRF_repeat_8=repeat8,
                                                           BRF_repeat_9=repeat9, BRF_repeat_10=repeat10)

drive_diff_EONR_fold_by_fold_BRF_true <- mix_BRF_true%>% mutate(diff_EONR_BRF_true_repeat_1=(BRF_repeat_1-true_EONR)^2,
                                                                diff_EONR_BRF_true_repeat_2=(BRF_repeat_2-true_EONR)^2,
                                                                diff_EONR_BRF_true_repeat_3=(BRF_repeat_3-true_EONR)^2,
                                                                diff_EONR_BRF_true_repeat_4=(BRF_repeat_4-true_EONR)^2,
                                                                diff_EONR_BRF_true_repeat_5=(BRF_repeat_5-true_EONR)^2,
                                                                diff_EONR_BRF_true_repeat_6=(BRF_repeat_6-true_EONR)^2,
                                                                diff_EONR_BRF_true_repeat_7=(BRF_repeat_7-true_EONR)^2,
                                                                diff_EONR_BRF_true_repeat_8=(BRF_repeat_8-true_EONR)^2,
                                                                diff_EONR_BRF_true_repeat_9=(BRF_repeat_9-true_EONR)^2,
                                                                diff_EONR_BRF_true_repeat_10=(BRF_repeat_10-true_EONR)^2)%>% .[-c(1:12) ] 




mse_BRF_true<-as.data.frame(t(drive_diff_EONR_fold_by_fold_BRF_true))%>% 
  rename(
    fold_1 =V1 ,
    fold_2 = V2,  fold_3 = V3,  fold_4 = V4,  fold_5 = V5,  fold_6 = V6)%>% mutate(SUM=rowSums(.))

final_mse_BRF_true_average_mse_across_repeat <- mean(mse_BRF_true$SUM) # 11346.11


#################################################################################
#mes RF & true

mix_RF_true <- cbind(RF_folds_with_repeat,true_updated )%>% rename(.,true_EONR=.,
                                                                   RF_repeat_1=repeat1,
                                                                   RF_repeat_2=repeat2, RF_repeat_3=repeat3,
                                                                   RF_repeat_4=repeat4, RF_repeat_5=repeat5,
                                                                   RF_repeat_6=repeat6, RF_repeat_7=repeat7, RF_repeat_8=repeat8,
                                                                   RF_repeat_9=repeat9, RF_repeat_10=repeat10)

drive_diff_EONR_fold_by_fold_RF_true <- mix_RF_true%>% mutate(diff_EONR_RF_true_repeat_1=(RF_repeat_1-true_EONR)^2,
                                                              diff_EONR_RF_true_repeat_2=(RF_repeat_2-true_EONR)^2,
                                                              diff_EONR_RF_true_repeat_3=(RF_repeat_3-true_EONR)^2,
                                                              diff_EONR_RF_true_repeat_4=(RF_repeat_4-true_EONR)^2,
                                                              diff_EONR_RF_true_repeat_5=(RF_repeat_5-true_EONR)^2,
                                                              diff_EONR_RF_true_repeat_6=(RF_repeat_6-true_EONR)^2,
                                                              diff_EONR_RF_true_repeat_7=(RF_repeat_7-true_EONR)^2,
                                                              diff_EONR_RF_true_repeat_8=(RF_repeat_8-true_EONR)^2,
                                                              diff_EONR_RF_true_repeat_9=(RF_repeat_9-true_EONR)^2,
                                                              diff_EONR_RF_true_repeat_10=(RF_repeat_10-true_EONR)^2)%>% .[-c(1:12) ] 




mse_RF_true<-as.data.frame(t(drive_diff_EONR_fold_by_fold_RF_true ))%>% 
  rename(
    fold_1 =V1 ,
    fold_2 = V2,  fold_3 = V3,  fold_4 = V4,  fold_5 = V5,  fold_6 = V6)%>% mutate(SUM=rowSums(.))

final_mse_RF_true_average_mse_across_repeat <- mean(mse_RF_true$SUM) #11808.33


#################################################################################

















































