##############################Local EONR########################################
#Field 2


read_data <- readRDS("Shared/Data/all_sim_data.rds")

#data field #2
data_field_2 <- read_data[3, ]$reg_data[[1]]$data[[2]]
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
library(data.table)
library(tidyverse)

################################################################################
data_for_obtaining_true_field_2 <- data_field_2[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                                                    "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                                                    "plateau_1_2" ,"yield" , "X","Y" ,  "N_tgt","b1","b2","Nk"  )]
# %>% tibble::rowid_to_column(., "id")

data_to_work_with_field_2 <- data_field_2[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                                              "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                                              "plateau_1_2" ,"yield" , "X","Y" ,  "N_tgt")]
# %>% tibble::rowid_to_column(., "id")


data_sf_2_field_2 <- st_as_sf(data_to_work_with_field_2, coords = c("X", "Y"))


# X = 
#   theta_b2_2  theta_b2_1 theta_b1_2 theta_b1_1
# plateau_2_1 plateau_2_2 plateau_1_1 plateau_1_2
# Nk_2_1   Nk_2_2   Nk_1_1   Nk_1_2


################################################################################
try_field_2 <- function(n){
  
  skcv_folds_try <- spatial_clustering_cv(data_sf_2_field_2, v = 6) 
  
  scv_try<-
    skcv_folds_try %>% 
    rowwise() %>% 
    mutate(
      training_data = list(analysis(splits)),
      test_data = list(assessment(splits))
      
    )
  data_to_re <- scv_try
}

list_of_repeat_field_2 <- lapply(1:10, try_field_2)

#################################################################################
#BRF

l_field_2 <- function(n){ 
  yy <- list_of_repeat_field_2[n]%>%data.frame()
  
  brf <- lapply(1:6, function(i){
    
    train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
    
    test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()%>%subset(., select=-c(yield))
    # boosted regression forest train   
    
    X <-as.matrix(train_data)%>%subset(., select=-c(yield))    #.[,-c(13,15,16,17)]
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
brf_results_field_2 <- mclapply(1:10, l_field_2 , mc.cores = detectCores() - 1)


#################################################################################
df_rep1_field_2<-  brf_results_field_2 [1]%>%data.frame()%>% setnames(.,"repeat1")
df_rep2_field_2 <- brf_results_field_2 [2]%>%data.frame()%>% setnames(.,"repeat2")
df_rep3_field_2 <- brf_results_field_2 [3]%>%data.frame()%>% setnames(.,"repeat3")
df_rep4_field_2 <- brf_results_field_2 [4]%>%data.frame()%>% setnames(.,"repeat4")
df_rep5_field_2 <- brf_results_field_2 [5]%>%data.frame()%>% setnames(.,"repeat5")
df_rep6_field_2 <- brf_results_field_2 [6]%>%data.frame()%>% setnames(.,"repeat6")
df_rep7_field_2 <- brf_results_field_2 [7]%>%data.frame()%>% setnames(.,"repeat7")
df_rep8_field_2 <- brf_results_field_2 [8]%>%data.frame()%>% setnames(.,"repeat8")
df_rep9_field_2 <- brf_results_field_2 [9]%>%data.frame()%>% setnames(.,"repeat9")
df_rep10_field_2 <-brf_results_field_2 [10]%>%data.frame()%>% setnames(.,"repeat10")

BRF_folds_2_field_2 <- cbind(df_rep1_field_2,df_rep2_field_2,df_rep3_field_2,df_rep4_field_2,
                             df_rep5_field_2,df_rep6_field_2,df_rep7_field_2,df_rep8_field_2,df_rep9_field_2,df_rep10_field_2)
#%>%mutate(mean_of_repeats=rowMeans(.))

#################################################################################

# Linear 10 rep, S_learner

linear_function_field_2 <- function(n){ 
  yy <- list_of_repeat_field_2[n]%>%data.frame()
  
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

linear_results_field_2 <- mclapply(1:10, linear_function_field_2, mc.cores = detectCores() - 1)



#################################################################################

lin1_field_2<- linear_results_field_2[1]%>%data.frame()%>% setnames(.,"repeat1")
lin2_field_2 <- linear_results_field_2[2]%>%data.frame()%>% setnames(.,"repeat2")
lin3_field_2 <- linear_results_field_2[3]%>%data.frame()%>% setnames(.,"repeat3")
lin4_field_2 <- linear_results_field_2[4]%>%data.frame()%>% setnames(.,"repeat4")
lin5_field_2 <- linear_results_field_2[5]%>%data.frame()%>% setnames(.,"repeat5")
lin6_field_2 <- linear_results_field_2[6]%>%data.frame()%>% setnames(.,"repeat6")
lin7_field_2 <- linear_results_field_2[7]%>%data.frame()%>% setnames(.,"repeat7")
lin8_field_2 <- linear_results_field_2[8]%>%data.frame()%>% setnames(.,"repeat8")
lin9_field_2 <- linear_results_field_2[9]%>%data.frame()%>% setnames(.,"repeat9")
lin10_field_2 <-linear_results_field_2[10]%>%data.frame()%>% setnames(.,"repeat10")

lin_folds_2_field_2 <- cbind(lin1_field_2,lin2_field_2,lin3_field_2,lin4_field_2,lin5_field_2,
                             lin6_field_2,lin7_field_2,lin8_field_2,lin9_field_2,lin10_field_2)
#%>%mutate(mean_of_repeats=rowMeans(.))

#################################################################################
#RF

rf_with_repeat_field_2 <- function(n){ 
  yy <- list_of_repeat_field_2[n]%>%data.frame()
  
  rf <- lapply(1:6, function(i){
    
    train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
    
    test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()
    
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
RF_with_repeat_results_field_2 <- mclapply(1:10, rf_with_repeat_field_2, mc.cores = detectCores() - 1)



#################################################################################
df1_RF_with_repeat_field_2<-  RF_with_repeat_results_field_2 [1]%>%data.frame()%>% setnames(.,"repeat1")
df2_RF_with_repeat_field_2 <- RF_with_repeat_results_field_2 [2]%>%data.frame()%>% setnames(.,"repeat2")
df3_RF_with_repeat_field_2 <- RF_with_repeat_results_field_2 [3]%>%data.frame()%>% setnames(.,"repeat3")
df4_RF_with_repeat_field_2 <- RF_with_repeat_results_field_2 [4]%>%data.frame()%>% setnames(.,"repeat4")
df5_RF_with_repeat_field_2 <- RF_with_repeat_results_field_2 [5]%>%data.frame()%>% setnames(.,"repeat5")
df6_RF_with_repeat_field_2 <- RF_with_repeat_results_field_2 [6]%>%data.frame()%>% setnames(.,"repeat6")
df7_RF_with_repeat_field_2 <- RF_with_repeat_results_field_2 [7]%>%data.frame()%>% setnames(.,"repeat7")
df8_RF_with_repeat_field_2 <- RF_with_repeat_results_field_2 [8]%>%data.frame()%>% setnames(.,"repeat8")
df9_RF_with_repeat_field_2 <- RF_with_repeat_results_field_2 [9]%>%data.frame()%>% setnames(.,"repeat9")
df10_RF_with_repeat_field_2 <-RF_with_repeat_results_field_2 [10]%>%data.frame()%>% setnames(.,"repeat10")

RF_folds_with_repeat_field_2 <- cbind(df1_RF_with_repeat_field_2,df2_RF_with_repeat_field_2,df3_RF_with_repeat_field_2,
                                      df4_RF_with_repeat_field_2,df5_RF_with_repeat_field_2,df6_RF_with_repeat_field_2,df7_RF_with_repeat_field_2,
                                      df8_RF_with_repeat_field_2,df9_RF_with_repeat_field_2,df10_RF_with_repeat_field_2)
#%>%mutate(mean_of_repeats=rowMeans(.))

#################################################################################
#gam

gdf <- data_frame(N_tgt=seq(109,242,by=1)) 



#gam

gam_fun_update_field_2 <- function(n){ 
  yy <- list_of_repeat_field_2[n]%>%data.frame()
  
  gam <- lapply(1:6, function(i){
    
    #train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
    test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()
    
    # gam  train   
    
    gam_train <- 
      
      gam(yield ~ s(N_tgt, k=3), data = test_data)
    
    # gam predict 
    
    predict_gam <- predict(gam_train, gdf)%>% data_frame()
    
    
    
    gdf<-  gdf %>% mutate(y_hat=predict_gam$.)
    
    gdf <- gdf%>% mutate(profit=(pCorn*y_hat)-(pN*N_tgt)) %>% data.table()
    
    
    EONR <- gdf[which.max(profit), ]$N_tgt 
    
    
    
    
    
    
    data_to_rn <- data_frame(ff=EONR)
    return(data_to_rn)
  }
  )%>%unlist%>%data_frame()  
  
  
  data_to_reu <- gam
  return(data_to_reu)
}

#gam_results_modified <- mclapply(1:10, gam_fun_update, mc.cores = detectCores() - 1)

gam_seq_field_2 <- mclapply(1:10, gam_fun_update_field_2, mc.cores = detectCores() - 1)

#################################################################################

gam1_updated_field_2<-  gam_seq_field_2[1]%>%data.frame()%>% setnames(.,"repeat1")
gam2_updated_field_2 <- gam_seq_field_2[2]%>%data.frame()%>% setnames(.,"repeat2")
gam3_updated_field_2 <- gam_seq_field_2[3]%>%data.frame()%>% setnames(.,"repeat3")
gam4_updated_field_2 <- gam_seq_field_2[4]%>%data.frame()%>% setnames(.,"repeat4")
gam5_updated_field_2 <- gam_seq_field_2[5]%>%data.frame()%>% setnames(.,"repeat5")
gam6_updated_field_2 <- gam_seq_field_2[6]%>%data.frame()%>% setnames(.,"repeat6")
gam7_updated_field_2 <- gam_seq_field_2[7]%>%data.frame()%>% setnames(.,"repeat7")
gam8_updated_field_2 <- gam_seq_field_2[8]%>%data.frame()%>% setnames(.,"repeat8")
gam9_updated_field_2 <- gam_seq_field_2[9]%>%data.frame()%>% setnames(.,"repeat9")
gam10_updated_field_2 <-gam_seq_field_2[10]%>%data.frame()%>% setnames(.,"repeat10")

gam_folds_seq_field_2 <- cbind(gam1_updated_field_2,gam2_updated_field_2,gam3_updated_field_2,
                               gam4_updated_field_2,gam5_updated_field_2,
                               gam6_updated_field_2,gam7_updated_field_2,gam8_updated_field_2,gam9_updated_field_2,gam10_updated_field_2)
#%>%mutate(mean_of_repeats=rowMeans(.))

#################################################################################

#################################################################################

#True_eonr
data_for_obtaining_true_field_2  <- data_for_obtaining_true_field_2 %>%
  .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
  .[, opt_N := pmin(Nk, opt_N)] %>%
  .[, opt_N := pmax(0, opt_N)]

#######MES gam vs others########################################################
######MES gam vs others#########################################################
##### MSE gam vs others##########################################################
######MES gam vs others##########################################################
######MES gam vs others##########################################################
######MES gam vs others#########################################################
######MES gam vs others#########################################################

#################################################################################
#mes gam & linear

mix_gam_linear_field_2 <- cbind(gam_folds_seq_field_2,lin_folds_2_field_2)%>% .[-c(21) ] %>%rename(., linear_repeat_1=repeat1.1,
                                                                                                   linear_repeat_2=repeat2.1, linear_repeat_3=repeat3.1,linear_repeat_4=repeat4.1, linear_repeat_5=repeat5.1, linear_repeat_6=repeat6.1,
                                                                                                   linear_repeat_7=repeat7.1, linear_repeat_8=repeat8.1,linear_repeat_9=repeat9.1,linear_repeat_10=repeat10.1,
                                                                                                   gam_repeat_1=repeat1,
                                                                                                   gam_repeat_2=repeat2, gam_repeat_3=repeat3,
                                                                                                   gam_repeat_4=repeat4, gam_repeat_5=repeat5,
                                                                                                   gam_repeat_6=repeat6, gam_repeat_7=repeat7, gam_repeat_8=repeat8,
                                                                                                   gam_repeat_9=repeat9, gam_repeat_10=repeat10)

drive_diff_EONR_fold_by_fold_linear_gam_field_2 <- mix_gam_linear_field_2 %>% mutate(diff_EONR_gam_linear_repeat_1=(gam_repeat_1-linear_repeat_1)^2,
                                                                                     diff_EONR_gam_linear_repeat_2=(gam_repeat_2-linear_repeat_2)^2,
                                                                                     diff_EONR_gam_linear_repeat_3=(gam_repeat_3-linear_repeat_3)^2,
                                                                                     diff_EONR_gam_linear_repeat_4=(gam_repeat_4-linear_repeat_4)^2,
                                                                                     diff_EONR_gam_linear_repeat_5=(gam_repeat_5-linear_repeat_5)^2,
                                                                                     diff_EONR_gam_linear_repeat_6=(gam_repeat_6-linear_repeat_6)^2,
                                                                                     diff_EONR_gam_linear_repeat_7=(gam_repeat_7-linear_repeat_7)^2,
                                                                                     diff_EONR_gam_linear_repeat_8=(gam_repeat_8-linear_repeat_8)^2,
                                                                                     diff_EONR_gam_linear_repeat_9=(gam_repeat_9-linear_repeat_9)^2,
                                                                                     diff_EONR_gam_linear_repeat_10=(gam_repeat_10-linear_repeat_10)^2)%>% .[-c(1:20) ] 




mse_gam_linear_field_2<-as.data.frame(t(drive_diff_EONR_fold_by_fold_linear_gam_field_2))%>% 
  rename(
    fold_1 =V1 ,
    fold_2 = V2,  fold_3 = V3,  fold_4 = V4,  fold_5 = V5,  fold_6 = V6)%>% mutate(SUM=rowSums(.))

final_mse_gam_linear_average_mse_across_repeat_field_2 <- mean(mse_gam_linear_field_2$SUM)  #6345.638  #6929.005  #6975.225

#################################################################################
#mes gam & BRF

mix_gam_BRF_field_2 <- cbind(gam_folds_seq_field_2,BRF_folds_2_field_2)%>% .[-c(21) ] %>%rename(., BRF_repeat_1=repeat1.1,
                                                                                                BRF_repeat_2=repeat2.1, BRF_repeat_3=repeat3.1,BRF_repeat_4=repeat4.1, BRF_repeat_5=repeat5.1, BRF_repeat_6=repeat6.1,
                                                                                                BRF_repeat_7=repeat7.1, BRF_repeat_8=repeat8.1,BRF_repeat_9=repeat9.1,BRF_repeat_10=repeat10.1,
                                                                                                gam_repeat_1=repeat1,
                                                                                                gam_repeat_2=repeat2, gam_repeat_3=repeat3,
                                                                                                gam_repeat_4=repeat4, gam_repeat_5=repeat5,
                                                                                                gam_repeat_6=repeat6, gam_repeat_7=repeat7, gam_repeat_8=repeat8,
                                                                                                gam_repeat_9=repeat9, gam_repeat_10=repeat10)

drive_diff_EONR_fold_by_fold_BRF_gam_field_2 <- mix_gam_BRF_field_2 %>% mutate(diff_EONR_gam_BRF_repeat_1=(gam_repeat_1-BRF_repeat_1)^2,
                                                                               diff_EONR_gam_BRF_repeat_2=(gam_repeat_2-BRF_repeat_2)^2,
                                                                               diff_EONR_gam_BRF_repeat_3=(gam_repeat_3-BRF_repeat_3)^2,
                                                                               diff_EONR_gam_BRF_repeat_4=(gam_repeat_4-BRF_repeat_4)^2,
                                                                               diff_EONR_gam_BRF_repeat_5=(gam_repeat_5-BRF_repeat_5)^2,
                                                                               diff_EONR_gam_BRF_repeat_6=(gam_repeat_6-BRF_repeat_6)^2,
                                                                               diff_EONR_gam_BRF_repeat_7=(gam_repeat_7-BRF_repeat_7)^2,
                                                                               diff_EONR_gam_BRF_repeat_8=(gam_repeat_8-BRF_repeat_8)^2,
                                                                               diff_EONR_gam_BRF_repeat_9=(gam_repeat_9-BRF_repeat_9)^2,
                                                                               diff_EONR_gam_BRF_repeat_10=(gam_repeat_10-BRF_repeat_10)^2)%>% .[-c(1:20) ] 




mse_gam_BRF_field_2<-as.data.frame(t(drive_diff_EONR_fold_by_fold_BRF_gam_field_2 ))%>% 
  rename(
    fold_1 =V1 ,
    fold_2 = V2,  fold_3 = V3,  fold_4 = V4,  fold_5 = V5,  fold_6 = V6)%>% mutate(SUM=rowSums(.))

final_mse_gam_BRF_average_mse_across_repeat_field_2 <- mean(mse_gam_BRF_field_2$SUM)    #12246.31  #10760.92

#################################################################################
#mes gam & RF

mix_gam_RF_field_2 <- cbind(gam_folds_seq_field_2,RF_folds_with_repeat_field_2)%>% .[-c(21) ] %>%rename(., RF_repeat_1=repeat1.1,
                                                                                                        RF_repeat_2=repeat2.1, RF_repeat_3=repeat3.1,RF_repeat_4=repeat4.1, RF_repeat_5=repeat5.1, RF_repeat_6=repeat6.1,
                                                                                                        RF_repeat_7=repeat7.1, RF_repeat_8=repeat8.1,RF_repeat_9=repeat9.1,RF_repeat_10=repeat10.1,
                                                                                                        gam_repeat_1=repeat1,gam_repeat_2=repeat2, gam_repeat_3=repeat3,gam_repeat_4=repeat4, gam_repeat_5=repeat5,
                                                                                                        gam_repeat_6=repeat6, gam_repeat_7=repeat7, gam_repeat_8=repeat8,
                                                                                                        gam_repeat_9=repeat9, gam_repeat_10=repeat10)

drive_diff_EONR_fold_by_fold_RF_gam_field_2 <- mix_gam_RF_field_2 %>% mutate(diff_EONR_gam_RF_repeat_1=(gam_repeat_1-RF_repeat_1)^2,
                                                                             diff_EONR_gam_RF_repeat_2=(gam_repeat_2-RF_repeat_2)^2,
                                                                             diff_EONR_gam_RF_repeat_3=(gam_repeat_3-RF_repeat_3)^2,
                                                                             diff_EONR_gam_RF_repeat_4=(gam_repeat_4-RF_repeat_4)^2,
                                                                             diff_EONR_gam_RF_repeat_5=(gam_repeat_5-RF_repeat_5)^2,
                                                                             diff_EONR_gam_RF_repeat_6=(gam_repeat_6-RF_repeat_6)^2,
                                                                             diff_EONR_gam_RF_repeat_7=(gam_repeat_7-RF_repeat_7)^2,
                                                                             diff_EONR_gam_RF_repeat_8=(gam_repeat_8-RF_repeat_8)^2,
                                                                             diff_EONR_gam_RF_repeat_9=(gam_repeat_9-RF_repeat_9)^2,
                                                                             diff_EONR_gam_RF_repeat_10=(gam_repeat_10-RF_repeat_10)^2)%>% .[-c(1:20) ] 




mse_gam_RF_field_2<-as.data.frame(t(drive_diff_EONR_fold_by_fold_RF_gam_field_2))%>% 
  rename(
    fold_1 =V1 ,
    fold_2 = V2,  fold_3 = V3,  fold_4 = V4,  fold_5 = V5,  fold_6 = V6)%>% mutate(SUM=rowSums(.))

final_mse_gam_RF_average_mse_across_repeat_field_2 <- mean(mse_gam_RF_field_2$SUM)     #12475.69  #13202.21  #11071.61


################################################################################
#######plot gam vs others######################################################
#######plot gam vs others#######################################################
#######plot gam vs others#######################################################
#######plot gam vs others########################################################
#######plot gam vs others#######################################################
######plot gam vs others########################################################
######plot gam vs others#######################################################
#######plot gam vs others#####################################################
#####plot gam vs others########################################################


################################################################################
#gam_vs_linear

f1_combined_gam_linear_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[1,]),
                                             linear=t(lin_folds_2_field_2[1,]))%>%as.data.table()


f2_combined_gam_linear_field_2  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[2,]),
                                              linear=t(lin_folds_2_field_2[2,]))%>%as.data.table()


f3_combined_gam_linear_field_2  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[3,]),
                                              
                                              linear=t(lin_folds_2_field_2[3,]))%>%as.data.table()

f4_combined_gam_linear_field_2  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[4,]),
                                              
                                              linear=t(lin_folds_2_field_2[4,]))%>%as.data.table()

f5_combined_gam_linear_field_2  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[5,]),
                                              
                                              linear=t(lin_folds_2_field_2[5,]))%>%as.data.table()

f6_combined_gam_linear_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[6,]),
                                             
                                             linear=t(lin_folds_2_field_2[6,]))%>%as.data.table()


gam_linear_combined_field_2 <- rbind(f1_combined_gam_linear_field_2,f2_combined_gam_linear_field_2,f3_combined_gam_linear_field_2,
                                     f4_combined_gam_linear_field_2,f5_combined_gam_linear_field_2,f6_combined_gam_linear_field_2)%>%
  subset(., select = -Repeat)




plot_gam_linear_combined_field_2 <- ggplot(gam_linear_combined_field_2, aes(x=gam,y=linear, colour = "red")) + 
  geom_point()+geom_abline(slope=1, intercept = 0)+ xlim(100, 235)+ ylim(100, 235)

################################################################################
#gam_vs_BRF

f1_combined_gam_BRF_field_2  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[1,]),
                                           BRF=t(BRF_folds_2_field_2[1,]))%>%as.data.table()



f2_combined_gam_BRF_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[2,]),
                                          BRF=t(BRF_folds_2_field_2[2,]))%>%as.data.table()



f3_combined_gam_BRF_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[3,]),
                                          
                                          BRF=t(BRF_folds_2_field_2[3,]))%>%as.data.table()




f4_combined_gam_BRF_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[4,]),
                                          
                                          BRF=t(BRF_folds_2_field_2[4,]))%>%as.data.table()



f5_combined_gam_BRF_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[5,]),
                                          
                                          BRF=t(BRF_folds_2_field_2[5,]))%>%as.data.table()



f6_combined_gam_BRF_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[6,]),
                                          
                                          BRF=t(BRF_folds_2_field_2[6,]))%>%as.data.table()



gam_BRF_combined_field_2 <- rbind(f1_combined_gam_BRF_field_2,f2_combined_gam_BRF_field_2,f3_combined_gam_BRF_field_2,
                                  f4_combined_gam_BRF_field_2,f5_combined_gam_BRF_field_2,f6_combined_gam_BRF_field_2)%>%
  subset(., select = -Repeat)




plot_gam_BRF_combined_field_2 <- ggplot(gam_BRF_combined_field_2, aes(x=gam,y=BRF, colour = "red")) + 
  geom_point()+geom_abline(slope=1, intercept = 0)+ xlim(100, 235)+ ylim(100, 235)

################################################################################
#gam_vs_RF

f1_combined_gam_RF_field_2  <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[1,]),
                                          RF=t(RF_folds_with_repeat_field_2[1,]))%>%as.data.table()



f2_combined_gam_RF_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[2,]),
                                         RF=t(RF_folds_with_repeat_field_2[2,]))%>%as.data.table()



f3_combined_gam_RF_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[3,]),
                                         
                                         RF=t(RF_folds_with_repeat_field_2[3,]))%>%as.data.table()




f4_combined_gam_RF_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[4,]),
                                         
                                         RF=t(RF_folds_with_repeat_field_2[4,]))%>%as.data.table()



f5_combined_gam_RF_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[5,]),
                                         
                                         RF=t(RF_folds_with_repeat_field_2[5,]))%>%as.data.table()



f6_combined_gam_RF_field_2 <- data_frame(Repeat=c(1,2,3,4,5,6,7,8,9,10),gam=t(gam_folds_seq_field_2[6,]),
                                         
                                         RF=t(RF_folds_with_repeat_field_2[6,]))%>%as.data.table()



gam_RF_combined_field_2 <- rbind(f1_combined_gam_RF_field_2,f2_combined_gam_RF_field_2,f3_combined_gam_RF_field_2,
                                 f4_combined_gam_RF_field_2,f5_combined_gam_RF_field_2,f6_combined_gam_RF_field_2)%>%
  subset(., select = -Repeat)




plot_gam_RF_combined_field_2 <- ggplot(gam_RF_combined_field_2, aes(x=gam,y=RF, colour = "red")) + 
  geom_point()+geom_abline(slope=1, intercept = 0)+ xlim(100, 235)+ ylim(100, 235)

################################################################################


if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
install.packages("cowplot")
library(cowplot)
install.packages("ggpubr")
library(ggpubr)


All_plot_field_2 <- plot_grid( plot_true_linear_combined_field_2, plot_true_BRF_combined_field_2, plot_true_RF_combined_field_2,
                               plot_gam_linear_combined_field_2, 
                               plot_gam_BRF_combined_field_2, plot_gam_RF_combined_field_2 + rremove("x.text"), 
                               
                               
                               ncol = 2, nrow = 4)


################################################################################
# gam vs linear & BRF & RF

plot_gam_vs_linear_BRF_RF_field_2 <- plot_grid(plot_gam_linear_combined_field_2, 
                                               plot_gam_BRF_combined_field_2, plot_gam_RF_combined_field_2 + rremove("x.text"), 
                                               
                                               ncol = 1, nrow = 3)

################################################################################
# gam vs true

#plot_true_gam_combined

################################################################################

#######Compare true one to others & get MSE#####################################
################################################################################
#######################Compare true one to the others & get MSE#################
#################Compare true one to others & get MSE###########################
################################################################################
##################Compare true one to others & get MSE##########################
################################################################################
################################################################################
################################################################################
####Compare true one to the others & get MSE####################################

#########################RF###################################BRF##############
#RF_train  

#Train RF on the entire data


RF_train_field_2 <- 
  ranger(
    yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
      Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2+ N_tgt,     
    data = data_to_work_with_field_2
  )

#RF function to predict EONR for each observation on the entire data

rf_entire_data_field_2 <- function(i){ 
  row=1
  times = 134
  
  # random forest predict 
  #prediction for each cell
  data_cell <- data_to_work_with_field_2[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  pred <- predict(RF_train_field_2, data_cell)$predictions
  
  data_cell$y_hat <- pred
  
  data_cell <- data_cell%>% mutate(profit=(pCorn*y_hat)-(pN*N_tgt))
  
  
  EONR <- data_cell [which.max(profit), ]$N_tgt  #each cell
  data_to_return <- data_frame(EONR=EONR)  #each cell
  return(data_to_return)
  
}

RF_each_observation_field_2 <- mclapply(1:nrow(data_to_work_with_field_2), rf_entire_data_field_2, mc.cores = detectCores() - 1)


RF_each_observation_results_field_2 <- RF_each_observation_field_2 %>%unlist()%>%data_frame()   #unique: 109 127 161 


df_rf_vs_true_each_observation_field_2 <- data_frame(true_eonr=data_for_obtaining_true_field_2$opt_N,RF=RF_each_observation_results_field_2$.)

MSE_true_RF_field_2 <- df_rf_vs_true_each_observation_field_2 %>% mutate(diff=(true_eonr-RF)^2)
MSE_RF_true_field_2 <- sum(MSE_true_RF_field_2$diff)  #2134257  #2150198  #2197379

#########################BRF###################################BRF##############
#BRF_train  

train_data_field_2 <- data_to_work_with_field_2%>% subset(., select=-c(X,Y))

test_data_field_2 <- data_to_work_with_field_2%>% subset(., select=-c(X,Y,yield))



x_field_2 <-as.matrix(train_data_field_2)%>%subset(., select=-c(yield))    #.[,-c(13,15,16,17)]
y_field_2 <- train_data_field_2$yield

boosted.forest_train_field_2 <- boosted_regression_forest(x_field_2, y_field_2)

#BRF function to predict EONR for each observation on the entire data

Brf_entire_data_field_2 <- function(i){ 
  row=1
  times = 134
  
  # BRF predict 
  #prediction for each cell
  data_cell <- test_data_field_2[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  pred <- predict(boosted.forest_train_field_2, data_cell)$predictions
  
  data_cell$y_hat <- pred
  
  data_cell <- data_cell%>% mutate(profit=(pCorn*y_hat)-(pN*N_tgt))
  
  
  EONR <- data_cell [which.max(profit), ]$N_tgt  #each cell
  data_to_return <- data_frame(EONR=EONR)  #each cell
  return(data_to_return)
  
}

BRF_each_observation_field_2 <- mclapply(1:nrow(test_data_field_2), Brf_entire_data_field_2, mc.cores = detectCores() - 1)

BRF_each_observation_results_field_2 <- BRF_each_observation_field_2 %>%unlist()%>%data_frame()   #unique: 109 111 144 178


df_brf_vs_true_each_observation_field_2 <- data_frame(true_eonr=data_for_obtaining_true_field_2$opt_N,BRF=BRF_each_observation_results_field_2$.)

MSE_true_BRF_field_2 <- df_brf_vs_true_each_observation_field_2 %>% mutate(diff=(true_eonr-BRF)^2)
MSE_BRF_true_field_2 <- sum(MSE_true_BRF_field_2$diff)  #1650970   1693758   1632587

#########################Linear###################################Linear########
# linear regression  train   
train_data_lin_field_2 <- data_to_work_with_field_2

test_data_lin_field_2 <- data_to_work_with_field_2

# linear regression  train   

linear_field_2 <- lm(yield ~  theta_b2_2*N_tgt + theta_b2_1*N_tgt + 
                       theta_b1_2*N_tgt + theta_b1_1*N_tgt + Nk_2_1*N_tgt + Nk_2_2*N_tgt + Nk_1_1*N_tgt + Nk_1_2*N_tgt + plateau_2_1*N_tgt +
                       plateau_2_2*N_tgt + plateau_1_1*N_tgt + plateau_1_2*N_tgt + I(N_tgt^2),
                     data = train_data_lin_field_2)


lin_entire_data_field_2 <- function(i){ 
  
  # linear predict 
  
  row=1
  times = 134
  #prediction for each cell
  test_data_cell <- test_data_lin_field_2[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  predict_linear <-  linear_field_2 %>% predict(test_data_cell)
  
  
  test_data_cell$y_hat <-  predict_linear
  
  test_data_cell <- test_data_cell%>% mutate(profit=(pCorn*y_hat)-(pN*N_tgt))
  
  
  EONR <- test_data_cell [which.max(profit), ]$N_tgt  #each cell
  data_to_retur <- data_frame(EONR=EONR)  #each cell
  return(data_to_retur)
  
}

lin_entire_results_field_2 <- mclapply(1:1440, lin_entire_data_field_2, mc.cores = detectCores() - 1)
lin_res_field_2 <-lin_entire_results_field_2%>% unlist()%>% data_frame() 

df_linear_vs_true_each_observation_field_2 <- data_frame(true_eonr=data_for_obtaining_true_field_2$opt_N,Lin=lin_res_field_2$.)

MSE_true_lin_field_2 <- df_linear_vs_true_each_observation_field_2 %>% mutate(diff=(true_eonr-Lin)^2)
MSE_lin_true_field_2 <- sum(MSE_true_lin_field_2$diff)  #442117.3
################################################################################

#################plot true vs others############################################
#################plot true vs others############################################
#################plot true vs others############################################
##################plot true vs others###########################################
##################plot true vs others###########################################
##################plot true vs others##########################################
################plot true vs others############################################
####################plot true vs others##########################################


################################################################################
#true_VS_linear
true_linear_combined_field_2 <- data_frame(true_EONR=data_for_obtaining_true_field_2$opt_N,linear=lin_res_field_2$.)

plot_true_linear_combined_field_2 <- ggplot(true_linear_combined_field_2, aes(x=true_EONR,y=linear, colour = "red")) + 
  geom_point(position = position_dodge(width = .3)) +geom_abline(slope=1, intercept = 0)+ xlim(100, 235)+ ylim(100, 235)

################################################################################
#true_VS_BRF 
true_BRF_combined_field_2 <- data_frame(true_EONR=data_for_obtaining_true_field_2$opt_N,BRF=BRF_each_observation_results_field_2$.)

plot_true_BRF_combined_field_2 <- ggplot(true_BRF_combined_field_2, aes(x=true_EONR,y=BRF, colour = "red")) + 
  geom_point() +geom_abline(slope=1, intercept = 0)+ xlim(80, 235)+ ylim(80, 235)


################################################################################
#true_VS_RF 
true_RF_combined_field_2 <- data_frame(true_EONR=data_for_obtaining_true_field_2$opt_N,RF=RF_each_observation_results_field_2$.)

plot_true_RF_combined_field_2 <- ggplot(true_RF_combined_field_2, aes(x=true_EONR,y=RF, colour = "red")) + 
  geom_point() +geom_abline(slope=1, intercept = 0)+ xlim(100, 235)+ ylim(100, 235)
################################################################################
# true vs linear & BRF & RF

plot_true_vs_linear_BRF_RF_field_2 <- plot_grid(plot_true_linear_combined_field_2, 
                                                plot_true_BRF_combined_field_2, plot_true_RF_combined_field_2 + rremove("x.text"), 
                                                ncol = 1, nrow = 3)


################################################################################


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
####################Yield response function#####################################
################################################################################


################################################################################
## RF 


# RF_train <- 
#   ranger(
#     yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
#       Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2+ N_tgt,     
#     data = data_to_work_with
#   )


RF_yield_response_field_2 <- function(i){ 
  row=1
  times = 134
  
  # random forest predict 
  #prediction for each cell
  data_cell <- data_to_work_with_field_2[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  pred <- predict(RF_train_field_2, data_cell)$predictions
  
  data_cell$y_hat <- pred
  df <- data_cell %>% subset(.,select=c(N_tgt,y_hat))
  return(df)
  
}

yield_response_rf_field_2 <- mclapply(1:nrow(data_to_work_with_field_2), RF_yield_response_field_2, mc.cores = detectCores() - 1)

# ggplot(true_RF_combined_field_2, aes(x=true_EONR,y=RF, colour = "red")) + 
#   geom_point() +geom_abline(slope=1, intercept = 0)+ xlim(100, 235)+ ylim(100, 235)
#some response functions 
t1_field_2 <- yield_response_rf_field_2[[1]]
plot_t1_field_2 <- ggplot(t1_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11500, 13000)

t2_field_2 <- yield_response_rf_field_2[[2]]
plot_t2_field_2 <- ggplot(t2_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11500, 13000)

t3_field_2 <- yield_response_rf_field_2[[3]]
plot_t3_field_2 <- ggplot(t3_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11500, 13000)

t4_field_2 <- yield_response_rf_field_2[[1438]]
plot_t4_field_2 <- ggplot(t4_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11500, 13000)

t5_field_2 <- yield_response_rf_field_2[[1439]]
plot_t5_field_2 <- ggplot(t5_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11500, 13000)

t6_field_2 <- yield_response_rf_field_2[[1440]]
plot_t6_field_2 <- ggplot(t6_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11500, 13000)

plot_response_function_RF_field_2 <- plot_grid( plot_t1_field_2, plot_t2_field_2, plot_t3_field_2,
                                                plot_t4_field_2, 
                                                plot_t5_field_2, plot_t6_field_2 + rremove("x.text"), 
                                                
                                                
                                                ncol = 2, nrow = 4)

################################################################################
## BRF
train_data_brf_field_2 <- data_to_work_with_field_2%>% subset(., select=-c(X,Y))

test_data_brf_field_2 <- data_to_work_with_field_2%>% subset(., select=-c(X,Y,yield))

x_brf_field_2 <-as.matrix(train_data_brf_field_2)%>%subset(., select=-c(yield))   
y_brf_field_2 <- train_data_brf_field_2$yield

boosted.forest_train_yr_field_2 <- boosted_regression_forest(x_brf_field_2, y_brf_field_2)

BRF_yield_response_field_2 <-function(i){ 
  row=1
  times = 134
  
  # BRF predict 
  #prediction for each cell
  data_cell <- test_data_brf_field_2[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  pred <- predict(boosted.forest_train_yr_field_2, data_cell)$predictions
  
  data_cell$y_hat <- pred
  
  df <- data_cell %>% subset(.,select=c(N_tgt,y_hat))
  return(df)
  
}

yield_response_brf_field_2 <- mclapply(1:nrow(data_to_work_with_field_2), BRF_yield_response_field_2, mc.cores = detectCores() - 1)


#some response functions 
b1_field_2 <- yield_response_brf_field_2[[1]]
plot_b1_field_2 <- ggplot(b1_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

b2_field_2 <- yield_response_brf_field_2[[2]]
plot_b2_field_2 <- ggplot(b2_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

b3_field_2 <- yield_response_brf_field_2[[3]]
plot_b3_field_2 <- ggplot(b3_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

b4_field_2 <- yield_response_brf_field_2[[1438]]
plot_b4_field_2 <- ggplot(b4_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

b5_field_2 <- yield_response_brf_field_2[[1439]]
plot_b5_field_2 <- ggplot(b5_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

b6_field_2 <- yield_response_brf_field_2[[1440]]
plot_b6_field_2 <- ggplot(b6_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

plot_response_function_BRF_field_2 <- plot_grid( plot_b1_field_2, plot_b2_field_2, plot_b3_field_2,
                                                 plot_b4_field_2, 
                                                 plot_b5_field_2, plot_b6_field_2 + rremove("x.text"), 
                                                 
                                                 
                                                 ncol = 2, nrow = 4)

################################################################################

################################################################################
#linear
train_data_l_field_2 <- data_to_work_with_field_2

test_data_l_field_2 <- data_to_work_with_field_2

# linear regression  train 
linear_l_field_2 <- lm(yield ~  theta_b2_2*N_tgt + theta_b2_1*N_tgt + 
                         theta_b1_2*N_tgt + theta_b1_1*N_tgt + Nk_2_1*N_tgt + Nk_2_2*N_tgt + Nk_1_1*N_tgt + Nk_1_2*N_tgt + plateau_2_1*N_tgt +
                         plateau_2_2*N_tgt + plateau_1_1*N_tgt + plateau_1_2*N_tgt + I(N_tgt^2),
                       data = train_data_l_field_2)


Linear_yield_response_field_2 <- function(i){ 
  # linear predict 
  
  row=1
  times = 134
  #prediction for each cell
  test_data_cell <- test_data_l_field_2[i,]%>% subset(., select=-c(N_tgt))%>% 
    .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
  
  predict_linear <-  linear_l_field_2 %>% predict(test_data_cell)
  
  
  test_data_cell$y_hat <-  predict_linear
  
  df <- test_data_cell %>% subset(.,select=c(N_tgt,y_hat))
  return(df)
  
}

yield_response_linear_field_2 <- mclapply(1:1440, Linear_yield_response_field_2, mc.cores = detectCores() - 1)




#some response functions 
l1_field_2 <- yield_response_linear_field_2[[1]]
plot_l1_field_2 <- ggplot(l1_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

l2_field_2 <- yield_response_linear_field_2[[2]]
plot_l2_field_2 <- ggplot(l2_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

l3_field_2 <- yield_response_linear_field_2[[3]]
plot_l3_field_2 <- ggplot(l3_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

l4_field_2 <- yield_response_linear_field_2[[1438]]
plot_l4_field_2 <- ggplot(l4_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

l5_field_2 <- yield_response_linear_field_2[[1439]]
plot_l5_field_2 <- ggplot(l5_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

l6_field_2 <- yield_response_linear_field_2[[1440]]
plot_l6_field_2 <- ggplot(l6_field_2, aes(x=N_tgt,y=y_hat, colour = "red")) + 
  geom_point() + xlim(100, 250)+ ylim(11000, 13500)

plot_response_function_linear_field_2 <- plot_grid( plot_l1_field_2, plot_l2_field_2, plot_l3_field_2,
                                                    plot_l4_field_2, 
                                                    plot_l5_field_2, plot_l6_field_2 + rremove("x.text"), 
                                                    
                                                    
                                                    ncol = 2, nrow = 4)


################################################################################









######################Yield prediction as a criteria############################

read_data <- readRDS("Shared/Data/all_sim_data.rds")

#data field #2
data_field_2 <- read_data[3, ]$reg_data[[1]]$data[[2]]
#data <- data[3, ]$reg_data[[1]]$data[[2]]

data_to_work_with_field_2 <- data_field_2[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                                              "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                                              "plateau_1_2" ,"yield" , "X","Y" ,  "N_tgt")]
# %>% tibble::rowid_to_column(., "id")

data_sf_2_field_2 <- st_as_sf(data_to_work_with_field_2, coords = c("X", "Y"))

################################################################################

try_field_2 <- function(n){
  
  skcv_folds_try <- spatial_clustering_cv(data_sf_2_field_2, v = 6) 
  
  scv_try<-
    skcv_folds_try %>% 
    rowwise() %>% 
    mutate(
      training_data = list(analysis(splits)),
      test_data = list(assessment(splits))
      
    )
  data_to_re <- scv_try
}

list_of_repeat_field_2 <- lapply(1:10, try_field_2)
################################################################################
#gam

# train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
# 
# test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()%>%subset(., select=-c(yield))


gam_fun_yield_based_selection <- function(n){ 
  yy <- list_of_repeat_field_2[n]%>%data.frame()
  
  gam <- lapply(1:6, function(i){
    
    train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
    test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()
    
    # gam  train   
    
    gam_train <- 
      
      gam(yield ~ s(N_tgt, k=3), data = test_data)
    
    # gam predict 
    
    predict_gam <- predict(gam_train, test_data)%>% data_frame()
    
    # rf  train   
    RF <- 
      ranger(
        yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
          Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2+ N_tgt,     
        data = train_data
      ) 
    # rf predict
    
    pred_rf <- predict(RF, test_data)$predictions 
    
    #mse gam rf
    
    gam_mines_rf <- (predict_gam-pred_rf)^2
    
    mse_gam_rf_y <- sum(gam_mines_rf$.)
    #data_frame(gam_mines_rf=c(gam_mines_rf$.))  
    
    # linear train  
    linear <- lm(yield ~  theta_b2_2*N_tgt + theta_b2_1*N_tgt + 
                   theta_b1_2*N_tgt + theta_b1_1*N_tgt + Nk_2_1*N_tgt + Nk_2_2*N_tgt + Nk_1_1*N_tgt + Nk_1_2*N_tgt + plateau_2_1*N_tgt +
                   plateau_2_2*N_tgt + plateau_1_1*N_tgt + plateau_1_2*N_tgt + I(N_tgt^2),
                 data = train_data)
    
    # linear predict 
    
    predict_linear <-  linear %>% predict(test_data)
    
    #mse gam linear
    
    gam_mines_linear <- (predict_gam-predict_linear)^2
    
    mse_gam_linear_y <- sum(gam_mines_linear$.)
    
    
    #BRF train 
    
    # boosted regression forest train   
    
    X <-as.matrix(train_data)%>%subset(., select=-c(yield))    #.[,-c(13,15,16,17)]
    Y <- train_data$yield
    
    boosted.forest <- boosted_regression_forest(X, Y)
    
    # boosted forest predict 
    
    
    pred_brf <- predict(boosted.forest, test_data%>%subset(., select=-c(yield)))$predictions
    
    #mse gam brf
    
    gam_mines_brf <- (predict_gam-pred_brf)^2
    
    mse_gam_brf_y <- sum(gam_mines_brf$.)
    
    
    
    
    data_to_rn <- data_frame(mse_gam_rf=c(mse_gam_rf_y), mse_gam_linear=c(mse_gam_linear_y), mse_gam_brf=c(mse_gam_brf_y))
    return(data_to_rn)
  })
  #%>%unlist%>%data_frame()  
  
  
  data_to_reu <-do.call(rbind.data.frame,gam)
  return(data_to_reu)
}

#gam_results_modified <- mclapply(1:10, gam_fun_update, mc.cores = detectCores() - 1)

gam_yield_base_selection <- mclapply(1:10, gam_fun_yield_based_selection, mc.cores = detectCores() - 1)


all_res_together <-do.call(cbind.data.frame,gam_yield_base_selection)


mse_fold_repeates_gam_rf <- subset(all_res_together, select=c( 1,4,7,10,13,16,19,22,25,28))



mse_fold_repeates_gam_rf[nrow(mse_fold_repeates_gam_rf) + 1,] <- c(mean(mse_fold_repeates_gam_rf$mse_gam_rf),mean(mse_fold_repeates_gam_rf$mse_gam_rf.1),
                                                                   mean(mse_fold_repeates_gam_rf$mse_gam_rf.2),
                                                                   mean(mse_fold_repeates_gam_rf$mse_gam_rf.3),mean(mse_fold_repeates_gam_rf$mse_gam_rf.4),mean(mse_fold_repeates_gam_rf$mse_gam_rf.5),mean(mse_fold_repeates_gam_rf$mse_gam_rf.6),
                                                                   mean(mse_fold_repeates_gam_rf$mse_gam_rf.7),mean(mse_fold_repeates_gam_rf$mse_gam_rf.8),mean(mse_fold_repeates_gam_rf$mse_gam_rf.9))


mse_gam_rf_prediction_based_averaged_across_repeat <- rowMeans(mse_fold_repeates_gam_rf[7, ])  #574268940



#################################################################################
mse_fold_repeates_gam_linear <- subset(all_res_together, select=c( 2,5,8,11,14,17,20,23,26,29))



mse_fold_repeates_gam_linear[nrow(mse_fold_repeates_gam_linear) + 1,] <- c(mean(mse_fold_repeates_gam_linear$mse_gam_linear),mean(mse_fold_repeates_gam_linear$mse_gam_linear.1),
                                                                           mean(mse_fold_repeates_gam_linear$mse_gam_linear.2),
                                                                           mean(mse_fold_repeates_gam_linear$mse_gam_linear.3),mean(mse_fold_repeates_gam_linear$mse_gam_linear.4),mean(mse_fold_repeates_gam_linear$mse_gam_linear.5),mean(mse_fold_repeates_gam_linear$mse_gam_linear.6),
                                                                           mean(mse_fold_repeates_gam_linear$mse_gam_linear.7),mean(mse_fold_repeates_gam_linear$mse_gam_linear.8),mean(mse_fold_repeates_gam_linear$mse_gam_linear.9))


mse_gam_linear_prediction_based_averaged_across_repeat <- rowMeans(mse_fold_repeates_gam_linear[7, ])  #718505733

#################################################################################


mse_fold_repeates_gam_brf <- subset(all_res_together, select=c( 3,6,9,12,15,18,21,24,27,30))



mse_fold_repeates_gam_brf[nrow(mse_fold_repeates_gam_brf) + 1,] <- c(mean(mse_fold_repeates_gam_brf$mse_gam_brf),mean(mse_fold_repeates_gam_brf$mse_gam_brf.1),
                                                                     mean(mse_fold_repeates_gam_brf$mse_gam_brf.2),
                                                                     mean(mse_fold_repeates_gam_brf$mse_gam_brf.3),mean(mse_fold_repeates_gam_brf$mse_gam_brf.4),mean(mse_fold_repeates_gam_brf$mse_gam_brf.5),mean(mse_fold_repeates_gam_brf$mse_gam_brf.6),
                                                                     mean(mse_fold_repeates_gam_brf$mse_gam_brf.7),mean(mse_fold_repeates_gam_brf$mse_gam_brf.8),mean(mse_fold_repeates_gam_brf$mse_gam_brf.9))


mse_gam_brf_prediction_based_averaged_across_repeat <- rowMeans(mse_fold_repeates_gam_brf[7, ])  #688089810 

#################################################################################
##### True yield-prediction...

read_data <- readRDS("Shared/Data/all_sim_data.rds")

#data field #2
data_field_1 <- read_data[3, ]$reg_data[[1]]$data[[1]]
#data <- data[3, ]$reg_data[[1]]$data[[2]]

data_to_work_with_field_1 <- data_field_1[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                                              "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                                              "plateau_1_2" ,"yield" , "X","Y" ,  "N_tgt")]
# %>% tibble::rowid_to_column(., "id")

data_sf_2_field_1 <- st_as_sf(data_to_work_with_field_1, coords = c("X", "Y"))

################################################################################

############
#gam

# train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
# 
# test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()%>%subset(., select=-c(yield))


fun_true_vs_others_yield_based <- function(i){ 
  
  
  # folds <- lapply(1:6, function(i){
  
  train_data <- data_to_work_with_field_1
  test_data <- data_to_work_with_field_1
  
  # gam  train   
  
  # gam_train <- 
  #   
  #   gam(yield ~ s(N_tgt, k=3), data = data_to_work_with_field_1)
  # 
  # gam predict 
  
  # predict_gam <- predict(gam_train, data_to_work_with_field_1)%>% data_frame()
  
  # rf  train   
  RF <- 
    ranger(
      yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
        Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2+ N_tgt,     
      data = data_to_work_with_field_1
    ) 
  # rf predict
  
  pred_rf <- predict(RF, data_to_work_with_field_1)$predictions 
  
  #mse gam rf
  
  true_mines_rf <- (data_to_work_with_field_1$yield-pred_rf)^2
  
  mse_true_rf_y <- sum(true_mines_rf)
  #data_frame(gam_mines_rf=c(gam_mines_rf$.))  
  
  # linear train  
  linear <- lm(yield ~  theta_b2_2*N_tgt + theta_b2_1*N_tgt + 
                 theta_b1_2*N_tgt + theta_b1_1*N_tgt + Nk_2_1*N_tgt + Nk_2_2*N_tgt + Nk_1_1*N_tgt + Nk_1_2*N_tgt + plateau_2_1*N_tgt +
                 plateau_2_2*N_tgt + plateau_1_1*N_tgt + plateau_1_2*N_tgt + I(N_tgt^2),
               data = data_to_work_with_field_1)
  
  # linear predict 
  
  predict_linear <-  linear %>% predict(data_to_work_with_field_1)
  
  #mse gam linear
  
  true_mines_linear <- (data_to_work_with_field_1$yield-predict_linear)^2
  
  mse_true_linear_y <- sum(true_mines_linear)
  
  
  #BRF train 
  
  # boosted regression forest train   
  
  X <-as.matrix(data_to_work_with_field_1)%>%subset(., select=-c(yield))    #.[,-c(13,15,16,17)]
  Y <- data_to_work_with_field_1$yield
  
  boosted.forest <- boosted_regression_forest(X, Y)
  
  # boosted forest predict 
  
  
  pred_brf <- predict(boosted.forest, data_to_work_with_field_1%>%subset(., select=-c(yield)))$predictions
  
  #mse gam brf
  
  true_mines_brf <- (data_to_work_with_field_1$yield-pred_brf)^2
  
  mse_true_brf_y <- sum(true_mines_brf)
  
  
  
  
  data_to_rn <- data_frame(mse_true_rf=c(mse_true_rf_y), mse_true_linear=c(mse_true_linear_y), mse_true_brf=c(mse_true_brf_y))
  return(data_to_rn)
  # })
  # #%>%unlist%>%data_frame()  
  # 
  # 
  # data_to_reu <-do.call(rbind.data.frame,folds)
  # return(data_to_reu)
}

#gam_results_modified <- mclapply(1:10, gam_fun_update, mc.cores = detectCores() - 1)

true_yield_based_results <- mclapply(1:nrow(data_to_work_with_field_1), fun_true_vs_others_yield_based, mc.cores = detectCores() - 1)
true_yield_based_results <- fun_true_vs_others_yield_based(1)

all_res_together_field_1 <-do.call(cbind.data.frame,gam_yield_base_selection_field_1)


mse_fold_repeates_gam_rf_field_1 <- subset(all_res_together_field_1, select=c( 1,4,7,10,13,16,19,22,25,28))



mse_fold_repeates_gam_rf_field_1[nrow(mse_fold_repeates_gam_rf_field_1) + 1,] <- c(mean(mse_fold_repeates_gam_rf_field_1$mse_gam_rf),mean(mse_fold_repeates_gam_rf_field_1$mse_gam_rf.1),
                                                                                   mean(mse_fold_repeates_gam_rf_field_1$mse_gam_rf.2),
                                                                                   mean(mse_fold_repeates_gam_rf_field_1$mse_gam_rf.3),mean(mse_fold_repeates_gam_rf_field_1$mse_gam_rf.4),mean(mse_fold_repeates_gam_rf_field_1$mse_gam_rf.5),mean(mse_fold_repeates_gam_rf_field_1$mse_gam_rf.6),
                                                                                   mean(mse_fold_repeates_gam_rf_field_1$mse_gam_rf.7),mean(mse_fold_repeates_gam_rf_field_1$mse_gam_rf.8),mean(mse_fold_repeates_gam_rf_field_1$mse_gam_rf.9))


mse_gam_rf_prediction_based_averaged_across_repeat_field_1 <- rowMeans(mse_fold_repeates_gam_rf_field_1[7, ])  #584134293



#################################################################################
mse_fold_repeates_gam_linear_field_1 <- subset(all_res_together_field_1, select=c( 2,5,8,11,14,17,20,23,26,29))



mse_fold_repeates_gam_linear_field_1[nrow(mse_fold_repeates_gam_linear_field_1) + 1,] <- c(mean(mse_fold_repeates_gam_linear_field_1$mse_gam_linear),mean(mse_fold_repeates_gam_linear_field_1$mse_gam_linear.1),
                                                                                           mean(mse_fold_repeates_gam_linear_field_1$mse_gam_linear.2),
                                                                                           mean(mse_fold_repeates_gam_linear_field_1$mse_gam_linear.3),mean(mse_fold_repeates_gam_linear_field_1$mse_gam_linear.4),mean(mse_fold_repeates_gam_linear_field_1$mse_gam_linear.5),mean(mse_fold_repeates_gam_linear_field_1$mse_gam_linear.6),
                                                                                           mean(mse_fold_repeates_gam_linear_field_1$mse_gam_linear.7),mean(mse_fold_repeates_gam_linear_field_1$mse_gam_linear.8),mean(mse_fold_repeates_gam_linear_field_1$mse_gam_linear.9))


mse_gam_linear_prediction_based_averaged_across_repeat_field_1 <- rowMeans(mse_fold_repeates_gam_linear_field_1[7, ])  #1018065811

#################################################################################


mse_fold_repeates_gam_brf_field_1 <- subset(all_res_together_field_1, select=c( 3,6,9,12,15,18,21,24,27,30))



mse_fold_repeates_gam_brf_field_1[nrow(mse_fold_repeates_gam_brf_field_1) + 1,] <- c(mean(mse_fold_repeates_gam_brf_field_1$mse_gam_brf),mean(mse_fold_repeates_gam_brf_field_1$mse_gam_brf.1),
                                                                                     mean(mse_fold_repeates_gam_brf_field_1$mse_gam_brf.2),
                                                                                     mean(mse_fold_repeates_gam_brf_field_1$mse_gam_brf.3),mean(mse_fold_repeates_gam_brf_field_1$mse_gam_brf.4),mean(mse_fold_repeates_gam_brf_field_1$mse_gam_brf.5),mean(mse_fold_repeates_gam_brf_field_1$mse_gam_brf.6),
                                                                                     mean(mse_fold_repeates_gam_brf_field_1$mse_gam_brf.7),mean(mse_fold_repeates_gam_brf_field_1$mse_gam_brf.8),mean(mse_fold_repeates_gam_brf_field_1$mse_gam_brf.9))


mse_gam_brf_prediction_based_averaged_across_repeat_field_1 <- rowMeans(mse_fold_repeates_gam_brf_field_1[7, ])  #744054537 

#################################################################################










##########################  CF  ################################################

#read_data <- readRDS("Shared/Data/all_sim_data.rds")

#data CF

data_for_CF <- read_data[3, ]$reg_data[[1]]$data[[1]]


data_CF <- data_for_CF[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                           "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                           "plateau_1_2" ,"yield" ,  "N_tgt")]


#tibble::rowid_to_column(., "id")%>%
#%>%subset(., N_tgt %in% c(109, 142))

################################################################################
# 1: CF

# Estimate E(Y|X)  and calculate y_tilde

#rf_trained_y <-
#regression_forest(
#X = data_CF[, .(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2,
#  Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, plateau_1_1, plateau_1_2)],
# Y = data_CF[, yield]
#)

rf_trained_y <- 
  ranger(
    yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
      Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2,     
    data = data_CF
  )
#=== out-of-bag prediction of Y ===#
data_CF[, y_hat := rf_trained_y$predictions]

#=== calculate y_tilde ===#
data_CF[, y_tilde := yield - y_hat]


# Estimate E(T|X) and calcualte T_tilde 

# rf_trained_t <-
#   probability_forest(
#     X = data_CF[, .(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2,
#                     Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, plateau_1_1, plateau_1_2)],
#     Y = data_CF[, factor(N_tgt)],
#   )

rf_trained_t <- 
  ranger(
    N_tgt ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
      Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2,     
    data = data_CF
  )
#=== out-of-bag prediction of T ===#
data_CF[, t_hat := rf_trained_t$predictions]

#=== calculate t_hat ===#
data_CF[, t_tilde := N_tgt - t_hat]



cf_trained <-
  causal_forest(
    X = data_CF[, .(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2,
                    Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, plateau_1_1, plateau_1_2)],
    Y = data_CF[, yield],
    W = data_CF[, N_tgt],
    Y.hat = data_CF[, y_hat],
    W.hat = data_CF[, t_hat],
  )


rscore_cf <- data_CF[, sum((y_tilde - cf_trained$predictions * t_tilde)^2)]  #288576069   #292848669  #292348951



################################################################################
################################################################################

#BRF

# Train a BRF_R_learner

data_for_brf_x <- data_CF %>% subset(., select=-c(yield,N_tgt,y_hat,t_hat))  

df <- data_frame(theta_b2_2=data_for_brf_x$theta_b2_2*data_for_brf_x$t_tilde, 
                 theta_b2_1=data_for_brf_x$theta_b2_1*data_for_brf_x$t_tilde,
                 theta_b1_2=data_for_brf_x$ theta_b1_2*data_for_brf_x$t_tilde,
                 theta_b1_1=data_for_brf_x$ theta_b1_1*data_for_brf_x$t_tilde,
                 Nk_2_1=data_for_brf_x$Nk_2_1*data_for_brf_x$t_tilde,
                 Nk_2_2=data_for_brf_x$Nk_2_2*data_for_brf_x$t_tilde,
                 Nk_1_1=data_for_brf_x$Nk_1_1*data_for_brf_x$t_tilde,
                 Nk_1_2=data_for_brf_x$Nk_1_2*data_for_brf_x$t_tilde,
                 plateau_2_1=data_for_brf_x$plateau_2_1*data_for_brf_x$t_tilde,
                 plateau_2_2=data_for_brf_x$plateau_2_2*data_for_brf_x$t_tilde,
                 plateau_1_1=data_for_brf_x$plateau_1_1*data_for_brf_x$t_tilde,
                 plateau_1_2=data_for_brf_x$plateau_1_2*data_for_brf_x$t_tilde,
)

x_brf_cf <-as.matrix(df)
y_brf_cf <- data_for_brf_x$y_tilde

boosted.forest_train_cf <- boosted_regression_forest(x_brf_cf , y_brf_cf)

# BRF r_score 

rscore_brf <- data_CF[, sum((y_tilde - boosted.forest_train_cf$predictions * t_tilde)^2)]  # 1.082753e+12   922029561188   951729736975
#format(6.14393e+11, scientific=FALSE)

################################################################################
# Linear R_learner 

linear_r_learner <- lm(y_tilde ~  I(theta_b2_2*t_tilde) + I(theta_b2_1*t_tilde) + 
                         I(theta_b1_2*t_tilde) + I(theta_b1_1*t_tilde) + I(Nk_2_1*t_tilde) + 
                         I(Nk_2_2*t_tilde) + I(Nk_1_1*t_tilde) + I(Nk_1_2*t_tilde) + I(plateau_2_1*t_tilde) +
                         I(plateau_2_2*t_tilde) + I(plateau_1_1*t_tilde) + I(plateau_1_2*t_tilde),
                       data = data_CF)




predict_l_w <-  linear_r_learner %>% predict(data_CF)

rscore_linear<- data_CF[, sum((y_tilde - predict_l_w * t_tilde)^2)]  # 958706349986  952096322786  972885628343

#summary(linear_r_learner)
################################################################################


# RF R_learner 

RF_train_r_learner <- 
  ranger(
    y_tilde ~ theta_b2_2*t_tilde + theta_b2_1*t_tilde + theta_b1_2*t_tilde + theta_b1_1*t_tilde + Nk_2_1*t_tilde + 
      Nk_2_2*t_tilde + Nk_1_1*t_tilde+ Nk_1_2*t_tilde+ plateau_2_1*t_tilde+ plateau_2_2*t_tilde+ plateau_1_1*t_tilde+ 
      plateau_1_2*t_tilde,     
    data = data_CF
  )

rscore_RF<- data_CF[, sum((y_tilde - RF_train_r_learner$predictions * t_tilde)^2)]  # 1.146052e+12  1.136716e+12   1.16116e+12
#format(1.146052e+12, scientific=FALSE)



####### More field CF

library(data.table)
library(tidyverse)
library(rsample)
library(xgboost)
library(rlearner)
library(rsample)
library(grf)
library(glmnet)

install.packages("rlearner")








#read_data <- readRDS("Shared/Data/all_sim_data.rds")

#data CF

more_field_cf <- function(m){
  
  
  
  data_for_CF_field_7 <- read_data[3, ]$reg_data[[1]]$data[[m]]
  
  
  data_CF_field_7 <- data_for_CF_field_7[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                                             "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                                             "plateau_1_2" ,"yield" ,  "N_tgt")]
  
  
  #tibble::rowid_to_column(., "id")%>%
  #%>%subset(., N_tgt %in% c(109, 142))
  
  ################################################################################
  # 1: CF
  
  # Estimate E(Y|X)  and calculate y_tilde
  
  #rf_trained_y <-
  #regression_forest(
  #X = data_CF[, .(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2,
  #  Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, plateau_1_1, plateau_1_2)],
  # Y = data_CF[, yield]
  #)
  
  rf_trained_y_field_7 <- 
    ranger(
      yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
        Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2,     
      data = data_CF_field_7
    )
  #=== out-of-bag prediction of Y ===#
  data_CF_field_7[, y_hat := rf_trained_y_field_7$predictions]
  
  #=== calculate y_tilde ===#
  data_CF_field_7[, y_tilde := yield - y_hat]
  
  
  # Estimate E(T|X) and calcualte T_tilde 
  
  # rf_trained_t <-
  #   probability_forest(
  #     X = data_CF[, .(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2,
  #                     Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, plateau_1_1, plateau_1_2)],
  #     Y = data_CF[, factor(N_tgt)],
  #   )
  
  rf_trained_t_field_7 <- 
    ranger(
      N_tgt ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
        Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2,     
      data = data_CF_field_7
    )
  #=== out-of-bag prediction of T ===#
  data_CF_field_7[, t_hat := rf_trained_t_field_7$predictions]
  
  #=== calculate t_hat ===#
  data_CF_field_7[, t_tilde := N_tgt - t_hat]
  
  
  
  cf_trained_field_7 <-
    causal_forest(
      X = data_CF_field_7[, .(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2,
                              Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, plateau_1_1, plateau_1_2)],
      Y = data_CF_field_7[, yield],
      W = data_CF_field_7[, N_tgt],
      Y.hat = data_CF_field_7[, y_hat],
      W.hat = data_CF_field_7[, t_hat],
    )
  
  
  rscore_cf_field_7 <- data_CF_field_7[, sum((y_tilde - cf_trained_field_7$predictions * t_tilde)^2)]  #354121300
  
  
  
  ################################################################################
  ################################################################################
  
  #BRF
  
  # Train a BRF_R_learner
  
  data_for_brf_x_field_7 <- data_CF_field_7 %>% subset(., select=-c(yield,N_tgt,y_hat,t_hat))  
  
  df_field_7 <- data_frame(theta_b2_2=data_for_brf_x_field_7$theta_b2_2*data_for_brf_x_field_7$t_tilde, 
                           theta_b2_1=data_for_brf_x_field_7$theta_b2_1*data_for_brf_x_field_7$t_tilde,
                           theta_b1_2=data_for_brf_x_field_7$ theta_b1_2*data_for_brf_x_field_7$t_tilde,
                           theta_b1_1=data_for_brf_x_field_7$ theta_b1_1*data_for_brf_x_field_7$t_tilde,
                           Nk_2_1=data_for_brf_x_field_7$Nk_2_1*data_for_brf_x_field_7$t_tilde,
                           Nk_2_2=data_for_brf_x_field_7$Nk_2_2*data_for_brf_x_field_7$t_tilde,
                           Nk_1_1=data_for_brf_x_field_7$Nk_1_1*data_for_brf_x_field_7$t_tilde,
                           Nk_1_2=data_for_brf_x_field_7$Nk_1_2*data_for_brf_x_field_7$t_tilde,
                           plateau_2_1=data_for_brf_x_field_7$plateau_2_1*data_for_brf_x_field_7$t_tilde,
                           plateau_2_2=data_for_brf_x_field_7$plateau_2_2*data_for_brf_x_field_7$t_tilde,
                           plateau_1_1=data_for_brf_x_field_7$plateau_1_1*data_for_brf_x_field_7$t_tilde,
                           plateau_1_2=data_for_brf_x_field_7$plateau_1_2*data_for_brf_x_field_7$t_tilde,
  )
  
  x_brf_cf_field_7 <-as.matrix(df_field_7)
  y_brf_cf_field_7 <- data_for_brf_x_field_7$y_tilde
  
  boosted.forest_train_cf_field_7 <- boosted_regression_forest(x_brf_cf_field_7 , y_brf_cf_field_7)
  
  # BRF r_score 
  
  rscore_brf_field_7 <- data_CF_field_7[, sum((y_tilde - boosted.forest_train_cf_field_7$predictions * t_tilde)^2)]  # 961578272932   
  #format(6.14393e+11, scientific=FALSE)
  
  ################################################################################
  # Linear R_learner 
  
  linear_r_learner_field_7 <- lm(y_tilde ~  I(theta_b2_2*t_tilde) + I(theta_b2_1*t_tilde) + 
                                   I(theta_b1_2*t_tilde) + I(theta_b1_1*t_tilde) + I(Nk_2_1*t_tilde) + 
                                   I(Nk_2_2*t_tilde) + I(Nk_1_1*t_tilde) + I(Nk_1_2*t_tilde) + I(plateau_2_1*t_tilde) +
                                   I(plateau_2_2*t_tilde) + I(plateau_1_1*t_tilde) + I(plateau_1_2*t_tilde),
                                 data = data_CF_field_7)
  
  
  
  
  predict_l_w_field_7 <-  linear_r_learner_field_7 %>% predict(data_CF_field_7)
  
  rscore_linear_field_7<- data_CF_field_7[, sum((y_tilde - predict_l_w_field_7 * t_tilde)^2)]  # 820252512902
  
  summary(linear_r_learner)
  ################################################################################
  
  
  # RF R_learner 
  
  RF_train_r_learner_field_7 <- 
    ranger(
      y_tilde ~ theta_b2_2*t_tilde + theta_b2_1*t_tilde + theta_b1_2*t_tilde + theta_b1_1*t_tilde + Nk_2_1*t_tilde + 
        Nk_2_2*t_tilde + Nk_1_1*t_tilde+ Nk_1_2*t_tilde+ plateau_2_1*t_tilde+ plateau_2_2*t_tilde+ plateau_1_1*t_tilde+ 
        plateau_1_2*t_tilde,     
      data = data_CF_field_7
    )
  
  rscore_RF_field_7<- data_CF_field_7[, sum((y_tilde - RF_train_r_learner_field_7$predictions * t_tilde)^2)]  #  1.001471e+12
  #format(1.146052e+12, scientific=FALSE)
  
  results_cf_more_fields <- data_frame(cf=rscore_cf_field_7, brf=rscore_brf_field_7, rf=rscore_RF_field_7,
                                       linear=rscore_linear_field_7)
}



results_cf_more_fields <-  mclapply(1:50, more_field_cf , mc.cores = detectCores() - 1)
results_cf_more_fields2 <-  mclapply(100:800, more_field_cf , mc.cores = detectCores() - 1)





















































