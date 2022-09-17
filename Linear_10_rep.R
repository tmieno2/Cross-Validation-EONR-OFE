# Linear 10 rep, S_learner

try <- function(n){
  
  skcv_folds_try <- spatial_clustering_cv(data_sf, v = 6) 
  
  scv_try<-
    skcv_folds %>% 
    rowwise() %>% 
    mutate(
      training_data = list(analysis(splits)),
      test_data = list(assessment(splits))
      
    )
  data_to_re <- scv_try
}

list_of_repeat <- lapply(1:10, try)

#################################################################################
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

linear_results <- lapply(1:10, linear_function)

#################################################################################

linear_df_rep1<- linear_results[1]%>%data.frame()%>% setnames(.,"repeat1")
linear_df_rep2 <- linear_results[2]%>%data.frame()%>% setnames(.,"repeat2")
linear_df_rep3 <- linear_results[3]%>%data.frame()%>% setnames(.,"repeat3")
linear_df_rep4 <- linear_results[4]%>%data.frame()%>% setnames(.,"repeat4")
linear_df_rep5 <- linear_results[5]%>%data.frame()%>% setnames(.,"repeat5")
linear_df_rep6 <- linear_results[6]%>%data.frame()%>% setnames(.,"repeat6")
linear_df_rep7 <- linear_results[7]%>%data.frame()%>% setnames(.,"repeat7")
linear_df_rep8 <- linear_results[8]%>%data.frame()%>% setnames(.,"repeat8")
linear_df_rep9 <- linear_results[9]%>%data.frame()%>% setnames(.,"repeat9")
linear_df_rep10 <-linear_results[10]%>%data.frame()%>% setnames(.,"repeat10")

linear_folds <- cbind(linear_df_rep1,linear_df_rep2,linear_df_rep3,linear_df_rep4,linear_df_rep5,
                      linear_df_rep6,linear_df_rep7,linear_df_rep8,linear_df_rep9,linear_df_rep10)%>%
  mutate(mean_of_repeats=rowMeans(.))

#################################################################################






















