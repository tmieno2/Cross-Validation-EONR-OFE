# Linear 10 rep, S_learner

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






















