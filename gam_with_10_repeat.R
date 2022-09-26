# Linear 10 rep, S_learner

try <- function(n){
  
  skcv_folds_try <- spatial_clustering_cv(data_sf, v = 6) 
  
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
gam_fun <- function(n){ 
  yy <- list_of_repeat[n]%>%data.frame()
  
  gam <- lapply(1:6, function(i){
    
    train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
    
    test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()
    
    # gam  train   
    
    gam_train <- 
      gam(yield ~ s(N_tgt), data = train_data)
    
    
    # gam predict 
    EONR_1 <- lapply(1:nrow(test_data), function(i){
      row=1
      times = 134
      #prediction for each cell
      test_data_cell <- test_data[i,]%>% subset(., select=-c(N_tgt))%>% 
        .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
      
      predict_gam <- predict(gam, test_data_cell)%>% data_frame()
      
      
      test_data_cell$y_hat <-  predict_gam
      
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
  
  
  data_to_reu <- gam
  return(data_to_reu)
}

#linear_results <- lapply(1:10, linear_function)
gam_results_10_repeats <- mclapply(1:10, linear_function, mc.cores = detectCores() - 1)


#################################################################################

gam1<-  gam_results_10_repeats[1]%>%data.frame()%>% setnames(.,"repeat1")
gam2 <- gam_results_10_repeats[2]%>%data.frame()%>% setnames(.,"repeat2")
gam3 <- gam_results_10_repeats[3]%>%data.frame()%>% setnames(.,"repeat3")
gam4 <- gam_results_10_repeats[4]%>%data.frame()%>% setnames(.,"repeat4")
gam5 <- gam_results_10_repeats[5]%>%data.frame()%>% setnames(.,"repeat5")
gam6 <- gam_results_10_repeats[6]%>%data.frame()%>% setnames(.,"repeat6")
gam7 <- gam_results_10_repeats[7]%>%data.frame()%>% setnames(.,"repeat7")
gam8 <- gam_results_10_repeats[8]%>%data.frame()%>% setnames(.,"repeat8")
gam9 <- gam_results_10_repeats[9]%>%data.frame()%>% setnames(.,"repeat9")
gam10 <-gam_results_10_repeats[10]%>%data.frame()%>% setnames(.,"repeat10")

gam_folds <- cbind(gam1,gam2,gam3,gam4,gam5,
                   gam6,gam7,gam8,gam9,gam10)%>%
  mutate(mean_of_repeats=rowMeans(.))

#################################################################################






















