# BRF for 10 rep, S_learner  
install.packages("parallel")


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
l <- function(n){ 
  yy <- list_of_repeat[n]%>%data.frame()
  
  brf <- lapply(1:6, function(i){
    
    train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()
    
    test_data <- yy[["test_data"]][[i]]%>%st_drop_geometry()%>%subset(., select=-c(yield))
    
    # boosted regression forest train   
    
    X <-as.matrix(train_data)%>%subset(., select=-c(yield))
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


brf_res <- mclapply(1:10, l, mc.cores = detectCores() - 1)


#################################################################################
df_rep1<- brf_res[1]%>%data.frame()%>% setnames(.,"repeat1")
df_rep2 <- brf_res[2]%>%data.frame()%>% setnames(.,"repeat2")
df_rep3 <- brf_res[3]%>%data.frame()%>% setnames(.,"repeat3")
df_rep4 <- brf_res[4]%>%data.frame()%>% setnames(.,"repeat4")
df_rep5 <- brf_res[5]%>%data.frame()%>% setnames(.,"repeat5")
df_rep6 <- brf_res[6]%>%data.frame()%>% setnames(.,"repeat6")
df_rep7 <- brf_res[7]%>%data.frame()%>% setnames(.,"repeat7")
df_rep8 <- brf_res[8]%>%data.frame()%>% setnames(.,"repeat8")
df_rep9 <- brf_res[9]%>%data.frame()%>% setnames(.,"repeat9")
df_rep10 <-brf_res[10]%>%data.frame()%>% setnames(.,"repeat10")

BRF_folds <- cbind(df_rep1,df_rep2,df_rep3,df_rep4,df_rep5,df_rep6,df_rep7,df_rep8,df_rep9,df_rep10)%>%
  mutate(mean_of_repeats=rowMeans(.))

#################################################################################






