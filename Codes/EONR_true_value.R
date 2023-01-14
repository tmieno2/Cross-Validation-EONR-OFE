# True EONR, Not sure if I'm correct

#Data

data_sf_true_eonr <- st_as_sf(data, coords = c("X", "Y"))


# Folds
skcv_folds_true <- spatial_clustering_cv(data_sf_true_eonr, v = 6) 

################################################################################
# training and test data
scv_true <-
  skcv_folds_true %>% 
  rowwise() %>% 
  mutate(
    training_data = list(analysis(splits)),
    test_data = list(assessment(splits))
  )

EONR_each_fold_true <- function(i){
  #train_data <- scv_true[["training_data"]][[i]]%>%st_drop_geometry()
  test_data <- scv_true[["test_data"]][[i]]%>%st_drop_geometry()
  
  data_true_eonr <-  test_data %>%
    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, opt_N := pmin(Nk, opt_N)] %>%
    .[, opt_N := pmax(0, opt_N)]
  
  
  EONR_ttrue <- data_true_eonr$opt_N 
  
  
  mean_each_fold_true <- mean(EONR_ttrue)
  
  data_to_r <- data_frame(ff=mean_each_fold_true)
  
  return(data_to_r)  
}

EONR_fold_true<- lapply(1:6, EONR_each_fold_true)%>%unlist()%>% data_frame() 

EONR_fold_true <- EONR_fold_true%>% setnames(. , "TRUE_EONR")





