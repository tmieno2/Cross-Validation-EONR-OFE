library(ranger)

#Data

data_to_work_with <- data[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                              "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                              "plateau_1_2" ,"yield" , "X","Y" ,  "N_tgt"  )]%>%
  
  tibble::rowid_to_column(., "id")

#--- recognize data as an sf ---#
data_sf <- st_as_sf(data_to_work_with, coords = c("X", "Y"))


# Folds
skcv_folds <- spatial_clustering_cv(data_sf, v = 6) 

################################################################################
# training and test data
scv <-
  skcv_folds %>% 
  rowwise() %>% 
  mutate(
    training_data = list(analysis(splits)),
    test_data = list(assessment(splits))
  )
################################################################################

train_data <- scv[["training_data"]][[1]]%>%st_drop_geometry()
test_data <- scv[["test_data"]][[1]]%>%st_drop_geometry()

################################################################################

EONR_each_fold <- function(i){
  train_data <- scv[["training_data"]][[i]]%>%st_drop_geometry()
  test_data <- scv[["test_data"]][[i]]%>%st_drop_geometry()
  
  # RF train  
  
  RF <- 
    ranger(
      yield ~ theta_b2_2 + theta_b2_1 + theta_b1_2 + theta_b1_1 + Nk_2_1 + Nk_2_2 + Nk_1_1+ 
        Nk_1_2+ plateau_2_1+ plateau_2_2+ plateau_1_1+ plateau_1_2+ N_tgt,     
      data = train_data
    )
  
  # RF predict 
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
  
  #not_return_dta <- data_frame(EONR_fold=EONR_1)
  #%>%unlist()%>% data_frame()%>%mean()
  data_to_r <- data_frame(ff=mean_each_fold)
  
  return(data_to_r)  
}

EONR_fold<- lapply(1:6, EONR_each_fold)%>%unlist()%>% data_frame() ## RF

################################################################################
#Train gam

gam_EONR_each_fold <- function(i){
  train_data <- scv[["training_data"]][[i]]%>%st_drop_geometry()
  test_data <- scv[["test_data"]][[i]]%>%st_drop_geometry()
  
#gam train  
  gam <- 
    gam(yield ~ s(N_tgt,k=2), data = train_data)
  
# gam predict 
  gam_EONR <- lapply(1:nrow(test_data), function(i){
    row=1
    times = 134
    
    #prediction for each cell
    gam_test_data_cell <- test_data[i,]%>% subset(., select=-c(N_tgt))%>% 
      .[rep(row, times),]%>% mutate(N_tgt=seq(109,242,by=1))
    
    gam_pred <- predict(gam, gam_test_data_cell)%>% data_frame()
    
    gam_test_data_cell$y_hat <- gam_pred
    
    gam_test_data_cell <- gam_test_data_cell%>% mutate(profit=(pCorn*y_hat)-(pN*N_tgt))
    
    
    gam_EONR <- gam_test_data_cell [which.max(profit), ]$N_tgt  #each cell
    data_to_return <- data_frame(EONR=gam_EONR)  #each cell
    return(data_to_return)
    
  })%>%unlist()%>% data_frame()
  
  gam_mean_each_fold <- mean(gam_EONR$.)
  
  gam_data_to_r <- data_frame(gg=gam_mean_each_fold)
  
  return(gam_data_to_r)  
}

gam_EONR_fold<- lapply(1:6, gam_EONR_each_fold)%>%unlist()%>% data_frame() ## gam


################################################################################
#difference in EONR by fold 

colnames(EONR_fold) <- c("RF_EONR")
colnames(gam_EONR_fold) <- c("gam_EONR")


# merge two data frames 
difference_in_EONR_by_fold <-cbind(EONR_fold, gam_EONR_fold)%>% 
  mutate(diff_EONR=(RF_EONR-gam_EONR)^2)%>% sum()

################################################################################













