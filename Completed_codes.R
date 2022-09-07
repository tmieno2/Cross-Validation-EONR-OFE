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

#Data

data <- readRDS("Shared/Data/all_sim_data.rds")
data <- data[3, ]$reg_data[[1]]$data[[1]]

main_data <- data[, c(3, 4, 5, 6, 25, 26,27,28, 31, 32, 33, 34,44, 46, 47, 48  )]%>%
  
  tibble::rowid_to_column(., "id")%>%
  subset(., N_tgt %in% c(109, 142))

#change the name
colnames(main_data) <- c("id", "x1","x2", "x3","x4", "x5","x6", "x7","x8", 
                         "x9","x10", "x11","x12","y","g1","g2","T")


# datamon$N_tgt [datamon$N_tgt == "142"] <- TRUE
# datamon$N_tgt [datamon$N_tgt == "109"] <- FALSE
# datamon$N_tgt <- as.logical(datamon$N_tgt)

#--- recognize it as an sf ---#
main_data_sf <- st_as_sf(main_data, coords = c("g1", "g2"))


# Folds
skcv_folds <- spatial_clustering_cv(main_data_sf, v = 6) 
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
#function

m <- function(i){
  data_train <- scv[[3]][[i]]%>%st_drop_geometry()
  data_test <- scv[[4]][[i]]%>%st_drop_geometry()
  
  #CF trained
  
  cf_trained <-
    causal_forest(
      X = select(data_train, starts_with("x")),
      Y = data_train[,y],
      W = data_train[,T],
      num.trees = 5000,
      seed = 1839
    )
  
  #CF predictions
  
  preds <- predict(
    cf_trained, 
    newdata = data_test[, 2:13], 
    estimate.variance = TRUE
  )
  data_test$preds <- preds$predictions
  
  treatment_effect_from_blue <- mean(data_test$preds) # 21.39055
  data_to_return <- data.frame(treatment_effect_CF=treatment_effect_from_blue)
  
  return(data_to_return)
}


treatment_effect_from_blue <- lapply(1:6, m)%>%unlist()%>% data_frame()

Averaged_treatment_effect_from_blue <- mean(treatment_effect_from_blue$.)   #17.4479   (16.96163)

################################################################################
gam_function <- function(i){
  test_d <- scv[[4]][[i]]%>%st_drop_geometry()
  
  
  
  gam_fit <- gam(y ~T+ s(x1,k=5)+s(x2,k=5)+s(x3,k=2)+s(x4,k=5)+s(x5,k=5)+s(x6,k=5)+s(x7,k=5)+s(x8,k=5)+
                   s(x9,k=5)+s(x10,k=5)+s(x11,k=5)+s(x12,k=5), data = test_d)
  
  coef_gam <- gam_fit$coefficients[2]  # gam_fit$coefficients[2]
  
  
  returned_data <- data_frame(gam_results=coef_gam)
  
  return(returned_data)
  
  
}

treatment_effect_from_red <- lapply(1:6, gam_function)%>%unlist()%>% data_frame()

Averaged_treatment_effect_from_red <- mean(treatment_effect_from_red$.)   #17.81239   (17.79306)

################################################################################

difference <- Averaged_treatment_effect_from_red-Averaged_treatment_effect_from_blue  #0.3644904   (0.8314325)

################################################################################