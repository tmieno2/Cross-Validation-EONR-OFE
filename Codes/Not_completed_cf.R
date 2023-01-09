############################ EONR CF #########################################

read_data <- readRDS("Shared/Data/all_sim_data.rds")
data_field_2 <- read_data[3, ]$reg_data[[1]]$data[[2]]

data <- data_field_2[, c("theta_b2_2", "theta_b2_1","theta_b1_2" ,"theta_b1_1","Nk_2_1","Nk_2_2" ,
                         "Nk_1_1","Nk_1_2" ,"plateau_2_1" ,"plateau_2_2" ,  "plateau_1_1" ,
                         "plateau_1_2" ,"yield" ,  "N_tgt")] %>% tibble::rowid_to_column(., "id")


# Create the treat and the control list 

treat <- sort(unique(data$N_tgt))[-1]     #143 177 210 243

control <- min(unique(data$N_tgt))        #110

# Create data based on the treat and control

generate_data_for_cf <- function(i) {
  
  cf_data <- data %>% 
    filter(N_tgt %in% c(control, treat[i]))%>% 
    mutate(treat = case_when(
      N_tgt == control  ~ 0,
      N_tgt == treat[i]  ~ 1)) %>%
    dplyr::select(c(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2, Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, 
                    plateau_1_1,plateau_1_2,yield,id, treat))
 
  
  # CF: Stage 1  
  rf_trained_y <-
    regression_forest(
      X = cf_data[, .(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2, Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, 
                   plateau_1_1,plateau_1_2)],
      Y = cf_data[, yield]
    ) 
  
  #=== out-of-bag prediction of Y ===#
  cf_data[, y_hat := rf_trained_y$predictions]
  
  #=== calculate y_hat ===#
  cf_data[, y_tilde := yield - y_hat]
  
  rf_trained_t <-
    probability_forest(
      X = cf_data[, .(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2, Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, 
                      plateau_1_1,plateau_1_2)],
      Y = cf_data[, factor(treat)],
    )
  
  #=== out-of-bag prediction of T ===#
  cf_data[, t_hat := rf_trained_t$predictions[, 2]]
  
  #=== calculate t_hat ===#
  cf_data[, t_tilde := treat - t_hat] 
  
  # train CF
  
  cf_trained <-
    causal_forest(
      X = cf_data[, .(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2, Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, 
                   plateau_1_1,plateau_1_2)],
      Y = cf_data[, yield],
      W = cf_data[, treat],
      Y.hat = cf_data[, y_hat],
      W.hat = cf_data[, t_hat],
      tune.parameters = "all"
    )
 
new_data <- data[, .(theta_b2_2, theta_b2_1, theta_b1_2, theta_b1_1, Nk_2_1, Nk_2_2, Nk_1_1, Nk_1_2, plateau_2_1, plateau_2_2, 
                     plateau_1_1,plateau_1_2)] %>% as.matrix()  
  
 pred <- predict(cf_trained, new_data)  
 return(cf_data) 
}

treat_control_data_for_cf <-  mclapply(1:length(treat), generate_data_for_cf , mc.cores = detectCores() - 1)


predict(trained_model, data_new)