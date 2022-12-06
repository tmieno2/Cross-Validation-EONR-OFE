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


rscore_cf <- data_CF[, sum((y_tilde - cf_trained$predictions * t_tilde)^2)]  #288576069   #292848669



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

rscore_brf <- data_CF[, sum((y_tilde - boosted.forest_train_cf$predictions * t_tilde)^2)]  # 1.082753e+12   922029561188
#format(6.14393e+11, scientific=FALSE)

################################################################################
# Linear R_learner 

linear_r_learner <- lm(y_tilde ~  I(theta_b2_2*t_tilde) + I(theta_b2_1*t_tilde) + 
                         I(theta_b1_2*t_tilde) + I(theta_b1_1*t_tilde) + I(Nk_2_1*t_tilde) + 
                         I(Nk_2_2*t_tilde) + I(Nk_1_1*t_tilde) + I(Nk_1_2*t_tilde) + I(plateau_2_1*t_tilde) +
                         I(plateau_2_2*t_tilde) + I(plateau_1_1*t_tilde) + I(plateau_1_2*t_tilde),
                       data = data_CF)




predict_l_w <-  linear_r_learner %>% predict(data_CF)

rscore_linear<- data_CF[, sum((y_tilde - predict_l_w * t_tilde)^2)]  # 958706349986  952096322786

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

rscore_RF<- data_CF[, sum((y_tilde - RF_train_r_learner$predictions * t_tilde)^2)]  # 1.146052e+12  1.136716e+12
#format(1.146052e+12, scientific=FALSE)
