
x_vars <-
  c(
    "b0_1", "b0_2",
    # "b1_2_1", "b1_2_2", "b1_1_1", "b1_1_2",
    # "b2_2_1", "b2_2_2", "b2_1_1", "b2_1_2",
    "Nk_2_1", "Nk_2_2", "Nk_1_1", "Nk_1_2",
    "plateau_2_1", "plateau_2_2", "plateau_1_1", "plateau_1_2",
    "theta_plateau_2", "theta_Nk_2", "theta_b0_2"
  )
nsim <- 1000
sim_range <- 1
field_pars <- all_sim_data$field_pars[[1]]
field_sf <- all_sim_data$field_sf[[1]]
reg_data <- all_sim_data$reg_data[[1]]
weights_matrix <- all_sim_data$weights_matrix[[1]]
pN <- all_sim_data$pN[[1]]
pCorn <- all_sim_data$pCorn[[1]]

field_pars <-
  field_pars %>%
  #--- True cell-level EONR ---#
  .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
  .[, opt_N := pmin(Nk, opt_N)] %>%
  .[, opt_N := pmax(0, opt_N)] %>%
  #--- True optimal profit ---#
  .[, yield_opt := gen_yield_QP(b0, b1, b2, Nk, opt_N)] %>%
  .[, profit_opt := pCorn * yield_opt - pN * opt_N]

Wls <- weights_matrix

sim_i <- x <- 1
sim_data <- reg_data

train_data_set <- sim_data[sim == sim_i, ]
cv_data_set <- sim_data[sim == ifelse(sim_i + 1 > nsim, 1, sim_i + 1), ]

#* define parameters
N_levels <- train_data_set$N_levels[[1]]
train_data <-
  copy(train_data_set$data[[1]]) %>%
  .[, x := (X - min(X)) / (max(X) - min(X))] %>%
  .[, y := (Y - min(Y)) / (max(Y) - min(Y))] %>%
  .[, xy := x * y] %>%
  .[, N2 := N^2]

reg_data <- train_data
cv_data <- cv_data_set