# /*===========================================================
#' # RF analysis
# /*===========================================================
# ! Run RF and do economic analysis
# 1. run all the chunks in 1_simulation.Rmd except the last one.
# 2. run source(here("GitControlled/Codes/Functions/prepare_debug_data.R"))

run_xgbrf_analysis <- function(reg_data, cv_data, x_vars, pN, pCorn, N_levels, model) {
  train_data <- copy(reg_data)
  test_data <- copy(cv_data$data[[1]])

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Define X and Y
  # /*+++++++++++++++++++++++++++++++++++
  all_x_vars <- c("N", x_vars) #* include N
  X_train <- extract_vars_dt(train_data, all_x_vars)
  X_cv <- extract_vars_dt(test_data, all_x_vars)
  Y <- train_data[, yield]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Run RF
  # /*+++++++++++++++++++++++++++++++++++
  trained_model <-
    xgboost(
      data = as.matrix(X_train),
      label = Y,
      max.depth = 4,
      eta = 0.1,
      nthread = 1,
      nrounds = 300,
      objective = "reg:squarederror",
      verbose = 0
    )

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Get performance measures
  # /*+++++++++++++++++++++++++++++++++++

  e_hat_train <-
    train_data %>%
    .[, y_hat := predict(
      trained_model,
      newdata = extract_vars_dt(., all_x_vars) %>% as.matrix()
    )] %>%
    .[, e_hat := yield - y_hat] %>%
    .[, .(aunit_id, e_hat)] %>%
    setnames("e_hat", "e_hat_train")

  e_hat_cv <-
    test_data %>%
    .[, y_hat := predict(
      trained_model,
      newdata = extract_vars_dt(., all_x_vars) %>% as.matrix()
    )] %>%
    .[, e_hat := yield - y_hat] %>%
    .[, .(aunit_id, e_hat)] %>%
    setnames("e_hat", "e_hat_cv")

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Find site-specific EONR
  # /*+++++++++++++++++++++++++++++++++++
  N_seq <-
    data.table(
      N = seq(min(train_data$N), max(train_data$N), by = 2)
    )

  results <-
    test_data %>%
    #* === remove N before merging with the N data ===*#
    .[, N := NULL] %>%
    expand_grid_df(., N_seq) %>%
    .[, yield_hat := predict(
      trained_model,
      newdata = extract_vars_dt(., all_x_vars) %>% as.matrix()
    )] %>%
    .[, pi_hat := pCorn * yield_hat - pN * N] %>%
    .[, .SD[which.max(pi_hat)], by = aunit_id] %>%
    .[, opt_N_hat := N] %>%
    .[, .(aunit_id, opt_N_hat)] %>%
    #--- RMSE ---
    .[e_hat_train, on = "aunit_id"] %>%
    .[e_hat_cv, on = "aunit_id"] %>%
    .[, sim := cv_data$sim]

  return(results)
}