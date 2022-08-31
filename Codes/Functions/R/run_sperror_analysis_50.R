# /*===========================================================
#' # Spatial Error Model
# /*===========================================================
# ! Run GWR and do economic analysis
# Debugging instruction
# 1. run all the chunks in 1_simulation.Rmd except the last one.
# 2. run source(here("GitControlled/Codes/Functions/prepare_debug_data.R"))
# 3. run Wls <- weights_matrix$Wls_50

# a <- run_sperror_analysis_50(reg_data, cv_data, x_vars, pN, pCorn, N_levels, Wls)
# a <- run_linear_analysis(reg_data, cv_data, x_vars, pN, pCorn, N_levels)

run_sperror_analysis_50 <- function(reg_data, cv_data, x_vars, pN, pCorn, N_levels, Wls) {
  #* define train and test datasets

  x_vars_in_formula <-
    x_vars %>%
    str_replace_all("\\(", "_") %>%
    str_replace_all("\\)", "") %>%
    str_replace_all("\\^", "__")

  train_data <-
    copy(reg_data) %>%
    extract_vars_dt(c("aunit_id", "yield", "N", "N2", x_vars)) %>%
    #* change variables names so they are usable in ser estimation
    setnames(
      x_vars,
      x_vars_in_formula
    )

  test_data <-
    copy(cv_data$data[[1]]) %>%
    extract_vars_dt(c("aunit_id", "yield", "N", "N2", x_vars)) %>%
    #* change variables names so they are usable in ser estimation
    setnames(
      x_vars,
      x_vars_in_formula
    )

  #* Scale the train data
  #* scaling helps model convergence
  scaler_data <-
    train_data %>%
    melt(id.vars = "aunit_id") %>%
    nest_by(variable) %>%
    mutate(scaler = 1 / max(data$value)) %>%
    data.table() %>%
    .[, .(variable, scaler)]

  scaled_train_data <-
    train_data %>%
    scale_data(., scaler_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Regression
  # /*+++++++++++++++++++++++++++++++++++
  #* === define the formula ===*#
  control_vars <- paste0(x_vars_in_formula, collapse = "+")

  int_vars <-
    paste0(
      paste0("I(N * ", x_vars_in_formula, ")", collapse = "+"),
      "+",
      paste0("I(N2 * ", x_vars_in_formula, ")", collapse = "+")
    )

  #* === define the regression formula ===*#
  feols_formula <-
    paste0(
      "yield ~ N + N2 + ",
      control_vars, "+",
      int_vars
    )

  # === estimating the spatial error model ===#
  #* Note: see sarlm_predict_test.Rmd for why
  #* the evaluation of eval(..) is necessary
  eval(parse(text = paste(
    "ser_res <- spatialreg::errorsarlm(formula = ",
    feols_formula,
    ", data = scaled_train_data, listw = Wls)",
    sep = ""
  )))

  # /*+++++++++++++++++++++++++++++++++++
  #' ## get performance (RMSE)
  # /*+++++++++++++++++++++++++++++++++++
  e_hat_train <-
    copy(scaled_train_data) %>%
    .[, yield := predict(ser_res, newdata = .) %>% data.frame() %>% pull(trend)] %>%
    scale_data(., scaler_data, back = TRUE) %>%
    .[, .(aunit_id, yield)] %>%
    setnames("yield", "y_hat") %>%
    .[train_data, on = "aunit_id"] %>%
    .[, e_hat := yield - y_hat] %>%
    .[, .(aunit_id, e_hat)] %>%
    setnames("e_hat", "e_hat_train")

  e_hat_cv <-
    copy(test_data) %>%
    scale_data(., scaler_data) %>%
    .[, yield := predict(ser_res, newdata = .) %>% data.frame() %>% pull(trend)] %>%
    scale_data(., scaler_data, back = TRUE) %>%
    .[, .(aunit_id, yield)] %>%
    setnames("yield", "y_hat") %>%
    .[cv_data$data[[1]], on = "aunit_id"] %>%
    .[, e_hat := yield - y_hat] %>%
    .[, .(aunit_id, e_hat)] %>%
    setnames("e_hat", "e_hat_cv")

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Find EONR
  # /*+++++++++++++++++++++++++++++++++++
  #* === define N space ===*#
  N_data <-
    data.table(
      N = seq(min(N_levels), max(N_levels), length = 50)
    ) %>%
    .[, N2 := N^2]

  #* === get site-specific EONR ===*#
  results <-
    copy(test_data) %>%
    .[, `:=`(
      N = NULL,
      N2 = NULL
    )] %>%
    #--- prediction data ---
    expand_grid_df(., N_data) %>%
    scale_data(., scaler_data) %>%
    .[, yield := predict(ser_res, newdata = .) %>% data.frame() %>% pull(trend)] %>%
    .[, .(aunit_id, yield, N)] %>%
    scale_data(., scaler_data, back = TRUE) %>%
    .[, pi_hat := pCorn * yield - pN * N] %>%
    #--- optimal N ---
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    #--- RMSE ---
    .[e_hat_train, on = "aunit_id"] %>%
    .[e_hat_cv, on = "aunit_id"] %>%
    .[, sim := cv_data$sim]

  return(results)
}