# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis

# x_vars <- c("Nk", "Nk^2", "log(plateau)", "sqrt(b0)")

run_linear_analysis <- function(reg_data, cv_data, x_vars, pN, pCorn, N_levels) {
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

  #* === define the formula ===*#
  feols_formula <-
    paste0(
      "yield ~ N + N2 + ",
      control_vars, "+",
      int_vars
    ) %>%
    formula()

  #* === run OLS ===*#
  ols_res <- fixest::feols(feols_formula, data = train_data)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## get performance (RMSE)
  # /*+++++++++++++++++++++++++++++++++++
  e_hat_train <-
    copy(train_data) %>%
    .[, y_hat := predict(ols_res, newdata = .)] %>%
    .[, e_hat := yield - y_hat] %>%
    .[, .(aunit_id, e_hat)] %>%
    setnames("e_hat", "e_hat_train")

  e_hat_cv <-
    copy(test_data) %>%
    .[, y_hat := predict(ols_res, newdata = .)] %>%
    .[, e_hat := yield - y_hat] %>%
    .[, .(aunit_id, e_hat)] %>%
    setnames("e_hat", "e_hat_cv")

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Find EONR
  # /*+++++++++++++++++++++++++++++++++++
  N_data <-
    data.table(
      N = seq(min(N_levels), max(N_levels), length = 50)
    ) %>%
    .[, N2 := N^2]

  results <-
    copy(test_data) %>%
    .[, `:=`(
      N = NULL,
      N2 = NULL
    )] %>%
    #--- prediction data ---
    expand_grid_df(., N_data) %>%
    .[, y_hat := predict(ols_res, newdata = .)] %>%
    .[, pi_hat := pCorn * y_hat - pN * N] %>%
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