find_local_EONR_gam <- function(data, test_data) {
  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))

  gam <- gam(yield ~ s(N_tgt, k = 3), data = test_data)

  opt_EONR <-
    copy(N_data) %>%
    .[, y_hat := predict(gam, newdata = .)] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ]]

  return(opt_EONR)
}

find_local_EONR_RF <- function(data, X, Y, test_data) {
  #--- do tuning ---#
  regression_forest <- regression_forest(X, Y)

  N_data <- data.table(N_tgt = seq(min(data$N_tgt), max(data$N_tgt), by = 1))
  X_test <-
    test_data[, c(cov_list, "aunit_id"), with = FALSE] %>%
    .[, N_tgt := NULL] %>%
    reshape::expand.grid.df(N_data, .) %>%
    data.table()

  opt_EONR <-
    copy(X_test) %>%
    .[, y_hat := predict(regression_forest, newdata = .[, cov_list, with = FALSE])] %>%
    .[, pi_hat := (pCorn * y_hat) - (pN * N_tgt)] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id]

  return(opt_EONR)
}
