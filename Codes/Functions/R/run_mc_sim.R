run_mc_sim <- function(x_vars, models_data, all_sim_data, sim_range, exp_sets = NULL) {
  #* print scenario
  print(x_vars)

  if (is.null(exp_sets)) {
    exp_sets <- all_sim_data$exp_set_id
  }

  control_vars <- paste0(x_vars, collapse = "+")

  int_vars <-
    paste0(
      paste0("I(N * ", x_vars, ")", collapse = "+"),
      "+",
      paste0("I(N2 * ", x_vars, ")", collapse = "+")
    )

  #* spatialreg::errorsarlm cannot handle formula defined inside a function, so making it global here
  feols_formula <<-
    paste0(
      "yield ~ N + N2 + ",
      control_vars, "+",
      int_vars
    ) %>%
    formula()

  nsim <- 1000

  # reg_data <- all_sim_data$reg_data[[3]]
  all_results <-
    all_sim_data %>%
    #* filter out exp_set_id (by default, it does not filter out any)
    filter(exp_set_id %in% exp_sets) %>%
    mutate(sim_results = list(
      get_eonr_by_exp_setting(
        sim_range = sim_range,
        models_data = models_data,
        x_vars = x_vars,
        field_pars = field_pars,
        field_sf = field_sf,
        reg_data = reg_data,
        weights_matrix = weights_matrix,
        pN = pN,
        pCorn = pCorn,
        nsim = nsim
      )
    )) %>%
    dplyr::select(exp_set_id, sim_results) %>%
    unnest()

  return(all_results)
}