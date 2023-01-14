# Linear 10 rep, S_learner

try <- function(n) {
  skcv_folds_try <- spatial_clustering_cv(data_sf_2, v = 6)

  scv_try <-
    skcv_folds_try %>%
    rowwise() %>%
    mutate(
      training_data = list(analysis(splits)),
      test_data = list(assessment(splits))
    )
  data_to_re <- scv_try
}

list_of_repeat <- lapply(1:10, try)

#################################################################################
EONR_true <- function(n) {
  yy <- list_of_repeat[n] %>% data.frame()

  true <- lapply(1:6, function(i) {

    # train_data <- yy[["training_data"]][[i]]%>%st_drop_geometry()

    test_data <- yy[["test_data"]][[i]] %>% st_drop_geometry()

    # true
    True_EONR <- test_data %>%
      st_drop_geometry() %>%
      .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
      .[, opt_N := pmin(Nk, opt_N)] %>%
      .[, opt_N := pmax(0, opt_N)]

    data_to_retur <- mean(True_EONR$opt_N)
    return(data_to_retur)
  }) %>%
    unlist() %>%
    data_frame()

  data_to_reu <- true
  return(data_to_reu)
}

true <- mclapply(1:10, EONR_true, mc.cores = detectCores() - 1)


#################################################################################

true1 <- true[1] %>%
  data.frame() %>%
  setnames(., "repeat1")
true2 <- true[2] %>%
  data.frame() %>%
  setnames(., "repeat2")
true3 <- true[3] %>%
  data.frame() %>%
  setnames(., "repeat3")
true4 <- true[4] %>%
  data.frame() %>%
  setnames(., "repeat4")
true5 <- true[5] %>%
  data.frame() %>%
  setnames(., "repeat5")
true6 <- true[6] %>%
  data.frame() %>%
  setnames(., "repeat6")
true7 <- true[7] %>%
  data.frame() %>%
  setnames(., "repeat7")
true8 <- true[8] %>%
  data.frame() %>%
  setnames(., "repeat8")
true9 <- true[9] %>%
  data.frame() %>%
  setnames(., "repeat9")
true10 <- true[10] %>%
  data.frame() %>%
  setnames(., "repeat10")

true_folds <- cbind(
  true1, true2, true3, true4, true5,
  true6, true7, true8, true9, true10
) %>%
  mutate(mean_of_repeats = rowMeans(.))

#################################################################################
