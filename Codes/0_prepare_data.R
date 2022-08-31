# /*===========================================================
#' # Preparation
# /*===========================================================

# === Packages ===#
library(sp)
library(spdep)
library(spatialreg)
library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(dplyr)
library(magrittr)
library(gstat)
library(GWmodel)
library(scam)
library(parallel)
library(magic)
library(stringr)
library(ggplot2)
library(tictoc)
library(here)
options(stringsAsFactors = FALSE)


# === Set working directory ===#
setwd(here())

# === load functions ===#
#* source all the functions in the Functions folder
fs::dir_ls(here("GitControlled", "Codes", "Functions", "R"), full.names = TRUE) %>%
  lapply(., function(x) source(x))

# /*===========================================================
#' # Create a field
# /*===========================================================
# ! Define field and plot characteristics here

field_data <-
  # ! This is where you set the experiment parameters
  CJ(
    # plot_length = 12, # the length of a plot (in number of cells)
    plot_length = 12, # the length of a plot (in number of cells)
    plot_width = 3, # the width of a plot (in number of cells)
    cell_buffer = 1,
    aunit_length = 2, # the length of an analysis unit (in number of cells)
    aunit_width = 3, # the width of an analysis unit (in number of cells)
    cell = 6, # the length of a cell in meter
    #* how wide the field is
    field_col = c(36, 72, 144), # the number of cell columns
    #* how tall the field is
    field_row = 72, # the number of row columns
    sp_range = c(600),
    # gstat_model = "Exp",
    gstat_model = "Sph",
    #* prices
    pCorn = 6.25 / 25.4, # $/kg
    pN = 1 / 0.453592 # $/kg
  ) %>%
  rowwise() %>%
  mutate(field_sf = list(
    make_field(
      field_col = field_col,
      field_row = field_row,
      aunit_length = aunit_length,
      aunit_width = aunit_width,
      cell = cell,
      cell_buffer = cell_buffer
    )
  )) %>%
  #* assign experiment setting ID
  ungroup() %>%
  mutate(exp_set_id = 1:n()) %>%
  relocate(exp_set_id) %>%
  rowwise()

# /*===========================================================
#' # Add trial design layout
# /*===========================================================
#* (use the fixed Latin Square design)
field_with_design <-
  field_data %>%
  rowwise() %>%
  mutate(design_layout = list(
    make_design_layout(plot_length, field_col)
  )) %>%
  dplyr::select(-plot_length) %>%
  unnest(cols = "design_layout") %>%
  filter(design_name == "Latin Square Fixed 5") %>%
  rowwise() %>%
  mutate(plot_block_id_data = list(
    gen_plot_block_ids(
      field_sf = field_sf,
      plot_length = plot_length,
      plot_width = plot_width,
      cols_plot_in_block = cols_plot_in_block,
      rows_plot_in_block = rows_plot_in_block,
      cell_buffer = cell_buffer
    )
  )) %>%
  mutate(field_sf = list(
    left_join(field_sf, plot_block_id_data, by = "cell_id")
  )) %>%
  dplyr::select(-plot_block_id_data) %>%
  mutate(
    data_file_name =
      paste0(
        stringr::str_replace_all(design_name, " ", ""),
        "_", field_col, ".rds"
      ) %>%
        paste0("Shared/Data/", .)
  )

# /*===========================================================
#' # Generate true field parameters
# /*===========================================================
set.seed(243730)

field_parameters <-
  field_with_design %>%
  mutate(field_pars = list(
    gen_field_pars(
      sp_range = sp_range,
      gstat_model = gstat_model,
      field_sf = field_sf,
      nsim = 1000
    )
  ))

#* save the field parameters
saveRDS(field_parameters, here("Shared/Data/field_parameters.rds"))

# /*===========================================================
#' # Create regression data and wright matrix
# /*===========================================================
#* create regression data for each trial specification

# field_parameters <- readRDS(here("Shared/Data/field_parameters.rds"))

# field_sf <- field_parameters$field_sf[[1]]
# field_pars <- field_parameters$field_pars[[1]]
# design_name <- field_parameters$design_name[[1]]

all_sim_data <-
  field_parameters %>%
  mutate(reg_data = list(
    gen_reg_data(field_pars, field_sf, design_name)
  )) %>%
  mutate(weights_matrix = list(
    list(
      Wls_50 = gen_weights_matrix(reg_data = reg_data, cutoff = 50),
      Wls_100 = gen_weights_matrix(reg_data = reg_data, cutoff = 100)
    )
  ))


#* save the data
saveRDS(all_sim_data, here("Shared/Data/all_sim_data.rds"))