
#! graphing codes to create figures and tables
#! will be called by `manuscript.rmd`



# /*===========================================================
#' # Boxplot of yield RMSE
# /*===========================================================
# fig.id = "rmse-yield-boxplot",
# fig.cap = "The yield prediction performances of different models"

gdata <- est_data
#------ boxplot ------#
source(here("GitControlled", "Codes", "Modules", "figure_boxplot_rmse_yield.R"))
rmse_yield_boxplot <- g_plot



# /*===========================================================
#' # Boxplot of EONR RMSE
# /*===========================================================
# fig.id = "rmse-eonr-boxplot",
# fig.cap = "The yield prediction performances of different models"

gdata <- est_data
#------ boxplot ------#
source(here("GitControlled", "Codes", "Modules", "figure_boxplot_rmse_eonr.R"))
rmse_eonr_boxplot <- g_plot



# /*===========================================================
#' # Boxplot of profit
# /*===========================================================
# fig.id = "pi-boxplot",
# fig.cap = "The profit performances of different models"

gdata <- est_data
#------ boxplot ------#
source(here("GitControlled", "Codes", "Modules", "figure_boxplot_profit.R"))
pi_boxplot <- g_plot



# /*===========================================================
#' # Boxplot: yield, EONR, profit combined
# /*===========================================================
# fig.id = "comb-boxplot-large", "comb-boxplot-medium", "comb-boxplot-small"
# fig.cap = "The performances of different models"

gdata <- est_data

#------ data reshaping ------#
gdata_long <- est_data %>% 
    .[, .(field_size, model, sim, rmse_cv, rmse_eonr, profit)] %>% 
    #=== Wide to Long: melt()
    melt(id.vars = c('field_size','model','sim')) %>% 
    #--- label field size ---#
    .[field_size=="9.3 ha", fsize := "small"] %>% 
    .[field_size=="18.7 ha", fsize := "medium"] %>% 
    .[field_size=="37.3 ha", fsize := "large"] %>% 
    #--- label variable type ---#
    .[variable=="rmse_cv", var_type := "Yield RMSE (kg/ha)"] %>% 
    .[variable=="rmse_eonr", var_type := "EONR RMSE (kg/ha)"] %>% 
    .[variable=="profit", var_type := "Profit ($/ha)"] %>% 
    .[, var_type := factor(var_type, levels = c("Yield RMSE (kg/ha)", 
                                                "EONR RMSE (kg/ha)", 
                                                "Profit ($/ha)"))] %>% 
    print()
#------ boxplot ------#
for(s in unique(gdata_long$fsize)){
    podg <- position_dodge(0.6)
    assign(paste0("comb_boxplot_", s), 
           gdata_long[fsize==s] %>% 
               ggplot(data = ., 
                      aes(x = model, y = value, fill = model)) +
               stat_boxplot(geom = "errorbar", width = 0.4, position = podg, lwd = 0.25) +
               geom_boxplot(position = podg, width = 0.4, outlier.shape = NA, lwd = 0.25) +
               scale_fill_manual(values = c("white", "white", "white", "gray75", "gray75")) +
               facet_wrap(~var_type, ncol = 3, scales = "free_y") +
               ylab('') +
               xlab('') +
               theme_bw() +
               theme(
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   legend.position='none',
                   legend.title = element_blank(),
                   legend.key.size = unit(0.4, 'cm'),
                   axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
                   axis.text=element_text(color='black')
               )
    )
}



# /*===========================================================
#' # Single simulation illustration: yield & EONR
# /*===========================================================
# fig.id = "single-simu-yield", "single-simu-EONR"
# fig.cap = "Illustration, single simulation"

sim_example = 58
gdata <- aunit_data[sim==sim_example, ]
mean_dt <- gdata %>% 
    .[, .(
        rmse_yield = mean((yield - yield_hat)^2, na.rm = TRUE) %>% sqrt() %>% round(1),
        rmse_EONR = mean((EONR - opt_N_hat)^2) %>% sqrt() %>% round(1),
        x_yield = quantile(yield, 0.05),
        y_yield = quantile(yield_hat, 0.99),
        x_EONR = quantile(EONR, 0.05),
        y_EONR = quantile(opt_N_hat, 0.99) + 15
    ),
    by = .(model)] %>% 
    print()

#* predicted vs true yield
single_simu_yield <- 
    source(here("GitControlled", "Codes", "Modules", "figure_single_simu_yield.R"))$value
#* predicted vs true EONR
single_simu_EONR <- 
    source(here("GitControlled", "Codes", "Modules", "figure_single_simu_EONR.R"))$value



# /*===========================================================
#' # Ratio to True EONR 
# /*===========================================================
# fig.id = "eonr-ratio-kernel",
# fig.cap = ""

gdata <- aunit_data
#------ kernel density ------#
source(here("GitControlled","Codes","Modules","figure_eonr_ratio_kernel.R"))
eonr_ratio_kernel <- g_plot

