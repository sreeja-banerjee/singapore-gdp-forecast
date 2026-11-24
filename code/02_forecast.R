# ============================================================
# SINGAPORE GDP GROWTH - 1-STEP-AHEAD FORECASTS
# ============================================================
# Adapted from group project done in Econometrics course (Bocconi)
# Main change: every regressor is lagged by one quarter. 
# To forecast GDP growth in 't' , data till 't-1' used
# Packages to install:
packages2 <- c("dplyr", "tidyr", "readr", "ggplot2")
new_packages2 <- packages2[!(packages2 %in% installed.packages()[, "Package"])]
if(length(new_packages2) > 0) install.packages(new_packages2)

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# STEP 1 - Import data
# -----------------------------
# gdp_g, us_g, ea_g, neer_g = growth rates, perc change quarter on quarter; annualised
# d_unemp  = unemployment rate change
# fedfunds = US Fed funds rate 
# infl     = CPI inflation, perc change year on year 

sg <- read_csv("data/processed/sg_quarterly.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  rename(GDP_growth = gdp_g) %>%
  arrange(date)

# Check if CPI was downloaded (if CPI_TABLE_ID was left empty in 01_download.R,
# there is no inflation data and the models are run without it)
has_infl <- "infl" %in% names(sg) && any(!is.na(sg$infl))

# -----------------------------
# STEP 2 - Create lagged variables
# -----------------------------
# Useing dplyr::lag

sg <- sg %>%
  mutate(
    GDP_lag1      = dplyr::lag(GDP_growth, 1),
    GDP_lag2      = dplyr::lag(GDP_growth, 2),
    GDP_lag3      = dplyr::lag(GDP_growth, 3),
    GDP_lag4      = dplyr::lag(GDP_growth, 4),
    US_lag1       = dplyr::lag(us_g, 1),
    US_lag2       = dplyr::lag(us_g, 2),
    EA_lag1       = dplyr::lag(ea_g, 1),
    EA_lag2       = dplyr::lag(ea_g, 2),
    NEER_lag1     = dplyr::lag(neer_g, 1),
    NEER_lag2     = dplyr::lag(neer_g, 2),
    Unemp_lag1    = dplyr::lag(d_unemp, 1),
    Fedfunds_lag1 = dplyr::lag(fedfunds, 1)
  )

if(has_infl){
  sg <- sg %>%
    mutate(
      Infl_lag1 = dplyr::lag(infl, 1),
      Infl_lag2 = dplyr::lag(infl, 2)
    )
}

# Flag the Covid periods (2020Q1 to 2021Q2)
# To make sure the model does not fluctuate too much, report accuracy
# with and without them.
COVID_START <- as.Date("2020-01-01")
COVID_END   <- as.Date("2021-04-01")

sg <- sg %>%
  mutate(covid = date >= COVID_START & date <= COVID_END)

# Also 2021Q3-2022Q2 out of estimation: lagged values fall into COVID duration 
sg <- sg %>% mutate(covid_lags = date > COVID_END & date <= as.Date("2022-04-01"))

# -----------------------------
# STEP 3 - Models
# -----------------------------

# Domestic model: the Croatia model rebuilt for Singapore, all lagged one quarter.
# The current account is replaced by the S$NEER, because MAS runs monetary
# policy through the exchange rate.
if(has_infl){
  domestic_formula <- GDP_growth ~ GDP_lag1 + Unemp_lag1 + Fedfunds_lag1 + NEER_lag1 + Infl_lag1
} else {
  domestic_formula <- GDP_growth ~ GDP_lag1 + Unemp_lag1 + Fedfunds_lag1 + NEER_lag1
}

# External demand model: with US growth, euro-area growth and the S$NEER
external_formula <- GDP_growth ~ GDP_lag1 + US_lag1 + EA_lag1 + NEER_lag1

# VAR(2) with GDP growth, US growth, euro-area growth, NEER (and inflation if there).

if(has_infl){
  var_formula <- GDP_growth ~ GDP_lag1 + GDP_lag2 + US_lag1 + US_lag2 + EA_lag1 + EA_lag2 +
    NEER_lag1 + NEER_lag2 + Infl_lag1 + Infl_lag2
} else {
  var_formula <- GDP_growth ~ GDP_lag1 + GDP_lag2 + US_lag1 + US_lag2 + EA_lag1 + EA_lag2 +
    NEER_lag1 + NEER_lag2
}

# AR(p) models for p = 1 to 4. Keep model with lowest BIC for each quarter.
ar_formulas <- list(
  GDP_growth ~ GDP_lag1,
  GDP_growth ~ GDP_lag1 + GDP_lag2,
  GDP_growth ~ GDP_lag1 + GDP_lag2 + GDP_lag3,
  GDP_growth ~ GDP_lag1 + GDP_lag2 + GDP_lag3 + GDP_lag4
)

# -----------------------------
# STEP 4 - 1-step-ahead forecasts
# -----------------------------
# From 2005Q1: estimate models using data up to t-1,then forecast
# GDP growth in quarter t. Same train/test idea as the Croatia project

forecast_quarters <- sg$date[sg$date >= as.Date("2005-01-01") & !is.na(sg$GDP_growth)]

# Empty table to store results from the different forecast models
forecast_results <- data.frame(
  Quarter           = forecast_quarters,
  Actual_GDP        = NA,
  COVID             = NA,
  Mean_forecast     = NA,
  RW_forecast       = NA,
  AR_forecast       = NA,
  AR_lags           = NA,
  Domestic_forecast = NA,
  External_forecast = NA,
  VAR_forecast      = NA
)

for(i in 1:length(forecast_quarters)){
  quarter_index <- forecast_quarters[i]

  # Training data: before t, excluding Covid
  train_data <- sg %>% filter(date < quarter_index, covid == FALSE, covid_lags == FALSE)
  test_data  <- sg %>% filter(date == quarter_index)

  # Actual GDP growth to compare to the models
  forecast_results$Actual_GDP[i] <- test_data$GDP_growth
  forecast_results$COVID[i]      <- test_data$covid

  # Historical mean: average GDP growth up to t-1
  forecast_results$Mean_forecast[i] <- mean(train_data$GDP_growth, na.rm = TRUE)

  # Random walk: growth in t is the same as growth in t-1
  forecast_results$RW_forecast[i] <- test_data$GDP_lag1

  # AR(p) chosen by BIC. All four are compared on the same rows.
  # The chosen one is then re-estimated on all the training data.
  ar_data <- train_data %>% filter(!is.na(GDP_lag4))
  bic_values <- c(
    BIC(lm(ar_formulas[[1]], data = ar_data)),
    BIC(lm(ar_formulas[[2]], data = ar_data)),
    BIC(lm(ar_formulas[[3]], data = ar_data)),
    BIC(lm(ar_formulas[[4]], data = ar_data))
  )
  best_p <- which.min(bic_values)

  ar_model <- lm(ar_formulas[[best_p]], data = train_data)
  forecast_results$AR_forecast[i] <- predict(ar_model, newdata = test_data)
  forecast_results$AR_lags[i]     <- best_p

  # Domestic model 
  domestic_model <- lm(domestic_formula, data = train_data)
  forecast_results$Domestic_forecast[i] <- predict(domestic_model, newdata = test_data)

  # External demand model
  external_model <- lm(external_formula, data = train_data)
  forecast_results$External_forecast[i] <- predict(external_model, newdata = test_data)

  # VAR(2)
  var_model <- lm(var_formula, data = train_data)
  forecast_results$VAR_forecast[i] <- predict(var_model, newdata = test_data)
}

# -----------------------------
# STEP 5 - Forecast errors
# -----------------------------
# Error = actual - forecast

forecast_results <- forecast_results %>%
  mutate(
    Error_Mean     = Actual_GDP - Mean_forecast,
    Error_RW       = Actual_GDP - RW_forecast,
    Error_AR       = Actual_GDP - AR_forecast,
    Error_Domestic = Actual_GDP - Domestic_forecast,
    Error_External = Actual_GDP - External_forecast,
    Error_VAR      = Actual_GDP - VAR_forecast
  )

print(head(forecast_results))

# -----------------------------
# STEP 6 - Forecast accuracy
# -----------------------------
#   MAE and RMSE calculation as project, plus two additions:
#   rel_RMSE = model RMSE / AR RMSE  (< 1 means model outperforms AR)
#   Diebold-Mariano test:stat significance of difference 
#   in squared errors vs the AR.If stat < 0 : model outperforms AR

dm_test <- function(error_model, error_ar){
  # loss differential each quarter
  d <- error_model^2 - error_ar^2              
  n <- length(d)
  dm_stat <- mean(d) / sqrt(mean((d - mean(d))^2) / n)
  # small-sample correction (Harvey et al., 1-step forecasts)
  dm_stat <- dm_stat * sqrt((n - 1) / n)       
  p_value <- 2 * pt(-abs(dm_stat), df = n - 1)
  c(dm_stat, p_value)
}

make_accuracy_table <- function(results, sample_name){
  dm_mean     <- dm_test(results$Error_Mean,     results$Error_AR)
  dm_rw       <- dm_test(results$Error_RW,       results$Error_AR)
  dm_domestic <- dm_test(results$Error_Domestic, results$Error_AR)
  dm_external <- dm_test(results$Error_External, results$Error_AR)
  dm_var      <- dm_test(results$Error_VAR,      results$Error_AR)

  accuracy <- data.frame(
    sample = sample_name,
    model  = c("Historical mean", "Random walk", "AR (BIC)",
               "Domestic ADL", "External-demand ADL", "VAR GDP equation"),
    n      = nrow(results),
    RMSE = c(
      sqrt(mean(results$Error_Mean^2)),
      sqrt(mean(results$Error_RW^2)),
      sqrt(mean(results$Error_AR^2)),
      sqrt(mean(results$Error_Domestic^2)),
      sqrt(mean(results$Error_External^2)),
      sqrt(mean(results$Error_VAR^2))
    ),
    MAE = c(
      mean(abs(results$Error_Mean)),
      mean(abs(results$Error_RW)),
      mean(abs(results$Error_AR)),
      mean(abs(results$Error_Domestic)),
      mean(abs(results$Error_External)),
      mean(abs(results$Error_VAR))
    ),
    DM_stat = c(dm_mean[1], dm_rw[1], NA, dm_domestic[1], dm_external[1], dm_var[1]),
    DM_p    = c(dm_mean[2], dm_rw[2], NA, dm_domestic[2], dm_external[2], dm_var[2])
  )

  accuracy %>%
    mutate(rel_RMSE = RMSE / RMSE[model == "AR (BIC)"]) %>%
    relocate(rel_RMSE, .after = RMSE)
}

accuracy_table <- bind_rows(
  make_accuracy_table(forecast_results, "Full evaluation window"),
  make_accuracy_table(forecast_results %>% filter(COVID == FALSE), "Excluding COVID quarters")
)

print(accuracy_table)

write.csv(accuracy_table, "output/tables/forecast_accuracy.csv", row.names = FALSE)

# Long version of the results (one row per quarter and model), used for the graphs
forecasts <- bind_rows(
  forecast_results %>% transmute(date = Quarter, actual = Actual_GDP, covid = COVID, model = "Historical mean",     forecast = Mean_forecast,     error = Error_Mean),
  forecast_results %>% transmute(date = Quarter, actual = Actual_GDP, covid = COVID, model = "Random walk",         forecast = RW_forecast,       error = Error_RW),
  forecast_results %>% transmute(date = Quarter, actual = Actual_GDP, covid = COVID, model = "AR (BIC)",            forecast = AR_forecast,       error = Error_AR),
  forecast_results %>% transmute(date = Quarter, actual = Actual_GDP, covid = COVID, model = "Domestic ADL",        forecast = Domestic_forecast, error = Error_Domestic),
  forecast_results %>% transmute(date = Quarter, actual = Actual_GDP, covid = COVID, model = "External-demand ADL", forecast = External_forecast, error = Error_External),
  forecast_results %>% transmute(date = Quarter, actual = Actual_GDP, covid = COVID, model = "VAR GDP equation",    forecast = VAR_forecast,      error = Error_VAR)
) %>%
  arrange(date)

write.csv(forecasts, "output/tables/forecasts.csv", row.names = FALSE)

BENCHMARK <- "AR (BIC)"

# ------------------------------------------------------------
# Figures
# ------------------------------------------------------------
# Colours: fixed categorical order (blue, orange, aqua), validated for
# colour-vision deficiency; line types and the legend carry identity too.
series_cols <- c("Actual" = "#1f1f1e",
                 "AR (BIC)" = "#2a78d6",
                 "External-demand ADL" = "#eb6834",
                 "VAR GDP equation" = "#1baf7a")
series_lty  <- c("Actual" = "solid", "AR (BIC)" = "solid",
                 "External-demand ADL" = "22", "VAR GDP equation" = "42")

theme_chart <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#e6e5e0", linewidth = 0.3),
        axis.title = element_text(colour = "#5c5b55"),
        legend.position = "top",
        legend.title = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(colour = "#5c5b55", hjust = 0))

plot_models <- c("AR (BIC)", "External-demand ADL", "VAR GDP equation")

fc_plot_data <- bind_rows(
  forecasts %>% distinct(date, actual) %>% transmute(date, series = "Actual", value = actual),
  forecasts %>% filter(model %in% plot_models) %>%
    transmute(date, series = as.character(model), value = forecast)
) %>%
  mutate(series = factor(series, levels = names(series_cols)))

p_fc <- ggplot(fc_plot_data, aes(date, value, colour = series, linetype = series)) +
  annotate("rect", xmin = COVID_START, xmax = COVID_END + 90, ymin = -Inf, ymax = Inf,
           fill = "#f0efec") +
  geom_hline(yintercept = 0, colour = "#b5b4ad", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = series_cols) +
  scale_linetype_manual(values = series_lty) +
  labs(title = "Singapore real GDP growth: actual vs one-step-ahead forecasts",
       x = NULL, y = "% q/q, annualised",
       caption = "Shaded: COVID quarters, excluded from estimation. Forecasts use only data up to the previous quarter.") +
  theme_chart

ggsave("output/figures/forecasts_vs_actual.png", p_fc, width = 9, height = 5, dpi = 150, bg = "white")

# Cumulative sum of (benchmark squared error - model squared error).
# A rising line means the model is beating the AR benchmark over that stretch,
# which shows *when* a model earns its keep rather than one average number.
cse <- forecasts %>%
  filter(!covid) %>%
  select(date, model, error) %>%
  left_join(forecasts %>% filter(model == BENCHMARK, !covid) %>% select(date, e_bench = error),
            by = "date") %>%
  filter(model %in% setdiff(plot_models, BENCHMARK)) %>%
  group_by(model) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(cum_gain = cumsum(coalesce(e_bench^2 - error^2, 0))) %>%
  ungroup() %>%
  mutate(model = factor(as.character(model), levels = names(series_cols)))

p_cse <- ggplot(cse, aes(date, cum_gain, colour = model, linetype = model)) +
  geom_hline(yintercept = 0, colour = "#b5b4ad", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = series_cols) +
  scale_linetype_manual(values = series_lty) +
  labs(title = paste("Cumulative squared-error gain over the", BENCHMARK, "benchmark"),
       x = NULL, y = "Cumulative gain (pp squared)",
       caption = "Above zero and rising: the model is beating the benchmark. COVID quarters excluded.") +
  theme_chart

ggsave("output/figures/cumulative_sq_error_vs_ar.png", p_cse, width = 9, height = 5, dpi = 150, bg = "white")

message("\nSaved output/tables/forecasts.csv, output/tables/forecast_accuracy.csv and two figures in output/figures/")
# ============================================================
# STEP 7 - ROLLING ESTIMATION WINDOW
# ============================================================
# After considering results from above sections, rolling estimation section
# added as above models forecast growth to be higher than the actual consistently.
# Reason: all models estimated on data going all the way back to 1975.
# But trend growth has dropped in recent times and keeping old data is 
# overinflating the forecasts. Rolling window could be more appropriate.
# Trade-off: fewer observations,noisier coefficients.Check  whether trade
# off is worth by comparing RMSE and bias for
# Follow Steps 4-6 just change training data. 

WINDOW_YEARS <- 15

# Rolling-window results table:
forecast_results_rolling <- data.frame(
  Quarter           = forecast_quarters,
  Actual_GDP        = NA,
  COVID             = NA,
  Mean_forecast     = NA,
  RW_forecast       = NA,
  AR_forecast       = NA,
  AR_lags           = NA,
  Domestic_forecast = NA,
  External_forecast = NA,
  VAR_forecast      = NA
)

for(i in 1:length(forecast_quarters)){
  quarter_index <- forecast_quarters[i]
  
  # Start of the window:
  window_start <- as.Date(paste0(as.integer(format(quarter_index, "%Y")) - WINDOW_YEARS,
                                 format(quarter_index, "-%m-%d")))
  
  # Training data: last 15 years before 't' 
  train_data <- sg %>% filter(date < quarter_index, date >= window_start, covid == FALSE, covid_lags == FALSE)
  test_data  <- sg %>% filter(date == quarter_index)
  
  forecast_results_rolling$Actual_GDP[i] <- test_data$GDP_growth
  forecast_results_rolling$COVID[i]      <- test_data$covid
  
  # Historical mean: average of last 15 now
  forecast_results_rolling$Mean_forecast[i] <- mean(train_data$GDP_growth, na.rm = TRUE)
  
  # Random walk: same as before (does not use training)
  forecast_results_rolling$RW_forecast[i] <- test_data$GDP_lag1
  
  # AR(p): following Step 4
  ar_data <- train_data %>% filter(!is.na(GDP_lag4))
  bic_values <- c(
    BIC(lm(ar_formulas[[1]], data = ar_data)),
    BIC(lm(ar_formulas[[2]], data = ar_data)),
    BIC(lm(ar_formulas[[3]], data = ar_data)),
    BIC(lm(ar_formulas[[4]], data = ar_data))
  )
  best_p <- which.min(bic_values)
  
  ar_model <- lm(ar_formulas[[best_p]], data = train_data)
  forecast_results_rolling$AR_forecast[i] <- predict(ar_model, newdata = test_data)
  forecast_results_rolling$AR_lags[i]     <- best_p
  
  # Domestic model
  domestic_model <- lm(domestic_formula, data = train_data)
  forecast_results_rolling$Domestic_forecast[i] <- predict(domestic_model, newdata = test_data)
  
  # External demand model
  external_model <- lm(external_formula, data = train_data)
  forecast_results_rolling$External_forecast[i] <- predict(external_model, newdata = test_data)
  
  # VAR(2)
  var_model <- lm(var_formula, data = train_data)
  forecast_results_rolling$VAR_forecast[i] <- predict(var_model, newdata = test_data)
}

# Forecast errors: following Step 5
forecast_results_rolling <- forecast_results_rolling %>%
  mutate(
    Error_Mean     = Actual_GDP - Mean_forecast,
    Error_RW       = Actual_GDP - RW_forecast,
    Error_AR       = Actual_GDP - AR_forecast,
    Error_Domestic = Actual_GDP - Domestic_forecast,
    Error_External = Actual_GDP - External_forecast,
    Error_VAR      = Actual_GDP - VAR_forecast
  )

write.csv(forecast_results_rolling, "output/tables/forecasts_rolling.csv", row.names = FALSE)

# Forecast accuracy: following step 6
accuracy_table_rolling <- bind_rows(
  make_accuracy_table(forecast_results_rolling, "Full evaluation window"),
  make_accuracy_table(forecast_results_rolling %>% filter(COVID == FALSE), "Excluding COVID quarters")
)

cat("\nFORECAST ACCURACY WITH A", WINDOW_YEARS, "YEAR ROLLING WINDOW\n")
print(accuracy_table_rolling)

write.csv(accuracy_table_rolling, "output/tables/forecast_accuracy_rolling.csv", row.names = FALSE)

# -----------------------------
# Expanding vs rolling window: RMSE and bias
# -----------------------------
# Bias = average of (forecast - actual). 
# Positive means model over-forecasts growth. If falling trend growth was
# the issue as hypothesized, rolling window estimation should bring the
# biases down. (Covid excluded)

compare_windows <- function(results, window_name){
  results %>%
    filter(COVID == FALSE) %>%
    dplyr::select(Quarter, Error_Mean, Error_RW, Error_AR,
                  Error_Domestic, Error_External, Error_VAR) %>%
    pivot_longer(cols = -Quarter, names_to = "model", values_to = "error") %>%
    mutate(
      model = recode(model,
                     Error_Mean     = "Historical mean",
                     Error_RW       = "Random walk",
                     Error_AR       = "AR (BIC)",
                     Error_Domestic = "Domestic ADL",
                     Error_External = "External-demand ADL",
                     Error_VAR      = "VAR GDP equation"),
      period = case_when(
        Quarter <  as.Date("2008-01-01") ~ "2005_07",
        Quarter <  as.Date("2010-01-01") ~ "2008_09",
        Quarter <  as.Date("2020-01-01") ~ "2010_19",
        TRUE                             ~ "2021_26"
      )
    ) %>%
    group_by(model) %>%
    summarise(
      window       = window_name,
      RMSE         = sqrt(mean(error^2)),
      Bias_2005_07 = mean(-error[period == "2005_07"]),
      Bias_2008_09 = mean(-error[period == "2008_09"]),
      Bias_2010_19 = mean(-error[period == "2010_19"]),
      Bias_2021_26 = mean(-error[period == "2021_26"]),
      .groups = "drop"
    )
}

window_comparison <- bind_rows(
  compare_windows(forecast_results, "Expanding"),
  compare_windows(forecast_results_rolling, "Rolling")
) %>%
  mutate(model = factor(model, levels = c("Historical mean", "Random walk", "AR (BIC)",
                                          "Domestic ADL", "External-demand ADL", "VAR GDP equation"))) %>%
  arrange(model, window)

cat("\nEXPANDING VS ROLLING WINDOW (excluding COVID quarters)\n")
print(window_comparison %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

write.csv(window_comparison, "output/tables/expanding_vs_rolling.csv", row.names = FALSE)

# -----------------------------
# Graph: AR forecasts with the expanding vs rolling window
# -----------------------------
ar_window_data <- bind_rows(
  forecast_results %>% transmute(date = Quarter, series = "Actual", value = Actual_GDP),
  forecast_results %>% transmute(date = Quarter, series = "AR, expanding window", value = AR_forecast),
  forecast_results_rolling %>% transmute(date = Quarter, series = paste0("AR, ", WINDOW_YEARS, "-year rolling window"),
                                         value = AR_forecast)
) %>%
  mutate(series = factor(series, levels = c("Actual", "AR, expanding window",
                                            paste0("AR, ", WINDOW_YEARS, "-year rolling window"))))

window_cols <- setNames(c("#1f1f1e", "#2a78d6", "#eb6834"), levels(ar_window_data$series))
window_lty  <- setNames(c("solid", "solid", "22"), levels(ar_window_data$series))

p_window <- ggplot(ar_window_data, aes(date, value, colour = series, linetype = series)) +
  annotate("rect", xmin = COVID_START, xmax = COVID_END + 90, ymin = -Inf, ymax = Inf,
           fill = "#f0efec") +
  geom_hline(yintercept = 0, colour = "#b5b4ad", linewidth = 0.3) +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = window_cols) +
  scale_linetype_manual(values = window_lty) +
  coord_cartesian(ylim = c(-15, 25)) +
  labs(title = "AR forecasts of Singapore GDP growth: expanding vs rolling estimation window",
       x = NULL, y = "% q/q, annualised",
       caption = "Shaded: COVID quarters (y-axis cut at -15 and 25 so the rest of the sample is readable).") +
  theme_chart

ggsave("output/figures/ar_expanding_vs_rolling.png", p_window, width = 9, height = 5, dpi = 150, bg = "white")

message("Saved output/tables/forecasts_rolling.csv, forecast_accuracy_rolling.csv, ",
        "expanding_vs_rolling.csv and output/figures/ar_expanding_vs_rolling.png")

