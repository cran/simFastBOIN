## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  message = FALSE,  # Suppress messages
  warning = FALSE   # Suppress warnings
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from GitHub
# devtools::install_github("gosukehommaEX/simFastBOIN")

## ----setup--------------------------------------------------------------------
library(simFastBOIN)

## ----basic-simulation, message=FALSE------------------------------------------
# Define design parameters
target <- 0.30  # Target DLT rate (30%)
p_true <- c(0.10, 0.25, 0.40, 0.55, 0.70)  # True toxicity probabilities

# Run simulation (progress messages suppressed)
result <- sim_boin(
  n_trials = 1000,
  target = target,
  p_true = p_true,
  n_cohort = 10,
  cohort_size = 3,
  seed = 123
)

# Display results
print(result$summary)

## ----boin-standard------------------------------------------------------------
result_standard <- sim_boin(
  n_trials = 1000,
  target = 0.30,
  p_true = c(0.10, 0.25, 0.40, 0.55, 0.70),
  n_cohort = 10,
  cohort_size = 3,
  boundMTD = TRUE,              # Conservative MTD selection
  n_earlystop_rule = "with_stay",  # Stop when converged
  seed = 123
)

print(result_standard$summary, scenario_name = "BOIN Standard")

## ----extrasafe----------------------------------------------------------------
result_safe <- sim_boin(
  n_trials = 1000,
  target = 0.30,
  p_true = c(0.05, 0.10, 0.20, 0.30, 0.45),
  n_cohort = 10,
  cohort_size = 3,
  extrasafe = TRUE,  # Safety monitoring at lowest dose
  offset = 0.05,     # Safety cutoff adjustment
  seed = 123
)

print(result_safe$summary, scenario_name = "With Extra Safety")

## ----conservative-------------------------------------------------------------
result_conservative <- sim_boin(
  n_trials = 1000,
  target = 0.30,
  p_true = seq(0.05, 0.45, by = 0.05),
  n_cohort = 20,
  cohort_size = 3,
  extrasafe = TRUE,
  boundMTD = TRUE,
  n_earlystop_rule = "with_stay",
  seed = 123
)

print(result_conservative$summary, scenario_name = "Maximum Conservatism")

## ----multi-scenario, results='hide'-------------------------------------------
# Define multiple scenarios
scenarios <- list(
  list(name = "Scenario 1: MTD at DL3", 
       p_true = c(0.05, 0.10, 0.20, 0.30, 0.45)),
  list(name = "Scenario 2: MTD at DL4", 
       p_true = c(0.10, 0.15, 0.25, 0.30, 0.45)),
  list(name = "Scenario 3: All doses safe", 
       p_true = c(0.05, 0.10, 0.15, 0.20, 0.25))
)

# Run multi-scenario simulation
result_multi <- sim_boin_multi(
  scenarios = scenarios,
  target = 0.30,
  n_trials = 1000,
  n_cohort = 10,
  cohort_size = 3,
  seed = 123
)

## ----multi-scenario-display---------------------------------------------------
# Display aggregated results
print(result_multi)

## ----percent-format-----------------------------------------------------------
print(result$summary, percent = TRUE)

## ----markdown-format----------------------------------------------------------
print(result$summary, kable = TRUE, kable_format = "pipe")

## ----html-format, eval=FALSE--------------------------------------------------
# print(result$summary, kable = TRUE, kable_format = "html")

## ----detailed-results---------------------------------------------------------
result_detailed <- sim_boin(
  n_trials = 100,
  target = 0.30,
  p_true = c(0.10, 0.25, 0.40, 0.55, 0.70),
  n_cohort = 10,
  cohort_size = 3,
  return_details = TRUE,
  seed = 123
)

# Check first trial
trial_1 <- result_detailed$detailed_results[[1]]
cat("Trial 1 MTD:", trial_1$mtd, "\n")
cat("Trial 1 stopping reason:", trial_1$reason, "\n")

# Summary of stopping reasons
stopping_reasons <- table(sapply(result_detailed$detailed_results, 
                                function(x) x$reason))
print(stopping_reasons)

## ----design-comparison--------------------------------------------------------
# Baseline
result_baseline <- sim_boin(
  n_trials = 1000,
  target = 0.30,
  p_true = c(0.05, 0.10, 0.20, 0.30, 0.45, 0.60),
  n_cohort = 20,
  cohort_size = 3,
  seed = 123
)

# With boundMTD
result_boundMTD <- sim_boin(
  n_trials = 1000,
  target = 0.30,
  p_true = c(0.05, 0.10, 0.20, 0.30, 0.45, 0.60),
  n_cohort = 20,
  cohort_size = 3,
  boundMTD = TRUE,
  seed = 123
)

# Create comparison
comparison <- data.frame(
  Setting = c("Baseline", "boundMTD"),
  Avg_Patients = c(
    result_baseline$summary$avg_total_n_pts,
    result_boundMTD$summary$avg_total_n_pts
  ),
  MTD_Selection_at_DL4 = c(
    result_baseline$summary$mtd_selection_percent[4],
    result_boundMTD$summary$mtd_selection_percent[4]
  )
)

print(comparison)

## ----benchmark, eval=FALSE----------------------------------------------------
# # Benchmark with 10,000 trials
# system.time({
#   result_large <- sim_boin(
#     n_trials = 10000,
#     target = 0.30,
#     p_true = seq(0.05, 0.45, by = 0.05),
#     n_cohort = 48,
#     cohort_size = 3,
#     seed = 123
#   )
# })

## ----variable-cohort----------------------------------------------------------
result_variable <- sim_boin(
  n_trials = 1000,
  target = 0.30,
  p_true = c(0.10, 0.25, 0.40, 0.55, 0.70),
  n_cohort = 10,
  cohort_size = c(1, 3, 3, 3, 3, 3, 3, 3, 3, 3),  # First cohort: 1 patient
  seed = 123
)

## ----titration----------------------------------------------------------------
result_titration <- sim_boin(
  n_trials = 1000,
  target = 0.30,
  p_true = c(0.05, 0.10, 0.20, 0.30, 0.45),
  n_cohort = 20,
  cohort_size = 3,
  titration = TRUE,  # Enable titration phase
  seed = 123
)

