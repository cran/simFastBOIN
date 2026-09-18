## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(simFastBOIN)

## -----------------------------------------------------------------------------
boin_lambda(target = 0.30)

## -----------------------------------------------------------------------------
bd <- boin_boundary(target = 0.30, max_n = 18, extrasafe = TRUE)
print(bd, cohort_size = 3)

## -----------------------------------------------------------------------------
decisions <- boin_decision_table(target = 0.30, max_n = 18)

print(decisions, cohort_size = 3)

## ----decision-plot, fig.width = 8, fig.height = 6, eval = requireNamespace("ggplot2", quietly = TRUE)----
plot(decisions)

## -----------------------------------------------------------------------------
boin_stopping_table(bd, cohort_size = 3)

## -----------------------------------------------------------------------------
oc <- sim_boin(
  target = 0.30,
  p_true = c(0.05, 0.15, 0.30, 0.45, 0.60),
  n_cohort = 10,
  cohort_size = 3,
  n_trials = 2000,
  seed = 123
)

oc

## -----------------------------------------------------------------------------
oc$sel_percent
oc$percent_no_mtd
oc$overdose$pct_patients

## -----------------------------------------------------------------------------
scenarios <- list(
  "MTD at dose 2" = c(0.15, 0.30, 0.45, 0.60, 0.75),
  "MTD at dose 4" = c(0.02, 0.06, 0.15, 0.30, 0.50),
  "All doses toxic" = c(0.40, 0.55, 0.65, 0.75, 0.85)
)

sim_boin_multi(
  target = 0.30,
  scenarios = scenarios,
  n_cohort = 10,
  cohort_size = 3,
  n_trials = 2000,
  seed = 123
)

## -----------------------------------------------------------------------------
safe_curve <- c(0.01, 0.02, 0.05, 0.12, 0.30)

plain <- sim_boin(target = 0.30, p_true = safe_curve, n_cohort = 12,
                  cohort_size = 3, n_trials = 1000, seed = 1)
titrated <- sim_boin(target = 0.30, p_true = safe_curve, n_cohort = 12,
                     cohort_size = 3, n_trials = 1000, titration = TRUE, seed = 1)

rbind(plain = plain$n_pts_dose, titrated = titrated$n_pts_dose)

## -----------------------------------------------------------------------------
toxic_curve <- c(0.35, 0.45, 0.55, 0.65, 0.75)

c(
  plain = sim_boin(target = 0.30, p_true = toxic_curve, n_cohort = 12,
                   cohort_size = 3, n_trials = 1000, seed = 2)$percent_no_mtd,
  extrasafe = sim_boin(target = 0.30, p_true = toxic_curve, n_cohort = 12,
                       cohort_size = 3, n_trials = 1000, extrasafe = TRUE,
                       seed = 2)$percent_no_mtd
)

## -----------------------------------------------------------------------------
c(default = boin_decision_table(0.25, 9)["1", "3"],
  modified = boin_decision_table(0.25, 9, stay_on_1_of_3 = TRUE)["1", "3"])

## -----------------------------------------------------------------------------
trials <- boin_simulate(
  target = 0.30,
  p_true = c(0.05, 0.15, 0.30, 0.45, 0.60),
  n_cohort = 10,
  cohort_size = 3,
  n_trials = 1000,
  seed = 123
)

trials

## -----------------------------------------------------------------------------
free <- boin_select_mtd(trials$n_pts, trials$n_tox, target = 0.30)
bounded <- boin_select_mtd(trials$n_pts, trials$n_tox, target = 0.30,
                           bound_mtd = TRUE)

table(free$mtd, bounded$mtd, useNA = "ifany")

## -----------------------------------------------------------------------------
round(boin_isotonic(trials$n_pts[1:5, ], trials$n_tox[1:5, ]), 3)

## -----------------------------------------------------------------------------
oc_3p3(p_true = c(0.05, 0.15, 0.25, 0.45, 0.60))

## -----------------------------------------------------------------------------
expanded <- oc_3p3(p_true = c(0.05, 0.15, 0.25, 0.45, 0.60),
                   mtd_rule = "expand")

round(expanded$sel_percent, 1)
round(expanded$total_n_pts, 2)

## ----eval = FALSE-------------------------------------------------------------
# reference <- BOIN::get.oc(
#   target = 0.30, p.true = c(0.05, 0.15, 0.25, 0.45, 0.60),
#   ncohort = 20, cohortsize = 3, n.earlystop = 18,
#   ntrial = 1000, seed = 6
# )
# 
# ours <- sim_boin(
#   target = 0.30, p_true = c(0.05, 0.15, 0.25, 0.45, 0.60),
#   n_cohort = 20, cohort_size = 3, n_earlystop = 18,
#   n_trials = 1000, seed = 6
# )
# 
# all.equal(unname(ours$sel_percent), reference$selpercent)
# all.equal(unname(ours$n_pts_dose), reference$npatients)

