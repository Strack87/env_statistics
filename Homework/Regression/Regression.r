# =============================================================================
# Regression Homework — Environmental Engineering
# Author: Andrei Strachinaru
# =============================================================================

options(digits = 4)

# =============================================================================
# 0. DATA
# =============================================================================

q95 <- data.frame(
  DBMSNR    = c(200001, 200002, 200003, 200004, 200005, 200006, 200007, 200008,
                200009, 200010, 200011, 200012, 200013, 200014, 200015, 200016,
                200017, 200018, 200019, 200020, 200021, 200022, 200023, 200024,
                200025, 200026, 200027, 200028, 200029, 200030, 200031, 200032,
                200033, 200034, 200035, 200036),
  Q95       = c(4.13, 3.16, 4.63, 4.74, 3.24, 8.00, 2.83, 4.21, 3.31, 4.97,
                3.94, 3.23, 3.60, 5.19, 4.98, 5.87, 5.49, 8.78, 7.47, 6.28,
                3.07, 5.90, 1.71, 1.58, 2.14, 3.48, 2.81, 4.79, 4.67, 9.80,
                6.52, 3.58, 13.29, 2.97, 6.91, 3.15),
  H.MIN     = c(3.41, 3.63, 6.01, 2.67, 3.89, 3.97, 4.50, 3.53, 3.73, 3.62,
                5.44, 3.18, 2.74, 4.32, 2.77, 3.87, 3.47, 3.38, 4.33, 4.89,
                3.52, 3.41, 3.78, 3.97, 3.86, 4.22, 3.74, 3.82, 3.30, 5.35,
                4.82, 3.38, 3.61, 3.68, 3.71, 3.26),
  H.MEAN    = c(4.20497, 4.678, 7.137, 3.543, 5.25688, 4.454, 5.229, 4.42,
                4.004, 4.462, 6.024, 3.706, 3.726, 5.345, 3.97624, 5.14148,
                3.818, 4.82799, 5.077, 6.076, 4.168, 4.598, 4.404, 4.40,
                4.198, 4.937, 4.558, 4.498, 4.061, 6.313, 6.152, 4.10931,
                5.00748, 4.24, 5.162, 3.718),
  M.NEIG    = c(5.4, 11.3, 12.9, 3.3, 10.2, 3.9, 9.9, 7.9, 8.0, 3.5, 10.5,
                5.9, 7.0, 9.4, 4.5, 9.6, 9.3, 9.3, 9.9, 12.0, 8.0, 8.6,
                8.2, 7.2, 8.3, 10.2, 9.6, 6.7, 8.2, 12.1, 13.9, 8.5, 8.3,
                8.0, 10.8, 9.0),
  N.GES     = c(8.48586, 9.078, 10.761, 8.07, 9.31489, 9.355, 9.41, 8.475,
                8.578, 9.683, 10.149, 7.933, 7.829, 9.925, 8.84651, 9.57801,
                9.082, 9.64408, 9.792, 10.564, 8.689, 8.801, 9.051, 9.114,
                8.756, 9.084, 9.087, 9.304, 8.262, 10.389, 10.409, 8.92126,
                10.37569, 8.77, 9.281, 8.686),
  GEOL.BM   = c(1.43, 67.99, 0.78, 0.00, 1.18, 0.00, 1.11, 44.19, 0.40,
                0.00, 0.33, 1.08, 0.81, 0.09, 0.00, 0.64, 3.12, 1.85, 1.92,
                1.36, 1.97, 1.08, 0.87, 1.44, 1.69, 52.21, 0.79, 0.25, 0.09,
                0.40, 1.71, 3.85, 1.70, 5.73, 83.95, 9.77),
  GEOL.QUA2 = c(6.1, 0.0, 1.9, 76.1, 44.5, 68.8, 0.0, 0.2, 74.1, 77.9, 0.0,
                13.3, 18.8, 11.1, 75.0, 12.5, 0.0, 34.5, 1.0, 9.0, 42.5,
                14.2, 7.1, 0.0, 21.3, 0.0, 2.7, 38.6, 26.8, 8.9, 11.4, 13.5,
                62.0, 56.3, 0.0, 28.8),
  GEOL.TER  = c(90.99, 29.54, 94.61, 0.00, 51.98, 0.00, 97.34, 52.08, 24.54,
                4.20, 98.53, 82.71, 78.01, 88.10, 0.00, 85.98, 94.57, 61.72,
                94.41, 88.26, 52.21, 83.18, 90.46, 95.39, 74.10, 47.21, 95.21,
                59.69, 72.08, 89.72, 85.11, 81.11, 26.21, 35.26, 12.43, 59.62),
  GEOL.QUELL= c(0.05, 1.38, 1.93, 23.90, 1.16, 31.20, 0.44, 1.84, 0.56,
                17.70, 0.81, 1.83, 1.57, 0.62, 20.90, 0.24, 0.89, 0.18, 0.75,
                0.02, 1.35, 0.46, 0.70, 1.73, 1.22, 0.17, 0.51, 1.11, 1.04,
                0.58, 0.07, 0.89, 0.99, 1.18, 1.77, 1.64),
  BONU.ACK  = c(88.07, 43.51, -0.78, 92.50, 38.32, 97.30, 47.09, 65.51,
                56.70, 92.40, 31.57, 87.62, 86.89, 54.91, 84.40, 67.56, 74.38,
                65.35, 66.38, 10.44, 66.53, 81.82, 68.53, 73.76, 60.01, 28.99,
                73.51, 82.25, 84.21, 51.20, 17.29, 59.25, 38.00, 47.47, 21.45,
                78.73),
  BONU.GRU  = c(0.0, 52.2, 0.0, 2.4, 41.2, 1.9, 35.7, 26.9, 38.5, 6.8,
                27.1, 0.7, 4.3, 34.1, 6.9, 15.3, 20.5, 14.4, 25.5, 45.2,
                22.2, 5.3, 26.3, 22.1, 35.1, 51.0, 22.3, 10.9, 1.7, 6.4,
                35.0, 35.2, 33.5, 44.2, 51.6, 14.6),
  BONU.WAL  = c(9.8, 3.2, 100.0, 4.7, 19.3, 0.8, 16.1, 5.6, 4.4, 0.7, 41.1,
                10.6, 6.5, 6.7, 7.5, 14.9, 3.7, 18.1, 6.2, 43.0, 8.0, 10.9,
                3.7, 2.7, 1.5, 19.1, 3.4, 6.6, 11.1, 42.0, 46.0, 3.7, 25.7,
                5.7, 25.1, 6.5),
  SDENS     = c(7.65, 9.13, 8.74, 6.09, 9.86, 6.55, 9.40, 9.67, 8.64, 5.55,
                9.71, 7.08, 8.54, 9.91, 8.36, 9.17, 9.59, 7.97, 9.72, 10.09,
                10.14, 8.30, 11.12, 11.13, 9.79, 9.42, 10.02, 8.04, 8.17,
                10.86, 8.45, 10.93, 8.65, 10.47, 9.36, 10.96)
)

# Quick structural overview and summary statistics of key variables
str(q95)
summary(q95[, c("Q95", "H.MIN", "H.MEAN", "M.NEIG", "N.GES", "SDENS")])

# =============================================================================
# 1. EXPLORATORY DATA VISUALISATION
# =============================================================================

# --- 1a. Distribution of the response variable Q95 --------------------------
par(mfrow = c(1, 2))

hist(q95$Q95,
     breaks = 10,
     col    = "steelblue",
     border = "white",
     main   = "Distribution of Q95",
     xlab   = "Q95 (l/s/km2)",
     ylab   = "Frequency")
# Overlay a normal-density curve for visual reference
curve(dnorm(x, mean(q95$Q95), sd(q95$Q95)) * length(q95$Q95) * diff(range(q95$Q95)) / 10,
      add = TRUE, col = "red", lwd = 2)

boxplot(q95$Q95,
        col    = "steelblue",
        main   = "Boxplot of Q95",
        ylab   = "Q95 (l/s/km2)")
stripchart(q95$Q95, vertical = TRUE, method = "jitter",
           pch = 16, col = "navy", add = TRUE)

par(mfrow = c(1, 1))

# --- 1b. Scatterplots of Q95 vs each predictor ------------------------------
# Shows potential linear relationships and outliers before modelling
predictors <- c("H.MIN", "H.MEAN", "M.NEIG", "N.GES",
                "GEOL.BM", "GEOL.QUA2", "GEOL.TER", "GEOL.QUELL",
                "BONU.ACK", "BONU.GRU", "BONU.WAL", "SDENS")

par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))
for (v in predictors) {
  plot(q95[[v]], q95$Q95,
       xlab = v,
       ylab = "Q95",
       main = paste("Q95 vs", v),
       pch  = 16,
       col  = "steelblue")
  abline(lm(Q95 ~ q95[[v]], data = q95), col = "red", lwd = 1.5)
}
par(mfrow = c(1, 1))

# --- 1c. Correlation matrix (numeric variables only, excluding DBMSNR) -------
# High correlations between predictors may indicate multicollinearity
num_vars <- q95[, c("Q95", predictors)]
cor_mat  <- round(cor(num_vars), 2)
print(cor_mat)

# Visual correlation heatmap using base graphics
image(1:ncol(cor_mat), 1:nrow(cor_mat), t(cor_mat[nrow(cor_mat):1, ]),
      col  = colorRampPalette(c("firebrick", "white", "steelblue"))(100),
      axes = FALSE,
      main = "Correlation matrix of Q95 and predictors",
      xlab = "", ylab = "")
axis(1, at = 1:ncol(cor_mat), labels = colnames(cor_mat), las = 2, cex.axis = 0.7)
axis(2, at = 1:nrow(cor_mat), labels = rev(rownames(cor_mat)), las = 1, cex.axis = 0.7)

# --- 1d. Pairs plot for the continuous catchment variables ------------------
# Quick overview of all pairwise relationships
pairs(q95[, c("Q95", "H.MIN", "H.MEAN", "M.NEIG", "N.GES", "SDENS")],
      pch = 16, col = "steelblue", cex = 0.8,
      main = "Pairs plot — Q95 and selected predictors")

# =============================================================================
# 2. MULTIPLE LINEAR REGRESSION (full model)
# =============================================================================

# Fit OLS model with all 12 catchment predictors
lm_full <- lm(Q95 ~ H.MIN + H.MEAN + M.NEIG + N.GES +
                GEOL.BM + GEOL.QUA2 + GEOL.TER + GEOL.QUELL +
                BONU.ACK + BONU.GRU + BONU.WAL + SDENS,
              data = q95)

summary(lm_full)

# Extract specific values from the coefficient table
coef_summary <- coef(summary(lm_full))

# Regression coefficient of SDENS
sdens_coef <- coef_summary["SDENS", "Estimate"]
cat("Regression coefficient of SDENS:", round(sdens_coef, 4), "\n")

# t-statistic of M.NEIG
mneig_t <- coef_summary["M.NEIG", "t value"]
cat("t-statistic of M.NEIG:", round(mneig_t, 4), "\n")

# --- Goodness-of-fit metrics -------------------------------------------------
s <- summary(lm_full)
cat("R2                  :", round(s$r.squared,    4), "\n")
cat("Adjusted R2         :", round(s$adj.r.squared, 4), "\n")
cat("Residual Std. Error :", round(s$sigma,         4), "\n")

# --- Standard diagnostic plots -----------------------------------------------
# Checks: linearity, homoscedasticity, normality of residuals, leverage
par(mfrow = c(2, 2))
plot(lm_full)
par(mfrow = c(1, 1))

# --- Cook's distance: identify high-leverage observations --------------------
cook_d <- cooks.distance(lm_full)

# Row with the single largest Cook's distance
max_cook_row <- which.max(cook_d)
cat("Row with largest Cook's distance:", max_cook_row,
    " (Cook's D =", round(cook_d[max_cook_row], 4), ")\n")

# Spike plot — threshold line at 0.5 flags influential points
plot(cook_d, type = "h",
     main = "Cook's Distance — Full Linear Model",
     xlab = "Observation index",
     ylab = "Cook's distance")
abline(h = 0.5, col = "red", lty = 2)
text(max_cook_row, cook_d[max_cook_row],
     labels = max_cook_row, pos = 4, col = "red")

# --- Worst-fitting observation (largest absolute residual) -------------------
resids      <- residuals(lm_full)
max_res_row <- which.max(abs(resids))

cat("Row with largest residual:", max_res_row, "\n")
cat("Q95 value at that point  :", q95$Q95[max_res_row], "\n")
cat("Residual value           :", round(resids[max_res_row], 4), "\n")

# =============================================================================
# 3. ROBUST REGRESSION (Least Trimmed Squares)
# =============================================================================

library(robustbase)

# seed must be set immediately before ltsReg to ensure reproducibility
set.seed(3141)
lts_fit <- ltsReg(Q95 ~ H.MIN + H.MEAN + M.NEIG + N.GES +
                    GEOL.BM + GEOL.QUA2 + GEOL.TER + GEOL.QUELL +
                    BONU.ACK + BONU.GRU + BONU.WAL + SDENS,
                  data = q95)

summary(lts_fit)

# Extract results from the robust model
lts_sum   <- summary(lts_fit)
lts_coefs <- lts_sum$coefficients

# Variable with the smallest p-value (intercept excluded)
p_vals    <- lts_coefs[-1, "Pr(>|t|)"]
min_p_var <- names(which.min(p_vals))
cat("Variable with smallest p-value:", min_p_var,
    " (p =", formatC(min(p_vals), format = "e", digits = 3), ")\n")

# Robust R2
cat("R2 (robust):", round(lts_sum$r.squared, 4), "\n")

# =============================================================================
# 4. STEPWISE REGRESSION (starting from full dataset)
# =============================================================================

# Find the most significant single predictor in the full OLS model
ols_pvals <- coef_summary[-1, "Pr(>|t|)"]   # drop intercept
best_var  <- names(which.min(ols_pvals))
cat("Most important predictor in full OLS model:", best_var, "\n")

# Start stepwise from a one-predictor model; allow adding/removing terms
formula_start <- as.formula(paste("Q95 ~", best_var))
lm_start      <- lm(formula_start, data = q95)

lm_upper <- lm(Q95 ~ H.MIN + H.MEAN + M.NEIG + N.GES +
                 GEOL.BM + GEOL.QUA2 + GEOL.TER + GEOL.QUELL +
                 BONU.ACK + BONU.GRU + BONU.WAL + SDENS,
               data = q95)

# Bidirectional selection by AIC
step_model <- step(lm_start,
                   scope     = list(lower = ~1, upper = formula(lm_upper)),
                   direction = "both",
                   trace     = 1)

summary(step_model)

# Report stepwise model characteristics
step_vars <- names(coef(step_model))[-1]   # drop intercept
cat("Number of independent variables chosen:", length(step_vars), "\n")
cat("Variables in stepwise model:", paste(step_vars, collapse = ", "), "\n")
cat("Adjusted R2:", round(summary(step_model)$adj.r.squared, 4), "\n")
cat("First variable entered (start of stepwise):", best_var, "\n")

# =============================================================================
# 5. MODELLING WITHOUT HIGH-LEVERAGE POINTS
# =============================================================================

# Remove observations with Cook's distance > 0.5 from the full OLS model
leverage_threshold <- 0.5
high_lev_rows      <- which(cook_d > leverage_threshold)

cat("Rows removed (Cook's D > 0.5):", high_lev_rows, "\n")
cat("Cook's distances of removed rows:\n")
print(round(cook_d[high_lev_rows], 4))

q95_reduced <- q95[-high_lev_rows, ]
cat("Remaining observations:", nrow(q95_reduced), "\n")

# --- Full OLS model on the reduced dataset -----------------------------------
lm_reduced <- lm(Q95 ~ H.MIN + H.MEAN + M.NEIG + N.GES +
                   GEOL.BM + GEOL.QUA2 + GEOL.TER + GEOL.QUELL +
                   BONU.ACK + BONU.GRU + BONU.WAL + SDENS,
                 data = q95_reduced)

summary(lm_reduced)

# Variable with the smallest p-value in the reduced model
coef_reduced   <- coef(summary(lm_reduced))
p_vals_reduced <- coef_reduced[-1, "Pr(>|t|)"]
min_p_var_red  <- names(which.min(p_vals_reduced))

cat("Variable with smallest p-value (reduced model):", min_p_var_red,
    " (p =", formatC(min(p_vals_reduced), format = "e", digits = 3), ")\n")

# --- Stepwise regression on the reduced dataset ------------------------------
best_var_red      <- names(which.min(p_vals_reduced))
cat("Most important predictor (reduced dataset):", best_var_red, "\n")

formula_start_red <- as.formula(paste("Q95 ~", best_var_red))
lm_start_red      <- lm(formula_start_red, data = q95_reduced)

lm_upper_red <- lm(Q95 ~ H.MIN + H.MEAN + M.NEIG + N.GES +
                     GEOL.BM + GEOL.QUA2 + GEOL.TER + GEOL.QUELL +
                     BONU.ACK + BONU.GRU + BONU.WAL + SDENS,
                   data = q95_reduced)

step_model_red <- step(lm_start_red,
                       scope     = list(lower = ~1, upper = formula(lm_upper_red)),
                       direction = "both",
                       trace     = 1)

summary(step_model_red)

# Report and compare with the section-4 stepwise model
step_vars_red <- names(coef(step_model_red))[-1]

cat("Variables in stepwise model (reduced):", paste(step_vars_red, collapse = ", "), "\n")
cat("Adjusted R2 (reduced stepwise):", round(summary(step_model_red)$adj.r.squared, 4), "\n")

# Count how many regressors from the reduced stepwise model also appeared in section 4
common_vars <- intersect(step_vars, step_vars_red)
cat("\nRegressors also in section-4 stepwise model:", length(common_vars), "\n")
cat("Shared variables:", paste(common_vars, collapse = ", "), "\n")
