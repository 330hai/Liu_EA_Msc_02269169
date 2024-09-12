# Load required packages
library(lme4)
library(car)
library(multcomp)
library(lmtest)

# Fit the model without interaction effects
model_pollen_rate_weighted <- lmer(
  pollen_carrying_rate ~ treatment + factor(week) + 
    (1 | WGT/Colony.no.) + 
    (1 | Air_Temp_Avg) + 
    (1 | `RH (%) [Smp]`) + 
    (1 | `WS_kph_ (kilometers/hour) [Avg]`) + 
    (1 | `SlrW_Avg (W/m^2) [Avg]`),
  data = all_weeks_summary_filter
)

# View model summary
summary(model_pollen_rate_weighted)

# Calculate VIF
pollen_rate_vif_values <- vif(model_pollen_rate_weighted)
print(pollen_rate_vif_values)

# Check residual distribution (normality)
residuals_weighted <- resid(model_pollen_rate_weighted)
shapiro_test_weighted <- shapiro.test(residuals_weighted)
print(shapiro_test_weighted)

# Plot residual QQ plot
qqnorm(residuals_weighted)
qqline(residuals_weighted, col = "red")

# Get fitted values
fitted_values_weighted <- fitted(model_pollen_rate_weighted)

# Build a linear model object for using bptest
lm_model_weighted <- lm(residuals_weighted ~ fitted_values_weighted)

# Perform Breusch-Pagan test for homoscedasticity
bp_test_weighted <- bptest(lm_model_weighted)
print(bp_test_weighted)

# Calculate AIC to compare with previous models
pollen_aic_weighted <- AIC(model_pollen_rate_weighted)
print(paste("AIC of weighted model:", pollen_aic_weighted))

# Perform Tukey's post-hoc test
tukey_result_weighted <- glht(model_pollen_rate_weighted, linfct = mcp(treatment = "Tukey"))
summary(tukey_result_weighted)

# Residuals histogram
hist(residuals_weighted, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

# Calculate R-squared
pollen_r_squared <- r.squaredGLMM(model_pollen_rate_weighted)
print(pollen_r_squared)
