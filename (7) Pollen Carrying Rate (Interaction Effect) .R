# Load required packages
library(lme4)
library(car)
library(multcomp)
library(lmtest)
library(MuMIn)
library(lmerTest)

# Fit the model with interaction effects
model_pollen_rate_interaction <- lmerTest::lmer(
  pollen_carrying_rate ~ treatment * factor(week) + 
    (1 | WGT/Colony.no.) + 
    (1 | Air_Temp_Avg) + 
    (1 | `RH (%) [Smp]`) + 
    (1 | `WS_kph_ (kilometers/hour) [Avg]`) + 
    (1 | `SlrW_Avg (W/m^2) [Avg]`),
  data = all_weeks_summary_filter
)

# View model summary
summary(model_pollen_rate_interaction)

# Calculate VIF
pollen_rate_vif_values_interaction <- vif(model_pollen_rate_interaction)
print(pollen_rate_vif_values_interaction)

# Check residual distribution (normality)
residuals_interaction <- resid(model_pollen_rate_interaction)
shapiro_test_interaction <- shapiro.test(residuals_interaction)
print(shapiro_test_interaction)

# Plot residual QQ plot
qqnorm(residuals_interaction)
qqline(residuals_interaction, col = "red")

# Get fitted values
fitted_values_interaction <- fitted(model_pollen_rate_interaction)

# Build a linear model object for using bptest
lm_model_interaction <- lm(residuals_interaction ~ fitted_values_interaction)

# Perform Breusch-Pagan test for homoscedasticity
bp_test_interaction <- bptest(lm_model_interaction)
print(bp_test_interaction)

# Calculate AIC to compare with previous models
pollen_aic_interaction <- AIC(model_pollen_rate_interaction)
print(paste("AIC of interaction model:", pollen_aic_interaction))

# Residuals histogram
hist(residuals_interaction, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

# Calculate R-squared
pollen_r_squared_interaction <- r.squaredGLMM(model_pollen_rate_interaction)
print(pollen_r_squared_interaction)
