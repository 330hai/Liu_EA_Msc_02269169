# Load required packages
library(lme4)
library(car)
library(multcomp)
library(lmtest)
library(MuMIn)
library(lmerTest)

# Fit the model using lost rate (lost_rate) as the dependent variable
model_lost_rate <- lmer(
  lost_rate ~ treatment + factor(week) + 
    (1 | WGT/Colony.no.) + 
    (1 | Air_Temp_Avg) + 
    (1 | `RH (%) [Smp]`) + 
    (1 | `WS_kph_ (kilometers/hour) [Avg]`) + 
    (1 | `SlrW_Avg (W/m^2) [Avg]`),
  data = all_weeks_summary_filter
)

# View model summary
summary(model_lost_rate)

# Calculate VIF
lost_rate_vif_values <- vif(model_lost_rate)
print(lost_rate_vif_values)

# Check residual distribution (normality)
residuals_lost_rate <- resid(model_lost_rate)
shapiro_test_lost_rate <- shapiro.test(residuals_lost_rate)
print(shapiro_test_lost_rate)

# Plot residual QQ plot
qqnorm(residuals_lost_rate)
qqline(residuals_lost_rate, col = "red")

# Get fitted values
fitted_values_lost_rate <- fitted(model_lost_rate)

# Build a linear model object for using bptest
lm_model_lost_rate <- lm(residuals_lost_rate ~ fitted_values_lost_rate)

# Perform Breusch-Pagan test for homoscedasticity
bp_test_lost_rate <- bptest(lm_model_lost_rate)
print(bp_test_lost_rate)

# Plot Residuals vs Fitted
plot(fitted(model_lost_rate), residuals(model_lost_rate),
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)

# Calculate AIC to compare with previous models
lost_rate_aic <- AIC(model_lost_rate)
print(paste("AIC of lost rate model:", lost_rate_aic))

# Perform Tukey's post-hoc test
tukey_result_lost_rate <- glht(model_lost_rate, linfct = mcp(treatment = "Tukey"))
summary(tukey_result_lost_rate)

# Residuals histogram
hist(residuals_lost_rate, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

# Calculate R-squared
lost_rate_r_squared <- r.squaredGLMM(model_lost_rate)
print(lost_rate_r_squared)
