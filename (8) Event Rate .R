# Load required packages
library(lme4)
library(car)
library(multcomp)
library(lmtest)
library(MuMIn)
library(lmerTest)

# First, calculate the dependent variable: event rate (number of events / observation time)
all_weeks_summary_filter <- all_weeks_summary_filter %>%
  mutate(event_rate = event_count / Obs_Duration)  # Event rate

# Fit the model using event rate (event_rate) as the dependent variable
model_event_rate <- lmerTest::lmer(
  event_rate ~ treatment + factor(week) + 
    (1 | WGT/Colony.no.) + 
    (1 | Air_Temp_Avg) + 
    (1 | `RH (%) [Smp]`) + 
    (1 | `WS_kph_ (kilometers/hour) [Avg]`) + 
    (1 | `SlrW_Avg (W/m^2) [Avg]`),
  data = all_weeks_summary_filter
)

# View model summary
summary(model_event_rate)

# Calculate VIF
event_rate_vif_values <- vif(model_event_rate)
print(event_rate_vif_values)

# Check residual distribution (normality)
residuals_event_rate <- resid(model_event_rate)
shapiro_test_event_rate <- shapiro.test(residuals_event_rate)
print(shapiro_test_event_rate)

# Plot residual QQ plot
qqnorm(residuals_event_rate)
qqline(residuals_event_rate, col = "red")

# Get fitted values
fitted_values_event_rate <- fitted(model_event_rate)

# Build a linear model object for using bptest
lm_model_event_rate <- lm(residuals_event_rate ~ fitted_values_event_rate)

# Perform Breusch-Pagan test for homoscedasticity
bp_test_event_rate <- bptest(lm_model_event_rate)
print(bp_test_event_rate)

# Plot Residuals vs Fitted
plot(fitted(model_event_rate), residuals(model_event_rate),
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)

# Calculate AIC to compare with previous models
pollen_aic_event_rate <- AIC(model_event_rate)
print(paste("AIC of event rate model:", pollen_aic_event_rate))

# Perform Tukey's post-hoc test
tukey_result_event_rate <- glht(model_event_rate, linfct = mcp(treatment = "Tukey"))
summary(tukey_result_event_rate)

# Residuals histogram
hist(residuals_event_rate, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

# Calculate R-squared
event_r_squared <- r.squaredGLMM(model_event_rate)
print(event_r_squared)
