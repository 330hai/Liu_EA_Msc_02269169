library(ggplot2)
library(gghalves)
library(dplyr)
library(ggsignif)
library(lmerTest)

# Extract fixed effects coefficients
fixed_effects <- summary(model_event_rate)$coefficients

# Extract confidence intervals for each fixed effect
conf_intervals <- confint(model_event_rate, level = 0.95)

# Extract p-values using car::Anova
anova_results <- Anova(model_event_rate, type = "III")
fixed_effects_pvalues <- anova_results$`Pr(>Chisq)`

# Extract variance contributions of random effects
random_effects <- summary(model_event_rate)$varcor

# Extract AIC and R² for the model
model_aic <- AIC(model_event_rate)
model_r_squared <- r.squaredGLMM(model_event_rate)

# Save extracted confidence intervals, coefficients, and p-values as a data frame
model_summary_df <- as.data.frame(fixed_effects)

# Extract confidence intervals for the fixed effects
fixed_effects_conf_intervals <- conf_intervals[rownames(model_summary_df),]

# Merge confidence intervals into the fixed effects data frame
model_summary_df$conf_low <- fixed_effects_conf_intervals[,"2.5 %"]
model_summary_df$conf_high <- fixed_effects_conf_intervals[,"97.5 %"]
model_summary_df$p_value <- fixed_effects_pvalues

# Keep the fixed effects of interest (including intercept and relevant treatment groups)
model_summary_df <- model_summary_df[grep("Intercept|treatment|week", rownames(model_summary_df)),]

# View the extracted results
print(model_summary_df)

# Convert event rate to per-minute rate (multiply by 60)
all_data <- all_data %>%
  mutate(event_rate_per_min = event_rate * 60)

# Calculate group means for each treatment in each week (converted event rate)
group_means <- all_data %>%
  group_by(treatment, week) %>%
  summarise(mean_event_rate_per_min = mean(event_rate_per_min))

p <- ggplot(all_data, aes(x = interaction(treatment, week), y = event_rate_per_min)) +
  geom_half_violin(aes(fill = treatment), side = "r", trim = FALSE, position = position_nudge(x = 0.03)) +
  geom_half_point(aes(color = treatment), side = "l", position = position_nudge(x = -0.03), size = 2) +
  geom_boxplot(aes(fill = treatment), width = 0.12, position = position_nudge(x = 0.03), alpha = 0.4, color = "black") +
  scale_fill_manual(values = c("Control" = "#D9B9D4", "Prothio" = "#AEB2D1", "Strep" = "#B3D1ED")) +
  scale_color_manual(values = c("Control" = "#D9B9D4", "Prothio" = "#AEB2D1", "Strep" = "#B3D1ED")) +
  labs(x = "Treatment - Week", y = "Event Rate (per minute)") +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.position = "right",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  geom_point(data = group_means, aes(x = interaction(treatment, week), y = mean_event_rate_per_min),
             color = "darkred", size = 3, shape = 18) +
  expand_limits(y = c(0, max(all_data$event_rate_per_min) * 1.2))

# Add significance bridges for comparisons across weeks
p1 <- p +
  geom_signif(
    comparisons = list(c("Prothio.Week 1", "Prothio.Week 2"), 
                       c("Prothio.Week 1", "Prothio.Week 3"),
                       c("Prothio.Week 1", "Prothio.Week 4")),
    annotations = c(
      sprintf("p(week2) = %.2e", model_summary_df["factor(week)2", "p_value"]),
      sprintf("p(week3) = %.2e", model_summary_df["factor(week)3", "p_value"]),
      sprintf("p(week4) = %.2e", model_summary_df["factor(week)4", "p_value"])
    ),
    y_position = c(7.0, 7.5, 8.0),
    tip_length = 0.02
  ) +
  geom_signif(
    data = subset(all_data, week == "Week 1"), # Significance test for Week 1 only
    comparisons = list(c("Control.Week 1", "Prothio.Week 1"), c("Control.Week 1", "Strep.Week 1")),
    annotations = c(
      sprintf("p(Prothio) = %.2e", model_summary_df["treatmentProthio", "p_value"]),
      sprintf("p(Strep) = %.2e", model_summary_df["treatmentStrep", "p_value"])
    ),
    y_position = c(4.1, 5.1), # Control bridge height for Week 1
    tip_length = 0.02
  )

# Show the final plot
print(p1)

# Extract estimates
estimate_values <- c(
  # Control group
  model_summary_df["(Intercept)", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["factor(week)2", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["factor(week)3", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["factor(week)4", "Estimate"],
  
  # Prothio group
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentProthio", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentProthio", "Estimate"] + model_summary_df["factor(week)2", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentProthio", "Estimate"] + model_summary_df["factor(week)3", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentProthio", "Estimate"] + model_summary_df["factor(week)4", "Estimate"],
  
  # Strep group
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentStrep", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentStrep", "Estimate"] + model_summary_df["factor(week)2", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentStrep", "Estimate"] + model_summary_df["factor(week)3", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentStrep", "Estimate"] + model_summary_df["factor(week)4", "Estimate"]
)

# Print estimates for confirmation
print(estimate_values)

# Extract lower bounds of confidence intervals (conf_low)
conf_low_values <- c(
  # Control group
  model_summary_df["(Intercept)", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["factor(week)2", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["factor(week)3", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["factor(week)4", "conf_low"],
  
  # Prothio group
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentProthio", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentProthio", "conf_low"] + model_summary_df["factor(week)2", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentProthio", "conf_low"] + model_summary_df["factor(week)3", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentProthio", "conf_low"] + model_summary_df["factor(week)4", "conf_low"],
  
  # Strep group
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentStrep", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentStrep", "conf_low"] + model_summary_df["factor(week)2", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentStrep", "conf_low"] + model_summary_df["factor(week)3", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentStrep", "conf_low"] + model_summary_df["factor(week)4", "conf_low"]
)

# Print lower bounds of confidence intervals for confirmation
print(conf_low_values)

# Extract upper bounds of confidence intervals (conf_high)
conf_high_values <- c(
  # Control group
  model_summary_df["(Intercept)", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["factor(week)2", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["factor(week)3", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["factor(week)4", "conf_high"],
  
  # Prothio group
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentProthio", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentProthio", "conf_high"] + model_summary_df["factor(week)2", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentProthio", "conf_high"] + model_summary_df["factor(week)3", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentProthio", "conf_high"] + model_summary_df["factor(week)4", "conf_high"],
  
  # Strep group
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentStrep", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentStrep", "conf_high"] + model_summary_df["factor(week)2", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentStrep", "conf_high"] + model_summary_df["factor(week)3", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentStrep", "conf_high"] + model_summary_df["factor(week)4", "conf_high"]
)

# Print upper bounds of confidence intervals for confirmation
print(conf_high_values)

# Build final data frame
ci_data <- data.frame(
  week = rep(c("Week 1", "Week 2", "Week 3", "Week 4"), times = 3),
  treatment = factor(rep(c("Control", "Prothio", "Strep"), each = 4)),
  estimate = estimate_values,
  conf_low = conf_low_values,
  conf_high = conf_high_values
)

# Print data frame for confirmation
print(ci_data)

# Convert confidence intervals and estimates to per-minute units
ci_data$estimate <- ci_data$estimate * 60
ci_data$conf_low <- ci_data$conf_low * 60
ci_data$conf_high <- ci_data$conf_high * 60

# Use scientific notation for BP test value
bp_p_value <- 2.022e-05

# Generate annotation text
annotation_text <- paste0("R²(m) = ", r_squared_m, ", R²(c) = ", r_squared_c, " | BP Test: p = ", bp_p_value, " | Shapiro-Wilk: p = ", shapiro_p_value)

p2 <- ggplot(ci_data, aes(x = interaction(treatment, week), y = estimate, color = treatment, group = treatment)) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2, size = 1.1, position = position_dodge(width = 0.5)) + # Adjust error bar width
  geom_line(position = position_dodge(width = 0.5), linetype = "dashed", size = 1.1) + # Adjust dashed line width
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_jitter(data = all_data, aes(x = interaction(treatment, week), y = event_rate_per_min), # Add actual data points
              width = 0.2, height = 0, alpha = 0.6, color = "black", size = 1) + # Adjust point position and style
  scale_color_manual(values = c("Control" = "#D9B9D4", "Prothio" = "#AEB2D1", "Strep" = "#B3D1ED")) +
  labs(x = "Treatment - Week", y = "Event Rate (per minute)", color = "Treatment") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.position = "right",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  expand_limits(y = c(0, max(all_data$event_rate_per_min) * 1.2)) + # Dynamically set y-axis range
  annotate("text", x = 6, y = 6.5, hjust = 1, vjust = 1, # Dynamically adjust x position
           label = annotation_text,
           size = 4, color = "black") +
  theme(
    plot.margin = margin(t = 10, r = 10, b = 40, l = 10)
  )

# Show the final plot
print(p2)
