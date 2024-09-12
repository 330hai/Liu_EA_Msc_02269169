library(ggplot2)
library(gghalves)
library(dplyr)
library(ggsignif)
library(lmerTest)

# Extract fixed effects coefficients
fixed_effects <- summary(model_pollen_rate_interaction)$coefficients
print(fixed_effects)

# Extract confidence intervals for each fixed effect
conf_intervals <- confint(model_pollen_rate_interaction, level = 0.95)

# Extract p-values using car::Anova
anova_results <- Anova(model_pollen_rate_interaction, type = "III")
fixed_effects_pvalues <- anova_results$`Pr(>Chisq)`

# Extract variance contributions of random effects
random_effects <- summary(model_pollen_rate_interaction)$varcor

# Extract AIC and R² for the model
model_aic <- AIC(model_pollen_rate_interaction)
model_r_squared <- r.squaredGLMM(model_pollen_rate_interaction)

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

# Filter data
week1_summary_filter <- week1_summary %>%
  filter(event_count >= 40)
week2_summary_filter <- week2_summary %>%
  filter(event_count >= 40)
week3_summary_filter <- week3_summary %>%
  filter(event_count >= 40)
week4_summary_filter <- week4_summary %>%
  filter(event_count >= 40)

week1_summary_filter <- week1_summary_filter %>% mutate(week = "Week 1")
week2_summary_filter <- week2_summary_filter %>% mutate(week = "Week 2")
week3_summary_filter <- week3_summary_filter %>% mutate(week = "Week 3")
week4_summary_filter <- week4_summary_filter %>% mutate(week = "Week 4")

# Combine data from all weeks
all_data <- bind_rows(week1_summary_filter, week2_summary_filter, week3_summary_filter, week4_summary_filter)

# Calculate group means for each treatment in each week
group_means <- all_data %>%
  group_by(treatment, week) %>%
  summarise(mean_pollen_carrying_rate = mean(pollen_carrying_rate))

# Plot integrated data for all weeks
p <- ggplot(all_data, aes(x = interaction(treatment, week), y = pollen_carrying_rate)) +
  geom_half_violin(aes(fill = treatment), side = "r", trim = FALSE, position = position_nudge(x = 0.03)) +
  geom_half_point(aes(color = treatment), side = "l", position = position_nudge(x = -0.03), size = 2) +
  geom_boxplot(aes(fill = treatment), width = 0.12, position = position_nudge(x = 0.03), alpha = 0.4, color = "black") +
  scale_fill_manual(values = c("Control" = "#D9B9D4", "Prothio" = "#AEB2D1", "Strep" = "#B3D1ED")) +
  scale_color_manual(values = c("Control" = "#D9B9D4", "Prothio" = "#AEB2D1", "Strep" = "#B3D1ED")) +
  labs(x = "Treatment - Week", y = "Pollen Carrying Rate") +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(), # Hide minor grid lines
    panel.grid.major = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"), # Turn ticks inward
    legend.position = "right",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  geom_point(data = group_means, aes(x = interaction(treatment, week), y = mean_pollen_carrying_rate),
             color = "darkred", size = 3, shape = 18) + # Extra group mean points
  expand_limits(y = c(0, 1.25)) # Unified y-axis range

# Extract p-values for significance annotations
week_1_prothio_p_value <- sprintf("p = %.3e", model_summary_df["treatmentProthio", "p_value"])
week_1_strep_p_value <- sprintf("p = %.3e", model_summary_df["treatmentStrep", "p_value"])
week_2_prothio_p_value <- sprintf("p = %.3e", model_summary_df["treatmentProthio:factor(week)2", "p_value"])
week_2_strep_p_value <- sprintf("p = %.3e", model_summary_df["treatmentStrep:factor(week)2", "p_value"])
week_3_prothio_p_value <- sprintf("p = %.3e", model_summary_df["treatmentProthio:factor(week)3", "p_value"])
week_3_strep_p_value <- sprintf("p = %.3e", model_summary_df["treatmentStrep:factor(week)3", "p_value"])
week_4_prothio_p_value <- sprintf("p = %.3e", model_summary_df["treatmentProthio:factor(week)4", "p_value"])
week_4_strep_p_value <- sprintf("p = %.3e", model_summary_df["treatmentStrep:factor(week)4", "p_value"])

# Dynamically generate annotation text and apply to significance bridges
p1 <- p + 
  geom_signif(
    comparisons = list(c("Control.Week 1", "Prothio.Week 1"), c("Control.Week 1", "Strep.Week 1")),
    annotations = c(week_1_prothio_p_value, week_1_strep_p_value),
    y_position = c(1.1, 1.2),
    tip_length = 0.02
  ) +
  geom_signif(
    comparisons = list(c("Control.Week 2", "Prothio.Week 2"), c("Control.Week 2", "Strep.Week 2")),
    annotations = c(week_2_prothio_p_value, week_2_strep_p_value),
    y_position = c(1.3, 1.4),
    tip_length = 0.02
  ) +
  geom_signif(
    comparisons = list(c("Control.Week 3", "Prothio.Week 3"), c("Control.Week 3", "Strep.Week 3")),
    annotations = c(week_3_prothio_p_value, week_3_strep_p_value),
    y_position = c(1.5, 1.6),
    tip_length = 0.02
  ) +
  geom_signif(
    comparisons = list(c("Control.Week 4", "Prothio.Week 4"), c("Control.Week 4", "Strep.Week 4")),
    annotations = c(week_4_prothio_p_value, week_4_strep_p_value),
    y_position = c(1.7, 1.8),
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
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentProthio", "Estimate"] + model_summary_df["factor(week)2", "Estimate"] + model_summary_df["treatmentProthio:factor(week)2", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentProthio", "Estimate"] + model_summary_df["factor(week)3", "Estimate"] + model_summary_df["treatmentProthio:factor(week)3", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentProthio", "Estimate"] + model_summary_df["factor(week)4", "Estimate"] + model_summary_df["treatmentProthio:factor(week)4", "Estimate"],
  
  # Strep group
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentStrep", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentStrep", "Estimate"] + model_summary_df["factor(week)2", "Estimate"] + model_summary_df["treatmentStrep:factor(week)2", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentStrep", "Estimate"] + model_summary_df["factor(week)3", "Estimate"] + model_summary_df["treatmentStrep:factor(week)3", "Estimate"],
  model_summary_df["(Intercept)", "Estimate"] + model_summary_df["treatmentStrep", "Estimate"] + model_summary_df["factor(week)4", "Estimate"] + model_summary_df["treatmentStrep:factor(week)4", "Estimate"]
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
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentProthio", "conf_low"] + model_summary_df["factor(week)2", "conf_low"] + model_summary_df["treatmentProthio:factor(week)2", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentProthio", "conf_low"] + model_summary_df["factor(week)3", "conf_low"] + model_summary_df["treatmentProthio:factor(week)3", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentProthio", "conf_low"] + model_summary_df["factor(week)4", "conf_low"] + model_summary_df["treatmentProthio:factor(week)4", "conf_low"],
  
  # Strep group
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentStrep", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentStrep", "conf_low"] + model_summary_df["factor(week)2", "conf_low"] + model_summary_df["treatmentStrep:factor(week)2", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentStrep", "conf_low"] + model_summary_df["factor(week)3", "conf_low"] + model_summary_df["treatmentStrep:factor(week)3", "conf_low"],
  model_summary_df["(Intercept)", "conf_low"] + model_summary_df["treatmentStrep", "conf_low"] + model_summary_df["factor(week)4", "conf_low"] + model_summary_df["treatmentStrep:factor(week)4", "conf_low"]
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
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentProthio", "conf_high"] + model_summary_df["factor(week)2", "conf_high"] + model_summary_df["treatmentProthio:factor(week)2", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentProthio", "conf_high"] + model_summary_df["factor(week)3", "conf_high"] + model_summary_df["treatmentProthio:factor(week)3", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentProthio", "conf_high"] + model_summary_df["factor(week)4", "conf_high"] + model_summary_df["treatmentProthio:factor(week)4", "conf_high"],
  
  # Strep group
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentStrep", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentStrep", "conf_high"] + model_summary_df["factor(week)2", "conf_high"] + model_summary_df["treatmentStrep:factor(week)2", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentStrep", "conf_high"] + model_summary_df["factor(week)3", "conf_high"] + model_summary_df["treatmentStrep:factor(week)3", "conf_high"],
  model_summary_df["(Intercept)", "conf_high"] + model_summary_df["treatmentStrep", "conf_high"] + model_summary_df["factor(week)4", "conf_high"] + model_summary_df["treatmentStrep:factor(week)4", "conf_high"]
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

# Modify plot code for confidence intervals, hiding x-axis labels and adjusting y-axis to percentages
p2 <- ggplot(ci_data, aes(x = interaction(treatment, week), y = estimate * 100, color = treatment, group = treatment)) +
  geom_errorbar(aes(ymin = conf_low * 100, ymax = conf_high * 100), width = 0.2, size = 1.1, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5), linetype = "dashed", size = 1.1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_jitter(data = all_data, aes(x = interaction(treatment, week), y = pollen_carrying_rate * 100), # Convert actual data points to percentages
              width = 0.2, height = 0, alpha = 0.6, color = "black", size = 1) + # Adjust point position and style
  scale_color_manual(values = c("Control" = "#D9B9D4", "Prothio" = "#AEB2D1", "Strep" = "#B3D1ED")) +
  labs(x = "Treatment - Week", y = "Pollen Carrying Rate (%)", color = "Treatment") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.position = "right",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  # Add two red dashed lines representing 0% and 100%
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", size = 0.5)

# Add simplified annotation below the plot
p2 <- p2 + 
  annotate("text", x = 6, y = -35, hjust = 1, vjust = 1, 
           label = "R²(m) = 0.66, R²(c) = 0.75 | BP Test: p = 0.425 | Shapiro-Wilk: p = 0.160", 
           size = 4, color = "black") +
  theme(
    plot.margin = margin(t = 10, r = 10, b = 40, l = 10)
  )

# Show the plot
print(p2)
