source("00_setup.R")

# -----------------------------
# Q3(b): Predictive ability over time
# HD / stable version
# -----------------------------

# Reduced predictor set for wave-level modelling
q3b_predictors <- c(
  "Trusted",
  "LifeSatis",
  "GovtRespons",
  "PolExperts",
  "PolInterest",
  "Age"
)

# Keep only predictors that actually exist in the data
q3b_predictors <- intersect(q3b_predictors, names(VC_clean))

# Remove predictors with all NA or no variation in the full dataset
all_na_vars <- names(VC_clean)[sapply(VC_clean, function(x) all(is.na(x)))]
zero_var_vars <- names(VC_clean)[sapply(VC_clean, function(x) {
  vals <- na.omit(x)
  length(unique(vals)) <= 1
})]

q3b_predictors <- setdiff(q3b_predictors, c(all_na_vars, zero_var_vars))

# Confidence variables: all C* columns except Country
confidence_vars <- setdiff(grep("^C", names(VC_clean), value = TRUE), "Country")
confidence_vars <- setdiff(confidence_vars, c(all_na_vars, zero_var_vars))

# -----------------------------
# Robust function to fit models by wave
# -----------------------------
fit_models_by_wave <- function(data, confidence_vars, predictor_vars, min_rows = 20) {
  waves <- sort(unique(na.omit(data$Wave)))
  out <- data.frame()

  for (w in waves) {
    dw <- data[data$Wave == w, , drop = FALSE]

    for (cv in confidence_vars) {
      preds <- setdiff(predictor_vars, cv)

      needed_cols <- c("Wave", cv, preds)
      model_df <- dw[, needed_cols, drop = FALSE]

      # Keep complete cases only for this specific model
      model_df <- na.omit(model_df)

      # Skip if too few rows
      if (nrow(model_df) < min_rows) {
        cat("Skipping", cv, "in wave", w, "- only", nrow(model_df), "complete rows\n")
        next
      }

      # Remove predictors with no variation within this wave subset
      varying_preds <- preds[sapply(model_df[, preds, drop = FALSE], function(x) {
        length(unique(x)) > 1
      })]

      # Skip if no usable predictors remain
      if (length(varying_preds) == 0) {
        cat("Skipping", cv, "in wave", w, "- no varying predictors\n")
        next
      }

      formula_text <- paste(cv, "~", paste(varying_preds, collapse = " + "))

      model <- lm(as.formula(formula_text), data = model_df)
      sm <- summary(model)

      out <- rbind(out, data.frame(
        Wave = w,
        Organisation = cv,
        Adjusted_R2 = sm$adj.r.squared,
        R2 = sm$r.squared,
        N = nrow(model_df),
        Predictors_Used = length(varying_preds)
      ))
    }
  }

  out
}

# -----------------------------
# Run models
# -----------------------------
nld_wave_r2 <- fit_models_by_wave(nld, confidence_vars, q3b_predictors, min_rows = 20)
nld_wave_r2$Group <- "Netherlands"

others_wave_r2 <- fit_models_by_wave(others, confidence_vars, q3b_predictors, min_rows = 20)
others_wave_r2$Group <- "OtherCountries"

wave_r2_all <- rbind(nld_wave_r2, others_wave_r2)

# Save tables
write.csv(nld_wave_r2, "outputs/tables/q3b_nld_wave_r2_hd.csv", row.names = FALSE)
write.csv(others_wave_r2, "outputs/tables/q3b_others_wave_r2_hd.csv", row.names = FALSE)
write.csv(wave_r2_all, "outputs/tables/q3b_combined_wave_r2_hd.csv", row.names = FALSE)

# -----------------------------
# Graph 1: Predictive strength over time
# -----------------------------
library(ggplot2)
library(dplyr)

g_q3b_trends <- ggplot(wave_r2_all, aes(x = Wave, y = Adjusted_R2, color = Group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, linetype = "dashed", linewidth = 0.7) +
  facet_wrap(~Organisation, scales = "free_y") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Predictive Strength of Institutional Confidence Over Time",
    subtitle = "Adjusted R² by wave for the Netherlands and other countries",
    x = "Wave",
    y = "Adjusted R²"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/figures/q3b_predictive_strength_over_time_hd.png", g_q3b_trends, width = 12, height = 8)

# -----------------------------
# Graph 2: Gap in predictive strength
# -----------------------------
gap_r2 <- wave_r2_all %>%
  select(Wave, Organisation, Group, Adjusted_R2) %>%
  tidyr::pivot_wider(names_from = Group, values_from = Adjusted_R2) %>%
  mutate(Gap = Netherlands - OtherCountries)

g_q3b_gap <- ggplot(gap_r2, aes(x = Wave, y = Gap)) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Organisation, scales = "free_y") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Gap in Predictive Strength Over Time",
    subtitle = "Positive values indicate stronger predictive models in the Netherlands",
    x = "Wave",
    y = "Adjusted R² Gap"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

ggsave("outputs/figures/q3b_predictive_gap_over_time_hd.png", g_q3b_gap, width = 12, height = 8)

# -----------------------------
# Graph 3: Best-predicted organisation in each group and wave
# -----------------------------
best_by_wave <- wave_r2_all %>%
  group_by(Group, Wave) %>%
  slice_max(order_by = Adjusted_R2, n = 1, with_ties = FALSE) %>%
  ungroup()

write.csv(best_by_wave, "outputs/tables/q3b_best_predicted_by_wave_hd.csv", row.names = FALSE)

g_best <- ggplot(best_by_wave, aes(x = factor(Wave), y = Adjusted_R2, fill = Organisation)) +
  geom_col() +
  facet_wrap(~Group) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Best-Predicted Institution by Wave",
    subtitle = "Highest adjusted R² in each group and wave",
    x = "Wave",
    y = "Adjusted R²"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/figures/q3b_best_predicted_by_wave_hd.png", g_best, width = 10, height = 6)

cat("Q3(b) HD version complete.\n")
print(q3b_predictors)
print(head(wave_r2_all))