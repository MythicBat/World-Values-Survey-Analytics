source("scripts/00_setup.R")

# -----------------------------
# Q2(c): Model other countries and compare with Netherlands
# -----------------------------

fit_models <- function(data, confidence_vars, predictor_vars) {
  results <- list()

  for (cv in confidence_vars) {
    formula_text <- paste(cv, "~", paste(predictor_vars, collapse = " + "))
    model <- lm(as.formula(formula_text), data = data)

    results[[cv]] <- list(
      model = model,
      adj_r2 = summary(model)$adj.r.squared,
      r2 = summary(model)$r.squared,
      coeffs = summary(model)$coefficients
    )
  }

  results
}

get_top_predictors <- function(model_list, top_n = 5) {
  out <- data.frame()

  for (name in names(model_list)) {
    coefs <- as.data.frame(model_list[[name]]$coeffs)
    coefs$Predictor <- rownames(coefs)
    rownames(coefs) <- NULL

    coefs <- coefs[coefs$Predictor != "(Intercept)", ]
    coefs$AbsEstimate <- abs(coefs$Estimate)
    coefs <- coefs[order(-coefs$AbsEstimate), ]

    top <- head(coefs, top_n)
    top$Organisation <- name

    out <- rbind(out, top[, c("Organisation", "Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)")])
  }

  out
}

# Fit models
nld_models <- fit_models(nld, confidence_vars, predictor_vars)
other_models <- fit_models(others, confidence_vars, predictor_vars)

# Model strength tables
nld_r2 <- data.frame(
  Organisation = names(nld_models),
  Adjusted_R2 = sapply(nld_models, function(x) x$adj_r2)
)

other_r2 <- data.frame(
  Organisation = names(other_models),
  Adjusted_R2 = sapply(other_models, function(x) x$adj_r2)
)

r2_compare <- merge(nld_r2, other_r2, by = "Organisation", suffixes = c("_NLD", "_Other"))
r2_compare$Difference <- r2_compare$Adjusted_R2_NLD - r2_compare$Adjusted_R2_Other

write.csv(r2_compare, "outputs/tables/q2c_r2_comparison.csv", row.names = FALSE)

# Top predictors
nld_top <- get_top_predictors(nld_models, top_n = 5)
other_top <- get_top_predictors(other_models, top_n = 5)

r2_compare
nld_top

write.csv(nld_top, "outputs/tables/q2c_nld_top_predictors.csv", row.names = FALSE)
write.csv(other_top, "outputs/tables/q2c_other_top_predictors.csv", row.names = FALSE)

# Plot comparison
r2_long <- r2_compare %>%
  pivot_longer(
    cols = c(Adjusted_R2_NLD, Adjusted_R2_Other),
    names_to = "Group",
    values_to = "Adjusted_R2"
  )

g_r2_compare <- ggplot(r2_long, aes(x = Organisation, y = Adjusted_R2, fill = Group)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Predictive Strength: Netherlands vs Other Countries",
    x = "Organisation",
    y = "Adjusted R-squared"
  )

ggsave("outputs/figures/q2c_r2_comparison.png", g_r2_compare, width = 11, height = 6)

nld_top3 <- nld_top %>%
  group_by(Organisation) %>%
  slice_max(order_by = abs(Estimate), n = 3)

g_clean_heat <- ggplot(nld_top3, aes(x = Predictor, y = Organisation, fill = Estimate)) +
  geom_tile() +
  geom_text(aes(label = round(Estimate, 2))) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Top Predictors of Confidence (Netherlands)",
    subtitle = "Only strongest effects shown for clarity",
    x = "",
    y = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("outputs/figures/q2c_top_predictors.png", g_clean_heat, width = 11, height = 6)
cat("Q2(c) complete.\n")