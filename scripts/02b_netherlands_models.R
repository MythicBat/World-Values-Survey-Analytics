source("scripts/00_setup.R")

# ----------------------------
# 2(b): Predict confidence variables in Netherlands
# ----------------------------

# Function to fit the model
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

# Fit Netherlands models
nld_models <- fit_models(nld, confidence_vars, predictor_vars)

# Table of model strength
nld_r2 <- data.frame(
    Organization = names(nld_models),
    R2 = sapply(nld_models, function(x) x$r2),
    Adjusted_R2 = sapply(nld_models, function(x) x$adj_r2)
)

nld_r2 <- nld_r2[order(-nld_r2$Adjusted_R2), ]
write.csv(nld_r2, "outputs/tables/q2b_nld_model_strength.csv", row.names = FALSE)

nld_r2

# Function to extract strongest predictors
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
        top$Organization <- name

        out <- rbind(out, top[, c("Organization", "Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)")])
    }
    out
}

nld_top_predictors <- get_top_predictors(nld_models, top_n = 5)
write.csv(nld_top_predictors, "outputs/tables/q2b_nld_top_predictors.csv", row.names = FALSE)

nld_top_predictors

# Plot predictive length
g_nld_r2 <- ggplot(nld_r2, aes(x = reorder(Organization, Adjusted_R2), y = Adjusted_R2)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Adjusted_R2, 2)), hjust = -0.2) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Model Performance (Netherlands)",
    subtitle = "Adjusted R² by Institution",
    x = "",
    y = "Adjusted R²"
  ) +
  expand_limits(y = max(nld_r2$Adjusted_R2) + 0.05)
ggsave("outputs/figures/q2b_nld_model_strength.png", g_nld_r2, width = 10, height = 6)

# Plot top predictors for the strongest model
g_nld_top <- ggplot(nld_top_predictors, aes(x = Predictor, y = Organization, fill = Estimate)) +
  geom_tile() +
  geom_text(aes(label = round(Estimate, 2)), size = 3) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Strongest Predictors of Institutional Confidence in the Netherlands",
    subtitle = "Cells show the magnitude and direction of the largest coefficients",
    x = "Predictor",
    y = "Organization",
    fill = "|Estimate|"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
ggsave("outputs/figures/q2b_nld_top_predictors.png", g_nld_top, width = 12, height = 6)

cat("Q2(B) Complete. \n")