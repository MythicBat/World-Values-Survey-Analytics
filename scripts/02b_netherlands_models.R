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
            coeffs = summary(model)$coefficients
        )
    }

    results
}

# Fit Netherlands models
nld_models <- fit_models(nld, confidence_vars, predictor_vars)

# Table of model strength