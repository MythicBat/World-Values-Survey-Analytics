source("scripts/00_setup.R")

# ----------------------------
# Q3(b): Predictive ability over time
# ----------------------------

fit_models_by_wave <- function(data, confidence_vars, predictor_vars, min_rows = 30) {
    waves <- sort(unique(na.omit(data$Wave)))
    out <- data.frame()

    for (w in waves) {
        dw <- data[data$Wave == w, ]

        for (cv in confidence_vars) {
            preds <- settdiff(predictor_vars, cv)

            # Keep only columns needed for this model
            model_df <- dw[, c(cv, preds), drop = FALSE]

            # Remove rows with missing data
            model_df <- na.omit(model_df)

            # Skip invalid cases
            if (nrow(model_df) < min_rows) {
                cat("Skipping", cv, "in wave", w, "-only", nrow(model_df), "complete rows\n")
                next
            }

            # Remove zero-variance predictors inside this wave
            good_preds <- preds[sapply(model_df[, preds, drop = FALSE], function(x) length(unique(x)) > 1)]

            # If no predictors remain, skip
            if (length(good_preds) == 0) {
                cat("Skipping", cv, "in wave", w, "-no valid predictors\n")
                next
            }

            formula_text <- paste(cv, "~", paste(good_preds, collapse = " + "))
            model <- lm(as.formula(formula_text), data = model_df)

            out <- rbind(out, data.frame(
                Wave = w,
                Organization = cv,
                Adjusted_R2 = summary(model)$adj.r.squared,
                N = nrow(model_df),
                Predictors_Used = length(good_preds)
            ))
        }
    }
    out
}

# Run models
nld_wave_r2 <- fit_models_by_wave(nld, confidence_vars, predictor_vars, min_rows = 30)