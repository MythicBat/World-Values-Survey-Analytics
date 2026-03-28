source("scripts/00_setup.R")

# Basic Structure
dim_result <- dim(VC_clean)
str(VC_clean)
summary(VC_clean)

# Save Dimensions
dim_df <- data.frame(
    Metric = c("Rows", "Columns", "Netherlands Rows", "Other Countries Rows"),
    Value = c(nrow(VC_clean), ncol(VC_clean), nrow(nld), nrow(others))
)
write.csv(dim_df, "outputs/tables/q1_dimensions.csv", row.names = FALSE)

# Data Types
type_df <- data.frame(
    Variable = names(VC_clean),
    Class = sapply(VC_clean, class)
)
write.csv(type_df, "outputs/tables/q1_variable_types.csv", row.names = FALSE)

# Missing Values
missing_df <- data.frame(
    Variable = names(VC_clean),
    MissingCount = sapply(VC_clean, function(x) sum(is.na(x))),
    MissingPercent = round(sapply(VC_clean, function(x) mean(is.na(x)) * 100), 2)
)
write.csv(missing_df, "outputs/tables/q1_missing_values.csv", row.names = FALSE)

# Country / wave / year summaries
country_counts <- as.data.frame(table(VC_clean$Country))
colnames(country_counts) <- c("Country", "Count")
write.csv(country_counts, "outputs/tables/q1_country_counts.csv", row.names = FALSE)

wave_counts <- as.data.frame(table(VC_clean$Wave))
colnames(wave_counts) <- c("Wave", "Count")
write.csv(wave_counts, "outputs/tables/q1_wave_count.csv", row.names = FALSE)

year_counts <- as.data.frame(table(VC_clean$Year))
colnames(year_counts) <- c("Year", "Count")
write.csv(year_counts, "outputs/tables/q1_year_count.csv", row.names = FALSE)

# Summary for confidence variables
safe_summary <- function(x) {
    if (all(is.na(x))) {
        return(c(Mean = NA, SD = NA, Min = NA, Max = NA, Missing = length(x)))
    } else {
       return(c(
        Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE),
        Missing = sum(is.na(x))
       ))
    }
}

conf_summary <- data.frame(
    Variable = confidence_vars,
    t(sapply(VC_clean[confidence_vars], safe_summary))
)
write.csv(conf_summary, "outputs/tables/q1_confidence_summary.csv", row.names = FALSE)

# Summary for predictors
pred_summary <- data.frame(
    Variable = predictor_vars,
    t(sapply(VC_clean[predictor_vars], safe_summary))
)
write.csv(pred_summary, "outputs/tables/q1_predictor_summary.csv", row.names = FALSE)

# -----------------------------
# Graph 1: Missing value percentage
# -----------------------------
g_missing <- ggplot(missing_df, aes(x = reorder(Variable, MissingPercent), y = MissingPercent)) + 
  geom_col() + 
  coord_flip() + 
  theme_minimal() + 
  labs(
    title = "Missing Values by Variable",
    x = "Variable",
    y = "Missing Percentage"
  )

ggsave("outputs/figures/q1_missing_values.png", g_missing, width = 9, height = 8)

# --------------------------
# Graph 2: Distribution of confidence variables
# --------------------------
conf_long <- VC_clean %>%
  select(all_of(confidence_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Organization", values_to = "Confidence")

g_conf_hist <- ggplot(conf_long, aes(x = Confidence)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(~Organization, scales = "free_y") + 
  theme_minimal() + 
  labs(
    title = "Distribution of Confidence Variables",
    x = "Confidence Score",
    y = "Count"
  )

ggsave("outputs/figures/q1_confidence_distributions.png", g_conf_hist, width = 12, height = 8)