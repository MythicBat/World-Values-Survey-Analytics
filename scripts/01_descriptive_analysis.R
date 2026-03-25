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
conf_summary <- data.frame(
    Variable = confidence_vars,
    Mean = sapply(VC_clean[confidence_vars], function(x) mean(x, na.rm = TRUE)),
    SD = sapply(VC_clean[confidence_vars], function(x) sd(x, na.rm = TRUE)),
    Min = sapply(VC_clean[confidence_vars], function(x) min(x, na.rm = TRUE)),
    Max = sapply(VC_clean[confidence_vars], function(x) max(x, na.rm = TRUE)),
    Missing = sapply(VC_clean[confidence_vars], function(x) sum(is.na(x)))
)
write.csv(conf_summary, "outputs/tables/q1_confidence_summary.csv", row.names = FALSE)

names(VC_clean)
confidence_vars