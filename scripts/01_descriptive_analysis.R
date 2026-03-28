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
missing_df <- missing_df %>%
  arrange(desc(MissingPercent))

g_missing <- ggplot(missing_df, aes(x = reorder(Variable, MissingPercent), y = MissingPercent)) + 
  geom_col() + 
  coord_flip() + 
  theme_minimal(base_size = 12) + 
  labs(
    title = "Missing Data by Variable",
    subtitle = "Percentage of missing values after recoding survey missing codes to NA",
    x = "Variable",
    y = "Missing (%)"
  ) + 
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10)
  )
ggsave("outputs/figures/q1_missing_values.png", g_missing, width = 10, height = 8)

# --------------------------
# Graph 2: Distribution of confidence variables
# --------------------------
conf_long <- VC_clean %>%
  select(Group, all_of(confidence_vars)) %>%
  pivot_longer(cols = all_of(confidence_vars), names_to = "Organization", values_to = "Confidence") %>%
  filter(!is.na(Confidence))

conf_prop <- conf_long %>%
  group_by(Group, Organization, Confidence) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Group, Organization) %>%
  mutate(Proportion = n / sum(n))

g_conf_prop <- ggplot(conf_prop, aes(x = factor(Confidence), y = Proportion, fill = Group)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~Organization, scales = "free_y") + 
  theme_minimal(base_size = 12) + 
  labs(
    title = "Distribution of Confidence Scores by Organization",
    subtitle = "Proportion of responses in the Netherlands vs other countries",
    x = "Confidence Score",
    y = "Proportion"
  ) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("outputs/figures/q1_confidence_proportions.png", g_conf_prop, width = 12, height = 8)

# -------------------------
# Graph 3: 
# -------------------------
conf_means <- VC_clean %>%
  group_by(Group) %>%
  summarise(across(all_of(confidence_vars), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = -Group,
    names_to = "Organization",
    values_to = "MeanConfidence"
  )

g_conf_heat <- ggplot(conf_means, aes(x = Group, y = Organization, fill = MeanConfidence)) +
  geom_tile() +
  geom_text(aes(label = round(MeanConfidence, 2)), size = 4) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Average Confidence by Organization and Group",
    subtitle = "Compact Overview of Netherlands vs other countries",
    x = "",
    y = "",
    fill = "Mean"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"
  )
ggsave("outputs/figures/q1_confidence_heatmap.png", g_conf_heat, width = 8, height = 6)