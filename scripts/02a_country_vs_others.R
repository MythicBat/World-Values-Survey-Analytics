source("scripts/00_setup.R")

# ----------------------------
# 2(a): Compare Netherlands vs Other Countries
# Ignore time
# ----------------------------

# Mean comparison for confidence variables
conf_group_summary <- VC_clean %>%
  group_by(Group) %>%
  summarise(across(all_of(confidence_vars), ~mean(.x, na.rm = TRUE)))

write.csv(conf_group_summary, "outputs/tables/q2a_confidence_group_means.csv", row.names = FALSE)

nld_means <- conf_group_summary %>% filter(Group == "Netherlands")
other_means <- conf_group_summary %>% filter(Group == "OtherCountries")

diff_df <- data.frame(
  Organization = confidence_vars,
  NetherlandsMean = as.numeric(nld_means[1, confidence_vars]),
  OtherCountriesMean = as.numeric(other_means[1, confidence_vars])
)

diff_df$Difference <- diff_df$NetherlandsMean - diff_df$OtherCountriesMean
diff_df <- diff_df %>% arrange((Difference))

# Mean comparison for predictors
pred_group_summary <- VC_clean %>%
  group_by(Group) %>%
  summarise(across(all_of(predictor_vars), ~mean(.x, na.rm = TRUE)))

write.csv(pred_group_summary, "outputs/tables/q2a_predictor_group_mean.csv", row.names = FALSE)

# Long format for confidence bar plot
conf_group_long <- conf_group_summary %>%
  pivot_longer(cols = -Group, names_to = "Organization", values_to = "MeanConfidence")