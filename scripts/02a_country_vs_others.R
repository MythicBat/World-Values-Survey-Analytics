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

g_lollipop <- ggplot(diff_df, aes(y = reorder(Organization, Difference))) +
  geom_segment(aes(x = OtherCountriesMean, xend = NetherlandsMean, yend = reorder(Organization, Difference)), linewidth = 1) +
  geom_point(aes(x = OtherCountriesMean), size = 3, shape = 16) +
  geom_point(aes(x = NetherlandsMean), size = 3, shape = 17) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Average Confidence: Netherlands vs Other Countries",
    subtitle = "Positive gaps indicate higher confidence in the Netherlands",
    x = "Mean Confidence score",
    y = "Organization"
  ) +
  theme(
    plot.title = element_text(face = "bold")
  )
ggsave("outputs/figures/q2a_confidence_diff_lollipop.png", g_lollipop, width = 10, height = 8)

g_stacked <- ggplot(conf_prop, aes(x = Group, y = Proportion, fill = factor(Confidence))) +
  geom_col() +
  facet_wrap(~Organization) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Confidence Score Composition by Group",
    subtitle = "Distribution of ordinal responses for each social organization",
    x = "Group",
    y = "Proportion",
    fill = "Confidence Score"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
ggsave("outputs/figures/q2a_confidence_stacked.png", g_stacked, width = 12, height = 8)
# Mean comparison for predictors
pred_group_summary <- VC_clean %>%
  group_by(Group) %>%
  summarise(across(all_of(predictor_vars), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = -Group,
               names_to = "Predictor",
               values_to = "MeanValue")

write.csv(pred_group_summary, "outputs/tables/q2a_predictor_group_mean.csv", row.names = FALSE)

g_pred_heat <- ggplot(pred_group_summary, aes(x = Group, y = Predictor, fill = MeanValue)) +
  geom_tile() +
  geom_text(aes(label = round(MeanValue, 2)), size = 3) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Average Predictor Values by Group",
    subtitle = "Overview of Netherlands vs other countries",
    x = "",
    y = "",
    fill = "Mean"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )
ggsave("outputs/figures/q2a_predictor_group_heatmap.png", g_pred_heat, width = 9, height = 10)
