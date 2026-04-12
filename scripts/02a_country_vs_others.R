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

diff_df

g_diff <- ggplot(diff_df, aes(x = reorder(Organization, Difference), y = Difference, fill = Difference > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Confidence Group: Netherlands vs Other Countries",
    subtitle = "Positive values indicate higher confidence in the Netherlands",
    x = "",
    y = "Mean Difference (Netherlands - Other Countries)"
  )
ggsave("outputs/figures/q2a_confidence_diff.png", g_diff, width = 10, height = 8)

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
