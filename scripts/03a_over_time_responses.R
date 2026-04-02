source("scripts/00_setup.R")

# ----------------------------
# Q3(a): Compare responses over time
# Use Wave
# ----------------------------

# Mean confidence by wave for Netherlands
nld_wave_conf <- nld %>%
  group_by(Wave) %>%
  summarise(across(all_of(confidence_vars), ~mean(.x, na.rm = TRUE)))

write.csv(nld_wave_conf, "outputs/tables/confidence_by_wave.csv", row.names = FALSE)

# Mean confidence by wave for other countries
others_time_conf <- others %>%
  group_by(Wave) %>%
  summarise(across(all_of(confidence_vars), ~mean(.x, na.rm = TRUE)))

write.csv(others_time_conf, "outputs/tables/confidence_by_wave_others.csv", row.names = FALSE)

# Combine for plotting
nld_long <- nld_wave_conf %>%
   pivot_longer(cols = -Wave, names_to = "Organization", values_to = "MeanConfidence") %>%
   mutate(Group = "Netherlands")

others_long <- others_time_conf %>%
   pivot_longer(cols = -Wave, names_to = "Organization", values_to = "MeanConfidence") %>%
   mutate(Group = "Other Countries")

combine_time <- bind_rows(nld_long, others_long)
write.csv(combine_time, "outputs/tables/confidence_by_wave_combined.csv", row.names = FALSE)

# Plot
g_time_conf <- ggplot(combine_time, aes(x = Wave, y = MeanConfidence, color = Group)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Organization, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Confidence in Social Organization",
    x = "Wave",
    y = "Mean Confidence"
  )
ggsave(g_time_conf, filename = "outputs/figures/confidence_over_time.png", width = 12, height = 8)

# Differences by wave
wave_diff <- merge(
    nld_wave_conf,
    others_time_conf,
    by = "Wave",
    suffixes = c("_NLD", "_Other")
)
write.csv(wave_diff, "outputs/tables/confidence_wave_differences.csv", row.names = FALSE)

cat("Q3(a) complete.\n")