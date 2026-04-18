top_10_plot <- videos_analysis %>%
  slice_max(views, n = 10) %>%
  mutate(title = fct_reorder(title, views))


ggplot(top_10_plot, aes(x = views, y = title, fill = content_type)) +
  geom_col() +
  scale_x_continuous(labels = scales::label_number(scale = 1e-9, suffix = "B")) +
  labs(
    title = "Top 10 Most Viewed Youtube Videos",
    subtitle = "Views scaled in Billions",
    x = "Views",
    y = NULL,
    fill = "Category"
  ) +
  theme_minimal(base_size = 16) + # Increases overall font size
  theme(
    plot.title = element_text(size = 24, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, color = "grey30"),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14, face = "bold", color = "black"), # Clearer labels
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
  scale_fill_brewer(palette = "Set2")



lang_summary <- videos_analysis %>%
  count(detected_language) %>%
  top_n(50, n) # Just take the top 6 languages found in the data

# Final simple bar chart using steelblue
ggplot(lang_summary, aes(x = n, y = detected_language)) +
  geom_col(fill = "steelblue") +
  # Add numeric labels to the right of the bars
  geom_text(aes(label = n), hjust = -0.2, size = 5, fontface = "bold") +
  labs(
    title = "Language Prevalence in Top 1000 Videos",
    x = "Number of Videos",
    y = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    # This makes the language names on the left bold and dark
    axis.text.y = element_text(face = "bold", color = "black", size = 14),
    axis.title.x = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  ) +
  # Adds 20% extra space to the right so labels don't get cut off
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)))




ggplot(videos_analysis, aes(x = is_short, y = like_ratio, fill = is_short)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(
    title = "Effort vs. Reward: Do Shorts get better engagement?",
    subtitle = "Analysis of Like-to-View ratios across the Top 1000",
    x = "Is it a YouTube Short?",
    y = "Like Ratio (Likes per 100 Views)",
    fill = "Short Format"
  ) +
  theme_minimal()

ggplot(videos_analysis, aes(x = is_short, y = like_ratio, fill = has_hashtags)) +
  geom_boxplot(outlier.alpha = 0.1) +
  labs(
    title = "Does Metadata Save Long-Form Content?",
    subtitle = "Comparing Hashtags vs. No Hashtags across Formats",
    x = "Is it a Short?",
    y = "Like Ratio (%)",
    fill = "Has Hashtags?"
  ) +
  theme_minimal()

ggplot(model_results, aes(x = estimate, y = term)) +
  geom_point(size = 3, color = "midnightblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "midnightblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Regression Coefficients: What Drives Engagement?",
    subtitle = "The (Intercept) represents the baseline engagement for Long-form videos without hashtags",
    x = "Impact on Like Ratio (%)",
    y = "Model Term"
  ) +
  theme_minimal()

