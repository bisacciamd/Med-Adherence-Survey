# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)

# First, let's create the summary_stats data structure you need
summary_stats <- all_data %>%
  group_by(country) %>%
  summarise(
    # Basic counts
    n_total = n(),
  
    # Average ratings for interventions (1-4 scale)
    vouchers = mean(vouchers_rating, na.rm = TRUE),
    fine = mean(fine_rating, na.rm = TRUE),
    support_program = mean(support_program_rating, na.rm = TRUE),
    phone_reminders = mean(phone_reminders_rating, na.rm = TRUE),
    emoji = mean(emoji_rating, na.rm = TRUE),
    pension_collection = mean(pension_collection_rating, na.rm = TRUE),
    health_info = mean(health_info_rating, na.rm = TRUE)
  )

# Select the columns you want
summary_stats_selected <- summary_stats %>%
  select(country, n_total, vouchers, fine, support_program, phone_reminders, 
         emoji, pension_collection, health_info)

# Prepare data for visualization - pivot to long format for ratings
ratings_long <- summary_stats %>%
  select(country, vouchers:health_info) %>%
  pivot_longer(
    cols = vouchers:health_info,
    names_to = "intervention",
    values_to = "rating"
  ) %>%
  mutate(
    intervention_label = case_when(
      intervention == "vouchers" ~ "Grocery Vouchers",
      intervention == "fine" ~ "Fine for Forgetting",
      intervention == "support_program" ~ "Support Program",
      intervention == "phone_reminders" ~ "Phone Reminders",
      intervention == "emoji" ~ "Digital Emoji Feedback",
      intervention == "pension_collection" ~ "Pension Day Collection",
      intervention == "health_info" ~ "Health Information"
    )
  )

# Calculate overall average rating for each intervention across all countries
intervention_order <- ratings_long %>%
  group_by(intervention_label) %>%
  summarise(overall_avg = mean(rating, na.rm = TRUE)) %>%
  arrange(desc(overall_avg)) %>%
  pull(intervention_label)

# Apply the ordering to the data
ratings_long <- ratings_long %>%
  mutate(intervention_label = factor(intervention_label, levels = intervention_order))

# 1. Heatmap visualization with interventions ordered by overall average
heatmap_plot <- ggplot(ratings_long, aes(x = country, y = intervention_label, fill = rating)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = sprintf("%.2f", rating)), color = "white", size = 4, fontface = "bold") +
  scale_fill_gradient2(
    low = "#D32F2F",
    mid = "#FFA726",
    high = "#388E3C",
    midpoint = 2.5,
    limits = c(1, 4),
    name = "Average\nRating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Country-specific Intervention Ratings",
    subtitle = "Interventions ordered by overall effectiveness (1 = Not at all helpful, 4 = Very helpful)"
  )

# Display the plot
print(heatmap_plot)

# Save the plot
ggsave("ratings_heatmap_ordered.png", heatmap_plot, width = 10, height = 8)

# Create a version with row averages shown
ratings_with_avg <- ratings_long %>%
  bind_rows(
    ratings_long %>%
      group_by(intervention_label) %>%
      summarise(rating = mean(rating, na.rm = TRUE)) %>%
      mutate(country = "AVERAGE")
  )

# Heatmap with average column
heatmap_with_avg <- ggplot(ratings_with_avg, aes(x = country, y = intervention_label, fill = rating)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = sprintf("%.2f", rating)), 
            color = ifelse(ratings_with_avg$country == "AVERAGE", "black", "white"), 
            size = 4, 
            fontface = ifelse(ratings_with_avg$country == "AVERAGE", "bold", "plain")) +
  geom_vline(xintercept = length(unique(ratings_with_avg$country)) - 0.5, 
             color = "black", size = 1.5) +
  scale_fill_gradient2(
    low = "#D32F2F",
    mid = "#FFA726",
    high = "#388E3C",
    midpoint = 2.5,
    limits = c(1, 4),
    name = "Average\nRating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1,
                               face = ifelse(unique(ratings_with_avg$country) == "AVERAGE", "bold", "plain")),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Country-specific Intervention Ratings with Overall Average",
    subtitle = "Interventions ordered by overall effectiveness (1 = Not at all helpful, 4 = Very helpful)"
  )

# Display the plot with averages
print(heatmap_with_avg)

# Save the plot with averages
ggsave("ratings_heatmap_ordered_with_avg.png", heatmap_with_avg, width = 11, height = 8)

# Print the intervention order for reference
cat("\nInterventions ordered by overall effectiveness (highest to lowest):\n")
for (i in seq_along(intervention_order)) {
  avg_score <- ratings_long %>%
    filter(intervention_label == intervention_order[i]) %>%
    summarise(avg = mean(rating, na.rm = TRUE)) %>%
    pull(avg)
  cat(sprintf("%d. %s (%.2f)\n", i, intervention_order[i], avg_score))
}