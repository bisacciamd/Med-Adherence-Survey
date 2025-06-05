# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)

# First, let's create the summary_stats data structure you need
summary_stats <- all_data %>%
  group_by(source_language) %>%
  summarise(
    # Basic counts
    n_total = n(),
    
    # Adherence patterns (counts)
    adherent = sum(medication_adherence == "I always take them as the doctor told me", na.rm = TRUE),
    forget_lt_week = sum(medication_adherence == "Sometimes I forget (less than once a week)", na.rm = TRUE),
    forget_gt_week = sum(medication_adherence == "Sometimes I forget (more than once a week)", na.rm = TRUE),
    stopped = sum(medication_adherence == "I have stopped taking them", na.rm = TRUE),
    
    # Identity impact (count)
    impact_identity = sum(identity_impact == "Yes", na.rm = TRUE),
    
    # Average ratings for interventions (1-4 scale)
    vouchers = mean(vouchers_rating, na.rm = TRUE),
    fine = mean(fine_rating, na.rm = TRUE),
    support_program = mean(support_program_rating, na.rm = TRUE),
    phone_reminders = mean(phone_reminders_rating, na.rm = TRUE),
    emoji = mean(emoji_rating, na.rm = TRUE),
    pension_collection = mean(pension_collection_rating, na.rm = TRUE),
    health_info = mean(health_info_rating, na.rm = TRUE)
  ) %>%
  rename(Language = source_language)

# Select the columns you want
summary_stats_selected <- summary_stats %>%
  select(Language, n_total, adherent, forget_lt_week, forget_gt_week, stopped, 
         impact_identity, vouchers, fine, support_program, phone_reminders, 
         emoji, pension_collection, health_info)

# Print the summary table
print("Summary Statistics by Language:")
print(summary_stats_selected)

# Prepare data for visualization - pivot to long format for ratings
ratings_long <- summary_stats %>%
  select(Language, vouchers:health_info) %>%
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

# 1. Heatmap visualization
heatmap_plot <- ggplot(ratings_long, aes(x = Language, y = fct_reorder(intervention_label, rating), fill = rating)) +
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
    title = "Intervention Effectiveness Ratings by Language",
    subtitle = "1 = Not at all helpful, 4 = Very helpful"
  )

ggsave("ratings_heatmap.png", heatmap_plot, width = 10, height = 8)