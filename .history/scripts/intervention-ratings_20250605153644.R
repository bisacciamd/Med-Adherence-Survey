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
    health_info = mean(health_info_rating, na.rm = TRUE),
    country = country
  )
# Select the columns you want
summary_stats_selected <- summary_stats %>%
  select(country, n_total, vouchers, fine, support_program, phone_reminders, 
         emoji, pension_collection, health_info)

# #print the summary table
#print("Summary Statistics by Language:")
#print(summary_stats_selected)

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

# 1. Heatmap visualization
heatmap_plot <- ggplot(ratings_long, aes(x = country, y = fct_reorder(intervention_label, rating), fill = rating)) +
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
    title = "Country-specific ratings",
    subtitle = "1 = Not at all helpful, 4 = Very helpful"
  )

heatmap_plot
##ggsave("ratings_heatmap.png", heatmap_plot, width = 10, height = 8)

# 2. Lollipop chart - Overall average ratings
overall_ratings <- ratings_long %>%
  group_by(intervention_label) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),
    se = sd(rating, na.rm = TRUE) / sqrt(n())
  ) %>%
  arrange(avg_rating)

lollipop_plot <- ggplot(overall_ratings, aes(x = avg_rating, y = fct_reorder(intervention_label, avg_rating))) +
  geom_segment(aes(x = 1, xend = avg_rating, y = intervention_label, yend = intervention_label), 
               color = "gray70", size = 1) +
  geom_point(aes(color = avg_rating), size = 6) +
  geom_text(aes(label = sprintf("%.2f", avg_rating)), 
            color = "white", size = 3, fontface = "bold") +
  scale_color_gradient2(
    low = "#D32F2F",
    mid = "#FFA726",
    high = "#388E3C",
    midpoint = 2.5,
    limits = c(1, 4),
    guide = "none"
  ) +
  scale_x_continuous(limits = c(1, 4), breaks = 1:4) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Health information and reminders are perceived as helpful",
    subtitle = "1 = Not at all helpful, 4 = Very helpful",
    x = "Average Rating"
  )

lollipop_plot
#ggsave("ratings_lollipop.png", lollipop_plot, width = 10, height = 8)

# 3. Grouped bar chart by language
grouped_bar_plot <- ggplot(ratings_long, aes(x = intervention_label, y = rating, fill = Language)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 4.5), breaks = 0:4) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    legend.position = "top"
  ) +
  labs(
    title = "Limited variation by language",
    subtitle = "Comparison of average ratings across different language groups",
    y = "Average Rating (1-4)"
  )

grouped_bar_plot
#ggsave("ratings_grouped_bars.png", grouped_bar_plot, width = 12, height = 8)

# 4. Radar chart for each language
library(fmsb)

# Prepare data for radar chart
radar_data <- summary_stats %>%
  select(Language, vouchers:health_info) %>%
  column_to_rownames("Language") %>%
  as.data.frame()

# Add max and min rows for radar chart
radar_data <- rbind(
  max = rep(4, ncol(radar_data)),
  min = rep(1, ncol(radar_data)),
  radar_data
)

# Create radar chart
png("ratings_radar.png", width = 800, height = 800)
par(mar = c(2, 2, 2, 2))
radarchart(
  radar_data,
  axistype = 1,
  pcol = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
  pfcol = c(rgb(0.12, 0.47, 0.71, 0.5), 
            rgb(1, 0.5, 0.05, 0.5),
            rgb(0.17, 0.63, 0.17, 0.5),
            rgb(0.84, 0.15, 0.16, 0.5)),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = seq(1, 4, 1),
  cglwd = 0.8,
  vlcex = 0.8,
  title = "Intervention Ratings by Language - Radar View"
)
legend(
  x = 1.3, y = 1.3,
  legend = rownames(radar_data)[-c(1,2)],
  col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
  lty = 1,
  lwd = 2,
  cex = 0.8
)
dev.off()

# 5. Diverging bar chart showing distance from neutral (2.5)
diverging_data <- overall_ratings %>%
  mutate(
    diff_from_neutral = avg_rating - 2.5,
    positive = diff_from_neutral > 0
  )

diverging_plot <- ggplot(diverging_data, aes(x = fct_reorder(intervention_label, diff_from_neutral), 
                                             y = diff_from_neutral, fill = positive)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "#D32F2F", "TRUE" = "#388E3C"), guide = "none") +
  scale_y_continuous(limits = c(-1.5, 1.5), 
                     breaks = seq(-1.5, 1.5, 0.5),
                     labels = function(x) s#printf("%.1f", x + 2.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = "Intervention Ratings Relative to Neutral (2.5)",
    subtitle = "Green = Above neutral, Red = Below neutral",
    x = "",
    y = "Average Rating"
  )

diverging_plot
#ggsave("ratings_diverging.png", diverging_plot, width = 10, height = 8)

#print("\nVisualization files created:")
#print("1. ratings_heatmap.png - Heatmap of ratings by language and intervention")
#print("2. ratings_lollipop.png - Overall average ratings ranked")
#print("3. ratings_grouped_bars.png - Grouped bars by language")
#print("4. ratings_radar.png - Radar chart showing profile by language")
#print("5. ratings_diverging.png - Diverging bars from neutral rating")