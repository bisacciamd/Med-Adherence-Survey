source("https://raw.githubusercontent.com/bisacciamd/functions/refs/heads/main/themes.R")

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Read the intervention data
intervention_data <- read_csv("data/intervention_data.csv")
n <- nrow(all_data)
unique_countries <- length(unique(all_data$country))

# Create a donut chart for overall intervention preferences
# First, calculate average ratings by intervention type
intervention_summary <- intervention_data %>%
  group_by(intervention) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),
    n_responses = n()
  ) %>%
  arrange(desc(avg_rating)) %>%
  mutate(
    percentage = avg_rating / sum(avg_rating) * 100,
    ypos = cumsum(percentage) - 0.5 * percentage
  )

# Create basic donut chart
intervention_plot <- ggplot(intervention_summary, aes(x = 2, y = percentage, fill = intervention)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Set3", name = "Intervention Type") +
  labs(
    title = "Intervention Preferences Distribution",
    subtitle = paste("Survey of", n, "patients from", unique_countries, "countries")
  ) +
  geom_text(aes(y = ypos, label = paste0(round(percentage, 1), "%")), 
            x = 2.3, size = 3)

# Save the plot
#ggsave("intervention_donut_chart.png", donut_plot, width = 5, height = 5)

# Create a donut chart by language
# Calculate average ratings by intervention and language
intervention_by_language <- intervention_data %>%
  group_by(source_language, intervention) %>%
  summarise(
    avg_rating = mean(rating, na.rm = TRUE),
    .groups = "drop"
  )

# Create faceted donut charts for each language
language_donut <- intervention_by_language %>%
  group_by(source_language) %>%
  mutate(
    percentage = avg_rating / sum(avg_rating) * 100,
    ypos = cumsum(percentage) - 0.5 * percentage
  ) %>%
  ggplot(aes(x = 2, y = percentage, fill = intervention)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  facet_wrap(~ source_language, ncol = 3) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set3", name = "Intervention Type") +
  labs(
    title = "Intervention Preferences by Language",
    subtitle = "Distribution of Average Ratings"
  )

# Save the language-specific plot
#ggsave("intervention_donut_by_language.png", language_donut, width = 12, height = 6)

# Create a more detailed visualization with actual rating distributions
rating_distribution <- intervention_data %>%
  count(intervention, rating) %>%
  group_by(intervention) %>%
  mutate(percentage = n / sum(n) * 100)

# Create a stacked donut chart showing rating distributions
stacked_donut <- rating_distribution %>%
  mutate(rating_factor = factor(rating, levels = 1:4)) %>%
  ggplot(aes(x = 2, y = n, fill = rating_factor)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  facet_wrap(~ intervention, ncol = 4) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  scale_fill_manual(
    values = c("1" = "#FF6B6B", "2" = "#FFA06B", "3" = "#FFD93D", "4" = "#6BCF7F"),
    labels = c("1 (Not at all)", "2 (A little)", "3 (Quite)", "4 (Very much)"),
    name = "Rating"
  ) +
  labs(
    title = "Rating Distribution for Each Intervention",
    subtitle = "Proportion of Each Rating Score"
  )

# Save the stacked donut chart
#ggsave("intervention_rating_distribution.png", stacked_donut, width = 12, height = 8)

# Create summary statistics
summary_stats <- intervention_data %>%
  group_by(intervention) %>%
  summarise(
    avg_rating = round(mean(rating, na.rm = TRUE), 2),
    median_rating = median(rating, na.rm = TRUE),
    sd_rating = round(sd(rating, na.rm = TRUE), 2),
    n_responses = n(),
    rating_1 = sum(rating == 1, na.rm = TRUE),
    rating_2 = sum(rating == 2, na.rm = TRUE),
    rating_3 = sum(rating == 3, na.rm = TRUE),
    rating_4 = sum(rating == 4, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_rating))

print("Intervention Summary Statistics:")
print(summary_stats)

# Calculate percentages for each rating level
rating_percentages <- intervention_data %>%
  count(intervention, rating) %>%
  group_by(intervention) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  pivot_wider(
    names_from = rating,
    values_from = percentage,
    values_fill = 0,
    names_prefix = "rating_"
  )

print("\nPercentage of Each Rating by Intervention:")
print(rating_percentages)

# Create a simple pie chart for the most preferred interventions (rating 4)
top_rated <- intervention_data %>%
  filter(rating == 4) %>%
  count(intervention) %>%
  mutate(percentage = n / sum(n) * 100)

top_rated_pie <- ggplot(top_rated, aes(x = "", y = percentage, fill = intervention)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Set2", name = "Intervention") +
  labs(
    title = "Distribution of Highest Ratings (4 - Very Much)",
    subtitle = "Which interventions received the most '4' ratings?"
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))

# Save the top-rated pie chart
#ggsave("top_rated_interventions_pie.png", top_rated_pie, width = 10, height = 8)

print("\nAll visualizations have been saved:")
print("1. intervention_donut_chart.png - Overall intervention preferences")
print("2. intervention_donut_by_language.png - Preferences by language")
print("3. intervention_rating_distribution.png - Rating distributions for each intervention")
print("4. top_rated_interventions_pie.png - Distribution of highest ratings")

