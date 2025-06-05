# Load required libraries
library(ggplot2)
library(dplyr)

# Process adherence data
adh <- all_data |> 
  select(medication_adherence) |> 
  mutate(
    medication_adherence = 
      case_when(
        medication_adherence == "I always take them as the doctor told me" ~ "Adherent",
        TRUE ~ "Non-adherent"
      )
  )

# Calculate summary statistics
n <- nrow(all_data)
unique_countries <- length(unique(all_data$country))

# Calculate adherence summary with percentages
adherence_summary <- adh %>%
  group_by(medication_adherence) %>%
  summarise(
    n_responses = n()
  ) %>%
  mutate(
    percentage = n_responses / sum(n_responses) * 100,
    ypos = cumsum(percentage) - 0.5 * percentage
  )

# Create basic donut chart
adherence_plot <- ggplot(adherence_summary, aes(x = 2, y = percentage, fill = medication_adherence)) +
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
  scale_fill_manual(
    values = c("Adherent" = "#4CAF50", "Non-adherent" = "#FF5252"),
    name = "Adherence Status"
  ) +
  labs(
    title = paste0(round(nonadh_p, digits = 0), "% of responders
    do not take their medicines as prescribed"),
    subtitle = paste("Survey of", n, "patients from", unique_countries, "countries")
  ) #+geom_text(aes(y = ypos, label = paste0(round(percentage, 1), "%")), x = 2.3, size = 5, fontface = "bold")

# Save the plot
#ggsave("medication_adherence_donut.png", adherence_plot, width = 10, height = 8)

# #print summary statistics
#print("Medication Adherence Summary:")
#print(adherence_summary)

# Create a more detailed breakdown by language
adherence_by_language <- all_data %>%
  mutate(
    adherence_status = case_when(
      medication_adherence == "I always take them as the doctor told me" ~ "Adherent",
      TRUE ~ "Non-adherent"
    )
  ) %>%
  group_by(source_language, adherence_status) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(source_language) %>%
  mutate(
    percentage = n / sum(n) * 100,
    ypos = cumsum(percentage) - 0.5 * percentage
  )

# Create faceted donut charts by language
language_adherence_plot <- ggplot(adherence_by_language, aes(x = 2, y = percentage, fill = adherence_status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  facet_wrap(~ source_language, ncol = 2) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_fill_manual(
    values = c("Adherent" = "#4CAF50", "Non-adherent" = "#FF5252"),
    name = "Adherence Status"
  ) +
  labs(
    title = "Medication Adherence by Language",
    subtitle = "Distribution across survey languages"
  ) +
  geom_text(aes(label = paste0(round(percentage, 0), "%")), 
            position = position_stack(vjust = 0.5), size = 4)

# Save the language-specific plot
#ggsave("medication_adherence_by_language.png", language_adherence_plot, width = 10, height = 8)

# Create a detailed breakdown of all adherence patterns
detailed_adherence <- all_data %>%
  group_by(medication_adherence) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = n / sum(n) * 100,
    ypos = cumsum(percentage) - 0.5 * percentage
  ) %>%
  arrange(desc(n))

# Create pie chart for detailed adherence patterns
detailed_adherence_plot <- ggplot(detailed_adherence, aes(x = "", y = percentage, fill = medication_adherence)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  scale_fill_manual(
    values = c(
      "I always take them as the doctor told me" = "#4CAF50",
      "Sometimes I forget (less than once a week)" = "#FFC107",
      "Sometimes I forget (more than once a week)" = "#FF9800",
      "I have stopped taking them" = "#F44336"
    ),
    name = "Adherence Pattern"
  ) +
  labs(
    title = "Detailed Medication Adherence Patterns",
    subtitle = paste("All responses (n =", n, ")")
  ) +
  geom_text(aes(y = ypos, label = paste0(n, "\n(", round(percentage, 1), "%)")), 
            size = 4, fontface = "bold")

# Save the detailed plot
#ggsave("detailed_adherence_patterns.png", detailed_adherence_plot, width = 10, height = 8)

# #print detailed statistics
#print("\nDetailed Adherence Patterns:")
#print(detailed_adherence)

# Calculate adherence rate by country
adherence_by_country <- all_data %>%
  mutate(
    is_adherent = medication_adherence == "I always take them as the doctor told me"
  ) %>%
  group_by(country) %>%
  summarise(
    n_total = n(),
    n_adherent = sum(is_adherent, na.rm = TRUE),
    adherence_rate = n_adherent / n_total * 100
  ) %>%
  arrange(desc(adherence_rate))

#print("\nAdherence Rate by Country:")
#print(adherence_by_country)
