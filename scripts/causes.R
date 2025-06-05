causes <- all_data |> 
  select(medication_adherence, non_adherence_reasons) |> 
  filter(medication_adherence != "I always take them as the doctor told me")

# Load required libraries
library(tidyr)
library(stringr)

# Process the causes data to extract individual reasons
causes_expanded <- causes %>%
  # Split the comma-separated reasons into individual rows
  separate_rows(non_adherence_reasons, sep = ", ") %>%
  # Clean up any extra whitespace
  mutate(non_adherence_reasons = str_trim(non_adherence_reasons)) %>%
  # Filter out empty reasons
  filter(!is.na(non_adherence_reasons) & non_adherence_reasons != "")

# Count frequency of each reason
reason_counts <- causes_expanded %>%
  count(non_adherence_reasons, sort = TRUE) %>%
  mutate(
    percentage = n / sum(n) * 100,
    ypos = cumsum(percentage) - 0.5 * percentage,
    # Shorten long labels for display
    label_short = case_when(
      non_adherence_reasons == "They remind me that I'm ill and I don't like that" ~ "Reminder of illness",
      non_adherence_reasons == "They give me unpleasant effects" ~ "Unpleasant effects",
      non_adherence_reasons == "I don't trust these medicines" ~ "Don't trust medicines",
      non_adherence_reasons == "I don't think I need them" ~ "Don't think needed",
      TRUE ~ non_adherence_reasons
    )
  )

# Create donut chart
causes_donut <- ggplot(reason_counts, aes(x = 2, y = percentage, fill = label_short)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  scale_fill_brewer(palette = "Set3", name = "Reason") +
  labs(
    title = "Reasons for Non-Adherence",
    subtitle = "Among patients who forget medications less than once a week"
  ) +
  geom_text(aes(y = ypos, label = paste0(n, "\n(", round(percentage, 1), "%)")), 
            x = 2.3, size = 4, fontface = "bold")

# Save the plot
#ggsave("causes_donut_chart.png", causes_donut, width = 10, height = 8)

# #print summary statistics
#print("Non-Adherence Reasons Summary:")
#print(reason_counts)

# Create a more detailed visualization showing individual responses
individual_reasons <- causes %>%
  mutate(
    patient_id = row_number(),
    # Count number of reasons per patient
    n_reasons = str_count(non_adherence_reasons, ",") + 1
  ) %>%
  select(patient_id, n_reasons, non_adherence_reasons)

# Create bar chart of number of reasons per patient
reasons_per_patient <- ggplot(individual_reasons, aes(x = factor(n_reasons))) +
  geom_bar(fill = "#4CAF50", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Number of Reasons per Patient",
    x = "Number of Reasons Cited",
    y = "Number of Patients"
  ) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)

#ggsave("reasons_per_patient.png", reasons_per_patient, width = 8, height = 6)

# Create a co-occurrence matrix to see which reasons often appear together
# First, create a binary matrix of reasons
all_unique_reasons <- unique(unlist(strsplit(causes$non_adherence_reasons, ", ")))
all_unique_reasons <- all_unique_reasons[!is.na(all_unique_reasons) & all_unique_reasons != ""]

# Create binary columns for each reason
for(reason in all_unique_reasons) {
  causes[paste0("has_", make.names(reason))] <- grepl(reason, causes$non_adherence_reasons, fixed = TRUE)
}

# Calculate most common combinations
common_combinations <- causes %>%
  group_by(non_adherence_reasons) %>%
  summarise(count = n()) %>%
  filter(grepl(",", non_adherence_reasons)) %>%  # Only combinations
  arrange(desc(count))

#print("\nMost Common Reason Combinations:")
#print(common_combinations)

# Create a simplified pie chart with main categories
main_categories <- reason_counts %>%
  mutate(
    category = case_when(
      grepl("forget|Forget", non_adherence_reasons) ~ "Forgetfulness",
      grepl("trust|Trust", non_adherence_reasons) ~ "Lack of Trust",
      grepl("need|Need", non_adherence_reasons) ~ "Perceived Unnecessary",
      grepl("effect|Effect", non_adherence_reasons) ~ "Side Effects",
      grepl("remind|ill", non_adherence_reasons) ~ "Psychological Impact",
      grepl("refill|Refill", non_adherence_reasons) ~ "Access Issues",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(category) %>%
  summarise(
    total = sum(n),
    percentage = sum(percentage)
  ) %>%
  mutate(
    ypos = cumsum(percentage) - 0.5 * percentage
  )

# Create category donut chart
category_donut <- ggplot(main_categories, aes(x = 2, y = percentage, fill = category)) +
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
    values = c(
      "Forgetfulness" = "#2196F3",
      "Lack of Trust" = "#F44336",
      "Perceived Unnecessary" = "#FF9800",
      "Side Effects" = "#9C27B0",
      "Psychological Impact" = "#00BCD4",
      "Access Issues" = "#8BC34A",
      "Other" = "#607D8B"
    ),
    name = "Category"
  ) +
  labs(
    title = "Non-Adherence Reasons by Category",
    subtitle = "Grouped by underlying themes"
  ) +
  geom_text(aes(y = ypos, label = paste0(round(percentage, 1), "%")), 
            x = 2.3, size = 5, fontface = "bold")

#ggsave("category_donut_chart.png", category_donut, width = 10, height = 8)

#print("\nReason Categories Summary:")
#print(main_categories)