# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggwordcloud)
library(tidytext)
library(RColorBrewer)

# Process the causes data to extract individual reasons
causes_expanded <- causes %>%
  # Split the comma-separated reasons into individual rows
  separate_rows(non_adherence_reasons, sep = ", ") %>%
  # Clean up any extra whitespace
  mutate(non_adherence_reasons = str_trim(non_adherence_reasons)) %>%
  # Standardize similar reasons
  mutate(non_adherence_reasons = case_when(
    non_adherence_reasons == "Hard to refill" ~ "Hard to get refills",
    TRUE ~ non_adherence_reasons
  )) %>%
  # Filter out empty reasons
  filter(!is.na(non_adherence_reasons) & non_adherence_reasons != "")

# Create word frequency data for word cloud
word_freq <- causes_expanded %>%
  count(non_adherence_reasons, sort = TRUE) %>%
  mutate(
    # Shorten labels for better display
    reason_short = case_when(
      non_adherence_reasons == "They remind me that I'm ill and I don't like that" ~ "Illness reminder",
      non_adherence_reasons == "They give me unpleasant effects" ~ "Side effects",
      non_adherence_reasons == "I don't trust these medicines" ~ "Lack of trust",
      non_adherence_reasons == "I don't think I need them" ~ "Lack of perceived need",
      non_adherence_reasons == "They cost too much" ~ "Too expensive",
      non_adherence_reasons == "I forget" ~ "Forget",
      non_adherence_reasons == "Hard to get refills" ~ "Refill issues",
      TRUE ~ non_adherence_reasons
    )
  )

# Create basic word cloud
word_cloud_plot <- ggplot(word_freq, aes(label = reason_short, size = n, color = n)) +
  geom_text_wordcloud(
    area_corr = TRUE,
    eccentricity = 1,
    grid_margin = 3,
    seed = 42
  ) +
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "#FF9999", high = "#CC0000") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) +
  labs(
    title = "Reasons for Medication Non-Adherence",
    subtitle = "Size indicates frequency of mention"
  )

word_cloud_plot

# Save word cloud
#ggsave("word_cloud_reasons.png", word_cloud_plot, width = 10, height = 8, bg = "white")

# Create a more sophisticated packed circle visualization
library(packcircles)

# Prepare data for circle packing
packing_data <- word_freq %>%
  select(reason_short, n) %>%
  rename(label = reason_short, value = n)

# Generate packed circles layout
packing <- circleProgressiveLayout(packing_data$value, sizetype = 'area')
packing_data <- cbind(packing_data, packing)
packing_circles <- circleLayoutVertices(packing, npoints = 50)

# Create packed circles plot
packed_circles_plot <- ggplot() +
  # Draw circles
  geom_polygon(data = packing_circles, 
               aes(x, y, group = id, fill = factor(id)), 
               alpha = 1, 
               color = "white", 
               size = 2) +
  # Add text labels
  geom_text(data = packing_data, 
            aes(x, y, label = str_wrap(label, width = 10)), 
            size = 3.5, 
            fontface = "bold",
            color = "black") +
  # Add count labels
  geom_text(data = packing_data, 
            aes(x, y - radius/3, label = paste0("n = ", value)), 
            size = 3, 
            color = "black",
            fontface = "italic") +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  coord_equal() +
  labs(
    title = "Reasons for Medication Non-Adherence",
    subtitle = "Circle size represents frequency of each reason"
  )

# Save packed circles plot
packed_circles_plot
#ggsave("packed_circles_reasons.png", packed_circles_plot, width = 10, height = 8, bg = "white")

# Create a word frequency breakdown by key terms
# Extract individual words from reasons
words_data <- causes_expanded %>%
  unnest_tokens(word, non_adherence_reasons) %>%
  filter(!word %in% c("i", "they", "me", "that", "them", "think", "don't", "t")) %>%
  count(word, sort = TRUE) %>%
  filter(n > 1)  # Only words that appear more than once

# Create word frequency bubble plot
word_bubble_plot <- ggplot(words_data, aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(
    area_corr = TRUE,
    rm_outside = TRUE,
    eccentricity = 0.65,
    grid_margin = 1,
    seed = 123
  ) +
  scale_size_area(max_size = 25) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Key Words in Non-Adherence Reasons",
    subtitle = "Most frequent terms mentioned by patients"
  )

#word_bubble_plot

# Save word bubble plot
#ggsave("word_bubble_keywords.png", word_bubble_plot, width = 10, height = 8, bg = "white")