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
      non_adherence_reasons == "I don't think I need them" ~ "Don't need",
      non_adherence_reasons == "They cost too much" ~ "Too expensive",
      non_adherence_reasons == "I forget" ~ "Forget",
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

# Save word cloud
ggsave("word_cloud_reasons.png", word_cloud_plot, width = 10, height = 8, bg = "white")

# Create a bubble chart alternative
bubble_data <- word_freq %>%
  mutate(
    # Create x and y positions for bubbles
    x = row_number() %% 3,
    y = ceiling(row_number() / 3),
    # Add some randomness for natural look
    x = x + runif(n(), -0.3, 0.3),
    y = y + runif(n(), -0.3, 0.3)
  )

# Create bubble plot
bubble_plot <- ggplot(bubble_data, aes(x = x, y = y)) +
  geom_point(aes(size = n, color = n), alpha = 0.6) +
  geom_text(aes(label = reason_short), 
            size = 3.5, 
            fontface = "bold",
            check_overlap = FALSE) +
  scale_size(range = c(10, 30), guide = "none") +
  scale_color_gradient(low = "#FFB6C1", high = "#8B0000", guide = "none") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    title = "Medication Non-Adherence Reasons",
    subtitle = paste("Bubble size represents frequency (n =", sum(word_freq$n), "total mentions)")
  ) +
  coord_fixed()

# Save bubble plot
ggsave("bubble_plot_reasons.png", bubble_plot, width = 10, height = 8, bg = "white")

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
               alpha = 0.6, 
               color = "white", 
               size = 2) +
  # Add text labels
  geom_text(data = packing_data, 
            aes(x, y, label = str_wrap(label, width = 10)), 
            size = 3.5, 
            fontface = "bold",
            color = "white") +
  # Add count labels
  geom_text(data = packing_data, 
            aes(x, y - radius/3, label = paste0("n=", value)), 
            size = 3, 
            color = "white",
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
ggsave("packed_circles_reasons.png", packed_circles_plot, width = 10, height = 8, bg = "white")

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

# Save word bubble plot
ggsave("word_bubble_keywords.png", word_bubble_plot, width = 10, height = 8, bg = "white")

# Print summary
cat("Word bubble visualizations created:\n")
cat("1. word_cloud_reasons.png - Traditional word cloud\n")
cat("2. bubble_plot_reasons.png - Bubble chart with reasons\n")
cat("3. packed_circles_reasons.png - Packed circles visualization\n")
cat("4. word_bubble_keywords.png - Individual word frequency cloud\n")