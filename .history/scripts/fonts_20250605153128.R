# Install if needed
install.packages("showtext")

library(showtext)
library(ggplot2)

# Add emoji font support
font_add(family = "EmojiOne", regular = "EmojiOneColor-SVGinOT.ttf")  # if you have this font
# OR use system fonts
font_add("Arial Unicode MS", "Arial Unicode MS")  # Windows
# font_add("Apple Color Emoji", "Apple Color Emoji.ttc")  # macOS

# Enable showtext
showtext_auto()

# Your plotting code with emojis
summary_stats$country <- c("🇫🇷", "🇮🇹", "🇵🇹", "🇷🇴", "🇹🇭", "🇬🇧")