# Google Sheets URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1KWSuKv1y2AqXUU07o4elJclJeay7IJskL_yV7AGGydc/"

# Read all four language sheets
english_data <- read_sheet(sheet_url, sheet = "English")
italian_data <- read_sheet(sheet_url, sheet = "Italian")
portuguese_data <- read_sheet(sheet_url, sheet = "Portuguese")
french_data <- read_sheet(sheet_url, sheet = "French")

write.csv(english_data, "data/english_data.csv", row.names = FALSE)
write.csv(italian_data, "data/italian_data.csv", row.names = FALSE)
write.csv(portuguese_data, "data/portuguese_data.csv", row.names = FALSE)
write.csv(french_data, "data/french_data.csv", row.names = FALSE)