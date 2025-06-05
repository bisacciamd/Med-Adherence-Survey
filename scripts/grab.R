# Load required libraries
library(tidyverse)
library(dplyr)
library(readr)
library(googlesheets4)

# Authenticate (you may need to run this interactively the first time)
#gs4_auth()

# Update the data from Google Sheets
#source("scripts/update-data.R")

# Read the CSV files with proper column type specifications
french_data <- read_csv("data/french_data.csv", 
                       col_types = cols(.default = col_character()))
italian_data <- read_csv("data/italian_data.csv", 
                        col_types = cols(.default = col_character()))
portuguese_data <- read_csv("data/portuguese_data.csv", 
                           col_types = cols(.default = col_character()))
english_data <- read_csv("data/english_data.csv", 
                        col_types = cols(.default = col_character()))


# Define translation mappings for Question 2 (medication adherence)
q2_translations <- list(
  italian = c(
    "Le prendo sempre come mi ha detto il dottore" = "I always take them as the doctor told me",
    "A volte me le dimentico (meno di una volta a settimana)" = "Sometimes I forget (less than once a week)",
    "A volte me le dimentico (più di una volta a settimana)" = "Sometimes I forget (more than once a week)",
    "Ho smesso di prenderle" = "I have stopped taking them"
  ),
  portuguese = c(
    "Tomo sempre como o médico mandou" = "I always take them as the doctor told me",
    "Às vezes esqueço-me (menos de uma vez por semana)" = "Sometimes I forget (less than once a week)",
    "Às vezes esqueço-me (mais de uma vez por semana)" = "Sometimes I forget (more than once a week)",
    "Deixei de tomar" = "I have stopped taking them"
  ),
  french = c(
    "Je les prends toujours comme le médecin me l'a dit" = "I always take them as the doctor told me",
    "Parfois j'oublie (moins d'une fois par semaine)" = "Sometimes I forget (less than once a week)",
    "Parfois j'oublie (plus d'une fois par semaine)" = "Sometimes I forget (more than once a week)",
    "J'ai arrêté de les prendre" = "I have stopped taking them"
  )
)

# Define translation mappings for Question 3 (reasons for non-adherence)
q3_translations <- list(
  italian = c(
    "Me le dimentico" = "I forget",
    "Non mi fido di queste medicine" = "I don't trust these medicines",
    "Non penso di averne bisogno" = "I don't think I need them",
    "Penso che mi facciano peggiorare" = "I think they make me worse",
    "Mi danno effetti fastidiosi" = "They give me unpleasant effects",
    "Mi ricordano che sono malato/a e non mi piace" = "They remind me that I'm ill and I don't like that",
    "Costano troppo" = "They cost too much",
    "Altro motivo" = "Other reason"
  ),
  portuguese = c(
    "Esqueço-me" = "I forget",
    "Não confio nestes medicamentos" = "I don't trust these medicines",
    "Não acho que preciso deles" = "I don't think I need them",
    "Acho que me fazem piorar" = "I think they make me worse",
    "Dão-me efeitos desagradáveis" = "They give me unpleasant effects",
    "Lembram-me que estou doente e não gosto" = "They remind me that I'm ill and I don't like that",
    "São muito caros" = "They cost too much",
    "Outra razão" = "Other reason"
  ),
  french = c(
    "J'oublie" = "I forget",
    "Je n'ai pas confiance en ces médicaments" = "I don't trust these medicines",
    "Je ne pense pas en avoir besoin" = "I don't think I need them",
    "Je pense qu'ils me font empirer" = "I think they make me worse",
    "Ils me donnent des effets désagréables" = "They give me unpleasant effects",
    "Ils me rappellent que je suis malade et je n'aime pas ça" = "They remind me that I'm ill and I don't like that",
    "Ils coûtent trop cher" = "They cost too much",
    "Autre raison" = "Other reason"
  )
)

# Define translation mappings for Question 6 (identity impact)
q6_translations <- list(
  italian = c("Sì" = "Yes", "No" = "No"),
  portuguese = c("Sim" = "Yes", "Não" = "No"),
  french = c("Oui" = "Yes", "Non" = "No")
)

# Define rating translations (1-4 scale labels)
rating_translations <- list(
  italian = c(
    "Per niente" = "1",
    "Poco" = "2",
    "Abbastanza" = "3",
    "Moltissimo" = "4"
  ),
  portuguese = c(
    "Nada" = "1",
    "Pouco" = "2",
    "Bastante" = "3",
    "Muitíssimo" = "4"
  ),
  french = c(
    "Pas du tout" = "1",
    "Un peu" = "2",
    "Beaucoup" = "3",
    "Énormément" = "4"
  )
)

# Function to standardize rating values
standardize_ratings <- function(rating_value, language) {
  if (is.na(rating_value) || rating_value == "") return(NA)
  
  # Check if it's already a number
  if (rating_value %in% c("1", "2", "3", "4")) {
    return(as.numeric(rating_value))
  }
  
  # Try to translate from text
  if (language %in% names(rating_translations)) {
    translated <- rating_translations[[language]][rating_value]
    if (!is.na(translated)) {
      return(as.numeric(translated))
    }
  }
  
  # If translation fails, try to extract number
  num_match <- str_extract(rating_value, "[1-4]")
  if (!is.na(num_match)) {
    return(as.numeric(num_match))
  }
  
  return(NA)
}

# Function to harmonize Italian data
harmonize_italian <- function(data) {
  harmonized <- data %>%
    mutate(
      source_language = "Italian",
      timestamp = as.character(`Informazioni cronologiche`),
      country = as.character(`In quale Nazione vive?`), # Convert to character to ensure consistency
      
      # Translate medication adherence
      medication_adherence = sapply(`Come prende le sue medicine per la malattia cronica?`, function(x) {
        if (is.na(x) || x == "") return(NA)
        trans <- q2_translations$italian[x]
        if (is.na(trans)) x else trans
      }),
      
      # Translate non-adherence reasons
      non_adherence_reasons = sapply(`Se non prende le medicine regolarmente come prescritto dal dottore, quale è il motivo?`, function(x) {
        if (is.na(x) || x == "") return(NA)
        reasons <- trimws(unlist(strsplit(x, "[,;]")))
        translated <- sapply(reasons, function(r) {
          trans <- q3_translations$italian[r]
          if (is.na(trans)) r else trans
        })
        paste(translated, collapse = ", ")
      }),
      
      # Standardize ratings
      vouchers_rating = sapply(`Quanto la aiuterebbero queste cose a prendere le medicine regolarmente? [Se dichiarando di assumere le medicine, ricevesse buoni spesa]`, 
                              function(x) standardize_ratings(x, "italian")),
      fine_rating = sapply(`Quanto la aiuterebbero queste cose a prendere le medicine regolarmente? [Se dovesse pagare una multa quando le dimentica]`,
                          function(x) standardize_ratings(x, "italian")),
      support_program_rating = sapply(`Quanto la aiuterebbero queste cose a prendere le medicine regolarmente? [Se il farmacista o il dottore la iscrivesse a un programma di supporto]`,
                                     function(x) standardize_ratings(x, "italian")),
      phone_reminders_rating = sapply(`Quanto la aiuterebbero queste cose a prendere le medicine regolarmente? [Se ricevesse un promemoria sul telefono]`,
                                     function(x) standardize_ratings(x, "italian")),
      emoji_rating = sapply(`Quanto la aiuterebbero queste cose a prendere le medicine regolarmente? [Se ricevesse un pollice in su 👍🏻 o un'altra emoji sul telefono quando le prende]`,
                           function(x) standardize_ratings(x, "italian")),
      pension_collection_rating = sapply(`Quanto la aiuterebbero queste cose a prendere le medicine regolarmente? [Se potesse ritirare le medicine lo stesso giorno della pensione all'ufficio postale]`,
                                        function(x) standardize_ratings(x, "italian")),
      health_info_rating = sapply(`Quanto la aiuterebbero queste cose a prendere le medicine regolarmente? [Se ricevesse regolarmente informazioni sul perchè queste medicine sono importanti per la sua salute futura]`,
                                 function(x) standardize_ratings(x, "italian")),
      
      # Translate identity impact
      identity_impact = sapply(`La sua malattia cronica influenza come Lei si vede come persona?`, function(x) {
        if (is.na(x) || x == "") return(NA)
        trans <- q6_translations$italian[x]
        if (is.na(trans)) x else trans
      })
    ) %>%
    select(source_language, timestamp, country, medication_adherence, non_adherence_reasons,
           vouchers_rating, fine_rating, support_program_rating,
           phone_reminders_rating, emoji_rating, pension_collection_rating,
           health_info_rating, identity_impact)
  
  return(harmonized)
}

# Function to harmonize Portuguese data
harmonize_portuguese <- function(data) {
  harmonized <- data %>%
    mutate(
      source_language = "Portuguese",
      timestamp = as.character(`Informazioni cronologiche`),
      country = as.character(`Em que país vive?`), # Convert to character to ensure consistency
      
      # Translate medication adherence
      medication_adherence = sapply(`Como toma os seus medicamentos para a sua doença crónica?`, function(x) {
        if (is.na(x) || x == "") return(NA)
        trans <- q2_translations$portuguese[x]
        if (is.na(trans)) x else trans
      }),
      
      # Translate non-adherence reasons
      non_adherence_reasons = sapply(`Se não toma os medicamentos regularmente como receitado pelo médico, qual é a razão?`, function(x) {
        if (is.na(x) || x == "") return(NA)
        reasons <- trimws(unlist(strsplit(x, "[,;]")))
        translated <- sapply(reasons, function(r) {
          trans <- q3_translations$portuguese[r]
          if (is.na(trans)) r else trans
        })
        paste(translated, collapse = ", ")
      }),
      
      # Standardize ratings
      vouchers_rating = sapply(`Para cada uma das seguintes propostas, diga a que ponto estas o(a) poderiam ajudar a tomar os medicamentos regularmente? [Se recebesse vales de compras por tomá-los]`,
                              function(x) standardize_ratings(x, "portuguese")),
      fine_rating = sapply(`Para cada uma das seguintes propostas, diga a que ponto estas o(a) poderiam ajudar a tomar os medicamentos regularmente? [Se tivesse de pagar uma multa quando não os tomasse]`,
                          function(x) standardize_ratings(x, "portuguese")),
      support_program_rating = sapply(`Para cada uma das seguintes propostas, diga a que ponto estas o(a) poderiam ajudar a tomar os medicamentos regularmente? [Se o farmacêutico ou médico o(a) inscrevesse num programa de apoio (adesão à medicação)]`,
                                     function(x) standardize_ratings(x, "portuguese")),
      phone_reminders_rating = sapply(`Para cada uma das seguintes propostas, diga a que ponto estas o(a) poderiam ajudar a tomar os medicamentos regularmente? [Se recebesse lembretes no telemóvel]`,
                                     function(x) standardize_ratings(x, "portuguese")),
      emoji_rating = sapply(`Para cada uma das seguintes propostas, diga a que ponto estas o(a) poderiam ajudar a tomar os medicamentos regularmente? [Se recebesse um polegar para cima digital 👍 ou outro emoji quando os toma]`,
                           function(x) standardize_ratings(x, "portuguese")),
      pension_collection_rating = sapply(`Para cada uma das seguintes propostas, diga a que ponto estas o(a) poderiam ajudar a tomar os medicamentos regularmente? [Se pudesse levantar os medicamentos no mesmo dia que a sua reforma nos correios]`,
                                        function(x) standardize_ratings(x, "portuguese")),
      health_info_rating = sapply(`Para cada uma das seguintes propostas, diga a que ponto estas o(a) poderiam ajudar a tomar os medicamentos regularmente? [Se recebesse regularmente informações sobre porque estes medicamentos são importantes para a sua saúde futura]`,
                                 function(x) standardize_ratings(x, "portuguese")),
      
      # Translate identity impact
      identity_impact = sapply(`A sua doença crónica influencia como se vê como pessoa, i.e. a sua identidade?`, function(x) {
        if (is.na(x) || x == "") return(NA)
        trans <- q6_translations$portuguese[x]
        if (is.na(trans)) x else trans
      })
    ) %>%
    select(source_language, timestamp, country, medication_adherence, non_adherence_reasons,
           vouchers_rating, fine_rating, support_program_rating,
           phone_reminders_rating, emoji_rating, pension_collection_rating,
           health_info_rating, identity_impact)
  
  return(harmonized)
}

# Function to harmonize French data
harmonize_french <- function(data) {
  # First, let's check what columns we actually have
  #print("French data columns:")
  #print(names(data))
  
  # Get column names by position to avoid encoding issues
  col_names <- names(data)
  
  # Note: French data doesn't have pension collection rating
  harmonized <- data %>%
    mutate(
      source_language = "French",
      timestamp = as.character(.[[1]]), # Informazioni cronologiche
      country = as.character(.[[2]]), # Dans quel pays vivez-vous ?
      
      # Translate medication adherence
      medication_adherence = sapply(.[[3]], function(x) { # Comment prenez-vous vos médicaments...
        if (is.na(x) || x == "") return(NA)
        trans <- q2_translations$french[x]
        if (is.na(trans)) x else trans
      }),
      
      # Translate non-adherence reasons
      non_adherence_reasons = sapply(.[[4]], function(x) { # Si vous ne prenez pas vos médicaments...
        if (is.na(x) || x == "") return(NA)
        reasons <- trimws(unlist(strsplit(x, "[,;]")))
        translated <- sapply(reasons, function(r) {
          trans <- q3_translations$french[r]
          if (is.na(trans)) r else trans
        })
        paste(translated, collapse = ", ")
      }),
      
      # Standardize ratings using column positions
      vouchers_rating = sapply(.[[6]], function(x) standardize_ratings(x, "french")),
      fine_rating = sapply(.[[7]], function(x) standardize_ratings(x, "french")),
      support_program_rating = sapply(.[[8]], function(x) standardize_ratings(x, "french")),
      phone_reminders_rating = sapply(.[[9]], function(x) standardize_ratings(x, "french")),
      emoji_rating = sapply(.[[10]], function(x) standardize_ratings(x, "french")),
      pension_collection_rating = NA, # Not in French survey
      health_info_rating = sapply(.[[11]], function(x) standardize_ratings(x, "french")),
      
      # Translate identity impact
      identity_impact = sapply(.[[12]], function(x) { # Votre maladie chronique...
        if (is.na(x) || x == "") return(NA)
        trans <- q6_translations$french[x]
        if (is.na(trans)) x else trans
      })
    ) %>%
    select(source_language, timestamp, country, medication_adherence, non_adherence_reasons,
           vouchers_rating, fine_rating, support_program_rating,
           phone_reminders_rating, emoji_rating, pension_collection_rating,
           health_info_rating, identity_impact)
  
  return(harmonized)
}

# Function to harmonize English data
harmonize_english <- function(data) {
  # First, let's check what columns we actually have
  #print("English data columns:")
  #print(names(data))
  
  # English data has 12 columns:
  # 1. Timestamp
  # 2. Country
  # 3. Medication adherence
  # 4. Non-adherence reasons
  # 5. Motivators
  # 6-11. Rating questions (6 items)
  # 12. Identity impact
  
  harmonized <- data %>%
    mutate(
      source_language = "English",
      timestamp = as.character(.[[1]]), # Convert to character to avoid type issues
      country = as.character(.[[2]]), # Second column is country
      
      # Medication adherence - standardize to match other languages
      medication_adherence = sapply(.[[3]], function(x) {
        if (is.na(x) || x == "") return(NA)
        # Standardize English responses to match translated versions
        case_when(
          grepl("always take them as prescribed", x, ignore.case = TRUE) ~ "I always take them as the doctor told me",
          grepl("Sometimes I forget.*less than once a week", x) ~ "Sometimes I forget (less than once a week)",
          grepl("Sometimes I forget.*more than once a week", x) ~ "Sometimes I forget (more than once a week)",
          grepl("stopped taking", x, ignore.case = TRUE) ~ "I have stopped taking them",
          TRUE ~ x
        )
      }),
      
      # Non-adherence reasons
      non_adherence_reasons = .[[4]],
      
      # Skip motivators (column 5) and standardize ratings starting from column 6
      vouchers_rating = sapply(.[[6]], function(x) {
        if (is.na(x) || x == "") return(NA)
        # Convert text ratings to numbers if needed
        rating_val <- case_when(
          x == "Not at all" ~ 1,
          x == "A little" ~ 2,
          x == "Quite a bit" ~ 3,
          x == "Very much" ~ 4,
          x == "Well" ~ 3,  # Assuming "Well" maps to 3
          TRUE ~ as.numeric(x)
        )
        return(rating_val)
      }),
      fine_rating = sapply(.[[7]], function(x) {
        if (is.na(x) || x == "") return(NA)
        rating_val <- case_when(
          x == "Not at all" ~ 1,
          x == "A little" ~ 2,
          x == "Quite a bit" ~ 3,
          x == "Very much" ~ 4,
          x == "Well" ~ 3,
          TRUE ~ as.numeric(x)
        )
        return(rating_val)
      }),
      support_program_rating = sapply(.[[8]], function(x) {
        if (is.na(x) || x == "") return(NA)
        rating_val <- case_when(
          x == "Not at all" ~ 1,
          x == "A little" ~ 2,
          x == "Quite a bit" ~ 3,
          x == "Very much" ~ 4,
          x == "Well" ~ 3,
          TRUE ~ as.numeric(x)
        )
        return(rating_val)
      }),
      phone_reminders_rating = sapply(.[[9]], function(x) {
        if (is.na(x) || x == "") return(NA)
        rating_val <- case_when(
          x == "Not at all" ~ 1,
          x == "A little" ~ 2,
          x == "Quite a bit" ~ 3,
          x == "Very much" ~ 4,
          x == "Well" ~ 3,
          TRUE ~ as.numeric(x)
        )
        return(rating_val)
      }),
      emoji_rating = sapply(.[[10]], function(x) {
        if (is.na(x) || x == "") return(NA)
        rating_val <- case_when(
          x == "Not at all" ~ 1,
          x == "A little" ~ 2,
          x == "Quite a bit" ~ 3,
          x == "Very much" ~ 4,
          x == "Well" ~ 3,
          TRUE ~ as.numeric(x)
        )
        return(rating_val)
      }),
      pension_collection_rating = NA, # English survey doesn't have this question
      health_info_rating = sapply(.[[11]], function(x) {
        if (is.na(x) || x == "") return(NA)
        rating_val <- case_when(
          x == "Not at all" ~ 1,
          x == "A little" ~ 2,
          x == "Quite a bit" ~ 3,
          x == "Very much" ~ 4,
          x == "Well" ~ 3,
          TRUE ~ as.numeric(x)
        )
        return(rating_val)
      }),
      
      # Identity impact - column 12
      identity_impact = .[[12]]
    ) %>%
    select(source_language, timestamp, country, medication_adherence, non_adherence_reasons,
           vouchers_rating, fine_rating, support_program_rating,
           phone_reminders_rating, emoji_rating, pension_collection_rating,
           health_info_rating, identity_impact)
  
  return(harmonized)
}

# Harmonize all datasets
english_harmonized <- harmonize_english(english_data)
italian_harmonized <- harmonize_italian(italian_data)
portuguese_harmonized <- harmonize_portuguese(portuguese_data)
french_harmonized <- harmonize_french(french_data)

# Combine all harmonized data
all_data <- bind_rows(
  italian_harmonized,
  portuguese_harmonized,
  french_harmonized,
  english_harmonized
)

# Create summary statistics
summary_stats <- all_data %>%
  group_by(source_language) %>%
  summarise(
    n_responses = n(),
    adherent_pct = mean(medication_adherence == "I always take them as the doctor told me", na.rm = TRUE) * 100,
    sometimes_forget_less_pct = mean(medication_adherence == "Sometimes I forget (less than once a week)", na.rm = TRUE) * 100,
    sometimes_forget_more_pct = mean(medication_adherence == "Sometimes I forget (more than once a week)", na.rm = TRUE) * 100,
    stopped_pct = mean(medication_adherence == "I have stopped taking them", na.rm = TRUE) * 100,
    identity_impact_pct = mean(identity_impact == "Yes", na.rm = TRUE) * 100,
    avg_vouchers = mean(vouchers_rating, na.rm = TRUE),
    avg_fine = mean(fine_rating, na.rm = TRUE),
    avg_support = mean(support_program_rating, na.rm = TRUE),
    avg_reminders = mean(phone_reminders_rating, na.rm = TRUE),
    avg_emoji = mean(emoji_rating, na.rm = TRUE),
    avg_pension = mean(pension_collection_rating, na.rm = TRUE),
    avg_info = mean(health_info_rating, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# #print summary
#print("Survey Response Summary by Language:")
#print(summary_stats)

# Save harmonized data
write_csv(all_data, "harmonized_survey_data.csv")

# Create a function to analyze non-adherence reasons
analyze_reasons <- function(data) {
  # Filter out NA and empty responses
  valid_reasons <- data$non_adherence_reasons[!is.na(data$non_adherence_reasons) & data$non_adherence_reasons != ""]
  
  # Extract all reasons
  all_reasons <- unlist(strsplit(valid_reasons, ", "))
  all_reasons <- all_reasons[all_reasons != ""]
  
  if (length(all_reasons) == 0) {
    return(data.frame(reason = character(), count = numeric(), percentage = numeric()))
  }
  
  # Count frequency
  reason_freq <- table(all_reasons)
  reason_df <- data.frame(
    reason = names(reason_freq),
    count = as.numeric(reason_freq),
    percentage = as.numeric(reason_freq) / sum(reason_freq) * 100
  ) %>%
    arrange(desc(count))
  
  return(reason_df)
}

# Analyze reasons across all languages
#print("\nMost Common Non-Adherence Reasons (All Languages):")
all_reasons_analysis <- analyze_reasons(all_data)
#print(all_reasons_analysis)

# Analyze reasons by language
#print("\nNon-Adherence Reasons by Language:")
for (lang in unique(all_data$source_language)) {
  cat("\n", lang, ":\n", sep = "")
  lang_data <- all_data %>% filter(source_language == lang)
  lang_reasons <- analyze_reasons(lang_data)
  if (nrow(lang_reasons) > 0) {
    #print(lang_reasons)
  } else {
    cat("No non-adherence reasons reported\n")
  }
}

# Create visualization of intervention preferences
library(ggplot2)

# Reshape data for plotting
intervention_data <- all_data %>%
  select(source_language, vouchers_rating:health_info_rating) %>%
  pivot_longer(
    cols = vouchers_rating:health_info_rating,
    names_to = "intervention",
    values_to = "rating"
  ) %>%
  mutate(
    intervention = case_when(
      intervention == "vouchers_rating" ~ "Vouchers",
      intervention == "fine_rating" ~ "Fine",
      intervention == "support_program_rating" ~ "Support Program",
      intervention == "phone_reminders_rating" ~ "Phone Reminders",
      intervention == "emoji_rating" ~ "Emoji Feedback",
      intervention == "pension_collection_rating" ~ "Pension Collection",
      intervention == "health_info_rating" ~ "Health Information"
    )
  ) %>%
  filter(!is.na(rating))

# Create boxplot
intervention_plot <- ggplot(intervention_data, aes(x = intervention, y = rating, fill = source_language)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(position = position_dodge(width = 0.75), alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = 1:4, limits = c(0.5, 4.5)) +
  labs(
    title = "Intervention Preferences by Language",
    x = "Intervention Type",
    y = "Rating (1-4)",
    fill = "Language"
  )

# Save plot
#ggsave("plots/intervention_preferences.png", intervention_plot, width = 10, height = 6)

# Create a summary table of average ratings
rating_summary <- all_data %>%
  group_by(source_language) %>%
  summarise(
    Vouchers = mean(vouchers_rating, na.rm = TRUE),
    Fine = mean(fine_rating, na.rm = TRUE),
    `Support Program` = mean(support_program_rating, na.rm = TRUE),
    `Phone Reminders` = mean(phone_reminders_rating, na.rm = TRUE),
    `Emoji Feedback` = mean(emoji_rating, na.rm = TRUE),
    `Pension Collection` = mean(pension_collection_rating, na.rm = TRUE),
    `Health Information` = mean(health_info_rating, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

#print("\nAverage Intervention Ratings by Language:")
#print(rating_summary)

# Export individual language summaries
write_csv(english_harmonized, "english_harmonized.csv")
write_csv(italian_harmonized, "italian_harmonized.csv")
write_csv(portuguese_harmonized, "portuguese_harmonized.csv")
write_csv(french_harmonized, "french_harmonized.csv")

#print("\nData harmonization complete! Files saved:")
#print("- harmonized_survey_data.csv (all languages combined)")
#print("- english_harmonized.csv")
#print("- italian_harmonized.csv")
#print("- portuguese_harmonized.csv")
#print("- french_harmonized.csv")
#print("- intervention_preferences.png")