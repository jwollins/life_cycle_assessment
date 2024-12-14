### HAU Conservation Ag Experiment
## Life Cycle Assessment
## Joe Collins 
## 2024-06-24
### 02 - Data

setwd(dir = "~/OneDrive - Harper Adams University/Data/LCA/")

## 01 PACKAGES ####
source(file = "scripts/01_packages.R")



## 02 Data ####

# Load the data
file_path <- "data/application_data.xlsx"
all_dat <- read_excel(file_path, sheet = 1)




## Filter data ####

remove_fertiliser_rows <- function(df) {
  # Filter out rows where 'category' is 'Fertiliser'
  df_filtered <- df %>%
    filter(category != "Fertiliser")

  return(df_filtered)

}


fert_filter <- function(df) {
  # Filter out rows where 'category' is 'Fertiliser'
  fertilisers <- df %>%
    filter(category == "Fertiliser")
  
  return(fertilisers)
  
}



# run functions
data <- remove_fertiliser_rows(all_dat)
fert_data <- fert_filter(all_dat)


# Convert relevant columns to numeric, coercing non-numeric values to NA
data <- data %>%
  mutate(
    `w/w%` = as.numeric(`w/w%`),
    `w/v%` = as.numeric(`w/v%`),
    `g/l` = as.numeric(`g/l`)
  )

# Normalize the rates using vectorized operations
data <- data %>%
  mutate(
    normalized_rate_g_ha = case_when(
      forumulation == 'w/w%' ~ rate_unit_ha * (`w/w%` / 100),
      forumulation == 'w/v%' ~ rate_unit_ha * (`w/v%` / 100),
      forumulation == 'g/l'  ~ rate_unit_ha * `g/l`,
      TRUE ~ NA_real_  # Handle cases where the formulation doesn't match any known type
    )
  )



# Calculate the price per hectare
data <- data %>%
  mutate(
    price_per_hectare = (`pack_price_£` / pack_size_units) * (rate_unit_ha / quantity_purchased)
  )



# Calculate the price per hectare based on the normalized rate
data <- data %>%
  mutate(
    price_per_hectare_based_on_normalized_g_ha = (`pack_price_£` / pack_size_units) * normalized_rate_g_ha
  )


write.csv(x = data, file = "data/normalized_application_data.csv")

# Save the updated data to a new Excel file
output_file_path <- "data/normalized_application_data.xlsx"
write.xlsx(data, output_file_path)



summarize_data <- function(file_path) {
  # Load required library
  library(dplyr)
  
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Group by treatment, crop, and active ingredient
  summary <- data %>%
    group_by(treatment, crop, active_ingredient) %>%
    summarise(
      avg_normalized_rate_g_ha = mean(normalized_rate_g_ha, na.rm = TRUE),
      sum_normalized_rate_g_ha = sum(normalized_rate_g_ha, na.rm = TRUE),
      avg_price_per_hectare = mean(price_per_hectare_based_on_normalized_g_ha, na.rm = TRUE),
      sum_price_per_hectare = sum(price_per_hectare_based_on_normalized_g_ha, na.rm = TRUE)
    )
  
  return(summary)
}

# Usage
summary <- summarize_data("data/normalized_application_data.csv")


write.csv(x = summary, file = "data/summary_normalised_LCA_data.csv")










summarize_csv <- function(file_path) {
  # Read the CSV file into a DataFrame
  df <- read_csv(file_path)
  
  # Check if required columns are present
  required_columns <- c("treatment", "crop", "active_ingredient", "normalized_rate_g_ha")
  if (!all(required_columns %in% colnames(df))) {
    stop("CSV file must contain the following columns: ", paste(required_columns, collapse = ", "))
  }
  
  # Convert 'normalized_rate_g_ha' to kilograms per hectare
  df <- df %>%
    mutate(normalized_rate_kg_ha = normalized_rate_g_ha / 1000)
  
  # Group by 'treatment' and 'crop' and calculate the sum of 'normalized_rate_kg_ha'
  summary_df <- df %>%
    group_by(treatment, crop) %>%
    summarise(Total_Active_Ingredient_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')
  
  return(summary_df)
}

# Example usage
summary <- summarize_csv("data/normalized_application_data.csv")
# print(summary)


write_csv(x = summary, file = "data/AI_kg_ha_data.csv")
