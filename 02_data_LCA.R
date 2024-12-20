### HAU Conservation Ag Experiment
## Life Cycle Assessment
## Joe Collins 
## 2024-06-24
### 02 - Data

setwd(dir = "~/Documents/GitHub/lca/")

## 01 PACKAGES ####
source(file = "01_packages.R")



## 02 Data ####

setwd(dir = "~/OneDrive - Harper Adams University/Data/LCA/")

# Load the data
file_path <- "data/raw_data/application_data.xlsx"
all_dat <- read_excel(file_path, sheet = 1)





## 03 FUNCTIONS ####


# remove the fertilser rows of the df
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



summarize_data <- function(file_path) {
  # Load required library
  library(dplyr)
  
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Group by treatment, crop, and active ingredient
  summary <- data %>%
    group_by(treatment, crop, ai_name) %>%
    summarise(
      avg_normalized_rate_kg_ha = mean(normalized_rate_kg_ha, na.rm = TRUE),
      sum_normalized_rate_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE),
      avg_price_per_hectare = mean(price_per_hectare_based_on_normalized_g_ha, na.rm = TRUE),
      sum_price_per_hectare = sum(price_per_hectare_based_on_normalized_g_ha, na.rm = TRUE)
    )
  
  return(summary)
}


summarize_fert_data <- function(file_path) {
  # Load required library
  library(dplyr)
  
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Group by treatment, crop, and active ingredient
  summary <- data %>%
    group_by(treatment, crop, chem_element) %>%
    summarise(
      avg_normalized_rate_kg_ha = mean(normalized_rate_kg_ha, na.rm = TRUE),
      sum_normalized_rate_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE),
      avg_price_per_hectare = mean(price_per_hectare_based_on_normalized_g_ha, na.rm = TRUE),
      sum_price_per_hectare = sum(price_per_hectare_based_on_normalized_g_ha, na.rm = TRUE)
    )
  
  return(summary)
}




summarize_csv <- function(file_path) {
  # Read the CSV file into a DataFrame
  df <- read_csv(file_path)
  
  # Check if required columns are present
  required_columns <- c("treatment", "crop", "active_ingredient", "normalized_rate_kg_ha")
  if (!all(required_columns %in% colnames(df))) {
    stop("CSV file must contain the following columns: ", paste(required_columns, collapse = ", "))
  }
  
  # # Convert 'normalized_rate_g_ha' to kilograms per hectare
  # df <- df %>%
  #   mutate(normalized_rate_kg_ha = normalized_rate_g_ha / 1000)
  
  # Group by 'treatment' and 'crop' and calculate the sum of 'normalized_rate_kg_ha'
  summary_df <- df %>%
    group_by(treatment, crop) %>%
    summarise(Total_Active_Ingredient_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')
  
  return(summary_df)
}



summarize_sprays <- function(file_path) {
  # Read the CSV file into a DataFrame
  df <- read_csv(file_path)
  
  # Check if required columns are present
  required_columns <- c("treatment", "crop", "active_ingredient", "normalized_rate_kg_ha")
  if (!all(required_columns %in% colnames(df))) {
    stop("CSV file must contain the following columns: ", paste(required_columns, collapse = ", "))
  }
  
  # # Convert 'normalized_rate_kg_ha' to kilograms per hectare.    ***** KG / HA !!!! ****
  # df <- df %>%
  #   mutate(normalized_rate_kg_ha = normalized_rate_g_ha / 1000)
  
  # Group by 'treatment' and 'crop' and calculate the sum of 'normalized_rate_kg_ha'
  summary_df <- df %>%
    group_by(treatment, year, category) %>%
    summarise(Total_Active_Ingredient_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')
  
  return(summary_df)
}



summarize_fert_elements <- function(file_path) {
  # Read the CSV file into a DataFrame
  df <- read_csv(file_path)
  
  # Check if required columns are present
  required_columns <- c("treatment", "crop", "active_ingredient", "normalized_rate_kg_ha")
  if (!all(required_columns %in% colnames(df))) {
    stop("CSV file must contain the following columns: ", paste(required_columns, collapse = ", "))
  }
  
  # # Convert 'normalized_rate_g_ha' to kilograms per hectare
  # df <- df %>%
  #   mutate(normalized_rate_kg_ha = normalized_rate_g_ha / 1000)
  
  # Group by 'treatment' and 'crop' and calculate the sum of 'normalized_rate_kg_ha'
  summary_df <- df %>%
    group_by(treatment, year, chem_element) %>%
    summarise(Total_Active_Ingredient_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE), .groups = 'drop')
  
  return(summary_df)
}


## 04 CALCULATIONS ####

data <- all_dat


# Convert relevant columns to numeric, coercing non-numeric values to NA
suppressWarnings(data <- data %>%
                   mutate(
                     `w/w%` = as.numeric(`w/w%`),
                     `w/v%` = as.numeric(`w/v%`),
                     `g/l` = as.numeric(`g/l`)))
## IGNORE WARNINGS - NA'S ADDED. Blah blah...


#rename column by name
colnames(data)[colnames(data) == 'forumulation'] <- 'formulation'

# Correct rows with unexpected combinations based on known context
data <- data %>%
  mutate(
    # Correct 'w/w%' with unit 'l' (assuming it should be 'kg')
    unit = case_when(
      formulation == "w/w%" & unit == "l" ~ "kg",  # Assume it should be 'kg'
      formulation == "w/v%" & unit != "l" ~ "l",   # Assume it should be 'l' for 'w/v%'
      TRUE ~ unit
    ),
    # Correct 'w/w%' or 'w/v%' formulation if needed
    formulation = case_when(
      formulation == "w/w%" & unit == "l" ~ "w/v%",  # Assume it should be 'w/v%' for liquids
      TRUE ~ formulation
    )
  )



# Normalize the rates to kg/ha using vectorized operations
data <- data %>%
  mutate(
    # Normalized rate calculation
    normalized_rate_kg_ha = case_when(
      # w/w% formulation for solids (kg and g units)
      formulation == 'w/w%' & !is.na(`w/w%`) & unit %in% c("kg", "g") ~ 
        rate_unit_ha * (`w/w%` / 100) * if_else(unit == "kg", 1, 0.001),  # Convert g to kg
      
      # w/v% formulation for liquids (l unit)
      formulation == 'w/v%' & !is.na(`w/v%`) & unit == "l" ~ 
        rate_unit_ha * (`w/v%` / 100),
      
      # g/l formulation for liquids (l unit)
      formulation == 'g/l' & !is.na(`g/l`) & unit == "l" ~ 
        (rate_unit_ha * `g/l`) / 1000,  # Convert g to kg
      
      # For unexpected or missing values
      TRUE ~ NA_real_
    ),
    
    # Issue flag to identify rows with unexpected combinations
    issue_flag = case_when(
      formulation == 'w/w%' & unit == "l" ~ "Unexpected: w/w% with unit 'l'",
      formulation == 'w/v%' & unit != "l" ~ "Unexpected: w/v% with unit not 'l'",
      formulation == 'g/l' & unit != "l" ~ "Unexpected: g/l with unit not 'l'",
      is.na(formulation) | is.na(unit) | is.na(rate_unit_ha) ~ "Missing critical values",
      TRUE ~ NA_character_
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
    price_per_hectare_based_on_normalized_g_ha = (`pack_price_£` / pack_size_units) * normalized_rate_kg_ha
  )



### Filter data ####

# run functions
fert_data <- fert_filter(data)
spray_data <- remove_fertiliser_rows(data)

# save processed data sets
write.csv(x = spray_data, file = "data/processed_data/normalized_application_data.csv")
write.csv(x = fert_data, file = "data/processed_data/normalised_fert_data.csv")
write.csv(x = data, file = "data/processed_data/normalised_spray_fert_data.csv")

# Save the updated data to a new Excel file
output_file_path <- "data/processed_data/normalized_application_data.xlsx"
write.xlsx(spray_data, output_file_path)





## 05 GENERATE A SUMMARY TABLE ####


### spray summary ####
# use the summarize fucntion made earlier...
summary <- summarize_data("data/processed_data/normalized_application_data.csv")
# save the summary
write.csv(x = summary, file = "data/processed_data/summary_normalised_LCA_data.csv")

### fert summary ####
fert_summary <- summarize_fert_data("data/processed_data/normalised_fert_data.csv")
# save the summary
write.csv(x = fert_summary, file = "data/processed_data/fert_summary_normalised_LCA_data.csv")


### total AI summary ####
total_ai_summary <- summarize_csv("data/processed_data/normalized_application_data.csv")
# print(summary)
write_csv(x = total_ai_summary, file = "data/processed_data/AI_kg_ha_data.csv")

### AI category summary ####
ai_cat_sum <- summarize_sprays(file_path = "data/processed_data/normalized_application_data.csv")
write.csv(x = ai_cat_sum, file = "data/processed_data/AI_category_summary.csv")

### Fert element summary ####
fert_elem_sum <- summarize_fert_elements(file_path = "data/processed_data/normalised_fert_data.csv")
write.csv(x = fert_elem_sum, file = "data/processed_data/fert_elem_sum.csv")










