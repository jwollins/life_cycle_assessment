### HAU Conservation Ag Experiment
## Life Cycle Assessment
## Joe Collins 
## 2024-06-24
### 02 - Data





#________________________________________________________________________####
# PACKAGES ####
source(file = "01_packages.R")


library(tidyr)



#________________________________________________________________________####
# Data ####


# Load the data

file_path <- "sym_link_lca/data/raw_data/all_application_data.xlsx"
all_dat <- read_excel(file_path, sheet = 1)

# Reorder rows based on a factor column levels
all_dat$year <- factor(all_dat$year, levels = c("2022", "2023", "2024"))
# Reorder rows based on a factor column levels
all_dat$crop <- factor(all_dat$crop, levels = c("Spring beans", "Winter wheat", "Oilseed Rape", "Spring Barley"))
all_dat$treatment <- factor(all_dat$treatment, levels = c("Conservation", "Conventional"))






#________________________________________________________________________####
# FUNCTIONS ####

print(all_dat$category)

all_dat <- filter(all_dat, category != "Miscellaneous")


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
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Group by treatment, crop, and active ingredient
  summary <- data %>%
    group_by(treatment, crop, ai_name) %>%
    summarise(
      avg_normalized_rate_kg_ha = mean(normalized_rate_kg_ha, na.rm = TRUE),
      sum_normalized_rate_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE),
      avg_price_per_hectare = mean(price_per_hectare_based_on_normalized_g_ha, na.rm = TRUE),
      sum_price_per_hectare = sum(price_per_hectare_based_on_normalized_g_ha, na.rm = TRUE),
      category = category
    )
  
  return(summary)
}


summarize_fert_data <- function(file_path) {
  
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Group by treatment, crop, and active ingredient
  summary <- data %>%
    group_by(treatment, year, chem_element) %>%
    summarise(
      #avg_normalized_rate_kg_ha = mean(normalized_rate_kg_ha, na.rm = TRUE),
      sum_normalized_rate_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE),
      #avg_price_per_hectare = mean(price_per_hectare_based_on_normalized_g_ha, na.rm = TRUE),
      #sum_price_per_hectare = sum(price_per_hectare_based_on_normalized_g_ha, na.rm = TRUE)
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
    group_by(treatment, year) %>%
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













#________________________________________________________________________####
# CALCULATIONS ####

data <- all_dat





# ~ Formulations ####


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






# ~ normalise the rates ####

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





# ~ price per ha ####

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










#________________________________________________________________________####
# Filter data ####


# run functions
fert_data <- fert_filter(data)
spray_data <- remove_fertiliser_rows(data)



# ~ save processed data ####
write.csv(x = spray_data, file = "sym_link_lca/data/processed_data/normalized_application_data.csv")

write.csv(x = fert_data, file = "sym_link_lca/data/processed_data/normalised_fert_data.csv")

write.csv(x = data, file = "sym_link_lca/data/processed_data/normalised_spray_fert_data.csv")



# Save the updated data to a new Excel file
output_file_path <- "sym_link_lca/data/processed_data/normalized_application_data.xlsx"

write.xlsx(spray_data, output_file_path)










#________________________________________________________________________####
# GENERATE A SUMMARY TABLE ####




test <- read.csv(file = "sym_link_lca/data/processed_data/normalized_application_data.csv")





# ~ spray summary ####

# use the summarize fucntion made earlier...
summary <- summarize_data("sym_link_lca/data/processed_data/normalized_application_data.csv")

# Reorder rows based on a factor column levels
summary$crop <- factor(summary$crop, levels = c("Spring beans", "Winter wheat", "Oilseed Rape", "Spring Barley"))

summary <- summary[order(summary$crop), ]

# Round all columns to 4 decimal places
summary[] <- lapply(summary, function(x) if(is.numeric(x)) round(x, 4) else x)

# save the summary
write.csv(x = summary, file = "sym_link_lca/data/processed_data/summary_normalised_LCA_data.csv", row.names = FALSE)







# ~ fert summary ####

fert_summary <- summarize_fert_data("sym_link_lca/data/processed_data/normalised_fert_data.csv")

# Round all columns to 2 decimal places
fert_summary[] <- lapply(fert_summary, function(x) if(is.numeric(x)) round(x, 4) else x)

# save the summary
write.csv(x = fert_summary, file = "sym_link_lca/data/processed_data/fert_summary_normalised_LCA_data.csv", row.names = FALSE)

# complete missing entries


colnames(fert_summary)


# Create a complete set of combinations for treatment, year, and category
expanded_data <- expand_grid(
  treatment = unique(fert_summary$treatment),
  year = unique(fert_summary$year),
  chem_element = unique(fert_summary$chem_element)
)

# Join the complete combinations with the existing data
fert_sum_complete <- expanded_data %>%
  left_join(fert_summary, by = c("treatment", "year", "chem_element")) %>%
  mutate(
    sum_normalized_rate_kg_ha = replace_na(sum_normalized_rate_kg_ha, 0) # Replace NAs with 0
  )

# Print the updated dataset
print(fert_sum_complete)



# Calculate percentage difference between treatments for each crop
percentage_difference <- fert_sum_complete %>%
  group_by(chem_element,year) %>%
  summarise(
    Conservation = sum_normalized_rate_kg_ha[treatment == "Conservation"],
    Conventional = sum_normalized_rate_kg_ha[treatment == "Conventional"],
    Percentage_Difference = round(((Conventional - Conservation) / Conservation) * 100, digits = 2)
  )



# Create a LaTeX table
fert_table <- percentage_difference %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      caption = "My Table", 
      label = "MyLabel", 
      digits = 2) %>%
  kable_styling(
    latex_options = c("hold_position"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 10                    # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(fert_table)





## ~~ by treatment ####

# Summarise the table by treatment and category across all years
total_by_category <- percentage_difference %>%
  group_by(chem_element) %>%
  summarise(
    Conservation = sum(Conservation, na.rm = TRUE),
    Conventional = sum(Conventional, na.rm = TRUE),
    Percentage_Difference = case_when(
      Conservation == 0 & Conventional == 0 ~ NA_real_,  # No data
      Conservation == 0 ~ Inf,  # Infinite percentage difference
      TRUE ~ round(((Conventional - Conservation) / Conservation) * 100, 2)
    )
  ) %>%
  ungroup()


print(total_by_category)



# Create a LaTeX table
fert_table <- total_by_category %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      caption = "My Table", 
      label = "MyLabel", 
      digits = 2) %>%
  kable_styling(
    latex_options = c("hold_position"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 10                    # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(fert_table)










# ~ total AI summary ####

total_ai_summary <- summarize_csv("data/processed_data/normalized_application_data.csv")

# Round all columns to 2 decimal places
total_ai_summary[] <- lapply(total_ai_summary, function(x) if(is.numeric(x)) round(x, 4) else x)

# print(summary)
write_csv(x = total_ai_summary, file = "data/processed_data/AI_kg_ha_data.csv")

print(total_ai_summary)

# Create a LaTeX table
ai_table <- total_ai_summary %>%
  kbl(format = "latex", booktabs = TRUE, caption = "My Table", label = "MyLabel", digits = 2) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(ai_table)


# Calculate percentage difference between treatments for each crop
percentage_difference <- total_ai_summary %>%
  group_by(year) %>%
  summarise(
    Conservation = Total_Active_Ingredient_kg_ha[treatment == "Conservation"],
    Conventional = Total_Active_Ingredient_kg_ha[treatment == "Conventional"],
    Percentage_Difference = ((Conventional - Conservation) / Conservation) * 100
  )

# Print the results
print(percentage_difference)

# Create a LaTeX table
ai_table <- percentage_difference %>%
  kbl(format = "latex", booktabs = TRUE, caption = "My Table", label = "MyLabel", digits = 2) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(ai_table)







# ~ AI category summary ####

ai_cat_sum <- summarize_sprays(file_path = "data/processed_data/normalized_application_data.csv")
ai_cat_sum$year <- factor(ai_cat_sum$year, levels = c("2022", "2023", "2024"))
ai_cat_sum <- ai_cat_sum[order(ai_cat_sum$year), ]
# Round all columns to 2 decimal places
ai_cat_sum[] <- lapply(ai_cat_sum, function(x) if(is.numeric(x)) round(x, 4) else x)
write.csv(x = ai_cat_sum, file = "data/processed_data/AI_category_summary.csv", row.names = FALSE)

glimpse(ai_cat_sum)


# complete missing entries
library(tidyr)

# Create a complete set of combinations for treatment, year, and category
expanded_data <- expand_grid(
  treatment = unique(ai_cat_sum$treatment),
  year = unique(ai_cat_sum$year),
  category = unique(ai_cat_sum$category)
)

# Join the complete combinations with the existing data
ai_cat_sum_complete <- expanded_data %>%
  left_join(ai_cat_sum, by = c("treatment", "year", "category")) %>%
  mutate(
    Total_Active_Ingredient_kg_ha = replace_na(Total_Active_Ingredient_kg_ha, 0) # Replace NAs with 0
  )

# Print the updated dataset
print(ai_cat_sum_complete)



# Calculate percentage difference for each crop and category
percentage_difference_by_category <- ai_cat_sum_complete %>%
  group_by(category, year) %>%
  summarise(
    Conservation = Total_Active_Ingredient_kg_ha[treatment == "Conservation"],
    Conventional = Total_Active_Ingredient_kg_ha[treatment == "Conventional"],
    Percentage_Difference = round((((Conventional*1000) - (Conservation*1000)) / (Conservation*1000)) * 100, digits = 2)
  ) %>%
  ungroup()  # Ungroup to ensure no grouping remains in the output

# Print the results
print(percentage_difference_by_category)

# Create a LaTeX table
ai_table <- percentage_difference_by_category %>%
  kbl(format = "latex", booktabs = TRUE, caption = "My Table", label = "MyLabel", digits = 2) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(ai_table)




## summarise by treatment and category but not year 

# Summarise the table by treatment and category across all years
total_by_category <- percentage_difference_by_category %>%
  group_by(category) %>%
  summarise(
    Conservation = sum(Conservation, na.rm = TRUE),
    Conventional = sum(Conventional, na.rm = TRUE),
    Percentage_Difference = case_when(
      Conservation == 0 & Conventional == 0 ~ NA_real_,  # No data
      Conservation == 0 ~ Inf,  # Infinite percentage difference
      TRUE ~ round(((Conventional - Conservation) / Conservation) * 100, 2)
    )
  ) %>%
  ungroup()

# Print the summarized table
print(total_by_category)

# Create a LaTeX table
ai_table <- total_by_category %>%
  kbl(format = "latex", booktabs = TRUE, caption = "My Table", label = "MyLabel", digits = 2, ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 10                    # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(ai_table)



# ~ Fert element summary ####

fert_elem_sum <- summarize_fert_elements(file_path = "data/processed_data/normalised_fert_data.csv")
# Round all columns to 2 decimal places
fert_elem_sum[] <- lapply(fert_elem_sum, function(x) if(is.numeric(x)) round(x, 4) else x)
write.csv(x = fert_elem_sum, file = "data/processed_data/fert_elem_sum.csv", row.names = FALSE)









#________________________________________________________________####
# THESIS SUMMARY TABLES ####





# ~ Fert tables ####

fert_thesis <- fert_data[, c("date", "growth_stage", "crop", "treatment", "product", "chem_element", "normalized_rate_kg_ha")]

fert_thesis$normalized_rate_kg_ha <- round(x = fert_thesis$normalized_rate_kg_ha, 2)
#fert_thesis$rate_unit_ha <- round(x = fert_thesis$rate_unit_ha, 2)

# Modify column names: Capitalize the first letter of each word and remove underscores
colnames(fert_thesis) <- gsub("_", " ", colnames(fert_thesis))  # Replace underscores with spaces
colnames(fert_thesis) <- tools::toTitleCase(colnames(fert_thesis))  # Capitalize the first letter of each word

# Display the updated column names
print(colnames(fert_thesis))

fert_thesis_y1 <- subset(fert_thesis, Crop == "Spring beans")
fert_thesis_y2 <- subset(fert_thesis, Crop == "Winter wheat")
fert_thesis_y3 <- subset(fert_thesis, Crop == "Spring Barley" | Crop == "Oilseed Rape")



write.csv(x = fert_thesis_y1, 
          file = "~/OneDrive - Harper Adams University/Thesis/Overleaf_thesis/thesis_tables/y1_fert_table.csv", 
          row.names = FALSE)

write.csv(x = fert_thesis_y2, 
          file = "~/OneDrive - Harper Adams University/Thesis/Overleaf_thesis/thesis_tables/y2_fert_table.csv", 
          row.names = FALSE)

write.csv(x = fert_thesis_y3, 
          file = "~/OneDrive - Harper Adams University/Thesis/Overleaf_thesis/thesis_tables/y3_fert_table.csv", 
          row.names = FALSE)


## y1

# Create a LaTeX table
fert_thesis_y1 <- fert_thesis_y1 %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      caption = "My Table", 
      label = "MyLabel") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(fert_thesis_y1)

## y2

# Create a LaTeX table
fert_thesis_y2 <- fert_thesis_y2 %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      caption = "My Table", 
      label = "MyLabel") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(fert_thesis_y2)

## y3

# Create a LaTeX table
fert_thesis_y3 <- fert_thesis_y3 %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      caption = "My Table", 
      label = "MyLabel") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(fert_thesis_y3)






# ~ Ag Chem tables ####

agchem_thesis <- spray_data[, c("date", "growth_stage", "crop", "treatment", "category", "product", "active_ingredient", "normalized_rate_kg_ha")]

agchem_thesis$normalized_rate_kg_ha <- round(x = agchem_thesis$normalized_rate_kg_ha, 2)

# Modify column names: Capitalize the first letter of each word and remove underscores
colnames(agchem_thesis) <- gsub("_", " ", colnames(agchem_thesis))  # Replace underscores with spaces
colnames(agchem_thesis) <- tools::toTitleCase(colnames(agchem_thesis))  # Capitalize the first letter of each word

agchem_thesis_y1 <- subset(agchem_thesis, Crop == "Spring beans")
agchem_thesis_y2 <- subset(agchem_thesis, Crop == "Winter wheat")
agchem_thesis_y3 <- subset(agchem_thesis, Crop == "Spring Barley" | Crop == "Oilseed Rape")

agchem_thesis_y1 <- agchem_thesis_y1 %>%
  arrange(Date)

write.csv(x = agchem_thesis_y1, 
          file = "~/OneDrive - Harper Adams University/Thesis/Overleaf_thesis/thesis_tables/y1_agchem_table.csv", 
          row.names = FALSE)

agchem_thesis_y2 <- agchem_thesis_y2 %>%
  arrange(Date)

write.csv(x = agchem_thesis_y2, 
          file = "~/OneDrive - Harper Adams University/Thesis/Overleaf_thesis/thesis_tables/y2_agchem_table.csv", 
          row.names = FALSE)

agchem_thesis_y3 <- agchem_thesis_y3 %>%
  arrange(Date)

write.csv(x = agchem_thesis_y3, 
          file = "~/OneDrive - Harper Adams University/Thesis/Overleaf_thesis/thesis_tables/y3_agchem_table.csv", 
          row.names = FALSE)



## y1

# Create a LaTeX table
y1_agchem_table <- agchem_thesis_y1 %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      caption = "My Table", 
      label = "MyLabel") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(y1_agchem_table)

# Save the table to a .tex file
writeLines(y1_agchem_table, "~/OneDrive - Harper Adams University/Thesis/Overleaf_thesis/thesis_tables/y1_agchem_table.tex")

## y2

# Create a LaTeX table
y2_agchem_table <- agchem_thesis_y2 %>%
  kbl(format = "latex", booktabs = TRUE, caption = "My Table", label = "MyLabel") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(y2_agchem_table)

# Save the table to a .tex file
writeLines(y2_agchem_table, "~/OneDrive - Harper Adams University/Thesis/Overleaf_thesis/thesis_tables/y2_agchem_table.tex")


## y3

# Create a LaTeX table
y3_agchem_table <- agchem_thesis_y3 %>%
  kbl(format = "latex", booktabs = TRUE, caption = "My Table", label = "MyLabel") %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Avoid 'tabu'
    full_width = FALSE,                 # Set to FALSE for `tabular`
    font_size = 15                     # Adjust font size for readability
  ) %>%
  row_spec(0, bold = TRUE)

print(y3_agchem_table)

# Save the table to a .tex file
writeLines(y3_agchem_table, "~/OneDrive - Harper Adams University/Thesis/Overleaf_thesis/thesis_tables/y2_agchem_table.tex")

