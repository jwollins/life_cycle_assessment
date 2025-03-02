# stats 

setwd(rstudioapi::getActiveProject())

getwd()


#______________________________________________####
# PACKAGES ####
source(file = "01_packages.R")



#______________________________________________####
# Functions ####

source(file = "../phd_tools/fun_distribution_plots.R")
source(file = "../phd_tools/fun_glm_diagnostic_plots.R")



#______________________________________________####
# Data ####

# ~ Load the data ####

dat <- read.csv(file = "sym_link_lca/data/processed_data/normalised_spray_fert_data.csv")

# ~ set factors ####

# Reorder rows based on a factor column levels
dat$year <- factor(dat$year, levels = c("2022", "2023", "2024"))
# Reorder rows based on a factor column levels
dat$crop <- factor(dat$crop, levels = c("Spring beans", "Winter wheat", "Oilseed Rape", "Spring Barley"))
dat$treatment <- factor(dat$treatment, levels = c("Conventional", "Conservation"))



# ~ filter data ####


## ~~ spray data ####

unique(dat$category)

dat_herbicide_summary <- 
  filter(.data = dat, dat$category == "Herbicide") %>%
  group_by(year, treatment) %>%
  summarise(total_herbicide_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE),
            .groups = "drop")


dat_fungicide_summary <- 
  filter(.data = dat, dat$category == "Fungicide") %>%
  group_by(year, treatment) %>%
  summarise(total_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE),
            .groups = "drop")




## ~~ fert data ####



unique(dat$chem_element)

dat_N_summary <- 
  filter(.data = dat, dat$chem_element == "N") %>%
  group_by(year, treatment) %>%
  summarise(total_kg_ha = sum(normalized_rate_kg_ha, na.rm = TRUE),
            .groups = "drop")



#______________________________________________####
# Stats ####


# ~ Herbicides ####

distribution_plots(data = dat_herbicide_summary, 
                   variable = dat_herbicide_summary$total_herbicide_kg_ha, 
                   colour = dat_herbicide_summary$total_herbicide_kg_ha)


ggsave(filename = "sym_link_lca/plots/distributions/dist_herbicides.png", 
        width = 10, height = 2.25)


kruskal.test(total_herbicide_kg_ha ~ treatment, data = dat_herbicide_summary)

 







# ~ Fungicides ####

distribution_plots(data = dat_fungicide_summary, 
                   variable = dat_fungicide_summary$total_kg_ha, 
                   colour = dat_fungicide_summary$total_kg_ha)


ggsave(filename = "sym_link_lca/plots/distributions/dist_fungicide.png", 
       width = 10, height = 2.25)








#__________________####

# ~ N ####

distribution_plots(data = dat_N_summary, 
                   variable = dat_N_summary$total_kg_ha, 
                   colour = dat_N_summary$total_kg_ha)


ggsave(filename = "sym_link_lca/plots/distributions/dist_N.png", 
       width = 10, height = 2.25)



kruskal.test(total_kg_ha ~ treatment, data = dat_N_summary)







