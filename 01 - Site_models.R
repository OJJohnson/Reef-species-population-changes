# ------- O1 - Site Models ------ #


# 00 - Load packages ------------------------------------------------------

library(tidyverse)
library(lme4)

# 01 - Load the data ------------------------------------------------------

algae_data <- read_csv("inputs/algae_data.csv", show_col_types = FALSE)
algae_data <- algae_data %>% mutate(category = "algae")
algae_data$lat_long <- paste0(algae_data$latitude, "_", algae_data$longitude)

fish_data <- read_csv("inputs/fish_data.csv", show_col_types = FALSE)
fish_data <- fish_data %>% mutate(category = "fish")
fish_data$lat_long <- paste0(fish_data$latitude, "_", fish_data$longitude)

inverts_data <- read_csv("inputs/inverts_data.csv", show_col_types = FALSE)
inverts_data <- inverts_data %>% mutate(category = "invertebrates")
inverts_data$lat_long <- paste0(inverts_data$latitude, "_", inverts_data$longitude)

coral_data <- read_csv("inputs/coral_data.csv", show_col_types = FALSE)
coral_data <- coral_data %>% mutate(category = "coral")
coral_data$lat_long <- paste0(coral_data$latitude, "_", coral_data$longitude)

species_table <- read_csv("inputs/species_info.csv")
# 02 - Species Selection --------------------------------------------------

algae_endemic_declining <- algae_data %>% 
  filter(species_name %in% c("Phyllotricha decipiens", 
                             "Euptilota articulata", 
                             "Melanthalia obtusata", 
                             "Durvillaea potatorum", 
                             "Lessonia corrugata", 
                             "Camontagnea oxyclada", 
                             "Perithalia caudata", 
                             "Lenormandia marginata", 
                             "Cystophora moniliformis", 
                             "Caulpera trifaria", 
                             "Carpoglossum confluens", 
                             "Phacelocarpus peperocarpos", 
                             "Amphiroa anceps", 
                             "Phyllospora comosa"))

invertebrate_endemic_declining <- inverts_data %>%
  filter(species_name %in% c("Phasianella ventricosa", 
                             "Ptilometra australis", 
                             "Campanile symbolicum", 
                             "Goniocidaris impressa", 
                             "Aphelodoris varia", 
                             "Goniocidaris impressa", 
                             "Heliocidaris erythrogramma", 
                             "Hypselodoris bennetti", 
                             "Turbo cepoides", 
                             "Phyllacanthus irregularis", 
                             "Tosia australis", 
                             "Nectria ocellata", 
                             "Ceratosoma brevicaudatum", 
                             "Haliotis rubra"))




fish_endemic_declining <- fish_data %>%
  filter(species_name %in% c("Choerodon cauteroma", 
                             "Orectolobus halei", 
                             "Centropogon australis", 
                             "Enneapterygius howensis", 
                             "Cochleoceps orientalis", 
                             "Parapercis ramsayi", 
                             "Trygonorrhina fasciata", 
                             "Pseudolabrus guentheri", 
                             "Acanthistius ocellatus", 
                             "Anoplocapros inermis", 
                             "Parapercis haackei", 
                             "Parma mccullochi", 
                             "Parma victoriae", 
                             "Dotalabrus aurantiacus", 
                             "Austrolabrus maculatus"))


# 03 - Model and plot fish -----------------------------------------------------


# Fit LM to each location

fish_results <- data.frame()

unique_combinations <- unique(dplyr:: select(fish_endemic_declining, species_name, lat_long))


# Loop through each combination
for(i in 1:nrow(unique_combinations)) {
  
  # Extract current combination
  current_combination <- unique_combinations[i, ]
  current_species <- current_combination$species_name
  current_location <- current_combination$lat_long
  
  # Filter data for the current combination
  filtered_data <- filter(fish_data, species_name == current_species, lat_long == current_location)
  
  # Fit linear model
  lm_model <- lm(log_count ~ survey_year, data = filtered_data)
  
  # Diagnostic plots
  png(filename = paste0("figures/species_site_plots/fish/diagnostic_plots/", current_species, "_", current_location, "_diagnostic_plots.png"))
  par(mfrow = c(2, 2)) # Set up a 2x2 grid of plots
  
  # Residuals vs Fitted
  plot(lm_model, which = 1)
  
  # Normal QQ plot
  plot(lm_model, which = 2)
  
  # Scale-Location plot
  plot(lm_model, which = 3)
  
  # Residuals vs Leverage
  plot(lm_model, which = 5)
  
  dev.off() # Close the PNG file
  
  # Extract relevant information from the linear model
  model_summary <- summary(lm_model)
  coefficients <- coef(model_summary)
  p_values <- coef(summary(lm_model))[, "Pr(>|t|)"]  # p-values
  
  # Add results to fish_results table
  results_row <- c(current_species, current_location, coefficients, p_values)
  fish_results <- rbind(fish_results, results_row)
}

# Naming columns of fish_results table
colnames(fish_results) <- c("species", "lat_long", "intercept", "slope", "intercept_p_value", "slope_p_value")


# make plots for each of the models

species_list <- unique(fish_endemic_declining$species_name)

for(species in species_list) {
  
  species_data <- fish_data[fish_data$species_name == species, ]
  
  p <- species_plot <- ggplot(species_data, aes(x = survey_year, y = log_count)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm") +
    facet_wrap(latitude ~ longitude) +
    ggtitle(species)
  
  print(p)
  
  ggsave(filename = paste0("figures/species_site_plots/fish/plotted_lm/plot_", species, ".png"), plot = p)
  
}



# 04 - Model and plot Algae -------------------------------------------------------------------

# Fit LM to each location

algae_results <- data.frame()

unique_combinations <- unique(dplyr:: select(algae_endemic_declining, species_name, lat_long))

# Loop through each combination
for(i in 1:nrow(unique_combinations)) {
  
  # Extract current combination
  current_combination <- unique_combinations[i, ]
  current_species <- current_combination$species_name
  current_location <- current_combination$lat_long
  
  # Filter data for the current combination
  filtered_data <- filter(algae_data, species_name == current_species, lat_long == current_location)
  
  # Fit linear model
  lm_model <- lm(sqrt_cover ~ survey_year, data = filtered_data)
  
  # Extract relevant information from the linear model
  model_summary <- summary(lm_model)
  coefficients <- coef(model_summary)
  p_values <- coef(summary(lm_model))[, "Pr(>|t|)"]  # p-values
  
  # Add results to fish_results table
  results_row <- c(current_species, current_location, coefficients, p_values)
  algae_results <- rbind(algae_results, results_row)
}

# Naming columns of fish_results table
colnames(algae_results) <- c("species", "lat_long", "intercept", "slope", "intercept_p_value", "slope_p_value")


# make plots

species_list <- unique(algae_endemic_declining$species_name)

for(species in species_list) {
  
  species_data <- algae_data[algae_data$species_name == species, ]
  
  p <- species_plot <- ggplot(species_data, aes(x = survey_year, y = sqrt_cover)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm") +
    facet_wrap(latitude ~ longitude) +
    ggtitle(species)
  
  print(p)
  
  ggsave(filename = paste0("figures/species_site_plots/algae/plot_", species, ".png"), plot = p)
  
}


# 05 - model and plot Invertebrates -------------------------------------------------------------------------

# Fit LM to each location

invert_results <- data.frame()

unique_combinations <- unique(dplyr:: select(invertebrate_endemic_declining, species_name, lat_long))


# Loop through each combination
for(i in 1:nrow(unique_combinations)) {
  
  # Extract current combination
  current_combination <- unique_combinations[i, ]
  current_species <- current_combination$species_name
  current_location <- current_combination$lat_long
  
  # Filter data for the current combination
  filtered_data <- filter(inverts_data, species_name == current_species, lat_long == current_location)
  
  # Fit linear model
  lm_model <- lm(log_count ~ survey_year, data = filtered_data)
  
  # Extract relevant information from the linear model
  model_summary <- summary(lm_model)
  coefficients <- coef(model_summary)
  p_values <- coef(summary(lm_model))[, "Pr(>|t|)"]  # p-values
  
  # Add results to fish_results table
  results_row <- c(current_species, current_location, coefficients, p_values)
  invert_results <- rbind(invert_results, results_row)
}

# Naming columns of fish_results table
colnames(invert_results) <- c("species", "lat_long", "intercept", "slope", "intercept_p_value", "slope_p_value")


species_list <- unique(invertebrate_endemic_declining$species_name)

for(species in species_list) {
  
  species_data <- inverts_data[inverts_data$species_name == species, ]
  
  p <- species_plot <- ggplot(inverts_data, aes(x = survey_year, y = log_count)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm") +
    facet_wrap(latitude ~ longitude) +
    ggtitle(species)
  
  print(p)
  
  ggsave(filename = paste0("figures/species_site_plots/invertebrates/plot_", species, ".png"), plot = p)
  
}










# All model results


all_model_results <- rbind(fish_results, algae_results, invert_results)
all_model_results <- all_model_results[, 1:6]

stable_or_increasing_sites <- all_model_results %>%
  filter(slope >= 0)

stable_or_increasing_sites[c("latitude", "longitude")] <- str_split_fixed(stable_or_increasing_sites$lat_long, "_", 2)  

hope_spot_count <- stable_or_increasing_sites %>%
  group_by(lat_long) %>%
  dplyr:: summarise(species_count = n_distinct(species),
                    latitude = latitude, 
                    longitude = longitude)

hope_spot_count$longitude <- as.numeric(hope_spot_count$longitude)
hope_spot_count$latitude <- as.numeric(hope_spot_count$latitude)
hope_spot_count$latitude <- abs(hope_spot_count$latitude)

ggplot(hope_spot_count) +
  geom_tile(aes(x = longitude, y = latitude, fill = species_count)) +
  scale_fill_manual(low = "lightblue", high = "darkgreen") +
  scale_y_reverse()


