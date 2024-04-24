#Data and cleaning

# Read in raw data
#The raw data is in a wide format and is publicly available to download from https://portal.aodn.org.au/ 
# Raw in-situ (ATRC) Algae data
algae_raw <- read_csv("ep_m3.csv", 
                      show_col_types = FALSE)

# RLS/ATRC Method 1
fish_raw <- 
  read_csv("ep_m1_AUS.csv", 
           show_col_types = FALSE) %>% 
  mutate(survey_date = as.Date(survey_date, format = "%d/%m/%Y"))

# RLS/ATRC Method 2 (fish only)
cryptic_fish_raw <- 
  read_csv("ep_m2_cryptic_fish_AUS.csv", 
           show_col_types = FALSE) %>% 
  mutate(survey_date = as.Date(survey_date, format = "%Y-%m-%d"))

# RLS/ATRC Method 2 (invertebrates only)
inverts_raw <- read_csv("ep_m2_inverts_AUS.csv", 
                        show_col_types = FALSE)

coral_raw <- read_csv("PQ_FullRes_infilled.csv", 
                      show_col_types = FALSE)


# Covariate data
# species-level information
species_tbl <- 
  read_csv(file = "species_info.csv", 
           show_col_types = FALSE, 
           skip_empty_rows = TRUE) 

# temperature time-series (post-2008) data for each site
temperature_raw <- 
  read_csv(file =  "temperature_timeseries.csv",
           show_col_types = FALSE, 
           skip_empty_rows = TRUE)

# endemism dataframe
endemism <- 
  read_csv(file =  "endemism.csv",
           show_col_types = FALSE, 
           skip_empty_rows = TRUE)


# Data Cleaning
## Algae
# Here, I will use the 30 years of ATRC data which has been entered as
# NRNM "M3" data and will filter this data to extract just the "algal"
# phylum only (i.e. remove substrate type and other sessile invertebrate
#              species). This also requires amalgamating data for species that 
# have had taxonomic name changes over time, and importantly, species that were 
# not morphologically distinct enough to be consistently recognised by all
# in-situ data records were additionally removed from the analysis.
# 
# Temperate algae data is scored in a point-count method via a quadrat
# with 50 points - I will convert this data so that it is on a percentage
# cover (multiply totals by 2, to convert to % cover).

### Filtering species
#Algae filtering
sum(is.na(algae_raw$total))
# There are 28 examples where there is a "0" value for the raw algae data

# Total number of species being considered (n = 801):
num_species <- 
  algae_raw %>%
  distinct(species_name) %>%
  nrow()

# filter (and keep) out phylum containing algae only, taxonomic name changes, and species that were not morphologically distinctive enough to be consistently recognised by all in-situ data records (verified list by G. Edgar)
algae_rmspp <- 
  algae_raw %>% 
  filter(phylum %in% c("algae", 
                       "Heterokontophyta",
                       "Rhodophyta",
                       "Chlorophyta",
                       "Tracheophyta",
                       "Ochrophyta"
  )) %>% 
  filter(str_detect(species_name, "[A-Z]{1}[a-z]+\\s{1}[a-z]+$")) %>% 
  filter(!str_detect(species_name, "Unidentified")) %>% 
  filter(species_name %!in% c("Ritterella pedunculata",
                              "Herdmania grandis",
                              "Anthothoe albocincta",
                              "Ostrea angasi",
                              "Phlyctenanthus australis",
                              "Pyura australis",
                              "Mytilus galloprovincialis",
                              "Bare rock",
                              "Turf/sand/sediment matrix",
                              "Zonaria turneriana/angustata",
                              "Cladophora prolifera",
                              "Sargassum podacanthum",
                              "Codium galeatum",
                              "Bornetia binderiana",
                              "Holotrichia comosa",
                              "Sargassum distichum",
                              "Dictyota naevosa",
                              "Mesophyllum incisum",
                              "Hymenena curdieana",
                              "Acrosorium ciliolatum",
                              "Platoma cyclocolpum",
                              "Gracilaria secundata",
                              "Dictyomenia sonderi",
                              "Peltasta australis",
                              "Homoeostrichus sinclairii",
                              "Dictyota fastigiata",
                              "Grateloupia filicina",
                              "Phacelocarpus alatus",
                              "Callophyllis lambertii",
                              "Dictyota paniculata",
                              "Acrotylus australis",
                              "Jania pulchella",
                              "Delisea hypneoides",
                              "Gelinaria ulvoidea",
                              "Carpopeltis phyllophora",
                              "Cephalocystis furcellata",
                              "Wrangelia nobilis",
                              "Ptilota hannafordii",
                              "Rhodymenia verrucosa",
                              "Laurencia filiformis",
                              "Scinaia tsinglanensis",
                              "Rhodymenia leptophylla",
                              "Distromium multifidum",
                              "Codium fragile",
                              "Pterocladiella capillacea",
                              "Protokuetzingia australasica",
                              "Hypnea pannosa", 
                              "Grateloupia turuturu", 
                              "Taonia australasica", 
                              "Rhodophyllis multipartita", 
                              "Phymatolithon masonianum", 
                              "Coelarthrum opuntia", 
                              "Delisea elegans",
                              "Hymenena affinis", 
                              "Griffithsia monilis", 
                              "Rhodymenia obtusa", 
                              "Halymenia plana",
                              "Tsengia feredayae", 
                              "Champia stipitata", 
                              "Ptilonia australasica",
                              "Jania sagittata",
                              "Kallymenia tasmanica",
                              "Pseudobryopsis hainanensis", 
                              "Mychodea carnosa", 
                              "Craspedocarpus venosus", 
                              "Craspedocarpus tenuifolius", 
                              "Rhodymenia wilsonis",
                              "Nizymenia australis", 
                              "Codium duthieae", 
                              "Nizymenia furcata", 
                              "Dictyota gunniana", 
                              "Plocamium costatum", 
                              "Chondria incrassata", 
                              "Gracilaria preissiana", 
                              "Exallosorus olsenii",
                              "Curdiea angustata",
                              "Chlanidophora microphylla", 
                              "Amansia rhodantha", 
                              "Gelidium asperum", 
                              "Cladostephus spongiosus",
                              "Rhodopeltis australis", 
                              "Phacelocarpus sessilis",
                              "Rhodopeltis borealis",
                              "Laurencia majuscula",
                              "Sargassum paradoxum",
                              "Sporochnus radiciformis",
                              "Coelarthrum cliftonii", 
                              "Jania rosea",
                              "Erythroclonium sonderi", 
                              "Nizymenia conferta", 
                              "Gelidium australe", 
                              "Tylotus obtusatus", 
                              "Codium harveyi", 
                              "Sonderophycus coriaceus",
                              "Myriodesma integrifolium", 
                              "Acrosorium minus", 
                              "Champia zostericola",
                              "Callophycus serratus",
                              "Acanthophora dendroides",
                              "Rhodymenia prolificans", 
                              "Hymenocladia usnea", 
                              "Zonaria diesingiana", 
                              "Halopeltis australis", 
                              "Stypopodium flabelliforme", 
                              "Polysiphonia blandii",
                              "Zonaria turneriana", 
                              "Dictyota nigricans", 
                              "Jania micrarthrodia",
                              "Plocamium cirrhosum", 
                              "Rhodymenia sonderi", 
                              "Mychodea aciculare", 
                              "Phacelocarpus apodus", 
                              "Peyssonnelia novaehollandiae",
                              "Laurencia clavata", 
                              "Myriogramme gunniana",
                              "Melanthalia abscissa", 
                              "Polyopes constrictus",
                              "Distromium flabellatum",
                              "Carpopeltis elata",
                              "Callophycus laxus",
                              "Euptilocladia spongiosa",
                              "Sargassum spinuligerum", 
                              "Cladosiphon filum", 
                              "Dasya extensa", 
                              "Corallina officinalis", 
                              "Encyothalia cliftonii", 
                              "Dicranema revolutum", 
                              "Dictyopteris plagiogramma", 
                              "Nitospinosa tasmanica", 
                              "Echinothamnion mallardiae",
                              "Sargassum oligocystum", 
                              "Sargassum lacerifolium", 
                              "Erythroclonium muelleri", 
                              "Amphiroa gracilis", 
                              "Myriodesma tuberosum", 
                              "Sargassum linearifolium", 
                              "Heterosiphonia muelleri", 
                              "Sargassum ligulatum",
                              "Zostera nigricaulis")) %>% 
  mutate(survey_year = year(survey_date)) %>% 
  # ensure species that have had a taxonomic name change over time have the data points merged together (these species highlighted by GE)
  mutate(species_name = case_when(
    species_name == "Caulerpa annulata" ~ "Caulerpa hodgkinsoniae",
    species_name == "Sargassum decipiens" ~ "Phyllotricha varians",
    species_name == "Sargassum varians" ~ "Phyllotricha decipiens",
    species_name == "Sargassum verruculosum" ~ "Phyllotricha verruculosa", 
    species_name == "Sargassum heteromorphum" ~ "Sargassopsis heteromorphum", 
    TRUE ~ species_name
  )) %>% 
  select(
    survey_id,
    site_code,
    location,
    area,
    ecoregion,
    survey_year,
    phylum,
    species_name,
    quadrat,
    habitat_groups,
    total,
    taxon,
    latitude,
    longitude) %>% 
  as_tibble()  


algae_lat_lons <-
  algae_raw %>% 
  drop_na(latitude, longitude) %>% 
  summarise(latitude = mean(latitude), 
            longitude = mean(longitude), 
            .by = site_code)

```

### Cleaning counts
algae_clean <- 
  algae_rmspp %>% 
  filter(total <= 50) %>% # keep single observations less than or equal to 50
  filter(survey_year >= 2008) %>% #NEW - filter out data from 2008 onwards
  add_count(survey_id, quadrat, wt = total) %>% 
  mutate(cover = total*2) %>% 
  summarise(cover = mean(cover), #mean quadrat summary for survey
            .by = c(species_name, 
                    site_code,
                    location,
                    ecoregion,
                    survey_year)) 

```

### Dealing with NAs
# As per methods in Edgar et al. 2023, We want NA values to be changed to zeros 
# when all of the following conditions are met:
#   
#   1.  The site was surveyed that year.
# 2.  The species was observed at least once at that site in any other
# year.
# 3.  The species was recorded at that site at that year.
# 
# To do this we need to make a list of all the site\*year combinations,
# all site\*species combinations, and all site\*year\*taxon combinations
# **that have at least one observation**. If the row in question has a
# site\*year combination that is found in the overall site\*year
# combination list, then we know that site was surveyed in that year and
# therefore meets the first condition. We repeat for the first three
# conditions, if all three are met and the count is NA, we replace with a
# zero value.

#Algae NA convert
# site*year combinations
algae_site_byyear <-
  algae_clean |> 
  filter(!(is.na(cover)|cover == 0)) |> # not recorded
  select(site_code, survey_year) |> 
  distinct() |> 
  mutate(site_code_yr = paste(site_code, survey_year, sep = "_"))

# site*year*species combinations
algae_site_byspp <-
  algae_clean |> 
  filter(!(is.na(cover)|cover == 0)) |> # not recorded
  select(site_code, species_name) |> 
  distinct() |> 
  mutate(site_code_spp = paste(site_code, species_name, sep = "_"))

algae_site_byspp_nested <- 
  algae_site_byspp %>% 
  select(site_code, species_name) %>% 
  nest(.by = site_code)

# Chaetomorpha billardierii is in algae_site_byyear but not algae_clean

algae_data_addingzeros <- 
  algae_site_byyear %>% 
  select(site_code, survey_year) %>% 
  left_join(algae_site_byspp_nested, 
            by = join_by(site_code)) %>% 
  unnest(cols = c(data)) %>% 
  left_join(algae_clean %>% 
              select(site_code, 
                     survey_year, 
                     species_name, 
                     cover),
            by = join_by(site_code, survey_year, species_name)) %>% 
  left_join(algae_lat_lons, by = join_by(site_code)) %>% 
  mutate(cover = replace_na(cover, 0)) %>%
  mutate(sqrt_cover = sqrt(cover))

#| include = FALSE
algae_data_addingzeros |> filter(is.na(sqrt_cover)) |> nrow()


algae_site_species_keep <- 
  algae_data_addingzeros %>% 
  filter(sqrt_cover > 0) %>%
  add_count(site_code, species_name, name = "n_site_species") %>% 
  filter(n_site_species > 3) %>% 
  mutate(site_spp = paste(site_code, species_name, sep = "_")) %>% 
  pull(site_spp) %>% 
  unique()

# this will be used as the main dataframe
algae_data <- 
  algae_data_addingzeros |> 
  mutate(latitude  = round(latitude),
         longitude = round(longitude)) |> 
  mutate(transect_id = paste(site_code, survey_year, sep = "_")) %>% 
  mutate(n_sites_byspp = n(), .by = c(site_code, species_name)) %>% 
  mutate(n_obs_byspp = n(), .by = c(species_name)) %>% 
  filter(n_obs_byspp > n_sites_byspp) %>% 
  # site*species combinations with three or more obs
  add_count(site_code, species_name, name = "n_site_species") %>% 
  filter(n_site_species > 3) %>%
  filter(paste(site_code, species_name, sep = "_") %in% algae_site_species_keep) %>%
  # species must be see in more than one site
  filter(n_distinct(site_code) > 1, 
         .by = species_name) 


# check to see if filtering has worked
algae_clean %>% 
  count(site_code, species_name, 
        name = "n_obs") %>% 
  arrange(n_obs)

# check to see if filtering has worked
algae_clean %>% 
  select(site_code, species_name) %>% 
  distinct() %>% 
  count(species_name, name = "n_sites") %>% 
  arrange(n_sites)


## Fish
# Here, I will load in all fish data, this includes M1 data and all
# "cryptic fish" scored in M2 that has been separated into their own data
# set (both ATRC and RLS). To ensure fish counts Australia wide, and so
# there is not a concentration on the temperate region from the ATRC
# surveys from 1992, I will filter all the data so it is only from the
# inception of RLS, therefore from 2008 on wards.
# 
# As M1 fish and M2 fish are scored on a different scale/different search
# efforts, I will ensure all counts for fish are (reduced for M1) measured
# on a 50m2 scale. Additionally, ANY and ALL fish sighted on M1 surveys
# are recorded, so to ensure cryptic fish counts made on a M1 survey are
# counted towards the trend of the species, M1 and cryptic fish datasets
# will be combined, scaled to 50m2 (also to take into account the two
#                                   blocks per survey), filtered from 2008 onwards,
# in a site\*year combination (due to the case there may be multiple surveys at one site
# during a year). To prevent inflation of certain species, the fish data will be 
# logged before any modelling occurs.

# combine raw fish (M1) and cryptic fish (M2) datasets
fish_combined <- 
  fish_raw %>% 
  bind_rows(cryptic_fish_raw) %>% 
  mutate(site_yr = paste(site_code, year(survey_date), sep = "_"), 
         survey_year = year(survey_date))

### Cleaning counts
# Only method 1 and post-2008
fish_corrected_data <- 
  fish_combined %>%
  mutate(total_per50m2 = case_when(method == 1 ~ total/5, 
                                   TRUE ~ total)) %>% 
  filter(survey_year >= 2008) 


# Sum the total for each species within each unique sitexyear and block
fish_count_byblock <- 
  fish_corrected_data %>%
  mutate(site_yr = paste(site_code, year(survey_date), sep = "_")) %>%
  summarise(total_per50m2 = sum(total_per50m2), 
            .by = c(block, 
                    survey_id,
                    site_yr, 
                    site_code, 
                    survey_year, 
                    species_name))

# Calculate the average of the total per site x date combination
fish_average_data <- 
  fish_count_byblock %>%
  summarise(total_per50m2 = mean(total_per50m2), 
            .by = c(site_yr, 
                    survey_id, 
                    site_code, 
                    survey_year,
                    species_name)) 

# remove incorrectly named species
fish_filtered <- 
  fish_average_data %>% 
  filter(str_detect(species_name, "[A-Z]{1}[a-z]+\\s{1}[a-z]+$")) %>% 
  filter(!str_detect(species_name, "Unidentified")) %>% 
  filter(!str_detect(species_name, "\\[|\\]")) 

# create a dataframe "survey_list" which contains the other data which may be 
# some lat, lon are very slightly different (e.g. survey_id == 923400036)
fish_survey_list <- 
  fish_combined %>% 
  select(site_yr, 
         site_code, 
         survey_id,
         survey_year, 
         latitude, 
         longitude) %>% 
  distinct() %>% 
  summarise(latitude = mean(latitude),
            longitude = mean(longitude),
            .by = c(site_yr, 
                    site_code, 
                    survey_id,
                    survey_year))

fish_clean <-
  fish_filtered %>% 
  mutate(n_sites_byspp = n(), .by = c(site_code, species_name)) %>% 
  mutate(n_obs_byspp = n(), .by = c(species_name)) %>% 
  filter(n_obs_byspp > n_sites_byspp) %>% 
  left_join(fish_survey_list, 
            by = join_by(site_yr, survey_id,
                         site_code, 
                         survey_year)) %>% 
  summarise(total_per50m2 = mean(total_per50m2),
            .by = c(site_yr, species_name,
                    site_code, 
                    survey_year))

#Four lined cardinal name change from Apogon doederleini to Ostorhinchus doederleini - amend to current WoRMS listing:
fish_clean$species_name[fish_clean$species_name == "Apogon doederleini"] <- "Ostorhinchus doederleini" 

### Dealing with NAs
#Fish NA convert
# A list of all the site*year combinations
fish_site_byyear <-
  fish_clean |> 
  filter(!(is.na(total_per50m2)|total_per50m2 == 0)) |> # not recorded
  select(site_yr,
         site_code, 
         survey_year) |> 
  distinct() 

# A list of all the site*year*species combinations
fish_site_byspp <-
  fish_corrected_data |> 
  filter(!(is.na(total_per50m2)|total_per50m2 == 0)) |> # not recorded
  select(site_yr, species_name,
         site_code, 
         survey_year) |> 
  distinct() 

fish_site_byspp_nested <- 
  fish_site_byspp %>% 
  select(site_yr, species_name,
         site_code, 
         survey_year) %>% 
  nest(.by = c(site_yr, 
               site_code, 
               survey_year))

fish_data_addingzeros <- 
  fish_site_byyear %>% 
  select(site_yr) %>% 
  left_join(fish_site_byspp_nested, 
            by = join_by(site_yr)) %>% 
  unnest(cols = c(data)) %>% 
  left_join(fish_clean %>% 
              select(species_name,
                     site_yr,
                     site_code, 
                     survey_year,
                     total_per50m2)) %>% 
  mutate(total_per50m2 = replace_na(total_per50m2, 0)) %>%
  mutate(total_per50m2 = as.numeric(total_per50m2)) 


fish_data_addingzeros |> filter(is.na(total_per50m2)) |> nrow()

################ MinN attempt ##############
# Calculate the minimum density per species per site
min_density <- fish_data_addingzeros %>%
  filter(total_per50m2 > 0) %>%
  group_by(site_code, species_name) %>%
  summarise(minN = min(total_per50m2)) %>%
  ungroup()

# Join the min_density table to the main species count table
fish_data_with_minN <- fish_data_addingzeros %>%
  left_join(min_density, by = c("site_code", "species_name"))

# Continue with the rest of your existing code
fish_site_species_keep <- 
  fish_data_with_minN %>% 
  filter(total_per50m2 > 0) %>% 
  add_count(site_code, species_name, name = "n_site_species") %>% 
  filter(n_site_species > 3) %>% 
  mutate(site_spp = paste(site_code, species_name, sep = "_")) %>% 
  pull(site_spp) %>% 
  unique()

fish_data <- 
  fish_data_with_minN %>% 
  left_join(fish_survey_list %>% 
              summarise(latitude = mean(latitude), 
                        longitude = mean(longitude), 
                        .by = site_yr), 
            by = join_by(site_yr)) %>% 
  mutate(latitude = round(latitude),
         longitude = round(longitude), 
         log_count = log10(total_per50m2 + minN/2)) %>%  # Use minN for log transformation
  mutate(year = substr(site_yr, nchar(site_yr) - 3, nchar(site_yr)) %>% 
           as.numeric(),
         site_code = substr(site_yr, 1, nchar(site_yr) - 5)) %>% 
  add_count(site_code, species_name, name = "n_site_species") %>% 
  filter(n_site_species > 3) %>% 
  filter(paste(site_code, species_name, sep = "_") %in% fish_site_species_keep) %>%
  # species must be seen in more than one site
  filter(n_distinct(site_code) > 1, 
         .by = species_name)

#Total number of species being considered after filtering: 1669
num_species_fish <- 
  fish_data %>%
  distinct(species_name) %>%
  nrow()
print(num_species_fish)


#Check to see if filtering has worked
table(fish_data$site_code) %>% 
  sort() %>% 
  head()

## Invertebrates
# Here, I will load in all invertebrate data, both ATRC and RLS combined
# datasets. Invertebrates are surveyed on the 50m2 scale. The same as
# above with fish, to ensure there is not a concentration on the temperate
# region from the ATRC surveys from 1992, I will filter all M2
# invertebrate data so it is only from the inception of RLS, therefore
# from 2008 onwards.
# 
# As a check to make sure there are no cryptic fish that have made there
# way into this dataset, I will do a filter to ensure there are no
# observations with the phylum "Chordata" in the dataset, and the data is
# representative of observations to the species level. To prevent
# inflation of certain species, the invertebrate data will be logged
# before any modelling occurs.


##
sum(is.na(inverts_raw$total))

# Total number of M2 inverts being considered: 1373
invert_num_species <- 
  inverts_raw %>%
  distinct(species_name) %>%
  nrow()
print(invert_num_species)

#first let's check what "Phylum" groups are contained in the "invertebrate raw" dataframe
inverts_raw %>% 
  filter(phylum == "Chordata") %>% 
  nrow()
# data should have ZERO Chordates - 2252 observations found, filter out below

#filter (and keep) inverts columns
inverts_clean <- 
  inverts_raw %>% 
  filter(str_detect(species_name, "[A-Z]{1}[a-z]+\\s{1}[a-z]+$")) %>% 
  filter(!str_detect(species_name, "Unidentified")) %>% 
  mutate(survey_year = year(survey_date)) %>% 
  filter(!str_detect(species_name, "\\[|\\]")) %>% 
  filter(phylum %!in% c("Chordata")) %>%
  filter(species_name %!in% c("Asthenosoma varium",
                              "Equichlamys bifrons",
                              "Oxycomanthus bennetti",
                              "Cronia avellana",
                              "Neothyonidium magnum",
                              "Mimachlamys asperrima")) %>%
  dplyr::select(
    survey_id,
    site_code,
    location,
    area,
    ecoregion,
    survey_year,
    phylum,
    species_name,
    size_class,
    total,
    taxon,
    latitude,
    longitude,
    method,
    block) %>% 
  as_tibble() 

#Seastar species with two spelling versions of species name - amend to current WoRMS listing:
inverts_clean$species_name[inverts_clean$species_name == "Pentagonaster dubeni"] <- "Pentagonaster duebeni" 

#Update to P. chabrus name according to WoRMS
inverts_clean <- inverts_clean %>%
  mutate(species_name = ifelse(species_name == "Plagusia chabrus", "Guinusia chabrus", species_name))

inverts_clean <- inverts_clean %>%
  mutate(taxon = ifelse(taxon == "Plagusia chabrus", "Guinusia chabrus", taxon))

#Comanthus to Cenolia
inverts_clean <- inverts_clean %>%
  mutate(species_name = ifelse(species_name == "Comanthus tasmaniae", "Cenolia tasmaniae", species_name))

inverts_clean <- inverts_clean %>%
  mutate(taxon = ifelse(taxon == "Comanthus tasmaniae", "Cenolia tasmaniae", taxon))

#Pterynotus to Pterochelus at genus level
inverts_clean <- inverts_clean %>%
  mutate(species_name = ifelse(species_name == "Pterynotus triformis", "Pterochelus triformis", species_name))

inverts_clean <- inverts_clean %>%
  mutate(taxon = ifelse(taxon == "Pterynotus triformis", "Pterochelus triformis", taxon))


# #Inconsistent records of Goniocidaris tubaria - make all records "Goniocidaris impressa"
# inverts_clean$species_name[inverts_clean$species_name == "Goniocidaris tubaria"] <- "Goniocidaris impressa" 

#Total number of M2 species being considered after filtering: 1050
inverts_num_species_2 <- 
  inverts_clean %>%
  distinct(species_name) %>%
  nrow()
print(inverts_num_species_2)

#view list of M2 species
inverts_species_list <- 
  inverts_clean %>% 
  distinct(species_name) %>% 
  pull(species_name)

#Inverts addition
inverts_list <- 
  inverts_clean %>% 
  select(site_code,
         location,
         ecoregion,
         survey_year,
         phylum,
         species_name,
         taxon,
         latitude,
         longitude) %>% 
  distinct()


site_list_inverts <-
  inverts_clean %>% 
  select(site_code,
         location,
         ecoregion,
         latitude,
         longitude) %>% 
  distinct()


# Sum the total for each species within each unique survey_id
summed_data_inverts <- 
  inverts_clean %>%
  summarize(total_sum = sum(total), 
            .by = c(survey_id, species_name, survey_year, site_code))

# Calculate the average of the total per site x date combination
average_data_inverts <- summed_data_inverts %>%
  summarise(average_total = mean(total_sum), 
            .by = c(site_code, survey_year, survey_id, species_name))


#Resolving the Goniocidaris impressa and Goniocidaris tubaria issue
filtered_data <- inverts_clean %>%
  filter(species_name %in% c("Goniocidaris impressa", "Goniocidaris tubaria"))

# write_csv(filtered_data, "filtered_inverts_data.csv")

#Once clean read back in and add to existing data
filtered_inverts <- read_csv("filtered_inverts_data.csv", show_col_types = FALSE)

# Filter the species you want to replace
species_to_replace <- c("Goniocidaris impressa", "Goniocidaris tubaria")

# Filter the rows in inverts_clean that match the species to be replaced
inverts_clean <- inverts_clean %>%
  filter(!species_name %in% species_to_replace)

# Add the new filtered data from filtered_inverts to inverts_clean
inverts_clean <- bind_rows(inverts_clean, filtered_inverts)


### Filtering years
#filter NA's out of dataset M2
dat_no_na_inverts <-
  average_data_inverts |>
  drop_na(average_total)

#Filter out all data prior to 2008
#filter NA's out of dataset M1
inverts_clean <-
  average_data_inverts %>%
  filter(as.numeric(survey_year) >= 2008) %>% 
  mutate(site_yr = paste(site_code, survey_year, sep = "_")) %>% 
  summarise(total_per50m2 = mean(average_total),
            .by = c(site_code, survey_year, site_yr, species_name))
#view(inverts_clean)

inverts_clean |> 
  pull(survey_year) |> 
  range() #confirmed data range is 2008 - 2022

### Dealing with NAs
# A list of all the site*year combinations
inverts_site_byyear <-
  inverts_clean |> 
  filter(!(is.na(total_per50m2)|total_per50m2 == 0)) |> # not recorded
  select(site_yr, site_code, survey_year) |> 
  distinct() 


# A list of all the site*year*species combinations
inverts_site_byspp <-
  inverts_clean |> 
  filter(!(is.na(total_per50m2)|total_per50m2 == 0)) |> # not recorded
  select(site_yr, site_code, species_name, survey_year) |> 
  distinct()

inverts_site_byspp_nested <- 
  inverts_site_byspp %>% 
  select(site_yr, site_code, species_name, survey_year) %>% 
  nest(.by = c(site_yr, site_code, survey_year))

inverts_data_addingzeros <-
  inverts_site_byyear %>%
  select(site_yr) %>%
  left_join(inverts_site_byspp_nested) %>%
  unnest(cols = c(data)) %>%
  left_join(inverts_clean %>%
              select(species_name,
                     site_yr,
                     site_code,
                     survey_year,
                     total_per50m2)) %>%
  mutate(total_per50m2 = replace_na(total_per50m2, 0)) %>%
  mutate(total_per50m2 = as.numeric(total_per50m2))


#| include = FALSE: INVERTS
inverts_data_addingzeros |> filter(is.na(total_per50m2)) |> nrow()

################ MinN attempt ##############
# Calculate the minimum density per species per site
min_density_inverts <- inverts_data_addingzeros %>%
  filter(total_per50m2 > 0) %>%
  group_by(site_code, species_name) %>%
  summarise(minN = min(total_per50m2)) %>%
  ungroup()

# Join the min_density_inverts table to the main invertebrate data table
inverts_data_with_minN <- inverts_data_addingzeros %>%
  left_join(min_density_inverts, by = c("site_code", "species_name"))

# Continue with the rest of your existing code
inverts_site_species_keep <- 
  inverts_data_with_minN %>% 
  filter(total_per50m2 > 0) %>% 
  add_count(site_code, species_name, name = "n_site_species") %>% 
  filter(n_site_species > 3) %>% 
  mutate(site_spp = paste(site_code, species_name, sep = "_")) %>% 
  pull(site_spp) %>% 
  unique()

inverts_data <- 
  inverts_data_with_minN %>% 
  left_join(site_list_inverts) %>% 
  mutate(n_sites_byspp = n(), .by = c(site_code, species_name)) %>% 
  mutate(n_obs_byspp = n(), .by = c(species_name)) %>% 
  filter(n_obs_byspp > n_sites_byspp) %>% 
  mutate(latitude  = round(latitude),
         longitude = round(longitude),
         log_count = log10(total_per50m2 + minN/2)) %>%  # Use minN for log transformation
  add_count(site_code, species_name, name = "n_site_species") %>% 
  filter(n_site_species > 3) %>% 
  filter(paste(site_code, species_name, sep = "_") %in% inverts_site_species_keep) %>%
  # species must be seen in more than one site
  filter(n_distinct(site_code) > 1, 
         .by = species_name)

################# END ####################

#Total number of species being considered after filtering: 1039
num_species_inverts <-
  inverts_data %>%
  distinct(species_name) %>%
  nrow()
print(num_species_inverts)


#Check to see if filtering has worked
table(inverts_data$site_code) %>% 
  sort() %>% 
  head()


#Check to see if filtering has worked
table(inverts_data$site_code) %>% 
  sort() %>% 
  head()


## Coral
# Here, I will load in all coral data, from the RLS database - this data
# is obtained through the photo-quadrats taken during the survey and then
# these photos are scored to the highest taxonomic level by a coral
# taxonomic expert, resulting in a coral percentage cover obtained through
# analysis via a specialised program Squiddle+, a quincunx grid of 5
# points was overlaid on each image and corals under each point were
# recorded; thus, taxa under 100 points per transect are cataloged.
# 
# Observations that are only to species level have been used for this
# analysis and the data has been filtered to ensure it is only from the
# initiation of RLS, so from 2008 on ward.

### Cleaning counts
sum(is.na(coral_raw$percent_cover))

#first let's check what groups are contained in the "coral raw" data frame
summary_table_coral <- table(coral_raw$RLE_category)
#view(summary_table_coral) 

#change date format:
coral_raw <- coral_raw %>%
  mutate(survey_year = as.numeric(substring(survey_date, nchar(survey_date) - 3, nchar(survey_date))))

#filter (and keep) inverts columns
coral_clean <- 
  coral_raw %>% 
  filter(str_detect(label, "[A-Z]{1}[a-z]+\\s{1}[a-z]+$")) %>% 
  filter(str_detect(country, "Australia")) %>% 
  filter(str_detect(RLE_category, "Coral")) %>%
  filter(!str_detect(label, "\\[|\\]")) %>% 
  filter(!str_detect(label, "\\bcorals\\b")) %>% 
  filter(!str_detect(label, "\\bcoral\\b")) %>% 
  filter(!str_detect(label, "\\bsp\\b")) %>% 
  as_tibble() %>% 
  rename(species_name = label) %>% 
  mutate(site_yr = paste(site_code, survey_year, sep = "_")) %>% 
  summarise(percent_cover = mean(percent_cover), 
            .by = c(site_code, 
                    site_yr,
                    survey_year, 
                    species_name))


coral_list <- 
  coral_raw %>% 
  select(survey_id,
         site_code,
         location,
         area,
         species_name = label,
         survey_year,
         depth,
         percent_cover,
         latitude,
         longitude) %>% 
  distinct()

coral_site_list <- 
  coral_raw %>% 
  select(site_code,
         species_name = label,
         survey_year,
         latitude,
         longitude) %>% 
  distinct()


### Dealing with NAs

# A list of all the site*year combinations
site_byyear_coral <-
  coral_clean |> 
  filter(!(is.na(percent_cover)|percent_cover == 0)) |> # not recorded
  select(site_code, survey_year) |> 
  distinct() |> 
  mutate(site_code_yr = paste(site_code, survey_year, sep = "_"))

# A list of all the site*year*species combinations
site_byspp_coral <-
  coral_clean |> 
  filter(!(is.na(percent_cover)|percent_cover == 0)) |> # not recorded
  select(site_code, species_name) |> 
  distinct() |> 
  mutate(site_code_spp = paste(site_code, species_name, sep = "_"))

site_byspp_nested_coral <- 
  site_byspp_coral %>% 
  select(site_code, species_name) %>% 
  group_by(site_code) %>% 
  nest()

data_addingzeros_coral <- 
  site_byyear_coral %>% 
  select(site_code, survey_year) %>% 
  left_join(site_byspp_nested_coral) %>% 
  unnest(cols = c(data)) %>% 
  left_join(coral_clean %>% 
              select(site_code, 
                     survey_year,
                     species_name, 
                     percent_cover)) %>% 
  mutate(percent_cover = replace_na(percent_cover, 0)) %>%
  mutate(sqrt_pcover = sqrt(percent_cover)) 

#| include = FALSE: CORAL
data_addingzeros_coral |> filter(is.na(sqrt_pcover)) |> nrow()

##FINAL CLEAN
coral_site_species_keep <- 
  data_addingzeros_coral %>% 
  filter(sqrt_pcover > 0) %>% 
  add_count(site_code, species_name, name = "n_site_species") %>% 
  filter(n_site_species > 3) %>% 
  mutate(site_spp = paste(site_code, species_name, sep = "_")) %>% 
  pull(site_spp) %>% 
  unique()

coral_data <- 
  data_addingzeros_coral |> 
  left_join(coral_site_list) %>%
  mutate(n_sites_byspp = n(), .by = c(site_code, species_name)) %>% 
  mutate(n_obs_byspp = n(), .by = c(species_name)) %>% 
  filter(n_obs_byspp > n_sites_byspp) %>% 
  mutate(latitude  = round(latitude),
         longitude = round(longitude)) |> 
  mutate(transect_id = paste(site_code, survey_year, sep = "_")) %>% 
  add_count(site_code, species_name, name = "n_site_species") %>% 
  filter(n_site_species > 3) %>% 
  filter(paste(site_code, species_name, sep = "_") %in% coral_site_species_keep) %>%
  # species must be see in more than one site
  filter(n_distinct(site_code) > 1, 
         .by = species_name) 

#Total number of species being considered after filtering: 349
num_species_coral <- 
  coral_data %>%
  distinct(species_name) %>%
  nrow()
print(num_species_coral)

#Check to see if filtering has worked
table(coral_data$site_code) %>% 
  sort() %>% 
  head()

#Check to see if filtering has worked
table(coral_data$site_code)%>% 
  sort() %>% 
  head()

#Export all cleaned data ready for modelling
#Algae
file_name <- "algae_data.csv"
write.csv(algae_data, file_name, row.names = FALSE)

#Fish
file_name <- "fish_data.csv"
write.csv(fish_data, file_name, row.names = FALSE)

#Inverts
file_name <- "inverts_data.csv"
write.csv(inverts_data, file_name, row.names = FALSE)

#Coral
file_name <- "coral_data.csv"
write.csv(coral_data, file_name, row.names = FALSE)


## END ##