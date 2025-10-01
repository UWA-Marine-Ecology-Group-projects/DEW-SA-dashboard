#####################################################################
# Project: API query for Australian BRUV synthesis dashboard
# Data:    2024 BRUV Syntheses metadata, count, length and covariates
# Task:    Use GlobalArchive API to query data and save as an RDS
# Author:  Brooke Gibbons
# Date:    July 2024
#####################################################################

# Other Constraints
# The citation in a list of references is: "Reef Life Survey (RLS); Institute for Marine and Antarctic Studies (IMAS); Parks Victoria; Department of Primary Industries (DPI), New South Wales Government; Parks and Wildlife Tasmania; Department for Environment and Water (DEWNR), South Australia, Integrated Marine Observing System (IMOS), [year-of-data-download], IMOS - National Reef Monitoring Network - Survey metadata, [data-access-URL], accessed [date-of-access]."
# 
# Please also cite the associated data paper when using this data: Edgar, G., Stuart-Smith, R. Systematic global assessment of reef fish communities by the Reef Life Survey program. Sci Data 1, 140007 (2014). https://doi.org/10.1038/sdata.2014.6
# 
# Please also utilise and cite this paper when using this data: Graham J. Edgar, Antonia Cooper, Susan C. Baker, William Barker, Neville S. Barrett, Mikel A. Becerro, Amanda E. Bates, Danny Brock, Daniela M. Ceccarelli, Ella Clausius, Marlene Davey, Tom R. Davis, Paul B. Day, Andrew Green, Samuel R. Griffiths, Jamie Hicks, Ivan A. Hinojosa, Ben K. Jones, Stuart Kininmonth, Meryl F. Larkin, Natali Lazzari, Jonathan S. Lefcheck, Scott D. Ling, Peter Mooney, Elizabeth Oh, Alejandro Perez-Matus, Jacqueline B. Pocklington, Rodrigo Riera, Jose A. Sanabria-Fernandez, Yanir Seroussi, Ian Shaw, Derek Shields, Joe Shields, Margo Smith, German A. Soler, Jemina Stuart-Smith, John Turnbull, Rick D. Stuart-Smith. Establishing the ecological basis for conservation of shallow marine life using Reef Life Survey, Biological Conservation, Volume 252, 2020, https://doi.org/10.1016/j.biocon.2020.108855
# 
# Please also utilise and cite this paper when using this data: Edgar, G., & Barrett, N. (2012). An assessment of population responses of common inshore fishes and invertebrates following declaration of five Australian marine protected areas. Environmental Conservation, 39(3), 271-281. doi:10.1017/S0376892912000185
# 
# Usage Constraints
# Data, products and services from IMOS are provided "as is" without any warranty as to fitness for a particular purpose.
# 
# By using this data you are accepting the license agreement and terms specified above. You accept all risks and responsibility for losses, damages, costs and other consequences resulting directly or indirectly from using this site and any information or material available from it.

# Install CheckEM package ----
options(timeout = 9999999) # the package is large, so need to extend the timeout to enable the download.
# remotes::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install, if not then this line won't do anything

# Load libraries needed -----
library(CheckEM)
library(httr)
library(tidyverse)
library(RJSONIO)
library(devtools)
library(sf)
library(googlesheets4)

# Theme for plotting ----
ggplot_theme <- 
  ggplot2::theme_bw() +
  ggplot2::theme( # use theme_get() to see available options
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_blank(),
    # legend.position = "top",
    text = ggplot2::element_text(size = 12),
    strip.text.y = ggplot2::element_text(size = 12, angle = 0),
    axis.title.x = ggplot2::element_text(vjust = 0.3, size = 12),
    axis.title.y = ggplot2::element_text(vjust = 0.6, angle = 90, size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
    axis.line.x = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    axis.line.y = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
    strip.background = ggplot2::element_blank(),
    
    strip.text = ggplot2::element_text(size = 14, angle = 0),
    
    plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold.italic")
  )


# Set your API token to access GlobalArchive data shared with you ----
# It is extremely important that you keep your API token out of your scripts, and github repository!
# This function will ask you to put your API token in the console
# It will then create a folder in your project folder called "secrets" and saves your API token to use in the functions later
# The function adds the token into the .gitignore file so it will never be put in version control with Git
CheckEM::ga_api_set_token()

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# # Load the metadata, count and length ----
# # This way does not include the zeros where a species isn't present - it returns a much smaller dataframe
# CheckEM::ga_api_all_data(synthesis_id = "19",
#                          token = token,
#                          dir = "data/raw/",
#                          include_zeros = FALSE)

rls <- read.csv("data/raw/IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Global_reef_fish_abundance_and_biomass.csv", skip = 71) %>%
  dplyr::select(survey_id, location, site_code, site_name, latitude, longitude, survey_date, depth, family, reporting_name, size_class, total) %>%
  tidyr::separate(reporting_name, into = c("genus", "species"), remove = FALSE) %>%
  dplyr::rename(count = total, depth_m = depth) %>%
  glimpse()

rls_metadata <- read.csv("data/raw/IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Survey_metadata.csv", skip = 71) %>%
  dplyr::rename(depth_m = depth, latitude_dd = latitude, longitude_dd = longitude) %>%
  glimpse()

# TODO Need to fix synonyms
synonyms_in_rls <- dplyr::left_join(rls, CheckEM::aus_synonyms)  %>%
  dplyr::filter(count > 0) %>%
  dplyr::filter(!is.na(genus_correct)) %>%
  dplyr::mutate('old name' = paste(family, genus, species, sep = " ")) %>%
  dplyr::mutate('new name' = paste(family_correct, genus_correct, species_correct, sep = " ")) %>%
  dplyr::select('old name', 'new name') %>% # taken out sample
  dplyr::distinct()

rls_with_synonyms_changed <- dplyr::left_join(rls, CheckEM::aus_synonyms) %>%
  dplyr::mutate(genus = ifelse(!genus_correct%in%c(NA), genus_correct, genus)) %>%
  dplyr::mutate(species = ifelse(!is.na(species_correct), species_correct, species)) %>%
  dplyr::mutate(family = ifelse(!is.na(family_correct), family_correct, family)) %>%
  dplyr::select(-c(family_correct, genus_correct, species_correct)) %>%
  mutate(family = str_replace_all(family, "[^[:alnum:]]", "")) %>%
  mutate(genus = str_replace_all(genus, "[^[:alnum:]]", "")) %>%
  mutate(species = str_replace_all(species, c("[^[:alnum:]]" = "", "pusillusdoriferus" = "pusillus doriferus"))) %>%
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  dplyr::group_by(survey_id, location, site_code, site_name, latitude, longitude, survey_date, depth_m, family, genus, species, scientific) %>%
  dplyr::slice(which.max(count)) %>%
  ungroup()

# Species not in list ----
# TODO should check regions too
count_species_not_in_list <- rls_with_synonyms_changed %>%
  dplyr::anti_join(., CheckEM::australia_life_history, by = c("family", "genus", "species")) %>%
  dplyr::filter(count > 0) %>%
  dplyr::distinct(family, genus, species) 

# TODO fix these up with synonyms!

rls_count <- rls_with_synonyms_changed %>%
  glimpse()

rls_length <- rls_with_synonyms_changed %>%
  dplyr::mutate(length_mm = 10 * size_class) %>%
  glimpse()

# The campaigns we care about are:
campaign_list <- c("2015-16_SA_MPA_UpperGSV_StereoBRUVS",
                   "2015-201706_SA_MPA_StereoBRUVS",
                   "201712-201806_SA_MarineParkMonitoring_StereoBRUVS",
                   "201810-201903_SA_DEW_SAWater_Penneshaw_StereoBRUVS",
                   "201811_Comm_PearsonExpedition_StereoBRUVS",
                   "201811_SA_MPA_PearsonExpedition",
                   "201812-201906_SA_MarineParkMonitoring_StereoBRUVS",
                   "201905-201909_SA_DEW_SAWater_Sleaford Monitoring",
                   "202008-202008_SA_DEW_SAWater Monitoring_StereoBRUVS",
                   "202011-202011_SA Commonwealth Marine Park Monitoring_StereoBRUVS",
                   "202012-202105_SA_MarineParkMonitoring_StereoBRUVS",
                   "202110-202205_SA_MarineParkMonitoring_StereoBRUVS",
                   "202111-202205_SA Commonwealth Marine Park Monitoring_StereoBRUVS")


## Load in data again to save time ----
metadata <- readRDS("data/raw/metadata.RDS") %>% filter(campaignid %in% c(campaign_list))
count <- readRDS("data/raw/count.RDS") %>% left_join(metadata %>% select(sample_url, campaignid)) %>% filter(campaignid %in% c(campaign_list))
length <- readRDS("data/raw/length.RDS") %>% left_join(metadata %>% select(sample_url, campaignid)) %>% filter(campaignid %in% c(campaign_list))
benthos <- readRDS("data/raw/benthos_summarised.RDS") %>% filter(campaignid %in% c(campaign_list))
relief <- readRDS("data/raw/relief_summarised.RDS") %>% filter(campaignid %in% c(campaign_list))

species_list <- ga_api_species_list(token = token)

fish_species <- species_list %>%
  dplyr::filter(class_value %in% c("Actinopterygii", "Elasmobranchii", "Myxini"))

unique(species_list$class_value)

# Create metrics for dashboard ----
# Number of deployments ----
number_of_deployments <- nrow(metadata)
number_of_deployments

number_of_deployments_rls <- nrow(rls_metadata)

# Number of fish -----
number_of_fish <- count %>%
  semi_join(fish_species) %>%
  summarise(number_of_fish = sum(count)) %>%
  pull()

number_of_fish

number_of_fish_rls <- rls_count %>%
  semi_join(fish_species) %>%
  summarise(number_of_fish = sum(count)) %>%
  pull()

number_of_fish_rls

# Number of fish species ----
number_of_fish_species <- count %>%
  semi_join(fish_species) %>%
  distinct(family, genus, species) %>%
  nrow(.)

number_of_fish_species

number_of_fish_species_rls <- rls_count %>%
  semi_join(fish_species) %>%
  distinct(family, genus, species) %>%
  nrow(.)

number_of_fish_species_rls

# Number of non-fish species ----
number_of_nonfish_species <- count %>%
  anti_join(fish_species) %>%
  distinct(family, genus, species) %>%
  nrow(.)

number_of_nonfish_species

number_of_nonfish_species_rls <- rls_count %>%
  anti_join(fish_species) %>%
  distinct(family, genus, species) %>%
  nrow(.)

number_of_nonfish_species_rls

# Number of length measurements -----
number_of_measurements <- length %>%
  semi_join(fish_species) %>%
  dplyr::filter(!is.na(length_mm)) %>%
  summarise(number_of_measurements = sum(count)) %>%
  pull()

number_of_measurements

number_of_measurements_rls <- rls_length %>%
  semi_join(fish_species) %>%
  dplyr::filter(!is.na(length_mm)) %>%
  summarise(number_of_measurements = sum(count)) %>%
  pull()

number_of_measurements_rls

# Depths surveyed ----
min_depth <- metadata %>%
  filter(!depth_m == 0) %>%
  filter(depth_m == min(.$depth_m)) %>%
  distinct() %>%
  pull(depth_m) %>%
  unique()

min_depth_rls <- rls_count %>%
  filter(!depth_m == 0) %>%
  filter(depth_m == min(.$depth_m)) %>%
  distinct() %>%
  pull(depth_m) %>%
  unique()

max_depth <- max(metadata$depth_m)

max_depth_rls <- max(rls_count$depth_m)

# Average depth ----
mean_depth <- mean(metadata$depth_m)

mean_depth_rls <- mean(rls_metadata$depth_m)

# Years sampled ----
year_dat <- metadata %>% 
  dplyr::mutate(year = str_sub(date_time, 1, 4)) %>%
  dplyr::filter(!is.na(year))

unique(year_dat$year)

min_year <- min(year_dat$year)
max_year <- max(year_dat$year)

year_dat_rls <- rls_metadata %>% 
  dplyr::mutate(year = str_sub(survey_date, 1, 4)) %>%
  dplyr::filter(!is.na(year))

unique(year_dat_rls$year) %>% sort()

min_year_rls <- min(year_dat_rls$year)
max_year_rls <- max(year_dat_rls$year)

# Deployments with benthos ----
deployments_benthos <- benthos %>%
  distinct(sample_url) %>%
  nrow(.)

deployments_relief <- relief %>%
  distinct(sample_url) %>%
  nrow(.)

# Dataframes for plotting -----
simple_metadata <- metadata %>%
  distinct(sample_url, date_time, depth_m)

rls_simple_metadata <- rls_metadata %>%
  distinct(survey_id, survey_date, depth_m)

# Deployment locations ----
deployment_locations <- metadata %>%
  distinct(sample_url, campaignid, sample, latitude_dd, longitude_dd, date_time, depth_m) %>%
  dplyr::mutate(year = as.numeric(str_sub(date_time, 1, 4))) %>%
  dplyr::mutate(popup = paste0(
    "<b>Campaign:</b> ", campaignid, "<br/>",
    "<b>Sample:</b> ", sample, "<br/>",
    "<b>Year:</b> ", year, "<br/>",
    "<b>Depth (m):</b> ", round(depth_m, 1))) %>%
  dplyr::select(sample_url, depth_m, popup, longitude_dd, latitude_dd, year)

deployment_locations = st_as_sf(deployment_locations, coords = c("longitude_dd", "latitude_dd"))

names(deployment_locations)

mean_lon <- mean(metadata$longitude_dd)
mean_lat <- mean(metadata$latitude_dd)

# Deployment locations RLS ----
deployment_locations_rls <- rls_metadata %>%
  distinct(location, site_code, site_name, latitude_dd, longitude_dd, survey_date, depth_m) %>%
  dplyr::mutate(year = as.numeric(str_sub(survey_date, 1, 4))) %>%
  dplyr::mutate(popup = paste0(
    "<b>Location:</b> ", location, "<br/>",
    "<b>Site code:</b> ", site_code, "<br/>",
    "<b>Site name:</b> ", site_name, "<br/>",
    "<b>Year:</b> ", year, "<br/>",
    "<b>Depth (m):</b> ", round(depth_m, 1))) %>%
  dplyr::select(depth_m, popup, longitude_dd, latitude_dd, year)

deployment_locations_rls = st_as_sf(deployment_locations_rls, coords = c("longitude_dd", "latitude_dd"))

names(deployment_locations_rls)

mean_lon_rls <- mean(rls_metadata$longitude_dd)
mean_lat_rls <- mean(rls_metadata$latitude_dd)

# Top 20 species ----
top_species <- count %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(total_number = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(family, genus, species, australian_common_name, total_number) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
  dplyr::slice_max(order_by = total_number, n = 20) %>%
  dplyr::select(display_name, total_number) %>%
  dplyr::mutate(method = "stereo-BRUVs")

names(top_species)
top_species

top_species_rls <- rls_count %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(total_number = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(family, genus, species, australian_common_name, total_number) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
  dplyr::slice_max(order_by = total_number, n = 20) %>%
  dplyr::select(display_name, total_number) %>%
  dplyr::mutate(method = "UVC")

names(top_species_rls)
top_species_rls

top_species_combined <- bind_rows(top_species, top_species_rls)

# Top 200 species names ----
top_200_species_names <- count %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(total_number = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(family, genus, species, australian_common_name, total_number) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
  dplyr::slice_max(order_by = total_number, n = 200) %>%
  dplyr::pull(display_name)

# Top 200 bubble data ----
bubble_data_200 <- count %>%
  dplyr::left_join(metadata) %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(sample_url, family, genus, species, count, australian_common_name) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
  dplyr::filter(display_name %in% c(top_200_species_names)) %>%
  dplyr::select(sample_url, display_name, count) %>%
  dplyr::glimpse()

# Total abundance and species richness bubble data ----
metric_bubble_data <- count %>%
  dplyr::left_join(metadata) %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(sample_url, family, genus, species, count, longitude_dd, latitude_dd, date_time, depth_m, status, successful_count, australian_common_name) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(total_abundance = sum(count, na.rm = TRUE),
                   species_richness = n_distinct(family, genus, species)) %>%
  full_join(metadata) %>%
  replace_na(list(total_abundance = 0, species_richness = 0)) %>%
  dplyr::select(sample_url, total_abundance, species_richness) %>%
  pivot_longer(!c(sample_url), names_to = "metric", values_to = "value") %>%
  dplyr::left_join(metadata) %>%
  dplyr::ungroup() %>%
  dplyr::select(sample_url, metric, value, longitude_dd, latitude_dd) %>%
  glimpse()

# Total abundance and species richness bubble data RLS----
metric_bubble_data_rls <- rls_count %>%
  dplyr::left_join(rls_metadata) %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(survey_id, family, genus, species, count, longitude_dd, latitude_dd, depth_m, australian_common_name) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
  dplyr::group_by(survey_id) %>%
  dplyr::summarise(total_abundance = sum(count, na.rm = TRUE),
                   species_richness = n_distinct(family, genus, species)) %>%
  full_join(rls_metadata) %>%
  replace_na(list(total_abundance = 0, species_richness = 0)) %>%
  dplyr::select(survey_id, total_abundance, species_richness) %>%
  pivot_longer(!c(survey_id), names_to = "metric", values_to = "value") %>%
  dplyr::left_join(rls_metadata) %>%
  dplyr::ungroup() %>%
  dplyr::select(survey_id, metric, value, longitude_dd, latitude_dd) %>%
  glimpse()

# Fishes of Australia codes ----
dbca_googlesheet_url <- "https://docs.google.com/spreadsheets/d/1OuOt80TvJBCMPLR6oy7YhfoSD4VjC73cuKovGobxiyI/edit?usp=sharing"

foa_species_codes <- googlesheets4::read_sheet(dbca_googlesheet_url, sheet = "fishes_of_australia") %>%
  CheckEM::clean_names() %>%
  dplyr::select(-c(number)) %>%
  dplyr::mutate(species = case_when(
    genus %in% "Ophthalmolepis" & species %in% "lineolata" ~ "lineolatus",
    .default = as.character(species)
  )) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::left_join(CheckEM::australia_life_history) %>%
  dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
  dplyr::select(display_name, url)
2

foa_genus_codes <- readRDS("data/genus_foa_codes.RDS") %>%
  dplyr::mutate(species = "spp") %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::left_join(CheckEM::australia_life_history) %>%
  dplyr::mutate(display_name = paste0(scientific_name, " (", australian_common_name, ")")) %>%
  dplyr::select(display_name, url)

foa_codes <- bind_rows(foa_species_codes, foa_genus_codes) %>%
  dplyr::select(display_name, url) %>%
  dplyr::filter(display_name %in% c(top_200_species_names))

foa_codes <- data.table::data.table(foa_codes)

# length of top 200 ----
length_200 <- length %>%
  dplyr::left_join(species_list) %>%
  # dplyr::select(family, genus, species, australian_common_name, total_number) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
  dplyr::filter(display_name %in% c(top_200_species_names)) %>%
  left_join(metadata %>% dplyr::select(sample_url, sample, campaignid, status, longitude_dd, latitude_dd)) %>%
  dplyr::select(sample_url, display_name, count, length_mm, status, longitude_dd, latitude_dd)

# Add state and zones to length data for histograms -----
coastal_waters <- st_read("data/spatial/Coastal_Waters_areas_(AMB2020).shp")
# marine_parks <- st_read("data/spatial/shapefiles/western-australia_marine-parks-all.shp")

length_sf <- length_200 %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326)

length_sf <- st_transform(length_sf, st_crs(coastal_waters))

length_200_with_jurisdiction <- st_join(length_sf, coastal_waters %>% select(catlim)) %>%
  rename(jurisdiction = catlim) %>%
  replace_na(list(jurisdiction = "Federal Waters"))

unique(length_200_with_jurisdiction$jurisdiction)

names(length_200_with_jurisdiction)


# Plots ----
date_hist <- simple_metadata |>
  mutate(year = format(date_time, "%Y")) |>
  count(year) |>
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity", fill = "#0c3978", color = "black") +
  xlab("Year") +
  ylab("Number of deployments") +
  # ggtitle("Deployments by Year") +
  ggplot_theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

date_hist_rls <- rls_simple_metadata |>
  dplyr::mutate(year = as.numeric(str_sub(survey_date, 1, 4))) %>%
  count(year) |>
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity", fill = "#0c3978", color = "black") +
  xlab("Year") +
  ylab("Number of surveys") +
  # ggtitle("Deployments by Year") +
  ggplot_theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

depth_hist <- ggplot(simple_metadata, aes(x = depth_m)) +
  geom_histogram(binwidth = 5, fill = "#0c3978", color = "black") +
  xlab("Depth (m)") +
  ylab("Number of deployments") +
  scale_x_continuous(
    breaks = seq(
      floor(min(simple_metadata$depth_m, na.rm = TRUE) / 50) * 50,
      ceiling(max(simple_metadata$depth_m, na.rm = TRUE) / 50) * 50,
      by = 50
    )
  ) +
  ggplot_theme

depth_hist_rls <- ggplot(rls_simple_metadata, aes(x = depth_m)) +
  geom_histogram(binwidth = 5, fill = "#0c3978", color = "black") +
  xlab("Depth (m)") +
  ylab("Number of surveys") +
  ggplot_theme


# Combined plots ----
# TODO on Wednesday :)
bruv_years <- simple_metadata |>
  mutate(year = as.numeric(format(date_time, "%Y"))) |>
  count(year) |>
  mutate(method = "stereo-BRUVs")

rls_years <- rls_simple_metadata |>
  dplyr::mutate(year = as.numeric(str_sub(survey_date, 1, 4))) %>%
  count(year) |>
  mutate(method = "UVC")

date_hist_combined <- bind_rows(bruv_years, rls_years)|>
  ggplot(aes(x = year, y = n, fill = method)) +
  geom_bar(stat = "identity", color = "black") + #fill = "#0c3978", 
  xlab("Year") +
  ylab("Number of deployments") +
  scale_fill_manual(values = c("#f89f00", "#0c3978")) +
  # ggtitle("Deployments by Year") +
  ggplot_theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

date_hist_combined

# Combined depth plots ----
depth_combined <- bind_rows(simple_metadata %>% mutate(method = "stereo-BRUVs"), 
                            rls_simple_metadata %>% mutate(method = "UVC"))

# bruv_depth <- ggplot(simple_metadata, aes(x = depth_m)) +
#   geom_histogram(binwidth = 5, fill = "#0c3978", color = "black") +
#   xlab("Depth (m)") +
#   ylab("Number of deployments") +
#   scale_x_continuous(
#     breaks = seq(
#       floor(min(simple_metadata$depth_m, na.rm = TRUE) / 50) * 50,
#       ceiling(max(simple_metadata$depth_m, na.rm = TRUE) / 50) * 50,
#       by = 50
#     )
#   ) +
#   ggplot_theme

depth_hist_combined <- ggplot(depth_combined, aes(x = depth_m, fill = method)) +
  geom_histogram(binwidth = 5, color = "black") +
  xlab("Depth (m)") +
  ylab("Number of surveys") +
  scale_fill_manual(values = c("#f89f00", "#0c3978")) +
  ggplot_theme

depth_hist_combined

# Read in marineparks ----
state.mp <- read_sf("data/spatial/CONSERVATION_StateMarineParkNW_Zoning_GDA94.shp") %>%
  clean_names() %>%
  dplyr::mutate(zone = case_when(
    zone_type %in% "HPZ" ~ "Habitat Protection",
    zone_type %in% "SZ" ~ "Sanctuary (no-take)",
    zone_type %in% "GMUZ" ~ "General Managed Use",
    zone_type %in% "RAZ" ~ "Restricted Access (no-take)",
    zone_type %in% "RAZ_L" ~ "Restricted Access (no-take)",
    zone_type %in% "RAZ_D" ~ "Restricted Access (no-take)"
  )) %>% 
  dplyr::mutate(name = paste0(resname, ". Zone: ", zone_name, " (", zone, ")"))

unique(state.mp$zone)
unique(state.mp$name)

state.mp$zone <- fct_relevel(state.mp$zone, 
                             "Restricted Access (no-take)", 
                             "Sanctuary (no-take)", 
                             "Habitat Protection", 
                             "General Managed Use")

sa.state.mp <- st_cast(state.mp, "POLYGON")

unique(sa.state.mp$zone)

saveRDS(sa.state.mp, "app_data/spatial/sa.state.mp.RDS")

# Test leaflet maps

state.pal <- colorFactor(c("#f18080", # Restricted Access Zone (RAZ)
                           "#69a802", # Sanctuary Zone (SZ)
                           "#799CD2", # Habitat Protection (HPZ)
                           "#BED4EE" # General Managed Use Zone (GMUZ)
), sa.state.mp$zone)

leaflet() %>%
  addTiles(options = tileOptions(minZoom = 4, maxZoom = 10)) %>%
  setView(lng = 135, lat = -35.1, zoom = 6) %>%
  
  # Static polygon layers
  leafgl::addGlPolygons(data = sa.state.mp,
                        color = "black",
                        weight = 1,
                        fillColor = ~state.pal(zone),
                        fillOpacity = 0.8,
                        group = "State Marine Parks",
                        popup = sa.state.mp$name
                        )


# Create final dataframes and save ----
values <- structure(
  list(
    number_of_deployments = number_of_deployments,
    number_of_fish = number_of_fish,
    number_of_nonfish_species = number_of_nonfish_species,
    number_of_fish_species = number_of_fish_species,
    number_of_measurements = number_of_measurements,
    min_depth = min_depth,
    max_depth = max_depth,
    mean_depth = mean_depth,
    mean_lon = mean_lon,
    mean_lat = mean_lat,
    top_200_species_names = top_200_species_names,
    min_year = min_year,
    max_year = max_year,
    deployments_relief = deployments_relief,
    deployments_benthos = deployments_benthos,
    
    
    number_of_deployments_rls = number_of_deployments_rls,
    number_of_fish_rls = number_of_fish_rls,
    number_of_nonfish_species_rls = number_of_nonfish_species_rls,
    number_of_fish_species_rls = number_of_fish_species_rls,
    number_of_measurements_rls = number_of_measurements_rls,
    min_depth_rls = min_depth_rls,
    max_depth_rls = max_depth_rls,
    mean_depth_rls = mean_depth_rls,
    mean_lon_rls = mean_lon_rls,
    mean_lat_rls = mean_lat_rls,
    # top_200_species_names_rls = top_200_species_names_rls,
    min_year_rls = min_year_rls,
    max_year_rls = max_year_rls
  ),
  class = "data"
)

save(values, file = here::here("app_data/values.Rdata"))

dataframes <- structure(
  list(
    top_species = top_species,
    top_species_combined = top_species_combined,
    deployment_locations = deployment_locations,
    metric_bubble_data = metric_bubble_data,
    bubble_data_200 = bubble_data_200,
    foa_codes = foa_codes,
    length_200_with_jurisdiction = length_200_with_jurisdiction,
    simple_metadata = simple_metadata,
    
    
    deployment_locations_rls = deployment_locations_rls,
    metric_bubble_data_rls = metric_bubble_data_rls
    # bubble_data_200 = bubble_data_200,
    # length_200_with_jurisdiction = length_200_with_jurisdiction,
    #simple_metadata_rls = simple_metadata_rls
  ),
  class = "data"
)

save(dataframes, file = here::here("app_data/dataframes.Rdata"))

plots <- structure(
  list(
    date_hist = date_hist,
    depth_hist = depth_hist,
    date_hist_rls = date_hist_rls,
    depth_hist_rls = depth_hist_rls,
    date_hist_combined = date_hist_combined,
    depth_hist_combined = depth_hist_combined
  ),
  class = "data"
)

save(plots, file = here::here("app_data/plots.Rdata"))

