#####################################################################
# Project: API query for Australian BRUV synthesis dashboard
# Data:    2024 BRUV Syntheses metadata, count, length and covariates
# Task:    Use GlobalArchive API to query data and save as an RDS
# Author:  Brooke Gibbons
# Date:    July 2024
#####################################################################

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
CheckEM::ga_api_all_data(synthesis_id = "19",
                         token = token,
                         dir = "data/raw/",
                         include_zeros = FALSE)

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

# Number of fish -----
number_of_fish <- count %>%
  semi_join(fish_species) %>%
  summarise(number_of_fish = sum(count)) %>%
  pull()

number_of_fish

# Number of fish species ----
number_of_fish_species <- count %>%
  semi_join(fish_species) %>%
  distinct(family, genus, species) %>%
  nrow(.)

number_of_fish_species

# Number of non-fish species ----
number_of_nonfish_species <- count %>%
  anti_join(fish_species) %>%
  distinct(family, genus, species) %>%
  nrow(.)

number_of_nonfish_species

# Number of length measurements -----
number_of_measurements <- length %>%
  semi_join(fish_species) %>%
  dplyr::filter(!is.na(length_mm)) %>%
  summarise(number_of_measurements = sum(count)) %>%
  pull()

number_of_measurements

# Depths surveyed ----
min_depth <- metadata %>%
  filter(!depth_m == 0) %>%
  filter(depth_m == min(.$depth_m)) %>%
  distinct() %>%
  pull(depth_m) %>%
  unique()

max_depth <- max(metadata$depth_m)

# Average depth ----
mean_depth <- mean(metadata$depth_m)

# Years sampled ----
year_dat <- metadata %>% 
  dplyr::mutate(year = str_sub(date_time, 1, 4)) %>%
  dplyr::filter(!is.na(year))

unique(year_dat$year)

min_year <- min(year_dat$year)
max_year <- max(year_dat$year)

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

# Top 20 species ----
top_species <- count %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(total_number = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(family, genus, species, australian_common_name, total_number) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
  dplyr::slice_max(order_by = total_number, n = 20) %>%
  dplyr::select(display_name, total_number)

names(top_species)

# Top 200 species names ----
top_200_species_names <- count %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(total_number = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(species_list) %>%
  dplyr::select(family, genus, species, australian_common_name, total_number) %>%
  dplyr::mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
  # dplyr::slice_max(order_by = total_number, n = 200) %>%
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
  mutate(month = format(date_time, "%Y")) |>
  count(month) |>
  ggplot(aes(x = month, y = n)) +
  geom_bar(stat = "identity", fill = "#0072B2", color = "black") +
  xlab("Year") +
  ylab("Number of deployments") +
  # ggtitle("Deployments by Year") +
  ggplot_theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

depth_hist <- ggplot(simple_metadata, aes(x = depth_m)) +
  geom_histogram(binwidth = 5, fill = "#0072B2", color = "black") +
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
    deployments_benthos = deployments_benthos
  ),
  class = "data"
)

save(values, file = here::here("app_data/values.Rdata"))

dataframes <- structure(
  list(
    top_species = top_species,
    deployment_locations = deployment_locations,
    metric_bubble_data = metric_bubble_data,
    bubble_data_200 = bubble_data_200,
    foa_codes = foa_codes,
    length_200_with_jurisdiction = length_200_with_jurisdiction,
    simple_metadata = simple_metadata
  ),
  class = "data"
)

save(dataframes, file = here::here("app_data/dataframes.Rdata"))

plots <- structure(
  list(
    date_hist = date_hist,
    depth_hist = depth_hist
  ),
  class = "data"
)

save(plots, file = here::here("app_data/plots.Rdata"))

