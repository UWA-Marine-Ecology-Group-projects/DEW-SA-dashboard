library(shiny)
library(bslib)
library(leaflet)
library(ggplot2)
library(fontawesome)
library(scales)
library(leafgl)
library(colourvalues)
library(shinyWidgets)
library(sf)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggtext)

# how to make it faster?
# Helper functions
# add_marine_park_layers <- function(map) {
#   map %>%
#     leafgl::addGlPolygons(data = ngari.mp, color = "black", weight = 1,
#                           fillColor = "#7bbc63", fillOpacity = 0.8,
#                           group = "State Marine Parks", popup = ngari.mp$Name) %>%
#     
#     leafgl::addGlPolygons(data = state.mp, color = "black", weight = 1,
#                           fillColor = ~state.pal(zone), fillOpacity = 0.8,
#                           group = "State Marine Parks", popup = state.mp$COMMENTS) %>%
#     
#     addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
#               title = "State Zones", position = "bottomright",
#               group = "State Marine Parks") %>%
#     
#     leafgl::addGlPolygons(data = commonwealth.mp, color = "black", weight = 1,
#                           fillColor = ~commonwealth.pal(zone), fillOpacity = 0.8,
#                           group = "Australian Marine Parks", popup = commonwealth.mp$ZoneName) %>%
#     
#     addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
#               title = "Australian Marine Park Zones", position = "bottomright",
#               group = "Australian Marine Parks") %>%
#     
#     addLayersControl(
#       overlayGroups = c("Australian Marine Parks", "State Marine Parks", "Sampling locations"),
#       options = layersControlOptions(collapsed = FALSE), position = "topright"
#     ) %>%
#     
#     hideGroup("State Marine Parks") %>%
#     hideGroup("Australian Marine Parks")
# }
# 
# add_bubbles_to_map <- function(map, overzero, equalzero, value_col, max_val) {
#   if (nrow(overzero)) {
#     map <- map %>%
#       addGlPoints(data = overzero, 
#                   fillColor = "darkgreen", fillOpacity = 1, weight = 1,
#                   radius = ~ ((10 + (.[[value_col]] / max_val) * 50)),
#                   popup = as.character(.[[value_col]]),
#                   group = "Sampling locations")
#   }
#   if (nrow(equalzero)) {
#     map <- map %>%
#       addGlPoints(data = equalzero, 
#                   fillColor = "white", fillOpacity = 0.5, weight = 1,
#                   radius = 10,
#                   popup = as.character(.[[value_col]]),
#                   group = "Sampling locations")
#   }
#   map
# }

# use reactive
# deployment_points <- reactive({
#   dataframes$deployment_locations
# })

# # 🧼 Remove unused columns before rendering:
# Large sf objects with unnecessary columns slow rendering.

# st_as_sf(overzero) %>% dplyr::select(longitude_dd, latitude_dd, count)

# 🗂️ 3. Avoid full joins unless needed
# You have this:
# 
# data <- full_join(data, dataframes$deployment_locations)
# If you're just matching one-to-one on known keys, consider left_join() — it’s faster and avoids unnecessary row inflation.

# Load data
load("app_data/values.Rdata")
load("app_data/dataframes.Rdata")
load("app_data/plots.Rdata")

# Spatial files for maps ----
commonwealth.mp <- readRDS("app_data/spatial/commonwealth.mp.RDS")
# state.mp <- readRDS("app_data/spatial/state.mp.RDS")

# Pallettes for maps ----
# state.pal <- colorFactor(c("#bfaf02", # conservation
#                            "#7bbc63", # sanctuary = National Park
#                            "#fdb930", # recreation
#                            "#b9e6fb", # general use
#                            '#ccc1d6' # special purpose
# ), state.mp$zone)

commonwealth.pal <- colorFactor(c("#f6c1d9", # Sanctuary
                                  "#7bbc63", # National Park
                                  "#fdb930", # Recreational Use
                                  "#fff7a3", # Habitat Protection
                                  '#b9e6fb', # Multiple Use
                                  '#ccc1d6'# Special Purpose
), commonwealth.mp$zone)


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

# Legend for leaflet ----
add_legend <- function(map, colors, labels, sizes, opacity = 1, group, title) { #map, 
  colorAdditions <- glue::glue(
    "{colors}; border-radius: 50%; width:{sizes}px; height:{sizes}px"
  )
  labelAdditions <- glue::glue(
    "<div style='display: inline-block; height: {sizes}px; ",
    "margin-top: 4px;line-height: {sizes}px;'>{labels}</div>"
  )
  
  return(
    leaflet::addLegend(map,
                       colors = colorAdditions,
                       labels = labelAdditions,
                       opacity = opacity,
                       title = title,
                       position = "bottomleft",
                       group = group
    )
  )
}

# rsconnect::deployApp(
#   appDir = "H:/national-aus-synthesis-dashboard",
#   appName = "national-aus-synthesis-dashboard",
#   account = "marine-ecology",
#   server = "shinyapps.io",
#   appTitle = "Aus-synthesis",
#   forceUpdate = TRUE,
#   lint = FALSE,
#   logLevel = "verbose",
#   appFiles = c(
#     "global.R", "server.R", "ui.R",
#     "app_data/dataframes.Rdata",
#     "app_data/spatial/commonwealth.mp.RDS",
#     "app_data/spatial/ngari.mp.RDS",
#     "app_data/spatial/state.mp.RDS",
#     "app_data/values.Rdata"
#   )
# )

