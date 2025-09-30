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
library(tidytext)

# Load data
load("app_data/values.Rdata")
load("app_data/dataframes.Rdata")
load("app_data/plots.Rdata")

# Spatial files for maps ----
commonwealth.mp <- readRDS("app_data/spatial/commonwealth.mp.RDS")
# state.mp <- readRDS("app_data/spatial/state.mp.RDS")

state.mp <- readRDS("app_data/spatial/sa.state.mp.RDS")

# Pallettes for maps ----
state.pal <- colorFactor(c("#f18080", # Restricted Access Zone (RAZ)
                           "#69a802", # Sanctuary Zone (SZ)
                           "#799CD2", # Habitat Protection (HPZ)
                           "#BED4EE" # General Managed Use Zone (GMUZ)
), state.mp$zone)

# unique(state.mp$zone_type)

commonwealth.pal <- colorFactor(c("#f6c1d9", # Sanctuary
                                  "#7bbc63", # National Park
                                  "#fdb930", # Recreational Use
                                  "#fff7a3", # Habitat Protection
                                  '#b9e6fb', # Multiple Use
                                  '#ccc1d6'# Special Purpose
), commonwealth.mp$zone)

unique(commonwealth.mp$zone)


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

