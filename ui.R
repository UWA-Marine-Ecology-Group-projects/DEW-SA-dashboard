page_navbar(
  
  title = div(
    "DEW SA Dashboard (DRAFT)",
    favicon = "www/favicon.ico",  # path to your favicon,
    style = "display: flex; gap: 10px; align-items: center; padding-right: 15px;font-weight: bold;"
    # style = "padding-right: 1rem; font-weight: bold;"
    # style = "flex-grow: 1;"  # This pushes the rest of the nav content (like links/logos) to the right
  ),
  
  theme = bs_theme(
    bootswatch = "minty", 
    secondary = "#0c3978",
    primary = "#f89f00",
    success = "#e10038",
    base_font = font_google("Roboto"),
    code_font = font_google("Roboto")
  ),
  fillable = FALSE,
  
  
  # Dashboard ----
  nav_panel(
    "State-wide Summary",
    tags$head(
      tags$style(HTML("
               .leaflet-container {z-index:0}

               .leaflet-top, .leaflet-bottom {
    z-index: unset !important;
}

.leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
    z-index: 10000000000 !important;
}"))
    ),

# Top Row - Four Value Boxes
layout_column_wrap(
  
  card(
    min_height = 600,
    max_height = 700,
    
    shiny::a(
      h2(img(src = "stereo-BRUV_filled_transparent.png",
             height = "120px"
      ),
      "Baited Remote Underwater Stereo-video (stereo-BRUVs)",
      class = "custom-button btn btn-default action-button", # use primary for blue
      style = "font-weight:600; width:100%; text-align:center; font-size: 20px; color: white;"),
      href = paste0("https://benthic-bruvs-field-manual.github.io/")
    ),
    
    full_screen = TRUE,
    
    # Bruv stats ----
    layout_column_wrap(
      width = 1/2,
      value_box(
        title = "Deployments", value = scales::label_comma()(values$number_of_deployments), 
        theme_color = "secondary",
        showcase = icon("ship", class = "fa-xl")
      ),
      value_box(
        title = "Fish Counted", value = scales::label_comma()(values$number_of_fish), 
        theme_color = "secondary",
        showcase = icon("fish-fins", class = "fa-xl")
      ),
      value_box(
        title = "Fish Species", value = scales::label_comma()(values$number_of_fish_species), 
        theme_color = "secondary",
        showcase = icon("fish-fins", class = "fa-xl")
      ),
      
      value_box(
        title = "Other species", value =scales::label_comma()(values$number_of_nonfish_species),
        theme_color = "secondary",
        showcase = icon("shrimp", class = "fa-xl")
      ),
      
      
      value_box(
        title = "Length Measurements", value = scales::label_comma()(values$number_of_measurements),
        theme_color = "secondary",
        showcase = icon("ruler-vertical", class = "fa-xl")
      ),
      
      
      value_box(
        title = "Years Included", value = paste0((values$min_year), 
                                                 " - ",
                                                 (values$max_year)),
        theme_color = "secondary",
        showcase = icon("calendar", class = "fa-xl")
      ),
      
      
      value_box(
        title = "Depths Surveyed", value = paste0(scales::label_comma()(values$min_depth), 
                                                  " - ",
                                                  scales::label_comma()(values$max_depth),
                                                  " m"),
        theme_color = "secondary",
        showcase = icon("arrow-down-up-across-line", class = "fa-xl")
      ),
      value_box(
        title = "Average Depth", value = paste0(scales::label_comma()(values$mean_depth), " m"),
        theme_color = "secondary",
        showcase = icon("wave-square", class = "fa-xl")
      ),
      
      value_box(
        title = "Deployments with benthos", value = scales::label_comma()(values$deployments_benthos),
        theme_color = "secondary",
        showcase = icon("seedling", class = "fa-xl")
      ),
      value_box(
        title = "Deployments with relief", value = scales::label_comma()(values$deployments_relief),
        theme_color = "secondary",
        showcase = icon("mound", class = "fa-xl")
      )
      
    )
  ),
  
  # UVC stats ----
  card(
    min_height = 600,
    max_height = 700,
    
    shiny::a(
      h2(img(src = "uvc_new.png",
             height = "120px"
      ),
      "Underwater Visual Census (UVC)",
      class = "custom-button btn btn-default action-button", # use primary for blue
      style = "font-weight:600; width:100%; text-align:center; font-size: 20px; color: white;"),
      href = paste0("https://benthic-bruvs-field-manual.github.io/")
    ),
    
    full_screen = TRUE,
    layout_column_wrap(
      width = 1/2,
      value_box(
        title = "Surveys", value = scales::label_comma()(values$number_of_deployments_rls), 
        theme_color = "secondary",
        showcase = icon("ship", class = "fa-xl")
      ),
      value_box(
        title = "Fish Counted", value = scales::label_comma()(values$number_of_fish_rls), 
        theme_color = "secondary",
        showcase = icon("fish-fins", class = "fa-xl")
      ),
      value_box(
        title = "Fish Species", value = scales::label_comma()(values$number_of_fish_species_rls), 
        theme_color = "secondary",
        showcase = icon("fish-fins", class = "fa-xl")
      ),
      
      value_box(
        title = "Other species", value =scales::label_comma()(values$number_of_nonfish_species_rls),
        theme_color = "secondary",
        showcase = icon("shrimp", class = "fa-xl")
      ),
      
      value_box(
        title = "Length Measurements", value = scales::label_comma()(values$number_of_measurements_rls),
        theme_color = "secondary",
        showcase = icon("ruler-vertical", class = "fa-xl")
      ),
      
      value_box(
        title = "Years Included", value = paste0((values$min_year_rls), 
                                                 " - ",
                                                 (values$max_year_rls)),
        theme_color = "secondary",
        showcase = icon("calendar", class = "fa-xl")
      ),
      
      value_box(
        title = "Depths Surveyed", value = paste0(scales::label_comma()(values$min_depth_rls), 
                                                  " - ",
                                                  scales::label_comma()(values$max_depth_rls),
                                                  " m"),
        theme_color = "secondary",
        showcase = icon("arrow-down-up-across-line", class = "fa-xl")
      ),
      value_box(
        title = "Average Depth", value = paste0(scales::label_comma()(values$mean_depth_rls), " m"),
        theme_color = "secondary",
        showcase = icon("wave-square", class = "fa-xl")
      ),
      
      value_box(
        title = "Deployments with benthos", value = scales::label_comma()(values$deployments_benthos),
        theme_color = "secondary",
        showcase = icon("seedling", class = "fa-xl")
      ),
      value_box(
        title = "Deployments with relief", value = scales::label_comma()(values$deployments_relief),
        theme_color = "secondary",
        showcase = icon("mound", class = "fa-xl")
      )
      
    )
  ),
  
),

# # Firs row (maps) ----
# 
# layout_column_wrap(
#   width = 1/2,
#   
#   # First Card: Map of Deployments BRUVs
#   card(
#     min_height = 500,
#     full_screen = TRUE,
#     card_header("Map of stereo-BRUV deployments"),
#     
#     leafletOutput("map_deployments", height = "400px"),
#   ),
#   
#   # Second Card: Map of Deployments RLS
#   card(
#     min_height = 500,
#     full_screen = TRUE,
#     card_header("Map of UVC surveys"),
#     
#     leafletOutput("map_surveys", height = "400px"),
#   )
# ),
# 
# # Firs row (year and depths) ----
# layout_column_wrap(
#   width = 1/2,
#   div(
#     card(
#       card_header("stereo BRUV deployments by year"),
#       full_screen = TRUE,
#       max_height = 400,
#       plotOutput("date_hist")),
#     
#     card(
#       card_header("stereo-BRUV depth frequency histogram"),
#       full_screen = TRUE,
#       max_height = 400,
#       plotOutput("depth_hist")
#     )
#   ),
#   div(
#     card(
#       card_header("UVC surveys by year"),
#       full_screen = TRUE,
#       max_height = 400,
#       plotOutput("date_hist_rls")
#     ),
#     
#     card(
#       card_header("UVC depth frequency histogram"),
#       full_screen = TRUE,
#       max_height = 400,
#       plotOutput("depth_hist_rls")
#     )
#   )
# ),


layout_column_wrap(
  width = 1/2,
  card(
    min_height = 500,
    full_screen = TRUE,
    card_header("Map of Sampling Effort"),
    
    leafletOutput("map_combined", height = "400px"),
  ),
  
  div(
    card(
      card_header("Sampling Effort by Year"),
      full_screen = TRUE,
      max_height = 400,
      plotOutput("date_hist_combined")
    ),
    
    card(
      card_header("Sampling Effort by Depth"),
      full_screen = TRUE,
      max_height = 400,
      plotOutput("depth_hist_combined")
    )
  )),


# # Third Row - Abundant species Plot and Leaflet (plot1 and map1)
# layout_columns(
#   # width = 1/2,
#   col_widths = c(8, 4),  # 8/12 = ~66% and 4/12 = ~33% (close to 60/40)
card(
  min_height = 650,
  full_screen = TRUE,
  card_header("Most Abundant Species"),
  plotOutput("top_species_plot", height = "400px")
),

card(
  min_height = 700,
  full_screen = TRUE,
  card_header("Spatial distribution of assemblage metrics"),
  shinyWidgets::pickerInput(
    inputId = "assemblage",
    label = "Choose an assemblage metric:",
    width = "100%",
    choices = c("Total abundance", "Species richness"),
    multiple = FALSE,
    selected = "Total abundance",
    options = list(`actions-box` = TRUE, `live-search` = FALSE, `dropup-auto` = FALSE)
  ),
  
  card_body(fillable = TRUE,
            min_height = 600,
            layout_column_wrap(
              width = 1/2,
              div(
                h4("Stereo-BRUVs"),
                leafletOutput("assemblage_map", height = "60vh"
                )
              ),
              div(
                h4("UVC"),
                leafletOutput("assemblage_map_rls", height = "60vh"
                )
              )
            )
  )
),

# card(
#   leafletOutput("assemblage_map"
#                 )
# ),

# card(
#   min_height = 500,
#   full_screen = TRUE,
#   card_header("Spatial distribution of assemblage metrics"),
# 
#   # fixed-height controls
#   card_body(
#     shinyWidgets::pickerInput(
#       inputId = "assemblage",
#       label   = "Choose an assemblage metric:",
#       width   = "100%",
#       choices = c("Total abundance", "Species richness"),
#       selected = "Total abundance",
#       multiple = FALSE,
#       options = list(`actions-box` = TRUE, `live-search` = FALSE, `dropup-auto` = FALSE)
#     )
#   ),
# 
#   # fill-height area (works in fullscreen too)
#   card_body_fill(
#     bslib::layout_columns(
#       col_widths = c(6, 6),   # or 7/5 for ~60/40
#       fill = TRUE,            # <- make row fill vertically
#       gap  = "1rem",
# 
#       # left column
#       div(class = "d-flex flex-column h-100",
#           h4("Stereo-BRUVs", class = "mb-2"),
#           div(class = "flex-fill",
#               leafletOutput("assemblage_map", height = "100%")
#           )
#       ),
# 
#       # right column
#       div(class = "d-flex flex-column h-100",
#           h4("UVC", class = "mb-2"),
#           div(class = "flex-fill",
#               leafletOutput("assemblage_map_rls", height = "100%")
#           )
#       )
#     )
#   )
# ),

# Fourth Row - Species map and iframe
card(full_screen = TRUE,
     card_header("Investigate a species abundance data"),
     
     max_height = 750,
     shinyWidgets::pickerInput(
       inputId = "species_select",
       label = "Choose a species:",
       width = "100%",
       choices = c(values$top_species_names_combined),
       multiple = FALSE,
       selected = values$top_species_names_combined[1],
       options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = TRUE)
     ),
     
     layout_columns(
       # width = 1/3,
       col_widths = c(4, 4, 4),  # 8/12 = ~66% and 4/12 = ~33% (close to 60/40)
       div(
         h4("Stereo-BRUVs"),
         leafletOutput("species_map", height = "500px")
       ),
       
       div(
         h4("UVC"),
         leafletOutput("species_map_rls", height = "500px")
       ),
       
       htmlOutput("iframe")
     )
),

# # Fifth Row - Histograms
card(
  full_screen = TRUE,
  max_height = 800,
  shinyWidgets::pickerInput(
    inputId = "species_length",
    label = "Choose a species:",
    width = "100%",
    choices = c(values$top_species_names_combined),
    multiple = FALSE,
    selected = values$top_species_names_combined[1],
    options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = TRUE)
  ),
  layout_column_wrap(
    width = 1,
    card(card_header("Length frequency histogram"),
         plotOutput("length_histogram", height = "400px")),
    card(card_header("Normalised length frequency histogram"),
         plotOutput("length_histogram_density", height = "400px"))
  )
)
  ),

nav_panel(
  "Explore a Marine Park"),

nav_panel(
  "Status & Trends"),

nav_spacer(),

# Add logos to the top right corner
nav_item(
  tags$div(
    style = "display: flex; gap: 10px; align-items: center; padding-right: 15px;",
    tags$img(src = "dew_logo.png", height = "70px")
    
  )
)
)