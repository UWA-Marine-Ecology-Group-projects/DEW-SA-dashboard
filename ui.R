page_navbar(
  title = div(
    "Australian BRUV Synthesis",
    favicon = "www/favicon.ico",  # path to your favicon
    style = "flex-grow: 1;"  # This pushes the rest of the nav content (like links/logos) to the right
  ),
  theme = bs_theme(
    bootswatch = "minty", 
    primary = "#009E73",
    secondary = "#0072B2",
    success = "#E69F00",
    base_font = font_google("Roboto"),
    code_font = font_google("Roboto")
  ),
  fillable = FALSE,
  
  # Add Fish Collective as an external link (opens in new tab)
  nav_spacer(),
  # nav_item(
  #   a("Fish Collective", href = "https://fishcollective.github.io/", target = "_blank", class = "nav-link")
  # ),
  
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  
  # Dashboard ----
  nav_panel(
    "Dashboard",
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
      
      # First Card: 6 Colorful Value Boxes (2 across, 3 down)
      card(
        min_height = 600,
        max_height = 700,
        
        full_screen = TRUE,
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
      
      # Second Card: Map of Deployments
      card(
        min_height = 500,
        full_screen = TRUE,
        card_header("Map of deployments"),
        
        leafletOutput("map_deployments", height = "400px"),
        
        # div(style = "margin-bottom: 0em;",
        #     layout_columns(
        #       col_widths = c(4, 8),
        #       # checkboxInput("show_all_years", "Show all years", value = TRUE),
        #       
        #       materialSwitch(inputId = "show_all_years", "Show all years", value = TRUE, width = "100%"),
        #       
        #       conditionalPanel(
        #         condition = "!input.show_all_years",  # Only show slider if NOT showing all
        #         sliderInput("year_filter", "Select Year:",
        #                     width = "100%",
        #                     min = min(dataframes$deployment_locations$year, na.rm = TRUE),
        #                     max = max(dataframes$deployment_locations$year, na.rm = TRUE),
        #                     value = min(dataframes$deployment_locations$year, na.rm = TRUE),
        #                     step = 1,
        #                     sep = "",
        #                     animate = animationOptions(interval = 1500, loop = FALSE))
        #       )
        #     ))
      )
    ),
    
    # Histograms of metadata
    
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Deployments by year"),
        full_screen = TRUE,
        max_height = 400,
        plotOutput("date_hist")
      ),
      
      card(
        card_header("Depth frequency histogram"),
        full_screen = TRUE,
        max_height = 400,
        plotOutput("depth_hist")
      )
    ),
    
    # Third Row - Abundant species Plot and Leaflet (plot1 and map1)
    layout_column_wrap(
      width = 1/2,
      card(
        min_height = 650,
        full_screen = TRUE,
        card_header("Most Abundant Species"),
        plotOutput("top_species_plot", height = "400px")
      ),
      
      card(
        min_height = 650,
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
        
        # selectInput("metric_select", "Choose an assemblage metric", choices = c("Total abundance", "Species richness")),
        leafletOutput("assemblage_map", height = "400px")
      )
      
      
    ),
    
    # Fourth Row - Species map and iframe
    card(full_screen = TRUE,
         
         max_height = 700,
         
         layout_column_wrap(
           width = 1/2,
           
           card(
             full_screen = TRUE,
             card_header("Investigate a species abundance data"),
             
             shinyWidgets::pickerInput(
               inputId = "species_select",
               label = "Choose a species (note this only includes the top 200 most abundant species):",
               width = "100%",
               choices = c(values$top_200_species_names),
               multiple = FALSE,
               selected = values$top_200_species_names[1],
               options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = TRUE)
             ),
             
             leafletOutput("species_map", height = "400px")
           ),
           
           card(
             full_screen = TRUE,
             htmlOutput("iframe")
           )
         )),
    
    # Fifth Row - Histograms
    card(
      full_screen = TRUE,
      max_height = 800,
      shinyWidgets::pickerInput(
        inputId = "species_length",
        label = "Choose a species (note this only includes the top 200 most abundant species):",
        width = "100%",
        choices = c(values$top_200_species_names),
        multiple = FALSE,
        selected = values$top_200_species_names[1],
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

# nav_spacer(),
nav_item(
  a("The Fish Collective", href = "https://fishcollective.github.io/", target = "_blank", class = "nav-link")
),

# Add logos to the top right corner
nav_item(
  tags$div(
    style = "display: flex; gap: 10px; align-items: center; padding-right: 15px;",
    tags$img(src = "Fish Collective_White Cropped.png", height = "40px"),
    tags$img(src = "mac-logo-white-cropped.png", height = "40px"),  
    tags$img(src = "ardc.png", height = "40px")

  )
),
)