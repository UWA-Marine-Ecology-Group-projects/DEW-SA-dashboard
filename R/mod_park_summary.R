mod_park_summary_server <- function(id, park_r, dataframes, values, plots) {
  moduleServer(id, function(input, output, session) {
    
    # Resolve current park once per reactive invalidation ----
    current_park <- reactive({
      if (!is.null(park_r)) {
        park_r()               # reactive path (sidebar selector)
      } else {
        park_name              # static path (tabs-per-park)
      }
    })
    
    # Park heading ----
    output$parkheading <- renderText(
      current_park()
    )
    
    # Filter data ----
    df <- reactive({
      pk <- current_park()
      
      list(
        deployment_locations      = filter_by_location(dataframes$deployment_locations, pk),
        deployment_locations_rls  = filter_by_location(dataframes$deployment_locations_rls, pk),
        metric_bubble_data        = filter_by_location(dataframes$metric_bubble_data, pk),
        metric_bubble_data_rls    = filter_by_location(dataframes$metric_bubble_data_rls, pk),
        bubble_data               = filter_by_location(dataframes$bubble_data, pk),
        bubble_data_rls           = filter_by_location(dataframes$bubble_data_rls, pk),
        length_with_jurisdiction  = filter_by_location(dataframes$length_with_jurisdiction, pk),
        #foa_codes                 = dataframes$foa_codes,
        top_species_location      = filter_by_location(dataframes$top_species_location, pk),
        years_park                = filter_by_location(dataframes$years_park, pk),
        depth_combined            = filter_by_location(dataframes$depth_combined, pk)
      )
      
    })
    
    # All deployments for that location, set bounds for maps ----
    deployments <- reactive({
      dat <- bind_rows(df()$deployment_locations, df()$deployment_locations_rls)
    }
    )
    
    min_lat <- reactive({min(deployments()$latitude_dd, na.rm = TRUE)})
    min_lon <- reactive({min(deployments()$longitude_dd, na.rm = TRUE)})
    max_lat <- reactive({max(deployments()$latitude_dd, na.rm = TRUE)})
    max_lon <- reactive({max(deployments()$longitude_dd, na.rm = TRUE)})
    
    # Update species picker ----
    species_location <- reactive({
      dat <- df()$top_species_location %>%
        dplyr::group_by(display_name) %>%
        dplyr::summarise(total_number = sum(total_number))
      
      # Safety: bail if no data
      if (is.null(dat) || !nrow(dat)) return()
      dat <- dat[order(dat$total_number, decreasing = TRUE), ]
      
      # Extract and sort unique species names
      species_choices <- unique(dat$display_name)
      
      species_choices
    })
    
    # TODO order this by abundance in location
    observe({
      # Update the pickerInput inside the module
      updatePickerInput(
        session = session,
        inputId = "spe_select",
        choices = species_location(),
        selected = species_location()[1]  # optional default
      )
    })
    
    observe({
      # Update the pickerInput inside the module
      updatePickerInput(
        session = session,
        inputId = "spe_length",
        choices = species_location(),
        selected = species_location()[1]  # optional default
      )
    })
    
    # Map of deployments ----
    output$map_combined_park <- renderLeaflet({
      
      dat <- bind_rows(df()$deployment_locations, df()$deployment_locations_rls)
      
      m <- base_map() |> 
        addMapPane("points", zIndex = 420)
      
      if (!nrow(dat)) return(m |> setView(lng = 120, lat = -30, zoom = 5))
      
      pts <- ensure_sf_ll(dat)
      
      # method → colour mapping
      method_cols <- c("stereo-BRUVs" = "#f89f00", "UVC" = "#0c3978")
      
      m <- m |> 
        fitBounds(min_lon(), min_lat(), max_lon(), max_lat())
      
      # add points (no curly block after a pipe)
      if (has_leafgl()) {
        m <- leafgl::addGlPoints(
          m, 
          data = pts, 
          fillColor = method_cols[pts$method], 
          weight = 1, 
          popup = pts$popup, 
          group = "Sampling locations", pane = "points"
        )
      } else {
        m <- addCircleMarkers(
          m, data = pts, radius = 6, fillColor = "#f89f00", fillOpacity = 1,
          weight = 1, color = "black", popup = pts$popup,
          group = "Sampling locations", options = pathOptions(pane = "points")
        )
      }
      
      addLegend(m,
                "topright",
                colors = unname(method_cols),
                labels = names(method_cols),
                title = "Survey method",
                opacity = 1,
                group = "Sampling locations",
                layerId = "methodLegend"
      )
    })
    
    # Years histogram ----
    output$date_hist_combined <- renderPlot({
      dat <- df()$years_park
      y_min <- floor(min(dat$year, na.rm = TRUE))
      y_max <- ceiling(max(dat$year, na.rm = TRUE))
      yrs   <- seq(y_min, y_max, by = 1)
      
      ggplot(dat, aes(x = year, y = n, fill = method)) +
        geom_col(width = 0.8, color = "black") +        # narrow bars (in "years")
        labs(x = "Year", y = "Number of deployments/surveys") +
        scale_x_continuous(
          breaks = yrs,                                  # tick every year
          labels = yrs,                                  # label every year (or thin if crowded)
          limits = c(y_min - 0.5, y_max + 0.5),          # keep gaps on both sides
          expand = expansion(mult = c(0, 0.02))
        ) +
        scale_fill_manual(values = c("stereo-BRUVs" = "#f89f00", "UVC" = "#0c3978")) +
        ggplot_theme +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    # Depth histogram ----
    output$depth_hist_combined <- renderPlot({
      dat <- df()$depth_combined
      
      ggplot(dat, aes(x = depth_m, fill = method)) +
        geom_histogram(binwidth = 5, color = "black") +
        xlab("Depth (m)") +
        ylab("Number of deployments/surveys") +
        scale_fill_manual(values = c("stereo-BRUVs" = "#f89f00", "UVC" = "#0c3978")) +
        ggplot_theme
    })
    
    # Most abundant species stacked ----
    output$top_species_plot <- renderPlot({
      data <- df()$top_species_location %>%  # both methods
        tidyr::extract(
          display_name, into = c("sci", "common"),
          regex = "^(.*?)\\s*\\((.*?)\\)$", remove = FALSE
        ) %>%
        dplyr::mutate(
          label = paste0("<i>", sci, "</i><span> (", common, ")</span>")
        )
      
      ggplot2::ggplot(data, aes(
        x = reorder_within(label, total_number, method),
        y = total_number,
        fill = method
      )) +
        geom_bar(stat = "identity", 
                 # fill = "#0c3978", 
                 col = "black") +
        coord_flip() +
        xlab("Species") +
        ylab("Overall abundance") +
        scale_y_continuous(expand = expansion(mult = c(0, .1))) +
        scale_x_reordered() +
        scale_fill_manual(values = c("stereo-BRUVs" = "#f89f00", "UVC" = "#0c3978")) +
        ggplot_theme +
        theme(axis.text.y = ggtext::element_markdown(size = 12)) +
        facet_wrap(vars(method), scales = "free")
    })
    
    # New maps ----
    # init only (no req(), no df(), no inputs here)
    output$assem_map <- renderLeaflet({
      base_map() |> addMapPane("points", zIndex = 420) |> fitBounds(min_lon(), min_lat(), max_lon(), max_lat())
    })
    
    output$assem_map_rls <- renderLeaflet({
      base_map() |> addMapPane("points", zIndex = 420) |> fitBounds(min_lon(), min_lat(), max_lon(), max_lat())
    })
    
    output$spe_map <- renderLeaflet({
      base_map() |> addMapPane("points", zIndex = 420) |> fitBounds(min_lon(), min_lat(), max_lon(), max_lat())
    })
    
    output$spe_map_rls <- renderLeaflet({
      base_map() |> addMapPane("points", zIndex = 420) |> fitBounds(min_lon(), min_lat(), max_lon(), max_lat())
    })
    
    # Assemblage map data ----
    assem_data <- reactive({
      req(input$assem_park)
      metric_name <- input$assem_park |> tolower() |> stringr::str_replace_all(" ", "_") 
      
      dat <- df()$metric_bubble_data %>%
        filter(metric %in% c(metric_name))
      
      dat
    })
    
    outputOptions(output, "assem_map", suspendWhenHidden = FALSE)
    
    # Assemblage map ----
    observeEvent(assem_data(), {
      dat_sf <- assem_data() %>% sf::st_as_sf(coords = c("longitude_dd", "latitude_dd")) 
      max_ab <- max(dat_sf$value, na.rm = TRUE)
      
      overzero  <- dplyr::filter(dat_sf, value > 0)
      equalzero <- dplyr::filter(dat_sf, value %in% 0)
      
      if (nrow(overzero))  overzero  <- ensure_sf_ll(overzero)
      if (nrow(equalzero)) equalzero <- ensure_sf_ll(equalzero)
      
      # bubble legend (your helper)
      m <- leafletProxy("assem_map", session = session) |>
        clearGroup("Sampling locations") %>%
        add_bubble_legend(max_val = max_ab, title = input$assem_park, methodcol = "#f89f00")
      
      # add points
      if (has_leafgl()) {
        if (nrow(overzero)) {
          overzero$radius <- 10 + (overzero$value / max_ab) * 50
          m <- leafgl::addGlPoints(
            m, 
            data = overzero, 
            fillColor = "#f89f00", 
            fillOpacity = 1, 
            weight = 1,
            radius = overzero$radius, 
            popup = as.character(overzero$value),
            group = "Sampling locations", 
            pane = "points"
          )
        }
        
        if (nrow(equalzero)) {
          m <- addCircleMarkers(
            m, 
            data = equalzero, 
            radius = 5, 
            fillColor = "white",
            fillOpacity = 1, 
            weight = 1, 
            color = "black",
            popup = as.character(equalzero$value),
            group = "Sampling locations", 
            options = pathOptions(pane = "points")
          )
        }
      }
      m
    }, ignoreInit = FALSE)
    
    # Assemblage map data RLS ----
    assem_data_rls <- reactive({
      req(input$assem_park)
      metric_name <- input$assem_park |> tolower() |> stringr::str_replace_all(" ", "_")
      
      dat <- df()$metric_bubble_data_rls %>%
        filter(metric %in% c(metric_name))
      
      dat
    })
    
    outputOptions(output, "assem_map_rls", suspendWhenHidden = FALSE)
    
    # Assemblage map RLS ----
    observeEvent(assem_data_rls(), {
      dat_sf <- assem_data_rls() %>% sf::st_as_sf(coords = c("longitude_dd", "latitude_dd")) 
      max_ab <- max(dat_sf$value, na.rm = TRUE)
      
      overzero  <- dplyr::filter(dat_sf, value > 0)
      equalzero <- dplyr::filter(dat_sf, value %in% 0)
      
      if (nrow(overzero))  overzero  <- ensure_sf_ll(overzero)
      if (nrow(equalzero)) equalzero <- ensure_sf_ll(equalzero)
      
      # bubble legend (your helper)
      m <- leafletProxy("assem_map_rls", session = session) |>
        clearGroup("Sampling locations") %>%
        add_bubble_legend(max_val = max_ab, title = input$assem_park, methodcol = "#0c3978")
      
      # add points
      if (has_leafgl()) {
        if (nrow(overzero)) {
          overzero$radius <- 10 + (overzero$value / max_ab) * 50
          m <- leafgl::addGlPoints(
            m, 
            data = overzero, 
            fillColor = "#0c3978", 
            fillOpacity = 1, 
            weight = 1,
            radius = overzero$radius, 
            popup = as.character(overzero$value),
            group = "Sampling locations", 
            pane = "points"
          )
        }
        
        if (nrow(equalzero)) {
          m <- addCircleMarkers(
            m, 
            data = equalzero, 
            radius = 5, 
            fillColor = "white",
            fillOpacity = 1, 
            weight = 1, 
            color = "black",
            popup = as.character(equalzero$value),
            group = "Sampling locations", 
            options = pathOptions(pane = "points")
          )
        }
      }
      m
    }, ignoreInit = FALSE)
    
    # Species map data ----
    spe_data <- reactive({
      req(input$spe_select)
      dat <- df()$bubble_data %>%
        dplyr::filter(display_name %in% input$spe_select) %>%
        dplyr::full_join(df()$deployment_locations %>% dplyr::select(sample_url, location, latitude_dd, longitude_dd)) %>% 
        tidyr::replace_na(list(count = 0))
      
      dat
    })
    
    outputOptions(output, "spe_map", suspendWhenHidden = FALSE)
    
    # Species map ----
    observeEvent(spe_data(), {
      dat_sf <- spe_data() %>% sf::st_as_sf(coords = c("longitude_dd", "latitude_dd")) 
      max_ab <- max(dat_sf$count, na.rm = TRUE)
      
      overzero  <- dplyr::filter(dat_sf, count > 0) #%>% glimpse()
      equalzero <- dplyr::filter(dat_sf, count %in% 0) #%>% glimpse()
      
      if (nrow(overzero))  overzero  <- ensure_sf_ll(overzero)
      if (nrow(equalzero)) equalzero <- ensure_sf_ll(equalzero)
      
      # bubble legend (your helper)
      m <- leafletProxy("spe_map", session = session) |>
        clearGroup("Sampling locations") %>%
        add_bubble_legend(max_val = max_ab, 
                          title = "Total abundance", 
                          methodcol = "#f89f00")
      
      # # add points
      if (has_leafgl()) {
        if (nrow(overzero)) {
          overzero$radius <- 10 + (overzero$count / max_ab) * 50
          m <- leafgl::addGlPoints(
            m,
            data = overzero,
            fillColor = "#f89f00",
            fillOpacity = 1,
            weight = 1,
            radius = overzero$radius,
            popup = as.character(overzero$count),
            group = "Sampling locations",
            pane = "points"
          )
        }
      #   
        if (nrow(equalzero)) {
          m <- addCircleMarkers(
            m, 
            data = equalzero, 
            radius = 5, 
            fillColor = "white",
            fillOpacity = 1, 
            weight = 1, 
            color = "black",
            popup = as.character(equalzero$count),
            group = "Sampling locations", 
            options = pathOptions(pane = "points")
          )
        }
      }
      m
    }, ignoreInit = FALSE)
    
    # Species map data RLS----
    spe_data_rls <- reactive({
      req(input$spe_select)
      dat <- df()$bubble_data_rls %>%
        dplyr::filter(display_name %in% input$spe_select) %>%
        dplyr::full_join(df()$deployment_locations_rls #%>% dplyr::select(sample_url, location, latitude_dd, longitude_dd)
                         ) %>% 
        tidyr::replace_na(list(count = 0))
      
      dat
    })
    
    outputOptions(output, "spe_map_rls", suspendWhenHidden = FALSE)
    
    # Species map RLS----
    observeEvent(spe_data_rls(), {
      dat_sf <- spe_data_rls() %>% sf::st_as_sf(coords = c("longitude_dd", "latitude_dd")) 
      max_ab <- max(dat_sf$count, na.rm = TRUE)
      
      overzero  <- dplyr::filter(dat_sf, count > 0) #%>% glimpse()
      equalzero <- dplyr::filter(dat_sf, count %in% 0)#%>% glimpse()
      
      if (nrow(overzero))  overzero  <- ensure_sf_ll(overzero)
      if (nrow(equalzero)) equalzero <- ensure_sf_ll(equalzero)
      
      # bubble legend (your helper)
      m <- leafletProxy("spe_map_rls", session = session) |>
        clearGroup("Sampling locations") %>%
        add_bubble_legend(max_val = max_ab, 
                          title = "Total abundance", 
                          methodcol = "#0c3978")
      
      # # add points
      if (has_leafgl()) {
        if (nrow(overzero)) {
          overzero$radius <- 10 + (overzero$count / max_ab) * 50
          m <- leafgl::addGlPoints(
            m,
            data = overzero,
            fillColor = "#0c3978",
            fillOpacity = 1,
            weight = 1,
            radius = overzero$radius,
            popup = as.character(overzero$count),
            group = "Sampling locations",
            pane = "points"
          )
        }
        #   
        if (nrow(equalzero)) {
          m <- addCircleMarkers(
            m, 
            data = equalzero, 
            radius = 5, 
            fillColor = "white",
            fillOpacity = 1, 
            weight = 1, 
            color = "black",
            popup = as.character(equalzero$count),
            group = "Sampling locations", 
            options = pathOptions(pane = "points")
          )
        }
      }
      m
    }, ignoreInit = FALSE)
    
    output$iframe <- renderUI({
      dat <- dataframes$foa_codes[display_name %in% c(input$spe_select)] %>%
        dplyr::distinct(url) %>%
        dplyr::pull("url")
      
      tags$iframe(
        src = paste0(dat),
        style = "width: 100%; height: 100vh; border: none;",
        onload = "resizeIframe(this)"
      )
    })
    
    output$length_hist <- renderPlot({
      length <- df()$length_with_jurisdiction %>%
        dplyr::filter(display_name %in% input$spe_length)
      
      ggplot(length, aes(x = length_mm, fill = method)) +
        geom_histogram(binwidth = input$binwidth, #fill = "#0c3978", 
                       color = "black") +
        facet_grid(status ~ jurisdiction, scales = "free_y") +
        scale_fill_manual(values = c("stereo-BRUVs" = "#f89f00", "UVC" = "#0c3978")) +
        ggplot_theme +
        labs(
          x = "Length (mm)",
          y = "Frequency"
        )
    })
    
    # Length histogram ----
    output$length_hist_density <- renderPlot({
      length <- df()$length_with_jurisdiction %>%
        dplyr::filter(display_name %in% input$spe_length)
      
      ggplot(length, aes(x = length_mm, fill = status)) +
        geom_histogram(aes(y = ..density.., fill = method), binwidth = input$binwidth, #fill = "#0c3978",
                       color = "black", position = "identity") +
        facet_grid(status ~ jurisdiction, scales = "free_y") +
        labs(x = "Length (mm)", y = "Proportion (Density)") +
        scale_fill_manual(values = c("stereo-BRUVs" = "#f89f00", "UVC" = "#0c3978")) +
        ggplot_theme
    })
    
  })
}

mod_park_summary_ui <- function(id, title = NULL) {
  ns <- NS(id)
  tagList(
    
    div(
      style = "text-align: center; margin-bottom: 20px;",
      h3(textOutput(ns("parkheading")))
    ),
    
    layout_column_wrap(
      min_height = 800,
      width = 1/2,
      
      card(
        min_height = 500,
        full_screen = TRUE,
        card_header(if (is.null(title)) "Map of Sampling Effort" else paste(title, "– Sampling Effort")),
        leafletOutput(ns("map_combined_park"), height = "400px")),
      
      div(
        card(
          card_header("Sampling Effort by Year"),
          full_screen = TRUE, max_height = 390,
          plotOutput(ns("date_hist_combined"))
        ),
        card(
          card_header("Sampling Effort by Depth"),
          full_screen = TRUE, max_height = 390,
          plotOutput(ns("depth_hist_combined"))
        )
      )
    ),
    
    card(
      min_height = 650, full_screen = TRUE,
      card_header("Most Abundant Species"),
      plotOutput(ns("top_species_plot"), height = "400px")
    ),
    
    card(
      min_height = 750, 
      card_header("Spatial distribution of assemblage metrics"),
      
      shinyWidgets::pickerInput(
        inputId = ns("assem_park"), 
        label = "Choose an assemblage metric:",
        width = "100%", 
        choices = c("Total abundance", "Species richness"),
        multiple = FALSE, 
        selected = "Total abundance",
        options = list(`actions-box` = TRUE, `live-search` = FALSE, `dropup-auto` = FALSE)
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        max_height = 550,
        div(h4("Stereo-BRUVs"), 
            leafletOutput(ns("assem_map"), height = "58vh")),
        div(h4("UVC"),          
            leafletOutput(ns("assem_map_rls"), height = "58vh"))
      )
    ),
    
    card(
      full_screen = TRUE, 
      min_height = 750,
      card_header("Investigate a species abundance data"),
      
      shinyWidgets::pickerInput(
        inputId = ns("spe_select"),
        label = "Choose a species:",
        width = "100%",
        choices = NULL,           # initially empty
        multiple = FALSE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `dropup-auto` = TRUE
        )
      ),
      layout_columns(col_widths = c(4, 4, 4),
                     max_height = 550,
                     div(h4("Stereo-BRUVs"), 
                         leafletOutput(ns("spe_map"), height = "500px")),
                     div(h4("UVC"),          
                         leafletOutput(ns("spe_map_rls"), height = "500px")),
                     htmlOutput(ns("iframe"))
      )
    ),
    
    card(
      full_screen = TRUE, 
      max_height = 750,
      min_height = 700,
      layout_column_wrap(width = 1/2,
                         shinyWidgets::pickerInput(
                           inputId = ns("spe_length"),
                           label = "Choose a species:",
                           width = "100%",
                           choices = NULL,           # initially empty
                           multiple = FALSE,
                           options = list(
                             `actions-box` = TRUE,
                             `live-search` = TRUE,
                             `dropup-auto` = TRUE
                           )
                         ),
                         numericInput(ns("binwidth"), value = 50, min = 0, max = 1000, label = "Choose binwidth (mm):")
      ),
      layout_column_wrap(width = 1/2,
                         min_height = 400,
                         max_height = 600,
                         card(card_header("Length frequency histogram"),
                              plotOutput(ns("length_hist"), height = "600px")),
                         card(card_header("Normalised length frequency histogram"),
                              plotOutput(ns("length_hist_density"), height = "600px"))
      )
    )
  )
}

