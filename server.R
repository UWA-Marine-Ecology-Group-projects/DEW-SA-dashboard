server <- function(input, output) {
  
  output$map_deployments <- renderLeaflet({
    leaflet() %>%
      addTiles(options = providerTileOptions(minZoom = 1, maxZoom = 8)) %>%
      setView(lng = values$mean_lon, lat = values$mean_lat, zoom = 4) %>%
      
      # Static polygon layers
      # leafgl::addGlPolygons(data = state.mp,
      #                       color = "black", 
      #                       weight = 1,
      #                       fillColor = ~state.pal(zone), 
      #                       fillOpacity = 0.8,
      #                       group = "State Marine Parks",
      #                       popup = state.mp$COMMENTS) %>%
      
      leafgl::addGlPolygons(data = commonwealth.mp,
                            color = "black",
                            weight = 1,
                            fillColor = ~commonwealth.pal(zone),
                            fillOpacity = 0.8,
                            group = "Australian Marine Parks",
                            popup = commonwealth.mp$ZoneName) %>%

      # Legends
      # addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
      #           title="State Zones", position = "bottomright", group = "State Marine Parks") %>%

      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones", position = "bottomright", group = "Australian Marine Parks") %>%

      addLayersControl(
        overlayGroups = c("Australian Marine Parks", 
                          # "State Marine Parks", 
                          "Sampling locations"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) %>%
      # hideGroup("State Marine Parks") %>%
      hideGroup("Australian Marine Parks")
  })
  
  observe({
    all_points <- dataframes$deployment_locations
    points <- all_points
    
    # points <- if (input$show_all_years) {
    #   all_points
    # } else {
    #   dplyr::filter(all_points, year == input$year_filter)
    # }
    
    cols <- colour_values_rgb(-points$depth_m, palette = "viridis", include_alpha = FALSE) / 255
    
    depth_pal <- colorNumeric(
      palette = rev(viridisLite::viridis(256)),
      domain = all_points$depth_m  # Keep full domain for consistent color legend
    )
    
    leafletProxy("map_deployments", data = points) %>%
      clearGroup("Sampling locations") %>%
      leafgl::addGlPoints(
        data = points,
        fillColor = cols,
        weight = 1,
        popup = points$popup,
        group = "Sampling locations"
      ) %>%
      clearControls() %>%
      addLegend(
        "bottomleft",
        pal = depth_pal,
        values = all_points$depth_m,
        title = "Depth (m)",
        opacity = 1,
        group = "Sampling locations"
      )
  })
  
  # output$map_deployments <- renderLeaflet({
  #   # Base data
  #   all_points <- dataframes$deployment_locations
  #   
  #   # Filter if needed
  #   if (!input$show_all_years) {
  #     points <- dplyr::filter(all_points, year == input$year_filter)
  #   } else {
  #     points <- all_points
  #   }
  #   
  #   cols <- colour_values_rgb(
  #     -points$depth_m,  # flip the values so deep becomes shallow, etc.
  #     palette = "viridis",
  #     include_alpha = FALSE
  #   ) / 255
  #   
  #   # 1. Create palette and colours
  #   depth_pal <- colorNumeric(
  #     palette = rev(viridisLite::viridis(256)),  # Reverse viridis palette
  #     domain = points$depth_m
  #   )
  #   
  #   map <- leaflet(points) %>%
  #     addTiles() %>%
  #     
  #     addGlPoints(data = points, 
  #                 fillColor = cols, 
  #                 weight = 1,
  #                 popup = points$popup,
  #                 group = "Sampling locations") %>%
  #     
  #     setView(lng = values$mean_lon, lat = values$mean_lat, zoom = 4) %>%
  #     
  #     # State Marine Parks
  #     leafgl::addGlPolygons(data = state.mp,
  #                           color = "black", 
  #                           weight = 1,
  #                           fillColor = ~state.pal(zone), 
  #                           fillOpacity = 0.8,
  #                           group = "State Marine Parks",
  #                           popup = state.mp$COMMENTS) %>%
  # 
  #     # Add a legend
  #     addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
  #               title="State Zones",
  #               position = "bottomright", group = "State Marine Parks") %>%
  #     
  #     # Commonwealth Marine Parks
  #     leafgl::addGlPolygons(data = commonwealth.mp,
  #                           color = "black", 
  #                           weight = 1,
  #                           fillColor = ~commonwealth.pal(zone), 
  #                           fillOpacity = 0.8,
  #                           group = "Australian Marine Parks",
  #                           popup = commonwealth.mp$ZoneName) %>%
  # 
  #     # Add a legend
  #     addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
  #               title="Australian Marine Park Zones",
  #               position = "bottomright", group = "Australian Marine Parks") %>%
  #     
  #     addLegend(
  #       "bottomleft",
  #       pal = depth_pal,
  #       values = points$depth_m,
  #       title = "Depth (m)",
  #       opacity = 1, group = "Sampling locations"
  #     ) %>%
  #     
  #     addLayersControl(
  #       overlayGroups = c("Australian Marine Parks",
  #                         "State Marine Parks",
  #                         "Sampling locations"),
  #       options = layersControlOptions(collapsed = FALSE),
  #       position = "topright"
  #     )  %>% 
  #     hideGroup("State Marine Parks") %>%
  #     hideGroup("Australian Marine Parks")
  #     
  # })
  
  output$species_map <- renderLeaflet({
    
    data <- dataframes$bubble_data_200 %>%
      dplyr::filter(display_name %in% input$species_select)
    
    data <- full_join(data, dataframes$deployment_locations) %>%
      tidyr::replace_na(list(count = 0)) %>%
      glimpse()
    
    max_ab <- ifelse(nrow(data) > 0, max(data$count, na.rm = TRUE), 1)  # Avoid errors
    
    overzero <- filter(data, count > 0) # %>% glimpse()
    overzero = st_as_sf(overzero#, coords = c("longitude_dd", "latitude_dd")
                        )
    
    equalzero <- filter(data, count == 0)
    equalzero = st_as_sf(equalzero#, coords = c("longitude_dd", "latitude_dd")
                         )
    
    # Initial Leaflet map ----
    map <- leaflet(data) %>%
      addTiles(options = providerTileOptions(minZoom = 1, maxZoom = 8)) %>%
      
      setView(lng = values$mean_lon, lat = values$mean_lat, zoom = 4) %>%
      
      # State Marine Parks
      # leafgl::addGlPolygons(data = state.mp,
      #                       color = "black", 
      #                       weight = 1,
      #                       fillColor = ~state.pal(zone), 
      #                       fillOpacity = 0.8,
      #                       group = "State Marine Parks",
      #                       popup = state.mp$COMMENTS) %>%
      
      # Add a legend
      # addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
      #           title="State Zones",
      #           position = "bottomright", group = "State Marine Parks") %>%
      
      # Commonwealth Marine Parks
      leafgl::addGlPolygons(data = commonwealth.mp,
                            color = "black", 
                            weight = 1,
                            fillColor = ~commonwealth.pal(zone), 
                            fillOpacity = 0.8,
                            group = "Australian Marine Parks",
                            popup = commonwealth.mp$ZoneName) %>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks") %>%
      
      addLayersControl(
        overlayGroups = c("Australian Marine Parks",
                          "State Marine Parks",
                          "Sampling locations"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      )  %>%
      
      hideGroup("State Marine Parks") %>%
      hideGroup("Australian Marine Parks")%>%
      add_legend(colors = c("white", "green", "green"),
                 labels = c(0, round(max_ab / 2), max_ab),
                 sizes = c(5, 20, 40),
                 title = "Abundance",
                 group = "Sampling locations"
      )
    
    if (nrow(equalzero)) {
      map <- map %>%
        
        addGlPoints(data = equalzero, 
                    fillColor = "white", 
                    fillOpacity = 0.5,
                    weight = 1,
                    radius = 10,
                    popup = as.character(equalzero$count),
                    group = "Sampling locations") 
    }
    
    if (nrow(overzero)) {
      map <- map %>%
        
        addGlPoints(data = overzero, 
                    fillColor = "#009E73", 
                    fillOpacity = 1,
                    weight = 1,
                    radius = ~ ((10 + (overzero$count / max_ab) * 50)),
                    popup = as.character(overzero$count),
                    group = "Sampling locations") #%>%
    }
    

    
    map
  })
  
  # Assemblage map ----
  output$assemblage_map <- renderLeaflet({
    
    assemblage_metric <- tolower(str_replace_all(input$assemblage, " ", "_"))
    
    data <- dataframes$metric_bubble_data %>%
      dplyr::filter(metric %in% assemblage_metric)
    
    max_ab <- ifelse(nrow(data) > 0, max(data$value, na.rm = TRUE), 1)  # Avoid errors
    
    overzero <- filter(data, value > 0)
    overzero = st_as_sf(overzero, coords = c("longitude_dd", "latitude_dd"))
    
    equalzero <- filter(data, value == 0)
    equalzero = st_as_sf(equalzero, coords = c("longitude_dd", "latitude_dd"))
    
    # Initial Leaflet map ----
    map <- leaflet(data) %>%
      addTiles(options = providerTileOptions(minZoom = 1, maxZoom = 8)) %>%
      
      setView(lng = values$mean_lon, lat = values$mean_lat, zoom = 4) %>%
      
      # State Marine Parks
      # leafgl::addGlPolygons(data = state.mp,
      #                       color = "black", 
      #                       weight = 1,
      #                       fillColor = ~state.pal(zone), 
      #                       fillOpacity = 0.8,
      #                       group = "State Marine Parks",
      #                       popup = state.mp$COMMENTS) %>%
      
      # Add a legend
      # addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
      #           title="State Zones",
      #           position = "bottomright", group = "State Marine Parks") %>%
      
      # Commonwealth Marine Parks
      leafgl::addGlPolygons(data = commonwealth.mp,
                            color = "black", 
                            weight = 1,
                            fillColor = ~commonwealth.pal(zone), 
                            fillOpacity = 0.8,
                            group = "Australian Marine Parks",
                            popup = commonwealth.mp$ZoneName) %>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks") %>%
      
      addLayersControl(
        overlayGroups = c("Australian Marine Parks",
                          "State Marine Parks",
                          "Sampling locations"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      )  %>%
      
      hideGroup("State Marine Parks") %>%
      hideGroup("Australian Marine Parks")%>%
      add_legend(colors = c("white", "green", "green"),
                 labels = c(0, round(max_ab / 2), max_ab),
                 sizes = c(5, 20, 40),
                 title = input$assemblage,
                 group = "Sampling locations"
      )

    if (nrow(overzero)) {
      map <- map %>%
        
        addGlPoints(data = overzero, 
                    fillColor = "#009E73", 
                    fillOpacity = 1,
                    weight = 1,
                    radius = ~ ((10 + (overzero$value / max_ab) * 50)),
                    popup = as.character(overzero$value),
                    group = "Sampling locations") #%>%
    }

    if (nrow(equalzero)) {
      map <- map %>%
        
        addGlPoints(data = equalzero, 
                    fillColor = "white", 
                    fillOpacity = 0.5,
                    weight = 1,
                    radius = 10,
                    popup = as.character(equalzero$value),
                    group = "Sampling locations") 
    }
    
    map
  })
  
  ## Top twenty most common species ----
  output$top_species_plot <- renderPlot({
    
    data <- dataframes$top_species |>
      tidyr::extract(display_name, into = c("sci", "common"),
                     regex = "^(.*?)\\s*\\((.*?)\\)$", remove = FALSE) |>
      dplyr::mutate(
        label = paste0("<i>", sci, "</i><span> (", common, ")</span>")
      )
    
    ggplot2::ggplot(data, ggplot2::aes(x = reorder(label, total_number), y = total_number)) +
      ggplot2::geom_bar(stat = "identity", fill = "#0072B2", col = "black") +
      ggplot2::coord_flip() +
      ggplot2::xlab("Species") +
      ggplot2::ylab("Overall abundance") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
      ggplot_theme +  # Your custom theme
      ggplot2::theme(axis.text.y = ggtext::element_markdown(size = 12))  # <-- Apply AFTER
  })
  
  # Create species iframe
  output$iframe <- renderUI({
      
      dat <- dataframes$foa_codes[display_name %in% c(input$species_select)] %>%
        dplyr::distinct(url) %>%
        dplyr::pull("url")
      
    frame <- tags$iframe(src = paste0(dat),
                         style = "width: 100%; height: 100vh; border: none;",
                         onload = "resizeIframe(this)"
    )
    
    frame
    
  })
  
  output$length_histogram <- renderPlot({
    
    length <- dataframes$length_200_with_jurisdiction %>%
      dplyr::filter(display_name %in% input$species_length)
    
    # metadata <- all_data$synthesis_metadata
    # 
    # metadata <- metadata %>%
    #   dplyr::filter(network %in% input$network) %>%
    #   dplyr::filter(marine_park %in% input$marine_park) %>%
    #   dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)
    
    ggplot(length, aes(x = length_mm)) +
      geom_histogram(binwidth = 10, fill = "#0072B2", color = "black") +
      facet_grid(status ~ jurisdiction, scales = "free_y") +
      ggplot_theme +
      labs(
        x = "Length (mm)",
        y = "Frequency"
      )
    
  })
  
  
  output$length_histogram_density <- renderPlot({
    
    length <- dataframes$length_200_with_jurisdiction %>%
      dplyr::filter(display_name %in% input$species_length)
    
    ggplot(length, aes(x = length_mm, fill = status
                       )) +
      geom_histogram(aes(y = ..density..), binwidth = 10, fill = "#0072B2", color = "black", position = "identity") +
      facet_grid(status ~ jurisdiction, scales = "free_y") +
      labs(
        # title = "Normalized Histograms of Fish Length",
        x = "Length (mm)",
        y = "Proportion (Density)"
      ) +
      ggplot_theme
    
  })
  
  
  output$depth_hist <- renderPlot({

    plots$depth_hist
    
  })
  
  output$date_hist <- renderPlot({
    
    plots$date_hist

  })
  
  # Set priorities so plots render before map
  outputOptions(output, "map_deployments", priority = 5)
  outputOptions(output, "depth_hist", priority = 1)
  outputOptions(output, "date_hist", priority = 2)
  
}