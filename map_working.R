output$map <- renderLeaflet({
  
  leaflet(map_data_df) %>% 
    setView(lng = 36.821945,lat = -0.023559, zoom = 6) %>% 
    # addProviderTiles("MapBox", options = providerTileOptions(
    #   id = "mapbox.light",
    #   accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addTiles() %>%
    leaflet::addLegend(position = c("bottomleft"), 
                       pal = pal, 
                       values = ~map_input(),
                       labFormat = labelFormat(transform = function(x) 100*x),
                       title = switch(input$variableselected,
                                      "mpo_total_perc" ="Phone  Ownership % (Total popn)",
                                      "mpo_female_perc" = "Phone Ownership % (Women)",
                                      "mpo_male_perc" = "Phone  Ownership % (Men)",
                                      "uo_i_total_perc" = "Internet Use % (Total popn.)",
                                      "uo_i_male_perc" = "Internet Use % (Men)",
                                      "uo_i_female_perc" = "Internet Use % (Women)",
                                      "mobile_gender_gap" = "Phone  Ownership, Gender Gap %",
                                      "internet_gender_gap" = "Internet Use, Gender Gap %"))
    
  ### Specify the color scheme
  pal <- colorBin(
    palette = "YlOrRd",
    domain = map_input()
  )
  
  ### Specify how labels will be displayed
  labels <- sprintf(
    "<strong>%s</strong><br/>%s",
    map_data_df$county, map_input()
  ) %>% lapply(htmltools::HTML)
  
  ### Generate the graph.
  l <- leaflet(map_data_df) %>%
    setView(lng = 36.821945,lat = -0.023559, zoom = 6) %>% 
    # addProviderTiles("MapBox", options = providerTileOptions(
    #   id = "mapbox.light",
    #   accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(map_input()),
      color = "black",
      weight = 2,
      opacity = 1,
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 4,
        color = "red",
        dashArray = "",
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    leaflet::addLegend(position = c("bottomleft"), 
                       pal = pal, 
                       values = ~map_input(),
                       labFormat = labelFormat(transform = function(x) 100*x),
                       title = switch(input$variableselected,
                                      "mpo_total_perc" ="Phone  Ownership % (Total popn)",
                                      "mpo_female_perc" = "Phone Ownership % (Women)",
                                      "mpo_male_perc" = "Phone  Ownership % (Men)",
                                      "uo_i_total_perc" = "Internet Use % (Total popn.)",
                                      "uo_i_male_perc" = "Internet Use % (Men)",
                                      "uo_i_female_perc" = "Internet Use % (Women)",
                                      "mobile_gender_gap" = "Phone  Ownership, Gender Gap %",
                                      "internet_gender_gap" = "Internet Use, Gender Gap %"))
  
  
})





