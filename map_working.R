ke_county.shp

KenyaSHP <- read_sf("./ke_county.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)


KenyaSHP_geom <- st_geometry(KenyaSHP)

KenyaSHP <- st_transform(KenyaSHP, crs = 4326)

counties_data <- unique(data$County)

counties_KenyaSHP <- KenyaSHP %>% 
  st_drop_geometry() %>% 
  dplyr::select(.,county) %>% 
  pull() %>% 
  unique()

# convert dataframe columns to title case

data <- data %>% 
  ungroup() %>% 
  clean_names() %>% 
  mutate(., county = tools::toTitleCase(tolower(county)))


library(viridis)

### Inspect the county names that are different in each of the datasets
unique(data$county)[which(!unique(data$county) %in% counties_KenyaSHP)]


### Clean the county names so that they match in both datasets
data <- data %>% 
  mutate(county = ifelse(county == "Taita/Taveta", "Taita Taveta",
                         ifelse(county == "Tharaka-Nithi", "Tharaka-nithi",
                                ifelse(county == "Elgeyo/Marakwet", "Elgeyo-marakwet",
                                       ifelse(county == "Nairobi City", "Nairobi", county)))))


unique(data$county)[which(!unique(data$county) %in% counties_KenyaSHP)]


data_2 <- data %>% 
  filter(., admin_area == "County") %>% 
  dplyr::select(., -admin_area, -sub_county)

KenyaSHP$county <- trimws(KenyaSHP$county)
data_2$county <- trimws(data_2$county)

merged_data <- left_join(KenyaSHP, data_2, by = "county")

### Sort the data so that the County variable appears first
merged_data <- merged_data %>% 
  dplyr::select(county, everything())



# Plot using base R
plot(KenyaSHP$geometry, lty = 3, col = "darkgreen")


# Plot using ggplot

map_mpo_female <- ggplot(data = merged_data) +
  geom_sf(aes(geometry = geometry, fill = mpo_female)) +
  theme_void() +
  scale_fill_viridis_c(option = "viridis")

map_mpo_female

# Plot using cartography

# Plot using Leaflet

### Specify the color scheme
pal <- colorBin(
  palette = "YlOrRd",
  domain = merged_data$mpo_female
)

### Specify how labels will be displayed
labels <- sprintf(
  "<strong>%s</strong><br/>%s",
  merged_data$county, formatC(merged_data$mpo_female, format = "d", big.mark = ",")
) %>% lapply(htmltools::HTML)

### Generate the graph
leaflet(merged_data) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addTiles() %>% 
  addPolygons(fillColor = ~pal(mpo_female),
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
  addLegend(position = c("bottomright"), pal = pal, values = ~mpo_female)

