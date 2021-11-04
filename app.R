
# Resources:
    # https://github.com/johnmutiso/rKenyaCensusPlots/blob/master/Persons_per_household_map.R
    # https://github.com/brynmwangy/2019-Kenya-Census-App/blob/master/app.R
    # dashboardBody options : https://rstudio.github.io/shinydashboard/structure.html

# Next Steps
    # start with building out MPO overview tab - add valuebox showing summary info for entire country
        # add data to server function, and connect to outputs******
        # add data to map to MPO overview: (fix rename argument)
          #https://shelkariuki.netlify.app/post/firstmap/
          # https://rpubs.com/spoonerf/countrymapggplot2
    # Build out MPO gender tab — Map with mpo ownership by county + top counties by MPO

#library(devtools)
devtools::install_github('Shelmith-Kariuki/rKenyaCensus')

# Shapefiles from: https://data.humdata.org/dataset/47-counties-of-kenya

library(rKenyaCensus)

library(shiny)
library(shinydashboard)

library(ggplot2)
library(RColorBrewer)
library(plotly)
library(htmltools)
library(colorspace)
library(ggrepel)

library(dplyr)
library(DT)
library(tidyr)
library(janitor)
library(tidyverse)

library(sf)
library(sp)
library(magrittr)
library(maptools)
#library(raster)
library(plyr)
library(rgdal)
library(viridis)


# Map data ---------------------------------------------------------
# KECounties <- rKenyaCensus::KenyaCounties_SHP %>%
#     sf::st_as_sf() %>%
#     select(County, Area, geometry)
# 
# KenyaSHP <- read_sf("./kenya-counties.shp", quiet = TRUE, 
#                     stringsAsFactors = FALSE, as_tibble = TRUE)
# print(KenyaSHP[6:9], n = 3)



# Kenya<-getData("GADM", country="KE", level=0)
# kenya.map <-getData("GADM", country="KE", level=1)
# 
# # plot(Kenya)
# plot(kenya.map)

# Load Map data  ---------------------------------------------------------

KenyaSHP <- read_sf("./ke_county.shp", quiet = TRUE, 
                    stringsAsFactors = FALSE,as_tibble = TRUE)

# Modify map projection
KenyaSHP <- st_transform(KenyaSHP, crs = 4326)

# Load Census data  ---------------------------------------------------------
mob.phone.df <- V4_T2.32
internet.use.df <- V4_T2.33

df_list <- list(mob.phone.df, internet.use.df)

# Join mobile phone and internet data --------------------------------------
ke_data <- df_list %>% 
  reduce(left_join, by = c("County", "SubCounty", "AdminArea")) %>%
  clean_names(., case = "upper_camel") %>% 
  dplyr::select(., - one_of(c("TotalY", "MaleY", "FemaleY")))
  
ke_data <- ke_data %>% 
  dplyr::rename(., PopTotal = "TotalX", PopMale = "MaleX", PopFemale = "FemaleX") %>% 
  mutate(., AdminArea = case_when(
    SubCounty == "KENYA" ~ "National",
    SubCounty == "URBAN*" | SubCounty == "RURAL*" ~ "Rural-Urban",
    TRUE ~ AdminArea))

counties_KenyaSHP <- KenyaSHP %>% 
  st_drop_geometry() %>% 
  dplyr::select(.,county) %>% 
  pull() %>% 
  unique()

# convert column names in population dataset to lower title case
ke_data <- ke_data %>% 
  ungroup() %>% 
  clean_names() %>% 
  mutate(., county = tools::toTitleCase(tolower(county)))


### Inspect the county names that are different in each of the datasets
unique(ke_data$county)[which(!unique(ke_data$county) %in% counties_KenyaSHP)]


### Clean the county names so that they match in both datasets
ke_data <- ke_data %>% 
  mutate(county = ifelse(county == "Taita/Taveta", "Taita Taveta",
                         ifelse(county == "Tharaka-Nithi", "Tharaka-nithi",
                                ifelse(county == "Elgeyo/Marakwet", "Elgeyo-marakwet",
                                       ifelse(county == "Nairobi City", "Nairobi", county)))))

### Inspect the county names that are different in each of the datasets
# unique(data$county)[which(!unique(data$county) %in% counties_KenyaSHP)]

# prepare to merge census data with shapehile data
ke_data2 <- ke_data %>% 
  dplyr::filter(., admin_area == "County") %>% 
  dplyr::select(., -admin_area, -sub_county)

# trim any white spaces from merge column
KenyaSHP$county <- trimws(KenyaSHP$county)
ke_data2$county <- trimws(ke_data2$county)

map_data_df <- left_join(KenyaSHP, ke_data2, by = "county")

### Sort the data so that the County variable appears first
map_data_df <-map_data_df %>% 
  dplyr::select(county, everything())

#SHINY APP --------------------------------------------------

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",

    # Application title
    dashboardHeader(title = "Kenya 2019 Census"),
    
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Introduction", tabName = "intro", icon = icon("search")),
            menuItem("Mobile Phone Ownership Overview", tabName = "mobile_own", icon = icon("search")),
            menuItem("Mobile ownership, gender", tabName = "mobile_gender", icon = icon("search")))
        ),
    
    dashboardBody(
      tags$head( 
        tags$style(HTML(".fa{font-size: 17px;}"))
      ),
        tabItems(
            tabItem(tabName = "intro",
                    fluidRow(column(width = 6, offset = 3, align = "center", box(width = 12,
                                                                                 h1(tags$b("Kenya 2019 Census:\n")),
                                                                                 h2(tags$b("Mobile Phone Ownership")),
                                                                                 br(),
                                                                                 br(),
                                                                                 h4(p("This project will explore mobile phone ownership by gender in Kenya's 47 counties, 
                                                                                and provide insights to private and public sector parties interested in digital services (financial, agricultural etc).")),
                                                                                 br(),
                                                                                 h5(p("Research and visualizations by Jeremy Osir.\n
                                                                                Census Data library provided by Shel Kariuki, rKenyaCensus")))))
            ),
            
            tabItem(tabName = "mobile_own",
                    fluidRow(valueBoxOutput("PopTotal"),
                             valueBoxOutput("PopMale"),
                             valueBoxOutput("PopFemale"),
                             
                             valueBoxOutput("total.mpo"),
                             valueBoxOutput("male.mpo"),
                             valueBoxOutput("female.mpo")),
                    
                    fluidRow(
                      leafletOutput(outputId = "map")
                      
                    ),
  # Variable Selection for reactive map                  
                    fluidRow(
                      selectInput(
                        inputId = "variableselected",
                        label = "Select variable",
                        #data = map_data_df,
                        #choices = colnames(merged_map_data),
                        choices = c("mpo_male", "mpo_female", "mpo_male_perc","mpo_female_perc"),
                        selected = "mpo_female"

                    )
                    ),
            
            
            tabItem(tabName = "mobile_gender",
                    fluidRow())
            
        )
        
    )
))
    

# Define server logic required to draw a histogram
server <- function(input, output) {
  #some data manipulation to derive the values of the boxes
  
  pop.total<- dplyr::filter(ke_data, sub_county == "KENYA")$pop_total
  pop.male <- dplyr::filter(ke_data, sub_county == "KENYA")$pop_male
  pop.female <- dplyr::filter(ke_data, sub_county == "KENYA")$pop_female
  mpo.total <- dplyr::filter(ke_data, sub_county == "KENYA")$mpo_total_perc
  mpo.female <- dplyr::filter(ke_data, sub_county == "KENYA")$mpo_female_perc
  mpo.male <- dplyr::filter(ke_data, sub_county == "KENYA")$mpo_male_perc
  
# Value Box values
    output$PopTotal <- renderValueBox({
      valueBox(
        formatC(pop.total,format = "d", big.mark = ","),
        "Total Population",
        icon = icon("user", lib = "font-awesome")
      )
    })
    output$PopMale <- renderValueBox({
      valueBox(
        formatC(pop.male,format = "d", big.mark = ","),
        "Male Population",
        icon = icon("male", lib = "font-awesome")
      )
    })
    
    output$PopFemale <- renderValueBox({
      valueBox(
        formatC(pop.female,format = "d", big.mark = ","),
        "Female Population",
        icon = icon("female", lib = "font-awesome")
      )
    })
    
    output$total.mpo <- renderValueBox({
      valueBox(
        formatC(mpo.total,digits = 1, format = "f", big.mark = ","),
        "Mobile phone ownership %, total pop",
        icon = icon("mobile-alt", lib = "font-awesome")
      )
    })
    
    
    output$male.mpo <- renderValueBox({
      valueBox(
        formatC(mpo.male, digits = 1, format = "f", big.mark = ","),
        "Mobile phone ownership %, male",
        icon = icon("user", lib = "font-awesome")
      )
    })
    
    
    output$female.mpo <- renderValueBox({
      valueBox(
        formatC(mpo.female, digits = 1, format = "f", big.mark = ","),
        "Mobile phone ownership %, female",
        icon = icon("user", lib = "font-awesome")
      )
    })
    
   # # 'Static' Leaflet Map -- this one is displaying correctly
    output$map <- renderLeaflet({

      pal <- colorBin(palette = "YlOrRd",
                      domain = map_data_df$mpo_female
                      )
      #Specify labels

      labels <- sprintf(
          "<strong>%s</strong><br/>%s",
          map_data_df$county, formatC(map_data_df$mpo_female, format = "d", big.mark = ",")
        ) %>% lapply(htmltools::HTML)

      ## Generate the graph.
      l <- leaflet(data = map_data_df) %>%
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
          leaflet::addLegend(
            position = c("bottomright"), pal = pal, values = ~mpo_female)




    })

    
    
    
    # Reactive Leaflet map - this one is not displaying at all
    # Error message, line 344 <- Warning: Error in as.vector: cannot coerce type 'closure' to vector of type 'character'

# 
#     output$map <- renderLeaflet({
#     
# 
#       #compare class and output of static column call (mpo_female) to input$selected column
#       print(class(map_data_df$mpo_female))
#       print(map_data_df$mpo_female)
#       print("------------------------")
#       print(class(map_data_df[[input$variableselected]]))
#       print(map_data_df[[input$variableselected]])
# 
#     #create leaflet
# 
#     #   ### Specify the color scheme
#       pal <- colorBin(
#         palette = "YlOrRd",
#         domain = map_data_df[[input$variableselected]]
#       )
# 
#       ### Specify how labels will be displayed
#       # labels <- sprintf(
#       #   "<strong>%s</strong><br/>%s",
#       #   map$county, formatC(map_data_df[[input$variableselected]], format = "d", big.mark = ",")
#       # ) %>% lapply(htmltools::HTML)
# 
#       ### Generate the graph.
#       l <- leaflet(map_data_df) %>%
#         addProviderTiles("MapBox", options = providerTileOptions(
#           id = "mapbox.light",
#           accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
#         addTiles() %>%
#         addPolygons(fillColor = ~pal(input$variableselected),
#                     color = "black",
#                     weight = 2,
#                     opacity = 1,
#                     dashArray = "3",
#                     fillOpacity = 0.7,
#                     highlight = highlightOptions(
#                       weight = 4,
#                       color = "red",
#                       dashArray = "",
#                       bringToFront = TRUE),
#                     label = labels,
#                     labelOptions = labelOptions(
#                       style = list("font-weight" = "normal", padding = "3px 8px"),
#                       textsize = "15px",
#                       direction = "auto")) #%>%
#         # leaflet::addLegend(
#         #   position = c("bottomright"), pal = pal, values = ~map_data_df[[input$variableselected]])
# 
# 
# 
# 
# 
# 
#     })


    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


