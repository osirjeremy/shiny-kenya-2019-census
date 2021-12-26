
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

library(leaflet)
library(sf)
library(sp)
library(magrittr)
library(maptools)
#library(raster)
library(plyr)
library(rgdal)
library(viridis)
library(rmapshaper)


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",

    # Application title
    dashboardHeader(title = "Kenya 2019 Census"),
    
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Introduction", tabName = "intro", icon = icon("search")),
            menuItem("Mobile Phone Ownership", tabName = "mobile_own", icon = icon("search")),
            menuItem("Internet Use", tabName = "internet_use", icon = icon("search")))
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
                        choices = c("Mobile Phone Ownership (Male)" = "mpo_male", 
                                    "Mobile Phone Ownership (Female)" = "mpo_female", 
                                    "Mobile Phone Ownership % (Male)" = "mpo_male_perc",
                                    "Mobile Phone Ownership % (Female)" = "mpo_female_perc"),
                        selected = "Mobile Phone Ownership % (Female)")
                    ),
            
            
            tabItem(tabName = "internet_use",
                    fluidRow(valueBoxOutput("total.uoI"),
                             valueBoxOutput("male.uoI"),
                             valueBoxOutput("female.uoI")),
                    
                    
                    
                    
                    
                    
                    
                    fluidRow())
            
        )
        
    )
))
    

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # load data
  map_data_df <- readRDS(file = "map_data.Rds")
  ke_data <- readRDS(file = "ke_data.rds")
  
  #some data manipulation to derive the values of the boxes - consider moving this to cleaning file
  
  pop.total<- dplyr::filter(ke_data, sub_county == "KENYA")$pop_total
  pop.male <- dplyr::filter(ke_data, sub_county == "KENYA")$pop_male
  pop.female <- dplyr::filter(ke_data, sub_county == "KENYA")$pop_female
  mpo.total <- dplyr::filter(ke_data, sub_county == "KENYA")$mpo_total_perc
  mpo.female <- dplyr::filter(ke_data, sub_county == "KENYA")$mpo_female_perc
  mpo.male <- dplyr::filter(ke_data, sub_county == "KENYA")$mpo_male_perc
  uoI.total <- dplyr::filter(ke_data, sub_county == "KENYA")$uo_i_total_perc
  uoI.male <- dplyr::filter(ke_data, sub_county == "KENYA")$uo_i_male_perc
  uoI.female <- dplyr::filter(ke_data, sub_county == "KENYA")$uo_i_female_perc
  
# Value Box values
  
  
    output$PopTotal <- renderValueBox({
      valueBox(
        formatC(pop.total,format = "d", big.mark = ","),
        "Total Population",
        icon = icon("user", lib = "font-awesome")
      )
    })
    
    #Male population
    output$PopMale <- renderValueBox({
      valueBox(
        formatC(pop.male,format = "d", big.mark = ","),
        "Male Population",
        icon = icon("male", lib = "font-awesome")
      )
    })
    
    #Female Population
    output$PopFemale <- renderValueBox({
      valueBox(
        formatC(pop.female,format = "d", big.mark = ","),
        "Female Population",
        icon = icon("female", lib = "font-awesome")
      )
    })
    
    # Population owning mobile phones
    output$total.mpo <- renderValueBox({
      valueBox(
        formatC(mpo.total,digits = 1, format = "f", big.mark = ","),
        "Mobile phone ownership %, total popn.",
        icon = icon("mobile-alt", lib = "font-awesome")
      )
    })
    
    # Male Population owning mobile phones
    output$male.mpo <- renderValueBox({
      valueBox(
        formatC(mpo.male, digits = 1, format = "f", big.mark = ","),
        "Mobile phone ownership %, male",
        icon = icon("user", lib = "font-awesome")
      )
    })
    
    # Female Population owning mobile phones
    output$female.mpo <- renderValueBox({
      valueBox(
        formatC(mpo.female, digits = 1, format = "f", big.mark = ","),
        "Mobile phone ownership %, female",
        icon = icon("user", lib = "font-awesome")
      )
    })
    
    # Population that use the internet
    output$total.uoI <- renderValueBox({
      valueBox(
        formatC(mpo.female, digits = 1, format = "f", big.mark = ","),
        "Internet Use, %, total popn.",
        icon = icon("user", lib = "font-awesome")
      )
    })
    
    # Male Population that use the internet
    output$male.uoI <- renderValueBox({
      valueBox(
        formatC(uoI.male, digits = 1, format = "f", big.mark = ","),
        "Intenet Use, %, male",
        icon = icon("user", lib = "font-awesome")
      )
    })
    
    # Female Population that use the internet
    output$male.uoI <- renderValueBox({
      valueBox(
        formatC(uoI.male, digits = 1, format = "f", big.mark = ","),
        "Intenet Use, %, male",
        icon = icon("user", lib = "font-awesome")
      )
    })
    
    
    # Reactive Leaflet map
    
    #Connect input$variableselected from ui with server
    map_input = reactive({
      switch(input$variableselected,
             "mpo_female" = map_data_df$mpo_female,
             "mpo_male" = map_data_df$mpo_male,
             "mpo_female_perc" = map_data_df$mpo_female_perc,
             "mpo_male_perc" = map_data_df$mpo_male_perc)
      
    })


    output$map <- renderLeaflet({
    #   ### Specify the color scheme
    pal <- colorBin(
      palette = "YlOrRd",
      domain = map_input()
      )

    ### Specify how labels will be displayed
    labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      map_data_df$county, formatC(map_input(), format = "d", big.mark = ",")
    ) %>% lapply(htmltools::HTML)

    ### Generate the graph.
    l <- leaflet(map_data_df) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
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
                   leaflet::addLegend(
                     position = c("bottomright"), pal = pal, values = ~map_input(), title = switch(input$variableselected,
                                                                                      "mpo_female" = "Mobile Phone Ownership (Female)",
                                                                                      "mpo_male" = "Mobile Phone Ownership (Male)",
                                                                                      "mpo_female_perc" = "Mobile Phone Ownership % (Female)",
                                                                                      "mpo_male_perc" = "Mobile Phone Ownership % (Male)"))



    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


