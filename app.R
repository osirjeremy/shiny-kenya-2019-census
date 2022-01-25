
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

# 1. Add Mobile Data Table (add to existing Internet tab) = why should they be seperate?
# 2. Format tables using DT Clare Blog

# Add bar charts to DT Tabs

#format legend values:
# https://stackoverflow.com/questions/47410833/how-to-customize-legend-labels-in-r-leaflet
# https://stackoverflow.com/questions/38161073/manually-adding-legend-values-in-leaflet#comment63910354_38235047




#library(rKenyaCensus)

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
library(formattable)

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
                    # Shiny App Cover Page - summary of project and key questions. Digital Access, and gender gap
                    dashboardHeader(title = "Kenya 2019 Census"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Introduction", tabName = "intro", icon = icon("search")),
                        menuItem("Mind the (Gender) Gap", tabName = "digi_gap", icon = icon("search")),
                        menuItem("The Data", tabName = "the_data", icon = icon("search")), # add mobile ownership
                        menuItem("Implications and Further Research",tabName = "conclusion", icon = icon("search"))) # sumary slide
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "intro",
                                fluidRow(
                                  column(width = 12, 
                                         box(width = 12,
                                             h2(tags$b("Investigating Digital Connectivity and Divide in Kenya 2019 Census Data")),
                                             h4(p("This dashboard explores the rates of mobile phone ownership
                                                  and internet use across Kenyan counties, and investigates differences in adoption of these digital tools by regions and gender.")),
                                             h5(p("Research and visualizations by Jeremy Osir.\n
                                                                                Census Data library provided by Shel Kariuki, rKenyaCensus"))))
                                  ),
                                         # box(width = 12,
                                         #     plotlyOutput("bubblechart"))),
                                  fluidRow(
                                  column(width = 6, align = "left",
                                             selectInput(inputId = "variableselected",
                                                         label = "Select a Digital Indicator to display in the map",
                                                         choices = c("Mobile Phone Ownership % (Total pop)" = "mpo_total_perc",
                                                                     "Mobile Phone Ownership % (Men)" = "mpo_male_perc",
                                                                     "Mobile Phone Ownership % (Women)" = "mpo_female_perc",
                                                                     #"Mobile Ownership Gender Gap % (Male - Female)" = "mobile_gender_gap", # move to next tab
                                                                     "Internet Use % (Total)" = "uo_i_male_perc",
                                                                     "Internet Use % (Men)" = "uo_i_male_perc",
                                                                     "Internet Use % (Women)" = "uo_i_female_perc"),
                                                                     #"Internet Use Gender Gap % (male - female)" = "internet_gender_gap"), # move to next tab
                                                         selected = "Mobile Phone Ownership % (Women"),
                                             leafletOutput(outputId = "map", height = 500),
                                         column(width = 6,
                                                h4(p("A review of mobile phone ownership and internet access highlights that
                                                     digital connectivity tends to be highest in the central and southern regions
                                                     of the country,where urban centers like Nairobi are, and lowest in the northern regions.")))
                                  )
                                )
         
                        ),
                    
                    # Page 2: Overview of broad digital access across various counties. Which counties have highest access/lowest
                    tabItem(tabName = "digi_gap",
                            fluidRow(
                              column(width = 6, 
                                     tags$head(tags$style(HTML(".small-box {height: 70px}"))),
                                     valueBoxOutput("total.mpo.perc"),
                                     valueBoxOutput("male.mpo.perc"),
                                     valueBoxOutput("female.mpo.perc"),
                                     
                                     valueBoxOutput("total.uoI.perc"),
                                     valueBoxOutput("male.uoI.perc"),
                                     valueBoxOutput("female.uoI.perc"))),
                            fluidRow(
                              column(width = 8,
                                     plotOutput("mobile_gender_gap_chart", height = 800))),
                            
                            fluidRow(
                              column(width = 8,
                                     plotOutput("internet_gender_gap_chart", height = 800))),
                              
                            
                    ),
                            
                            
                    # Diving deeper into the gender gap
                    tabItem(tabName = "the_data",
                            fluidPage(
                              fluidRow(
                                
                                # replace with bar chart showing population with mobile phone and those without (ditto for internet)
                                valueBoxOutput("total.uoI"), 
                                valueBoxOutput("male.uoI"),
                                valueBoxOutput("female.uoI")),
                              
                              # reduce size of table and add pyramid chart showing counties with men owning more phones vs women (ditto internet)
                              fluidRow(titlePanel("Digital Gender Cap"),
                                       mainPanel(width = 6,
                                                 dataTableOutput("internet_table"))),
                              
                              fluidRow(
                                column(width = 6,
                                       p("Some text talking about the table")
                                       
                                ),
                                column(width = 6,
                                       p("Some text talking about the table")))
                            )
                    ),

                    
                    tabItem(tabName = "conclusion",
                            fluidPage(
                              
                              fluidRow(titlePanel("Implications and Further Research"),
                                       mainPanel(width = 12)
                              ),
                              
                              fluidRow(
                                column(width = 6,
                                       p("Some text talking the analysis")
                                       
                                ),
                                column(width = 6,
                                       p("Some text talking about future research")))
                            )
                    ))
                    )
)


    

# Define server logic for outputs
server <- function(input, output) {
  

  #load in processed dataframes and ggplots for quicker loading in shiny app
  map_data_df <- readRDS(file = "map_data.rds")
  ke_data <- readRDS(file = "ke_data.rds")
  mobile_table <- readRDS(file = "mobile_table.rds")
  internet_table <- readRDS(file = "internet_table.rds")
  
  mobile_gender_gap_chart <- readRDS(file="mobile_gender_gap_chart.rds")
  internet_gender_gap_chart <- readRDS(file="internet_gender_gap_chart.rds") 
  mpo_internet_bubble <- readRDS(file = "./mpo_internet_bubble.rds")
  
  #some data manipulation to derive the values of the ValueBoxes - consider moving this to cleaning file
  
  pop.total<- dplyr::filter(ke_data, sub_county == "KENYA")$pop_total
  pop.male.perc <- dplyr::filter(ke_data, sub_county == "KENYA")$pop_male_perc
  pop.female.perc <- dplyr::filter(ke_data, sub_county == "KENYA")$pop_female_perc
  mpo.total.perc <- dplyr::filter(ke_data, sub_county == "KENYA")$mpo_total_perc
  mpo.female.perc <- dplyr::filter(ke_data, sub_county == "KENYA")$mpo_female_perc
  mpo.male.perc <- dplyr::filter(ke_data, sub_county == "KENYA")$mpo_male_perc
  uoI.total.perc <- dplyr::filter(ke_data, sub_county == "KENYA")$uo_i_total_perc
  uoI.male.perc <- dplyr::filter(ke_data, sub_county == "KENYA")$uo_i_male_perc
  uoI.female.perc <- dplyr::filter(ke_data, sub_county == "KENYA")$uo_i_female_perc
  

  
# Value Box values
  
##Digital Access Overview tab
    output$pop.total <- renderValueBox({
      valueBox(
        value = tags$p(formatC(pop.total,format = "d", big.mark = ","), style = "font-size: 75%;"),
        subtitle = "Total Population"
      )
    })
    
    #Male population
    output$pop.male.perc <- renderValueBox({
      valueBox(
        value = tags$p(pop.male.perc, style = "font-size: 75%;"),
        subtitle = "Male Population"
      )
    })
    
    #Female Population
    output$pop.female.perc <- renderValueBox({
      valueBox(
        value = tags$p(pop.female.perc, style = "font-size: 75%;"),
        subtitle = "Female Population"
      )
    })
    
    # Population owning mobile phones
    output$total.mpo.perc <- renderValueBox({
      valueBox(
        value = tags$p(mpo.total.perc, style = "font-size: 75%;"),
        subtitle = "Mobile phone ownership %, total popn.",
        tags$i(class = "fas fa-mobile-alt", style = "font-size:32px;")
      )
    })
    
    # Male Population owning mobile phones
    output$male.mpo.perc <- renderValueBox({
      valueBox(
        value = tags$p(mpo.male.perc,digits = 1, style = "font-size: 75%;"),
        subtitle = "Mobile phone ownership %, male",
        tags$i(class = "fas fa-mobile-alt", style = "font-size:32px;")
      )
    })
    
    # Female Population owning mobile phones
    output$female.mpo.perc <- renderValueBox({
      valueBox(
        value = tags$p(mpo.female.perc, style = "font-size: 75%;"),
        subtitle = "Mobile phone ownership %, female",
        tags$i(class = "fas fa-mobile-alt", style = "font-size:32px;")
      )
    })
    
## Gender Gap
    
    # Population that use the internet
    output$total.uoI.perc <- renderValueBox({
      valueBox(
        value = tags$p(uoI.total.perc, style = "font-size: 75%;"),
        subtitle = "Internet Use, %, total popn.",
        tags$i(class ="fas fa-wifi", style = "font-size:32px;")
      )
    })
    
    # Male Population that use the internet
    output$male.uoI.perc <- renderValueBox({
      valueBox(
        value = tags$p(uoI.male.perc, style = "font-size: 75%;"),
        subtitle = "Internet Use, %, male popn.",
        tags$i(class ="fas fa-wifi", style = "font-size:32px;")
      )
    })
    
    # Female Population that use the internet
    output$female.uoI.perc <- renderValueBox({
      valueBox(
        value = tags$p(uoI.female.perc, style = "font-size: 75%;"),
        subtitle = "Internet Use, %, female popn.",
        tags$i(class ="fas fa-wifi", style = "font-size:32px;")
      )
    })
    
    internet_table <- DT::datatable(internet_table, 
                                    options = list(scrollX = TRUE)) %>% 
      formatPercentage(c("uo_i_total_perc","uo_i_male_perc","uo_i_female_perc",
                         "uo_dlt_total_perc","uo_dlt_male_perc","uo_dlt_female_perc",
                         "internet_gender_gap","dlt_gender_gap"))
    
    output$internet_table <- DT::renderDataTable(internet_table)
    
    output$bubblechart <- renderPlotly(
      plot1 <- ggplotly(mpo_internet_bubble , tooltip="text") %>% 
        layout(xaxis = list(autorange = TRUE),
               yaxis = list(autorange = TRUE))
      )
    
    output$mobile_gender_gap_chart <- renderPlot(mobile_gender_gap_chart)
    
    output$internet_gender_gap_chart <- renderPlot(internet_gender_gap_chart)
    
    
    
    # Reactive Leaflet map
    
    #Connect input$variableselected from ui with server
    map_input = reactive({
      switch(input$variableselected,
             "mpo_total_perc" = map_data_df$mpo_total_perc,
             "mpo_female_perc" = map_data_df$mpo_female_perc,
             "mpo_male_perc" = map_data_df$mpo_male_perc,
             "uo_i_total_perc" = map_data_df$uo_i_total_perc,
             "uo_i_male_perc" = map_data_df$uo_i_male_perc,
             "uo_i_female_perc" = map_data_df$uo_i_female_perc,
             "mobile_gender_gap" = map_data_df$mobile_gender_gap,
             "internet_gender_gap" = map_data_df$internet_gender_gap)
      
    })


    output$map <- renderLeaflet({
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)


