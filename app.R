#format values  for valuebox and map labels

library(shiny)
library(shinydashboard)

library(ggplot2)
library(RColorBrewer)
library(plotly)
library(htmltools)
library(colorspace)
library(ggrepel)

library(plyr)
library(dplyr)
#library(DT)
#library(tidyr)
#library(janitor)
#library(tidyverse)
library(formattable)

library(leaflet)
library(sf)
library(sp)
library(magrittr)
library(maptools)
library(rgdal)
library(viridis)
library(rmapshaper)

#library(rKenyaCensus)
#library(biscale)
library(cowplot)
library(extrafont)
library(systemfonts)



# Define UI 
ui <- dashboardPage(skin = "purple",
                    # Shiny App Cover Page - summary of project and key questions. Digital Access, and gender gap
                    dashboardHeader(title = "Kenya 2019 Census"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Introduction", tabName = "intro", icon = icon("search")),
                        menuItem("Mind the (Gender) Gap", tabName = "digi_gap", icon = icon("search")),
                        menuItem("Focus on Farming Households", tabName = "agriculture", icon = icon("search")),
                        menuItem("Conclusion",tabName = "conclusion", icon = icon("search")))
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "intro",
                                fluidRow(
                                  column(width = 12, 
                                         box(width = 12,
                                             h2(tags$b("Investigating Digital Connectivity and Divide in Kenya 2019 Census Data")),
                                             h4(p("This dashboard explores the rates of mobile phone ownership
                                                  and internet use across Kenyan counties, and investigates differences in adoption of these digital tools by regions and gender.
                                                  Insights from this analysis can inform the government's decisionmaking around where it should prioritize investments in digital infrastucture")),
                                             h5(p("Research and visualizations by Jeremy Osir.\n
                                                                                Census Data library provided by Shel Kariuki, rKenyaCensus"))))
                                ),
                                # box(width = 12,
                                #     plotlyOutput("bubblechart"))),
                                fluidRow(
                                  column(width = 6, align = "center",
                                         selectInput(inputId = "variableselected",
                                                     label = "Select a Digital Indicator to display in the map",
                                                     choices = c("Mobile Phone Ownership % (Total pop)" = "mpo_total_perc",
                                                                 "Mobile Phone Ownership % (Men)" = "mpo_male_perc",
                                                                 "Mobile Phone Ownership % (Women)" = "mpo_female_perc",
                                                                 #"Mobile Ownership Gender Gap % (Male - Female)" = "mobile_gender_gap", # move to next tab
                                                                 "Internet Use % (Total)" = "uo_i_total_perc",
                                                                 "Internet Use % (Men)" = "uo_i_male_perc",
                                                                 "Internet Use % (Women)" = "uo_i_female_perc"),
                                                     #"Internet Use Gender Gap % (male - female)" = "internet_gender_gap"), # move to next tab
                                                     selected = "Mobile Phone Ownership % (Women"),
                                         leafletOutput(outputId = "map", height = 500)),
                                  column(width = 6,
                                         box(width = 12,
                                             h3(tags$b("Digital Connectivity Explorer")),
                                             h4(p("The interactive map allows us to explore various digital connectivity metrics, specifically mobile phone ownership and internet access,
                                             across Kenyan counties by gender.")),
                                             
                                             h4(p("According to the data, digital connectivity tends to be highest in the central and southern counties of the country - particularly around
                                             the capital city Nairobi - with pockets of moderate digital connectivity in the western counties.")),
                                             
                                             h4(p("Conversely, digital connectivity is lowest in north and northeastern Kenya. According to the World Bank these regions, which are semi-arid 
                                             and often experience drought, are characterized by profound infrastructure deficits, including lack of access to roads, electricity, water, and to social services.
                                             World Bank data also indicate that the average poverty rate in these regions is double the national average (68% vs 34%).")),
                                             
                                             h4(p("County-level data highlights that men tend to own phones and use the internet more than women.")),
                                             br(),
                                             
                                             h4(tags$i("In the other tabs of this dashboard (see left), we take a closer look at how 
                                             digital connectivity varies between men and women (Mind the Gender Gap), and look at digital access
                                             for households that engage in different types of agriculture (see Farming tab)"))
                                             
                                             
                                             
                                             
                                         ))
                                ),
                                fluidRow(
                                  column(width = 12, offset = 0, style='padding-left:5px; padding-right:5px; padding-top:30px; padding-bottom:0px',
                                         box(width = 12,
                                             h3(tags$b("Higher Phone Ownership generally correlates with more internet Usage  ")),
                                             h4(p("The interactive bubble chart (below) shows the correlation between high phone ownership and internet usage.")),
                                             
                                             h4(p("With a few exceptions, larger population centers (shown by larger circles) such as
                                             Nairobi, Kiambu and Mombasa (top-right) had the highest connectivity.
                                             Phone ownership and internet usage in these countries exceeded 60% and 40%, respectively. ")),
                                             
                                             h4(p("Conversely, a few counties with large populations like Bungoma (1.5M), Narok (1M), Migori (1M), and 
                                             Kilifi (1.3M) had phone ownership rates of less than 40% and internet usage less than 15%.")),
                                             
                                             h4(tags$i("The counties in the bottom left quadrant of the chart (i.e. low phone ownership and low internet usage)
                                                       were predominantly counties in the north and northeastern parts of Kenya. This highlights the significant
                                                       lack of digital connectivity in these regions, which has broader implications on these communities ability to 
                                                       participate in the modern digital economy."))
                                         )
                                  )
                                ), 
                                fluidRow(
                                  column(width = 11, offset = 0, style='padding-left:15px; padding-right:15px; padding-top:0px; padding-bottom:0px',
                                         h4(tags$i("Hover cursor over the bubbles to see information on each county. Toggle the filters
                                                       at the top of the chart to highlight certain areas of the chart, or compare counties metrics.")),
                                         plotlyOutput("mpo_internet_bubble", height = 700)
                                  ))
                                
                        ),
                        
                        # Page 2: Difference between men and women
                        tabItem(tabName = "digi_gap",
                                fluidRow(
                                  h2("National Statistics"),
                                  column(width = 12,
                                         
                                         tags$head(tags$style(HTML(".small-box {height: 70px}"))),
                                         valueBoxOutput("total.mpo.perc"),
                                         valueBoxOutput("male.mpo.perc"),
                                         valueBoxOutput("female.mpo.perc"),
                                         
                                         valueBoxOutput("total.uoI.perc"),
                                         valueBoxOutput("male.uoI.perc"),
                                         valueBoxOutput("female.uoI.perc"))),
                                fluidRow(
                                  h2("County Statistics"),
                                  column(width = 8,
                                         
                                         plotOutput("mobile_gender_gap_chart", height = 800)),
                                  box(width = 4,
                                      h3(tags$b("Mobile Phones")),
                                      h4(p("At the national level, rates of mobile phone ownership are virtually identical
                                           between men and women (47.6 for men vs 47.0 for women.")),
                                      
                                      h4(p("However, when we compare the data at a county level, we see a much clearer picture of the
                                           digital divide between genders. In the first chart, we observe that men owned more phones 
                                           than women in 61% of counties. In some places, men's phone ownership rate was 8% higher than 
                                           that of women."))
                                      
                                  )
                                  
                                  
                                  
                                  
                                ),
                                
                                fluidRow(
                                  column(width = 8,
                                         plotOutput("internet_gender_gap_chart", height = 800)),
                                  box(width = 4,
                                      h3(tags$b("Internet access")),
                                      
                                      h4(p("The gender gap in internet usage was more stark. At the national level, men were more 5% likely to use the internet.
                                      Further, men used the internet more than women in EVERY single county, with their usage rate surpassing women's by 2%-8.5%.")),
                                      
                                      h4(p("The gender gap in digital connectivity has implications on women's ability to participate in the
                                           economy. In the next tab, we focus on connectivity amongst households that practice agriculture. According to
                                           national statistics women make up an estimated 59% of the agricultural labor force (World Bank 2019"))
                                  ))
                                
                                
                        ),
                        
                        
                        # Implications in agriculture
                        tabItem(tabName = "agriculture",
                                
                                fluidPage(
                                  fluidRow(
                                    box(width = 12, 
                                        h2(tags$b("Mobile phone ownership and Agriculture")),
                                        h4(p("Agriculture is one of the most important drivers of Kenya's economy. It contributes an estimated 33% to the
                                             country's Gross Domenstic Product (GDP) and employs approx. 60% of the population (World Bank).")),
                                        
                                        h4(p("It is important to note that women make up an estimated 59% of the agricultural labor orce (World Bank 2019),
                                             and the gender gap in digital connectivity (shown in previous tab) could have implications on women's ability to 
                                             maximize their agricultural productivity.")),
                                        
                                        h4(p("Digital agriculural services are becoming increasingly important in sub-Saharan African countries like Kenya. Digital tools, including mobile phones, are helping
                                             farming households increase their productivity, by improving access to finance, advisory, insurance, and market services.
                                             For example, MPESA, a phone-based service that allows individuals to make digital transactions without a bank account, has increased
                                             access to financial services, especially in rural areas where traditional banks are less prevalent. It has been
                                             attributed to a 2% decrease in poverty (Tanveet Suri, Mobile money. Annual Review of Economics 9, 2017).")),
                                        
                                        h4(p("Below are a series of charts that compare mobile phone ownership to the percent of households participating in
                                             various types of agriculture, namely: Subsistence Farming, Commercial Farming, Cattle Rearing.")),
                                        br(),
                                        h4(tags$i("Lighter blue shaded areas represent counties where there is a large proportion of households participation in agriculture, but low
                                             phone ownership. Dark blue shaded areas represent regions where there is high household participation in agriculture and high phone 
                                             ownership"))
                                        )),

                                  
                                  fluidRow(
                                    column(width = 8,style='padding-left:5px; padding-right:5px; padding-top:10px; padding-bottom:0px',
                                           plotOutput("chloro_commFHS", height = 700)),
                                    column(width = 4, style='padding-left:5px; padding-right:5px; padding-top:10px; padding-bottom:0px',
                                           box(width = 12, background = "blue",
                                               h3(tags$b("Commercial Farming")),
                                               
                                               h4(p("The central regions of the country, where the proportion (%) of commercial households is higher,
                                             tend to have higher rates of phone ownership. However, parts of western Kenya that have a relatively high proportion of 
                                             commercial farming households have much lower phone ownership, which likely impacts their productivty."))))
                                    
                                  ),
                                  
                                  fluidRow(
                                    column(width = 8,style='padding-left:5px; padding-right:5px; padding-top:30px; padding-bottom:0px',
                                           plotOutput("chloro_subsFHS", height = 700)),
                                    column(width = 4,style='padding-left:5px; padding-right:5px; padding-top:30px; padding-bottom:0px',
                                           box(width = 12,background = "blue",
                                               h3(tags$b("Subsistence Farming")),
                                               
                                               h4(p("Similar to the above, central regions of the country have the highest overlap between high proportion of subsistence farming
                                               households and high rates of phone ownership. However, some parts of western Kenya and the north east have a relatively high 
                                               proportion of agricultural households but much lower phone ownership"))))
                                    
                                  ),
                                  
                                  # fluidRow(
                                  #   column(width = 8,style='padding-left:5px; padding-right:5px; padding-top:30px; padding-bottom:0px',
                                  #          plotOutput("chloro_LS", height = 700)),
                                  #   column(width = 4, style='padding-left:5px; padding-right:5px; padding-top:30px; padding-bottom:0px',
                                  #          box(width = 12,background = "blue",
                                  #              h3(tags$b("Livestock Rearing")),
                                  #              
                                  #              h4(p("The northeast areas of the country which
                                  #            have higher rates of livestock rearing have the largest discrepancy between households participating in
                                  #            this type of agriculture and phone ownership."))))
                                  #   
                                  # ),
                                  
                                  
                                  fluidRow(
                                    column(width = 8,style='padding-left:5px; padding-right:5px; padding-top:30px; padding-bottom:0px',
                                           plotOutput("chloro_cattle", height = 750)),
                                    column( width = 4, style='padding-left:5px; padding-right:5px; padding-top:30px; padding-bottom:0px',
                                            box(width = 12, background = "blue",
                                    h3(tags$b("Cattle Rearing")),
                                    
                                    h4(p("Low digital penetration is particularly evident in the northern areas of the country, where
                                         cattle rearing is prevalent."))))
                                  )
                                )
                                  
                                
                        ),
                        
                        
                        tabItem(tabName = "conclusion",
                                fluidPage(
                                  
                                  fluidRow(h2("Lessons Learned and Further Research"), offset = 5,
                                           mainPanel(width = 12)
                                  ),
                                  
                                  fluidRow(
                                    column(width = 12,
                                           box(width = 12, 
                                               h3(tags$b("What we learned")),
                                               h4(tags$li("Digital connectivty is highest in central regions of Kenya and pockets of western Kenya; it is moderate in the east
                                                          and lowest in the north and north east")),
                                               h4(tags$li("National statistics obscure the gender gap in digital connectivity. 
                                                          The digital divide is most apparent at county level.")),
                                               h4(tags$li("Crop farming households in the central Kenya 
                                                          own mobile phones at higher rates than similar households in western Kenya.")),
                                               h4(tags$li("Livestock rearing households in north and northeast have lower mobile phone penetration rates compared to other regions.")),
                                               h4(tags$li("Lower digital connectivity in the northern regions of Kenya, and pockets of the west might hinder economic productivity."))
                                               )
                                           ),
                                    column(width = 12,
                                           box(width = 12, 
                                               h3(tags$b("What to do next")),
                                               h4(tags$li("Examine agricultural output data across counties to investigate economic
                                                          implications of low digital connectivity in a county")),
                                               h4(tags$li("Explore agricultural household data by gender at the county level. The census data did not include this breakdown"))

                                           )
                                    )
                                    )

                                )
                        ))
                    )
)



    

# Define server logic for outputs
server <- function(input, output) {
  

  #load in processed dataframes and charts for quicker loading in shiny app
  map_data_df <- readRDS(file = "./map_data2.rds")
  ke_data <- readRDS(file = "./ke_data.rds")
  mobile_table <- readRDS(file = "./mobile_table.rds")
  #internet_table <- readRDS(file = "internet_table.rds")
  chloro_commFHS <- readRDS(file ="./bi_chloro_CommFHS_Phone_uncomp.rds")
  chloro_subsFHS <- readRDS(file ="./bi_chloro_SubFHS_Phone_uncomp.rds")
  #chloro_LS <- readRDS(file = "./bi_chloro_LS_Phone_uncomp.rds")
  chloro_cattle <- readRDS(file = "./bi_chloro_cattle_Phone.rds")
  #chloro_chick <- readRDS(file = "./charts/bi_chloro_cattle_Phone.rds")
  
  
  mobile_gender_gap_chart <- readRDS(file="./mobile_gender_gap_chart.rds")
  internet_gender_gap_chart <- readRDS(file="./internet_gender_gap_chart.rds") 
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
    # output$pop.total <- renderValueBox({
    #   valueBox(
    #     value = tags$p(formatC(pop.total,format = "d", big.mark = ","), style = "font-size: 75%;"),
    #     subtitle = "Total Population",
    #     color = "red"
    #   )
    # })
    # 
    # #Male population
    # output$pop.male.perc <- renderValueBox({
    #   valueBox(
    #     value = tags$p(pop.male.perc, style = "font-size: 75%;"),
    #     subtitle = "Male Population"
    #   )
    # })
    # 
    # #Female Population
    # output$pop.female.perc <- renderValueBox({
    #   valueBox(
    #     value = tags$p(pop.female.perc, style = "font-size: 75%;"),
    #     subtitle = "Female Population"
    #   )
    # })
    
    # Population owning mobile phones
    output$total.mpo.perc <- renderValueBox({
      valueBox(
        value = tags$p(mpo.total.perc, style = "font-size: 75%;"),
        color = "red",
        subtitle = "Mobile phone ownership %, total popn.",
        tags$i(class = "fas fa-mobile-alt", style = "font-size:32px;")
      )
    })
    
    # Male Population owning mobile phones
    output$male.mpo.perc <- renderValueBox({
      valueBox(
        value = tags$p(mpo.male.perc,digits = 1, style = "font-size: 75%;"),
        color = "red",
        subtitle = "Mobile phone ownership %, male",
        tags$i(class = "fas fa-mobile-alt", style = "font-size:32px;")
      )
    })
    
    # Female Population owning mobile phones
    output$female.mpo.perc <- renderValueBox({
      valueBox(
        value = tags$p(mpo.female.perc, style = "font-size: 75%;"),
        color = "red",
        subtitle = "Mobile phone ownership %, female",
        tags$i(class = "fas fa-mobile-alt", style = "font-size:32px;")
      )
    })
    
## Gender Gap
    
    # Population that use the internet
    output$total.uoI.perc <- renderValueBox({
      valueBox(
        value = tags$p(uoI.total.perc, style = "font-size: 75%;"),
        color = "olive",
        subtitle = "Internet Use, %, total popn.",
        tags$i(class ="fas fa-wifi", style = "font-size:32px;")
      )
    })
    
    # Male Population that use the internet
    output$male.uoI.perc <- renderValueBox({
      valueBox(
        value = tags$p(uoI.male.perc, style = "font-size: 75%;"),
        color = "olive",
        subtitle = "Internet Use, %, male popn.",
        tags$i(class ="fas fa-wifi", style = "font-size:32px;")
      )
    })
    
    # Female Population that use the internet
    output$female.uoI.perc <- renderValueBox({
      valueBox(
        value = tags$p(uoI.female.perc, style = "font-size: 75%;"),
        color = "olive",
        subtitle = "Internet Use, %, female popn.",
        tags$i(class ="fas fa-wifi", style = "font-size:32px;")
      )
    })
    
    #internet_table <- DT::datatable(internet_table,
                                   # options = list(scrollX = TRUE)) 
    # format columns as %
    
    #output$internet_table <- DT::renderDataTable(internet_table)

    output$mpo_internet_bubble <- renderPlotly(
      plot1 <- ggplotly(mpo_internet_bubble , tooltip="text") %>% 
        layout(xaxis = list(autorange = TRUE),
               yaxis = list(autorange = TRUE))
      )
    
    output$mobile_gender_gap_chart <- renderPlot(mobile_gender_gap_chart)
    
    output$internet_gender_gap_chart <- renderPlot(internet_gender_gap_chart)
    
    output$chloro_commFHS <- renderPlot(chloro_commFHS)
    output$chloro_subsFHS <- renderPlot(chloro_subsFHS)
    output$chloro_cattle <- renderPlot(chloro_cattle)
    output$chloro_LS <- renderPlot(chloro_LS)
    
    
    
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
        palette = "RdYlBu",
        bins = 6,
        pretty = TRUE,
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


