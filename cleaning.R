# Cleaning Script

# 1. Create bubble chart with pop_total on x axis and percent (uoi and mobile) on y axis
# 2. Horizontal bar chart showing pop owning mobile phones/internet use
# 3. Bar chart for gender gaps



library(dplyr)
library(DT)
library(tidyr)
library(janitor)
library(tidyverse)
library(formattable)
library(forcats)
library(scales)
library(rKenyaCensus)
library(ggplot2)
library(tidyr)



cat <- force(DataCatalogue) # dataset dictionary

# Load Map data  ---------------------------------------------------------
# 
# KenyaSHP <- read_sf("./ke_county.shp", quiet = TRUE,
#                     stringsAsFactors = FALSE,as_tibble = TRUE)
# 
# # # Modify map projection
# KenyaSHP <- st_transform(KenyaSHP, crs = 4326)
# 
# # if trying to save dataframe to CSV, need to remove names from 'geometry' column using code below:
# # https://community.rstudio.com/t/polygons-not-getting-plotted-on-leaflet-map-since-update/17856/2
# # %>% 
# # ms_simplify(.)
# #names(st_geometry(KenyaSHP)) = NULL
 
# # # Load Census data  ---------------------------------------------------------
mob.phone.df <- V4_T2.32
internet.use.df <- V4_T2.33
farming.type.df <-  V4_T2.25
ag.type.df <- V4_T2.20
livestock.type.df <- V4_T2.23
 
df_list <- list(mob.phone.df, internet.use.df,farming.type.df, ag.type.df, livestock.type.df)

# # # Join mobile phone and internet data --------------------------------------
ke_data <- df_list %>%
  reduce(left_join, by = c("County", "SubCounty", "AdminArea")) %>%
  clean_names(., case = "upper_camel") %>%
  dplyr::select(., - one_of(c("TotalY", "MaleY", "FemaleY")))
#
ke_data <- ke_data %>%
  dplyr::rename(., PopTotal = "TotalX", PopMale = "MaleX", PopFemale = "FemaleX") %>%
  mutate(., AdminArea = case_when(
    SubCounty == "KENYA" ~ "National",
    SubCounty == "URBAN*" | SubCounty == "RURAL*" ~ "Rural-Urban",
    TRUE ~ AdminArea))
# 
# counties_KenyaSHP <- KenyaSHP %>%
#   st_drop_geometry() %>%
#   dplyr::select(.,county) %>%
#   pull() %>%
#   unique()
# 
# # # convert column names in population dataset to lower title case
ke_data <- ke_data %>%
  ungroup() %>%
  clean_names() %>%
  mutate(., county = tools::toTitleCase(tolower(county)))

# # ### Inspect the county names that are different in each of the datasets
# # unique(ke_data$county)[which(!unique(ke_data$county) %in% counties_KenyaSHP)]

#### Clean the county names so that they match in both datasets
ke_data <- ke_data %>%
  mutate(county = ifelse(county == "Taita/Taveta", "Taita Taveta",
                         ifelse(county == "Tharaka-Nithi", "Tharaka-nithi",
                                ifelse(county == "Elgeyo/Marakwet", "Elgeyo-marakwet",
                                       ifelse(county == "Nairobi City", "Nairobi", county)))))
 
#### Inspect the county names that are different in each of the datasets

# unique(data$county)[which(!unique(data$county) %in% counties_KenyaSHP)]
 
### prepare to merge census data with shapefile data
# ke_data2 <- ke_data %>%
#   dplyr::filter(., admin_area == "County") %>%
#   dplyr::select(., -admin_area, -sub_county)
 
# # # trim any white spaces from merge column
# KenyaSHP$county <- trimws(KenyaSHP$county)
# ke_data2$county <- trimws(ke_data2$county)
 
# map_data_df <- left_join(KenyaSHP, ke_data2, by = "county")
 
# # ### Sort the data so that the County variable appears first
# map_data_df <-map_data_df %>%
#   dplyr::select(county, everything())

#Format percent values for printing
map_data_df <- map_data_df %>% 
  dplyr::mutate(., 
                pop_male_perc = formattable::percent(x = pop_male/pop_total, digits = 1),
                pop_female_perc = formattable::percent(x = pop_female/pop_total, digits = 1),
                
                mpo_total_perc = formattable::percent(x = mpo_total/pop_total, digits =1),
                mpo_male_perc = formattable::percent(x = mpo_male/pop_male, digits =1),
                mpo_female_perc = formattable::percent(x = mpo_female/pop_female, digits =1),
                
                uo_i_total_perc = formattable::percent(x = uo_i_total/pop_total, digits =1),
                uo_i_male_perc = formattable::percent(x = uo_i_male/pop_male, digits =1),
                uo_i_female_perc = formattable::percent(x = uo_i_female/pop_female, digits =1),
                
                uo_dlt_total_perc = formattable::percent(x = uo_dlt_total/pop_total, digits =1),
                uo_dlt_male_perc = formattable::percent(x = uo_dlt_male/pop_male, digits =1),
                uo_dlt_female_perc = formattable::percent(x = uo_dlt_female/pop_female, digits =1)          
  )

map_data_df <- map_data_df %>% 
  dplyr::mutate(
    mobile_gender_gap = formattable::percent((mpo_male_perc - mpo_female_perc), digits =1),
    internet_gender_gap = formattable::percent((uo_i_male_perc - uo_i_female_perc), digits =1),
    dlt_gender_gap = formattable::percent((uo_dlt_male_perc - uo_dlt_female_perc), digits =1)
    )

ke_data <- ke_data %>% 
  dplyr::mutate(., 
                # population
                pop_male_perc = formattable::percent(x = pop_male/pop_total, digits = 1),
                pop_female_perc = formattable::percent(x = pop_female/pop_total, digits = 1),
                # mobile phone
                mpo_total_perc = formattable::percent(x = mpo_total/pop_total, digits =1),
                mpo_male_perc = formattable::percent(x = mpo_male/pop_male, digits =1),
                mpo_female_perc = formattable::percent(x = mpo_female/pop_female, digits =1),
                #internet use
                uo_i_total_perc = formattable::percent(x = uo_i_total/pop_total, digits =1),
                uo_i_male_perc = formattable::percent(x = uo_i_male/pop_male, digits =1),
                uo_i_female_perc = formattable::percent(x = uo_i_female/pop_female, digits =1),
                # digital gap
                mobile_gender_gap = formattable::percent((mpo_male_perc - mpo_female_perc), digits =1),
                internet_gender_gap = formattable::percent((uo_i_male_perc - uo_i_female_perc), digits =1),
                #farming households
                fhs_perc = formattable::percent(x = no_fhs/pop_total, digits =1),
                fhs_subs_perc = formattable::percent(x = no_fhs_subsistence/pop_total, digits =1),
                fhs_comm_perc = formattable::percent(x = no_fhs_commercial/pop_total, digits =1),
                #livestock households
                livestock_hs_perc = formattable::percent(x = livestock_production/pop_total, digits =1),
                chicken_indig_perc = formattable::percent(x = indigenous_chicken/pop_total, digits =1),
                cattle_indig_perc = formattable::percent(x = indigenous_cattle/pop_total, digits =1),
                cattle_dairy_exot_perc = formattable::percent(x = exotic_cattle_dairy/pop_total, digits =1),
                cattle_beef_exot_perc = formattable::percent(x = exotic_cattle_beef/pop_total, digits =1),
                goats_perc = formattable::percent(x = goats/pop_total, digits =1),
                sheep_perc = formattable::percent(x = sheep/pop_total, digits =1)
                )
  

#internet use data table
internet_table <- ke_data %>% 
  dplyr::filter(., admin_area == "County") %>% 
  dplyr::select(., county, pop_total,
                #use of internet columns
                uo_i_total, uo_i_total_perc,
                uo_i_male,  uo_i_male_perc,
                uo_i_female,  uo_i_female_perc,
                internet_gender_gap,
                # Farming households
                no_fhs, no_fhs_subsistence, no_fhs_commercial,
                fhs_perc, fhs_subs_perc,fhs_comm_perc
                )

#mobile phone ownership data table
mobile_table <- ke_data %>% 
  dplyr::filter(., admin_area == "County") %>% 
  dplyr::select(., county, pop_total, #pop_male, pop_female, 
                #mobile phone ownership columns
                mpo_total, mpo_total_perc,
                mpo_male,  mpo_male_perc,
                mpo_female,  mpo_female_perc,
                mobile_gender_gap,
                # Farming households
                no_fhs, no_fhs_subsistence, no_fhs_commercial,
                fhs_perc, fhs_subs_perc,fhs_comm_perc
                )

#Final dataframes--------------------------------------------------

# If using CSV, 
# write_csv(map_data_df, "./map_data_df2.csv")
#write_csv(ke_data ,"./ke_data_df2.csv")

# If Using R Datafliles save as below - RDS which stores them as R objects
saveRDS(map_data_df, "map_data.rds")
saveRDS(ke_data, "ke_data.rds")

# Also save internet & mobile phone tables as R Objects in working directory
saveRDS(internet_table,"internet_table.rds")
saveRDS(mobile_table, "mobile_table.rds")



# Pyramind chart????
# https://github.com/lewishounkpevi/tidytuesday/blob/main/code/Y2021-M1-W4_rkenyacensus/gender.R
# https://rpubs.com/Lewis1er/rkenyacensus
# https://klein.uk/teaching/viz/datavis-pyramids/
#   https://www.rdocumentation.org/packages/plotrix/versions/3.8-2/topics/pyramid.plot
# https://stackoverflow.com/questions/4559229/drawing-pyramid-plot-using-r-and-ggplot2
# https://towardsdatascience.com/analysis-of-the-kenya-2019-census-using-r-shiny-25ea763da5a3





# Pyramid Charts -- Internet Use across Genders 

internet_gender_df<- as.data.frame(internet_table) %>%
  pivot_longer(data = ., cols = c("uo_i_male", "uo_i_female"), 
               names_to = "gender", 
               values_to = "number_using_internet") %>% 
  mutate(., gender = ifelse(test = gender == "uo_i_female",yes =  "Female",no = "Male")) %>% 
  drop_na() %>%
  select(county,  gender,  number_using_internet) %>%
  arrange(county, number_using_internet) %>% 
  mutate(number_using_internet_2 = if_else(gender == "Female", -1*number_using_internet, number_using_internet),
         county = fct_reorder(county, desc(number_using_internet))) %>%
  mutate(county = fct_reorder(county, desc(number_using_internet))) 


internet_gender_pyramid <- internet_gender_df %>% 
  ggplot() +
  aes(y = county, x = number_using_internet_2, fill = gender) +
  geom_col() +
  scale_y_discrete () +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = seq(-round(max(internet_gender_df$number_using_internet_2), -5),
                                  round(max(internet_gender_df$number_using_internet_2), -5),
                                  500000),
                     labels = paste0(as.character(
                       c(seq(round(max(internet_gender_df$number_using_internet_2)/1000, -2),
                             0, -500),
                         seq(500,
                             round(max(internet_gender_df$number_using_internet_2)/1000, -2),
                             500))),
                       "k")) +
  labs(title = "Kenya County Gender Pyramid",
       x = " ",
       y = " ",
       caption = "Source : Kenya National Bureau of Statistics (February 2020)
       \nTidyTuesday rKenyacensus
       \nby: Lewis Hounkpevi") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = rel(0.8)), 
        axis.title.x = element_text(face = "bold", 
                                    margin = margin(b = 0.5, t = 0.5, unit = "cm")), 
        axis.title.y = element_text(face = "bold", 
                                    hjust = 0.5, vjust = 0.5, 
                                    color = "#440154FF", 
                                    margin = margin(r = 0.5, unit = "cm")), 
        title = element_text(face = "plain", size = 1, hjust = 0.5), 
        plot.title = element_text(size = 14, face = "bold", 
                                  color = "#440154FF", vjust = 1), 
        plot.subtitle = element_text(face = "bold", 
                                     color = "#440154FF", vjust = 1, hjust = 0.5, 
                                     margin = margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(size = rel(1), vjust = 1, hjust = 0), 
        legend.text = element_text(face = "bold", color = "#440154FF"), 
        legend.direction = "horizontal", 
        legend.position = "bottom", legend.title = element_blank(), 
        panel.grid.major = element_line(linetype = "solid", 
                                        color = "#E7E9EA", size = 0.5), 
        panel.border = element_rect(fill = NA, 
                                    color = "#E7E9EA"), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = NA, colour = NA, 
                                        size = 0.5))

internet_gender_pyramid    

saveRDS(internet_gender_pyramid, "./internet_gender_pyramid.rds")


# Bubble Chart
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(dplyr)
library(viridis)
library(ggthemr)
library(ggthemes)


#Practice Version
library(gapminder)
prac_bubble_data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

p <- prac_bubble_data %>%
  mutate(gdpPercap=round(gdpPercap,0)) %>%
  mutate(pop=round(pop/1000000,2)) %>%
  mutate(lifeExp=round(lifeExp,1)) %>%
  
  # Reorder countries to having big bubbles on top
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  
  # prepare text for tooltip
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
  
  # Classic ggplot
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme(legend.position="none")

# turn ggplot interactive with plotly
pp <- ggplotly(p, tooltip="text")
pp

int_bubble_data <- as.data.frame(internet_table) %>% 
  dplyr::select(., )

# First, create dataframe with mobile and internet data

## BUBBLE CHART
mobile_internet_df <- ke_data %>% 
  dplyr::filter(., admin_area == "County") %>% 
  dplyr::select(., county, 
                pop_total, pop_male, pop_female,
                #mobile phone ownership columns
                mpo_total, mpo_total_perc,
                mpo_male,  mpo_male_perc,
                mpo_female,  mpo_female_perc,
                mobile_gender_gap,
                #use of internet columns
                uo_i_total, uo_i_total_perc,
                uo_i_male,  uo_i_male_perc,
                uo_i_female,  uo_i_female_perc,
                internet_gender_gap) %>% 
  arrange(., desc(pop_total)) %>% 
  # prepare text for tooltip
  mutate(text = paste("County: ", county, 
                      "\nPopulation: ", formatC(pop_total, format = "d" ,big.mark = ","), 
                      "\n % of Popn owning mobile phones: ", mpo_total_perc,
                      "\n% of Popn that use internet: ", uo_i_total_perc, sep=""))

mpo_internet_bubble <- ggplot(data = mobile_internet_df, 
                              aes(x = uo_i_total_perc, y = mpo_total_perc, size = pop_total, color= county, text = text))+
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 12), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme(legend.position="none") +
  labs(title = "Internet Use and Mobile Phone Ownership in Kenyan Counties\n(Larger circles indicate larger populations)",
       x = "% of population that uses the internet",
       y = "% of population that owns a mobile phone")+
  theme(plot.title = element_text(size = rel(0.8), face = "bold"),
        axis.title.x = element_text(size = rel(0.8), face = "bold"),
        axis.title.y = element_text(size = rel(0.8), face = "bold")) +
  scale_x_continuous(breaks = seq(0,0.6,0.1),labels = scales::label_percent(accuracy = 1L)) +
  scale_y_continuous(breaks = seq(0,0.6,0.1),labels = scales::label_percent(accuracy = 1L))

mpo_internet_bubble

mobile_internet_interactive <- ggplotly(mpo_internet_bubble, tooltip="text")
mobile_internet_interactive 
saveRDS(mobile_internet_interactive, "./mpo_internet_bubble.rds", compress = FALSE)




## STACKED BAR

specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)


mobile_ownership_df <- as.data.frame(mobile_internet_data) %>% 
  dplyr::filter(., admin_area == "County"| admin_area == "National") %>% 
  dplyr::mutate(., county = ifelse(admin_area == "National",yes = "Kenya (total)", county)) %>% 
  dplyr::select(., county, 
                pop_total, pop_male, pop_female,
                #mobile phone ownership columns
                mpo_total, mpo_total_perc,
                mpo_male,  mpo_male_perc,
                mpo_female,  mpo_female_perc,
                mobile_gender_gap,
                #use of internet columns
                uo_i_total, uo_i_total_perc,
                uo_i_male,  uo_i_male_perc,
                uo_i_female,  uo_i_female_perc,
                internet_gender_gap) %>% 
  arrange(., desc(pop_total))


mobile_ownership_bar_df2 <- mobile_ownership_df %>% 
  select(.,county,
         pop_total, pop_male, pop_female,
         #mobile phone ownership columns
         mpo_total, mpo_total_perc,
         mpo_male,  mpo_male_perc,
         mpo_female,  mpo_female_perc) %>% 
  mutate(., 
         non_mpo_male = (pop_male - mpo_male),
         non_mpo_female = (pop_female - mpo_female)) %>% 
  select(., county, mpo_male, mpo_female, non_mpo_male, non_mpo_female) %>%
  dplyr::rename(Male_Own = mpo_male, Female_Own = mpo_female,
                Male_DontOwn = non_mpo_male, Female_DontOwn = non_mpo_female) %>% 
  filter(., county == "Kenya (total)") %>% 
  pivot_longer(., cols = c(Male_Own, Male_DontOwn,Female_Own, Female_DontOwn),names_to = "Species", values_to = "Value") %>%
  dplyr::mutate(condition =ifelse(test = Species == "Male_Own" | Species == "Female_Own", 
                           yes = "Own",
                           no =  "DontOwn")) %>% 
  dplyr::mutate(., gender = ifelse(test = Species == "Male_Own" | Species  == "Male_DontOwn", 
                              yes = "Male",
                              no =  "Female"))

# Guardian BarChart  
ggplot(mobile_ownership_bar_df2, aes(fill = gender, y =Value, x = condition ))+
  geom_bar(position = "stack", stat = "identity") +
  coord_flip()
    
    
mobile_non_owners_bar_df <- mobile_ownership_df %>% 
  select(.,county,
         pop_total, pop_male, pop_female,
         #mobile phone ownership columns
         mpo_total, mpo_total_perc,
         mpo_male,  mpo_male_perc,
         mpo_female,  mpo_female_perc) %>% 
  mutate(., 
         non_mpo_male = (pop_male - mpo_male),
         non_mpo_female = (pop_female - mpo_female)) %>% 
  select(., county, non_mpo_male, non_mpo_female) %>% 
  dplyr::rename(Male = non_mpo_male, Female = non_mpo_female) %>% 
  pivot_longer(., cols = c(Male, Female),names_to = "Gender", values_to = "No_do_not_own_mobile_phone") %>% 
  filter(., county == "Kenya (total)")

#Bar chart showing people who own mobile phones
bar_chart_mobile_owners <- ggplot(mobile_ownership_bar_df, aes(fill = Gender, y =No_own_mobile_phone, x = county ))+
  geom_bar(position = "stack", stat = "identity") +
  coord_flip()

bar_chart_mobile_owners

bar_chart_non_mobile_owners <- ggplot(mobile_non_owners_bar_df, aes(fill = Gender, y = No_do_not_own_mobile_phone, x = county ))+
  geom_bar(position = "stack", stat = "identity") +
  coord_flip()

bar_chart_non_mobile_owners


# Gender Gap
# for mobile gender gap viz ####
mobile_gendergap_df<- as.data.frame(mobile_internet_data) %>%
  dplyr::filter(., admin_area == "County"| admin_area == "National") %>%
  dplyr::mutate(., 
                county = ifelse(admin_area == "National",yes = "Kenya (total)", county),
                type = ifelse(mobile_gender_gap <0, "Women own more phones", "Men own more phones"),
                text = ifelse( mobile_gender_gap <0, 
                               yes = (paste("Women in ", county, " have a ", 
                                                                      abs(mobile_gender_gap), 
                             " higher rate of mobile phone ownership.")), 
                             no = (paste("Men in ", county, " have a ", 
                                         abs(mobile_gender_gap), 
                                         " higher rate of mobile phone ownership.", sep = "")))) %>% 
  select(county,  mobile_gender_gap, type, text) %>%
  arrange(desc(mobile_gender_gap))  %>%
  mutate(county = fct_reorder(county, desc(mobile_gender_gap))) 

# for internet gender gap viz ####

internet_gendergap_df<- as.data.frame(mobile_internet_data) %>%
  dplyr::filter(., admin_area == "County"| admin_area == "National") %>%
  dplyr::mutate(., 
                county = ifelse(admin_area == "National",yes = "Kenya (total)", county),
                type = ifelse(internet_gender_gap <0, "Women use internet more", "Men use internet more"),
                text = ifelse(internet_gender_gap <0, 
                               yes = (paste("Women in ", county, " have a ", 
                                            abs(internet_gender_gap), 
                                            " higher rate of mobile phone ownership.")), 
                               no = (paste("Men in ", county, " have a ", 
                                           abs(internet_gender_gap), 
                                           " higher rate of mobile phone ownership.", sep = "")))) %>% 
  select(county,  internet_gender_gap, type, text) %>%
  arrange(desc(internet_gender_gap))  %>%
  mutate(county = fct_reorder(county, desc(internet_gender_gap))) 



### Mobile Gender Gap ####

mobile_gender_gap_chart<- mobile_gendergap_df %>% 
  ggplot() +
  aes(y = county, x = mobile_gender_gap, fill = type) +
  geom_col() +
  scale_y_discrete () +
  scale_fill_viridis_d(limits = c("Women own more phones", "Men own more phones")) +
  scale_x_continuous(breaks = seq(-0.05,0.1, 0.025),labels = scales::label_percent(accuracy = 1L)) +
  labs(title = "Men own more phones than Women in 29 out of 47 Counties (61%) ",
       x = " ",
       y = " ") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = rel(1)), 
        axis.title.x = element_text(face = "bold", 
                                    margin = margin(b = 0.5, t = 0.5, unit = "cm")), 
        axis.title.y = element_text(face = "bold", 
                                    hjust = 0.5, vjust = 0.5, 
                                    color = "#440154FF", 
                                    margin = margin(r = 0.5, unit = "cm")), 
        title = element_text(face = "plain", size = 1, hjust = 0.5), 
        plot.title = element_text(size = 14, face = "bold", 
                                  color = "#440154FF", vjust = 1, hjust = 0), 
        plot.subtitle = element_text(face = "bold", 
                                     color = "#440154FF", vjust = 1, hjust = 0.5, 
                                     margin = margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(size = rel(1), vjust = 1, hjust = 0), 
        legend.text = element_text(face = "bold", color = "#440154FF", size = rel(1)), 
        legend.direction = "horizontal", 
        legend.position = "top", legend.title = element_blank(), 
        panel.grid.major = element_line(linetype = "solid", 
                                        color = "#E7E9EA", size = 0.5), 
        panel.border = element_rect(fill = NA, 
                                    color = "#E7E9EA"), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = NA, colour = NA, 
                                        size = 0.5))
mobile_gender_gap_chart
saveRDS(mobile_gender_gap_chart, file ="./mobile_gender_gap_chart.rds" )


#### Internet Gender Gap####
internet_gender_gap_chart<- internet_gendergap_df %>% 
  ggplot() +
  aes(y = county, x = internet_gender_gap, fill = type) +
  geom_col() +
  scale_y_discrete () +
  scale_fill_viridis_d(limits = c("Women use internet more", "Men use internet more")) +
  scale_x_continuous(breaks = c(0, 0.03,0.06,0.08),labels = scales::label_percent(accuracy = 1L)) +
  labs(title = "Men used the Internet more than Women in every County",
       x = " ",
       y = " ",
       caption = "Source : Kenya National Bureau of Statistics (February 2020)
       \nTidyTuesday rKenyacensus
       \nby: Jeremy Osir") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = rel(0.8)), 
        axis.title.x = element_text(face = "bold", 
                                    margin = margin(b = 0.5, t = 0.5, unit = "cm")), 
        axis.title.y = element_text(face = "bold", 
                                    hjust = 0.5, vjust = 0.5, 
                                    color = "#440154FF", 
                                    margin = margin(r = 0.5, unit = "cm")), 
        title = element_text(face = "plain", size = 1, hjust = 0.5), 
        plot.title = element_text(size = 14, face = "bold", 
                                  color = "#440154FF", vjust = 1), 
        plot.subtitle = element_text(face = "bold", 
                                     color = "#440154FF", vjust = 1, hjust = 0.5, 
                                     margin = margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(size = rel(1), vjust = 1, hjust = 0), 
        legend.text = element_text(face = "bold", color = "#440154FF"), 
        legend.direction = "horizontal", 
        legend.position = "none", legend.title = element_blank(), 
        panel.grid.major = element_line(linetype = "solid", 
                                        color = "#E7E9EA", size = 0.5), 
        panel.border = element_rect(fill = NA, 
                                    color = "#E7E9EA"), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = NA, colour = NA, 
                                        size = 0.5))
internet_gender_gap_chart

saveRDS(internet_gender_gap_chart, file ="./internet_gender_gap_chart.rds" )


