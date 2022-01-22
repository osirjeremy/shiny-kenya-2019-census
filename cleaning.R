#RCleaning

# Load Map data  ---------------------------------------------------------
# 
# KenyaSHP <- read_sf("./ke_county.shp", quiet = TRUE,
#                     stringsAsFactors = FALSE,as_tibble = TRUE)
# 
# # # Modify map projection
# KenyaSHP <- st_transform(KenyaSHP, crs = 4326)
# 
# # if trying to save df to CSV, need to remove names from geometry column using code below:
# # https://community.rstudio.com/t/polygons-not-getting-plotted-on-leaflet-map-since-update/17856/2
# # %>% 
# # ms_simplify(.)
# #names(st_geometry(KenyaSHP)) = NULL
# 
# # # Load Census data  ---------------------------------------------------------
# mob.phone.df <- V4_T2.32
# internet.use.df <- V4_T2.33
# 
# df_list <- list(mob.phone.df, internet.use.df)
# # 
# # # Join mobile phone and internet data --------------------------------------
# ke_data <- df_list %>%
#   reduce(left_join, by = c("County", "SubCounty", "AdminArea")) %>%
#   clean_names(., case = "upper_camel") %>%
#   dplyr::select(., - one_of(c("TotalY", "MaleY", "FemaleY")))
# 
# ke_data <- ke_data %>%
#   dplyr::rename(., PopTotal = "TotalX", PopMale = "MaleX", PopFemale = "FemaleX") %>%
#   mutate(., AdminArea = case_when(
#     SubCounty == "KENYA" ~ "National",
#     SubCounty == "URBAN*" | SubCounty == "RURAL*" ~ "Rural-Urban",
#     TRUE ~ AdminArea))
# 
# counties_KenyaSHP <- KenyaSHP %>%
#   st_drop_geometry() %>%
#   dplyr::select(.,county) %>%
#   pull() %>%
#   unique()
# 
# # # convert column names in population dataset to lower title case
# ke_data <- ke_data %>%
#   ungroup() %>%
#   clean_names() %>%
#   mutate(., county = tools::toTitleCase(tolower(county)))
# 
# # ### Inspect the county names that are different in each of the datasets
# # unique(ke_data$county)[which(!unique(ke_data$county) %in% counties_KenyaSHP)]
# 
# 
# # ### Clean the county names so that they match in both datasets
# ke_data <- ke_data %>%
#   mutate(county = ifelse(county == "Taita/Taveta", "Taita Taveta",
#                          ifelse(county == "Tharaka-Nithi", "Tharaka-nithi",
#                                 ifelse(county == "Elgeyo/Marakwet", "Elgeyo-marakwet",
#                                        ifelse(county == "Nairobi City", "Nairobi", county)))))
# # 
# # ### Inspect the county names that are different in each of the datasets
# # # unique(data$county)[which(!unique(data$county) %in% counties_KenyaSHP)]
# # 
# # # prepare to merge census data with shapehile data
# ke_data2 <- ke_data %>%
#   dplyr::filter(., admin_area == "County") %>%
#   dplyr::select(., -admin_area, -sub_county)
# 
# # # trim any white spaces from merge column
# KenyaSHP$county <- trimws(KenyaSHP$county)
# ke_data2$county <- trimws(ke_data2$county)
# 
# map_data_df <- left_join(KenyaSHP, ke_data2, by = "county")
# # 
# # ### Sort the data so that the County variable appears first
# map_data_df <-map_data_df %>%
#   dplyr::select(county, everything())


#SHINY APP --------------------------------------------------
#Load dataframes - load in processed dataframes for quicker loading

# write_csv(map_data_df, "./map_data_df2.csv")
# write_csv(ke_data ,"./ke_data_df2.csv")
# 
# map_data_df2 <-  read_csv("./map_data_df2.csv")
# ke_data <- read_csv("./ke_data_df2.csv")


#saveRDS(map_data_df, "map_data.rds")
map_data_df <- readRDS(file = "map_data.Rds")

#saveRDS(ke_data, "ke_data.rds")
ke_data <- readRDS(file = "ke_data.rds")
# saveRDS(data, file = "data.Rds")
# data.copy <- readRDS(file = "data.Rds")
# saveRDS(data, file = "data.Rds")