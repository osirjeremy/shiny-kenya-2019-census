library(tidyverse)
library(tidytuesdayR)
library(rKenyaCensus)
library(biscale)
library(cowplot)
library(extrafont)
library(dplyr)

library(sf)
library(biscale)
library(ggplot2)
library(cowplot)


k_map = rKenyaCensus::KenyaCounties_SHP %>%
  sf::st_as_sf()


k_data = ke_data %>%
  filter(., admin_area == "County") %>% 
  rename(., County = county) %>% 
  select(., c(County, pop_total, mpo_total,no_fhs, no_fhs_subsistence, no_fhs_commercial)) %>%
  mutate(., fhs_subsistence_perc= no_fhs/pop_total,
         mpo_total_perc = mpo_total/pop_total,
         County = toupper(County)) %>% 
  bi_class(x = fhs_subsistence_perc, y= mpo_total_perc , style = "quantile", dim = 3)

k_data <- k_data %>% 
  select(., c(County, fhs_subsistence_perc,mpo_total_perc, bi_class)) %>% 
  mutate(., County = ifelse(test = County ==  "TAITA TAVETA", yes = "TAITA/TAVETA", 
                   no = ifelse(County == "ELGEYO-MARAKWET", "ELGEYO/MARAKWET",
                               ifelse(County == "NAIROBI", "NAIROBI CITY", County))))

unique(k_data$County)[which(!unique(k_data$County) %in% k_map$County)]

# For some reason, the column data is getting passed as NA
bi_map <- left_join(k_map, k_data)

map_data <- bi_map %>% 
  select(., c(County, mpo_total_perc,fhs_subsistence_perc, bi_class))

map <-bi_map %>% 
  ggplot()+
  geom_sf(data = map_data, aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_theme() +
  bi_scale_fill(pal = "DkBlue", dim = 3)+
  theme(plot.title.position = "plot", 
        plot.caption.position = "plot",
        plot.title    = element_text(size = 15, hjust = .5),
        plot.caption  = element_text(size =  8, hjust = .5)) +
  labs(title = "Mobile Phone Ownership and Subsistence Farmers in Kenya")


legend = biscale::bi_legend(
  pal = "DkBlue",
  dim = 3,
  xlab = "More Subsist. Farmers",
  ylab = "Higher % Phones", 
  size = 9)

# combine map with legend
finalPlot <- ggdraw()+
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.15, 0.09, 0.2, 0.2) 

bi_chloro_SubFHS_Phone <- finalPlot 
bi_chloro_SubFHS_Phone
saveRDS(bi_chloro_SubFHS_Phone, file ="./bi_chloro_SubFHS_Phone.rds" )
