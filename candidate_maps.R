library(tidyverse)
library(rgdal)
library(leaflet)

legislators <- read_csv('Legislators.csv') %>% 
  mutate(pop = glue::glue('<strong>DISTRICT {District}</strong><br>Incumbent: {Full.Name}'), 
         chamber = str_sub(District,1, 1))
candidates <- read_csv('candidates_FINAL.csv')
house_retirements <- c(6, 56, 87, 43, 20, 31, 65, 3, 35, 72, 100) %>% paste0('H',.)
house_incumbents <- read_csv('Legislators.csv') %>% 
  mutate(chamber = str_sub(District,1, 1),
         District = as.numeric(str_sub(District, 2, nchar(District)))) %>%
  filter(chamber == 'H',
         Party == 'Democratic',
         !District %in% house_retirements)

district_crosswalk <- tibble(
  DistrictID =c('H1','H2','H3','H4','H5','H6','H7','H8','H9','H10','H11','H12','H13','H14','H15','H16','H17','H18','H19','H20','H21','H22','H23','H24','H25','H26','H27','H28','H29','H30','H31','H32','H33','H34','H35','H36','H37','H38','H39','H40','H41','H42','H43','H44','H45','H46','H47','H48','H49','H50','H51','H52','H53','H54','H55','H56','H57','H58','H59','H60','H61','H62','H63','H64','H65','H66','H67','H68','H69','H70','H71','H72','H73','H74','H75','H76','H77','H78','H79','H80','H81','H82','H83','H84','H85','H86','H87','H88','H89','H90','H91','H92','H93','H94','H95','H96','H97','H98','H99','H100','S1','S2','S3','S4','S5','S6','S7','S8','S9','S10','S11','S12','S13','S14','S15','S16','S17','S18','S19','S20','S21','S22','S23','S24','S25','S26','S27','S28','S29','S30','S31','S32','S33','S34','S35','S36','S37','S38'),
  DISTRICT = c(37,38,39,41,40,4,43,42,44,88,86,45,87,49,46,47,48,52,51,50,54,5,53,56,55,9,89,90,71,67,7,70,74,68,66,72,93,92,81,94,96,95,97,91,82,69,79,73,98,99,57,59,76,60,75,80,8,77,78,24,19,18,27,28,29,25,3,30,26,12,63,22,20,11,84,21,85,23,83,61,64,62,58,35,6,31,32,2,65,33,34,36,100,1,10,14,13,16,15,17,16,15,18,17,21,19,4,2,22,23,8,37,38,25,26,24,5,14,32,35,28,36,9,7,1,34,12,6,10,11,13,20,3,27,31,33,29,30),
  chamber = c('H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','H','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S','S')
)

house_dem <- candidates %>% 
  filter(Party == 'Democratic Party',
         Office == 'State Representative') %>% 
  mutate(merge_district = paste0('H', District),
         challenger_name = glue::glue('{`First Name`} {`Last Name`}'),
         ch_initial = glue::glue('{str_sub(`First Name`, 1, 1)}. {`Last Name`}')) %>% 
  select(merge_district, challenger_name, ch_initial, date_filed = `Date filed`) %>% 
  left_join(legislators %>% select(District, incumbent = Initial.Name), by = c('merge_district' = 'District')) %>% 
  filter(ch_initial != incumbent) %>% 
  select(-ch_initial, -incumbent) %>% 
  nest(-merge_district) %>% 
  mutate(challenger_pop = map_chr(data, function(d){
    d %>% 
      mutate(pop = glue::glue('<br>Challenger Name: {challenger_name}<br>Date Filed: {date_filed}<br>')) %>% 
      select(pop) %>% 
      as_vector() %>% 
      glue::collapse(sep = '')
  })) %>% 
  select(-data)

house <- legislators %>% 
  filter(chamber == 'H') %>% 
  left_join(house_dem, by = c('District' = 'merge_district')) %>% 
  mutate(retirement = District %in% house_retirements) %>% 
  mutate(status = case_when(Party == 'Democratic' & is.na(challenger_pop) & retirement == F ~ 'Incumbent',
                            Party == 'Democratic' & !is.na(challenger_pop) & retirement == F ~ 'Incumbent & Challenger',
                            Party == 'Democratic' & !is.na(challenger_pop) & retirement == T ~ 'Retiring Incumbent, Dem Running',
                            Party == 'Republican' & !is.na(challenger_pop) ~ 'Challenger',
                            Party == 'Republican' & is.na(challenger_pop) ~ 'Need Challenger',
                            Party == 'Democratic' & is.na(challenger_pop) & retirement == T ~ 'Retirement without Challenger'),
         color = case_when(status == 'Incumbent' ~ '#08519c',
                           status == 'Incumbent & Challenger' ~ '#3182bd',
                           status == 'Retiring Incumbent, Dem Running' ~ '#6baed6',
                           status == 'Challenger' ~ '#bdd7e7',
                           status == 'Need Challenger' ~ '#fc9272',
                           status == 'Retirement without Challenger' ~ '#de2d26')) %>% 
  mutate(pop = ifelse(is.na(challenger_pop), pop, paste0(pop, challenger_pop))) %>% 
  select(d = District, status, color, pop) %>% 
  left_join(district_crosswalk %>% filter(chamber == 'H') %>% select(-chamber), by = c('d' = 'DistrictID')) 
  


# house <- tibble(d = 1:100) %>% 
#   mutate(status = case_when(d %in% house_incumbents$District & d %in% house_dem$District ~ 'Incumbent & Challenger',
#                             d %in% house_incumbents$District ~ 'Incumbent',
#                             d %in% house_dem$District ~ 'Challenger',
#                             d %in% house_retirements ~ 'Retirement without Challenger',
#                             T ~ 'Need Challenger'),
#          color = case_when(status == 'Incumbent' ~ '#3182bd',
#                            status == 'Incumbent & Challenger' ~ '#9ecae1',
#                            status == 'Challenger' ~ '#deebf7',
#                            status == 'Need Challenger' ~ '#fc9272',
#                            status == 'Retirement without Challenger' ~ '#de2d26')) %>% 
#   mutate(d = paste0('H',d)) %>% 
#   left_join(district_crosswalk %>% filter(chamber == 'H') %>% select(-chamber), by = c('d' = 'DistrictID')) %>% 
#   left_join(legislators %>% select(District, pop), by = c('d' = 'District'))

senate_incumbents <- read_csv('Legislators.csv') %>% 
  mutate(chamber = str_sub(District,1, 1),
         District = as.numeric(str_sub(District, 2, nchar(District)))) %>% 
  filter(Party == 'Democratic',
         chamber == 'S')

senate_dem <-  candidates %>% 
  filter(Party == 'Democratic Party',
         Office == 'State Senator') %>% 
  mutate(merge_district = paste0('S', District),
         challenger_name = glue::glue('{`First Name`} {`Last Name`}'),
         ch_initial = glue::glue('{str_sub(`First Name`, 1, 1)}. {`Last Name`}')) %>% 
  select(merge_district, challenger_name, ch_initial, date_filed = `Date filed`) %>% 
  left_join(legislators %>% select(District, incumbent = Initial.Name), by = c('merge_district' = 'District')) %>% 
  filter(ch_initial != incumbent) %>% 
  select(-ch_initial, -incumbent) %>% 
  nest(-merge_district) %>% 
  mutate(challenger_pop = map_chr(data, function(d){
    d %>% 
      mutate(pop = glue::glue('<br>Challenger Name: {challenger_name}<br>Date Filed: {date_filed}<br>')) %>% 
      select(pop) %>% 
      as_vector() %>% 
      glue::collapse(sep = '')
  })) %>% 
  select(-data)

senate <-  legislators %>% 
  filter(chamber == 'S') %>% 
  mutate(even = as.numeric(str_sub(District, 2)) %% 2 == 0) %>%
  left_join(senate_dem, by = c('District' = 'merge_district')) %>% 
  mutate(status = case_when(even == F ~ 'No Race',
                            Party == 'Democratic' & is.na(challenger_pop) ~ 'Incumbent',
                            Party == 'Democratic' & !is.na(challenger_pop) ~ 'Incumbent & Challenger',
                            Party == 'Republican' & !is.na(challenger_pop) ~ 'Challenger',
                            Party == 'Republican' & is.na(challenger_pop) ~ 'Need Challenger'),
         color = case_when(status == 'No Race' ~ 'lightgray',
                           status == 'Incumbent' ~ '#08519c',
                           status == 'Incumbent & Challenger' ~ '#3182bd',
                           status == 'Challenger' ~ '#bdd7e7',
                           status == 'Need Challenger' ~ '#fc9272')) %>% 
  mutate(pop = ifelse(is.na(challenger_pop), pop, paste0(pop, challenger_pop))) %>% 
  select(d = District, status, color, pop) %>% 
  left_join(district_crosswalk %>% filter(chamber == 'S') %>% select(-chamber), by = c('d' = 'DistrictID')) 

house_map <- readOGR(dsn = './house_f', layer = 'house_map_shp')
house_map@data$id <- rownames(house_map@data)
house_map@data <- merge(house_map@data, house, by='DISTRICT')

senate_map <- readOGR(dsn = './senate_f', layer = 'senate_map_shp')
senate_map@data$id <- rownames(senate_map@data)
senate_map@data <- merge(senate_map@data, senate, by='DISTRICT')

house_leaflet <- leaflet(data = house_map) %>% 
  addTiles() %>% 
  addPolygons(data = house_map, stroke = F, fillOpacity = 0.45, color = ~color, popup = ~pop) %>% 
  addPolylines(weight = 2, color = 'black') %>% 
  addLegend(labels = c('Incumbent', 'Incumbent & Challenger', 'Retiring Incumbent, Dem Running', 'Challenger', 'Need Challenger', 'Retirement without Challenger'), 
            colors = c('#08519c', '#3182bd', '#6baed6', '#bdd7e7','#fc9272', '#de2d26'))

senate_leaflet <- leaflet(data = senate_map) %>% 
  addTiles() %>% 
  addPolygons(data = senate_map, stroke = F, fillOpacity = 0.45, color = ~color, popup = ~pop) %>% 
  addPolylines(weight = 2, color = 'black') %>% 
  addLegend(labels = c('Incumbent', 'Incumbent & Challenger', 'Challenger', 'Need Challenger', 'Retirement without Challenger'), 
            colors = c('#3182bd', '#9ecae1','#deebf7','#fc9272','#de2d26'))

save(house_leaflet, senate_leaflet, file = 'maps.rda')
