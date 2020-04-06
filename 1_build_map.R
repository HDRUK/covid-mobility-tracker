#1 analyse and map out
source('0_download_pdf.R')
library(raster)
library(sf)
library(sp)
library(tidyverse)

eng_wal_counties_unitary_authorities = geojsonio::geojson_read('geoJSONs/unitary_auth_counties_eng_wal.geojson', what = 'sp')
sco_counties_unitary_authorities = geojsonio::geojson_read('geoJSONs/scotland_la.geojson', what = 'sp')
ni_counties = geojsonio::geojson_read('geoJSONs/ni_counties.geojson', what = 'sp')

#prepare geoJSONs for combining
eng_wal_counties_unitary_authorities$bng_e = NULL
eng_wal_counties_unitary_authorities$bng_n = NULL
eng_wal_counties_unitary_authorities$long = NULL
eng_wal_counties_unitary_authorities$lat = NULL
eng_wal_counties_unitary_authorities$st_areashape = NULL
eng_wal_counties_unitary_authorities$st_lengthshape = NULL

ni_counties$id = ni_counties$COUNTY_ID
ni_counties$second_id = ni_counties$OBJECTID
ni_counties$name = ni_counties$CountyName
ni_counties$details = ni_counties$Area_SqKM

ni_counties$COUNTY_ID = NULL
ni_counties$OBJECTID = NULL
ni_counties$CountyName = NULL
ni_counties$Area_SqKM = NULL

names(sco_counties_unitary_authorities) = c('id', 'second_id', 'name', 'details')

sco_counties_unitary_authorities$details = 'Scotland'

names(eng_wal_counties_unitary_authorities) = c('id', 'second_id', 'name', 'details')

eng_sco_wal = rbind(sco_counties_unitary_authorities, eng_wal_counties_unitary_authorities)

eng_sco_wal_ni = rbind(eng_sco_wal, ni_counties)

#remove weird council names
uk_location_data %>% 
  mutate(location = stringr::str_to_title(location),
         location = gsub(' Council', '', location),
         location = gsub('Borough Of ', '', location),
         location = gsub(' County Borough', '', location),
         location = gsub('Na H-Eileanan An Iar', 'Eilean Siar', location),
         location = gsub('Wrexham Principal Area', 'Wrexham', location)) -> uk_location_data

#make the names correct/ line up with google
eng_sco_wal_ni$name %>% 
  tibble(name = .) %>%
  mutate(name = stringr::str_to_title(name),
         name = gsub('Londonderry', 'Derry And Strabane', name),
         name = gsub(', City Of', '', name),
         name = gsub(', County Of', '', name),
         name = gsub('Borough Of ', '', name),
         name = gsub('Orkney Islands', 'Orkney', name),
         name = gsub('City Of Edinburgh', 'Edinburgh', name),
         name = gsub('Bristol', 'City Of Bristol', name),
         name = gsub('Armagh', 'Armagh City And Banbridge And Craigavon', name)) -> location_eng_sco_wal_ni

eng_sco_wal_ni$name = location_eng_sco_wal_ni$name

#merge with google data
unmatched_locations = read_csv('unmatched_names.csv') 

eng_sco_wal_ni$name %>% 
  tibble(name = .) %>%
  left_join(unmatched_locations, by = 'name') %>% 
  mutate(merge_name = name) %>% 
  mutate(name = ifelse(is.na(new_name), name, new_name)) %>% 
  left_join(uk_location_data, by = c('name' = 'location')) %>% 
  dplyr::select(-new_name) %>% 
  mutate(value_perc = as.numeric(as.character(value*100)),
         value =  as.numeric(as.character(value)),
         over_threshold = ifelse(value_perc >= 75, 'Over 75%', 'Under 75%')) -> uk_location_data

# #now merge on by new_name
# 
# uk_location_data %>% 
#   distinct(name, new_name) -> name_mapping

#Now merge this into the polygon
uk_location_data %>% 
  dplyr::filter(entity == 'retail_recr') %>% 
  sp::merge(eng_sco_wal_ni, ., by.x = 'name', by.y = 'merge_name') -> retail_sp

uk_location_data %>% 
  dplyr::filter(entity == 'residential') %>% 
  sp::merge(eng_sco_wal_ni, ., by.x = 'name', by.y = 'merge_name') -> resid_sp

uk_location_data %>% 
  dplyr::filter(entity == 'parks') %>% 
  sp::merge(eng_sco_wal_ni, ., by.x = 'name', by.y = 'merge_name') -> parks_sp

uk_location_data %>% 
  dplyr::filter(entity == 'grocery_pharm') %>% 
  sp::merge(eng_sco_wal_ni, ., by.x = 'name', by.y = 'merge_name') -> gro_pharm_sp

uk_location_data %>% 
  dplyr::filter(entity == 'transit') %>% 
  sp::merge(eng_sco_wal_ni, ., by.x = 'name', by.y = 'merge_name') -> transit_sp

uk_location_data %>% 
  dplyr::filter(entity == 'workplace') %>% 
  sp::merge(eng_sco_wal_ni, ., by.x = 'name', by.y = 'merge_name') -> workplace_sp

#save and exit

rm(list=setdiff(ls(), c('uk_overall_data', 'uk_location_data', 'workplace_sp',
                        'transit_sp', 'gro_pharm_sp', 'parks_sp', 'resid_sp', 
                        'retail_sp', 'date_val')))

#save image
save.image('mobility_data_maps.RData')
