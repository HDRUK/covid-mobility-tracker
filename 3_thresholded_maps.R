#3 For thresholds
library(patchwork)
threshold_level = 75#BEWARE THIS HAS NO DIRECTION - can be made to have direction if desired

date_val %>% format('%d-%B-%Y') -> date_of_pull

plot_title = paste('\nUK Mobility')

#ggplot2 - annoying scales

#Residential THIS IS NEGATIVE AS IT SHOuLD GO UP!!
resid_sp_points = fortify(resid_sp, region = "id")
resid_sp_fort = dplyr::left_join(resid_sp_points, resid_sp@data, by="id")

p1 = resid_sp_fort %>% 
  mutate(compliance_threshold = ifelse(sqrt(value_perc^2)  >= threshold_level, paste0('Over ', sqrt(threshold_level^2), '% compliance'), paste0('Under ', sqrt(threshold_level^2), '% compliance'))) %>% 
  mutate(compliance_threshold = factor(compliance_threshold, levels = c(paste0('Under ', sqrt(threshold_level^2), '% compliance'), paste0('Over ', sqrt(threshold_level^2), '% compliance')))) %>% 
  ggplot(aes(y = lat, x = long, group = group)) + geom_polygon(aes(fill = compliance_threshold), size = 0.1, color = 'black') + coord_equal(ratio = 1.3) + scale_fill_manual(name='Increase in\n Residential Movements',
                                                                                                                                                                             values = c('#ffcfbe', '#39d943'), drop=FALSE) + 
  ggtitle(plot_title, subtitle = date_of_pull)

#Groceries and Pharmacy
gro_pharm_sp_points = fortify(gro_pharm_sp, region = "id")
gro_pharm_sp_fort = dplyr::left_join(gro_pharm_sp_points, gro_pharm_sp@data, by="id")

p2 = gro_pharm_sp_fort %>% 
  mutate(compliance_threshold = ifelse(sqrt(value_perc^2) >= threshold_level, paste0('Over ', sqrt(threshold_level^2), '% compliance'), paste0('Under ', sqrt(threshold_level^2), '% compliance'))) %>% 
  mutate(compliance_threshold = factor(compliance_threshold, levels = c(paste0('Under ', sqrt(threshold_level^2), '% compliance'), paste0('Over ', sqrt(threshold_level^2), '% compliance')))) %>% 
  ggplot(aes(y = lat, x = long, group = group)) + geom_polygon(aes(fill = compliance_threshold), size = 0.1, color = 'black') + coord_equal(ratio = 1.3) + scale_fill_manual(name='Reduction in\nPharmacy/Groceries Movements',
                                                                                                                                                                             values = c('#ffcfbe', '#39d943'), drop=FALSE) 


#Retail
retail_sp_points = fortify(retail_sp, region = "id")
retail_sp_fort = dplyr::left_join(retail_sp_points, retail_sp@data, by="id")

p3 = retail_sp_fort %>% 
  mutate(compliance_threshold = ifelse(sqrt(value_perc^2)  >= threshold_level, paste0('Over ', sqrt(threshold_level^2), '% compliance'), paste0('Under ', sqrt(threshold_level^2), '% compliance'))) %>% 
  mutate(compliance_threshold = factor(compliance_threshold, levels = c(paste0('Under ', sqrt(threshold_level^2), '% compliance'), paste0('Over ', sqrt(threshold_level^2), '% compliance')))) %>% 
  ggplot(aes(y = lat, x = long, group = group)) + geom_polygon(aes(fill = compliance_threshold), size = 0.1, color = 'black') + coord_equal(ratio = 1.3) + scale_fill_manual(name='Reduction in\nRetail Movements',
                                                                                                                                                                               values = c('#ffcfbe', '#39d943'), drop=FALSE) 

#Transit
transit_sp_points = fortify(transit_sp, region = "id")
transit_sp_fort = dplyr::left_join(transit_sp_points, transit_sp@data, by="id")

p4 = transit_sp_fort %>% 
  mutate(compliance_threshold = ifelse(sqrt(value_perc^2)  >= threshold_level, paste0('Over ', sqrt(threshold_level^2), '% compliance'), paste0('Under ', sqrt(threshold_level^2), '% compliance'))) %>% 
  mutate(compliance_threshold = factor(compliance_threshold, levels = c(paste0('Under ', sqrt(threshold_level^2), '% compliance'), paste0('Over ', sqrt(threshold_level^2), '% compliance')))) %>% 
  ggplot(aes(y = lat, x = long, group = group)) + geom_polygon(aes(fill = compliance_threshold), size = 0.1, color = 'black') + coord_equal(ratio = 1.3) + scale_fill_manual(name='Reduction in\nTransit Movements',
                                                                                                                                                                               values = c('#ffcfbe', '#39d943'), drop=FALSE) 

#Parks
parks_sp_points = fortify(parks_sp, region = "id")
parks_sp_fort = dplyr::left_join(parks_sp_points, parks_sp@data, by="id")

p5 = parks_sp_fort %>% 
  mutate(compliance_threshold = ifelse(sqrt(value_perc^2)  >= threshold_level, paste0('Over ', sqrt(threshold_level^2), '% compliance'), paste0('Under ', sqrt(threshold_level^2), '% compliance'))) %>% 
  mutate(compliance_threshold = factor(compliance_threshold, levels = c(paste0('Under ', sqrt(threshold_level^2), '% compliance'), paste0('Over ', sqrt(threshold_level^2), '% compliance')))) %>% 
  ggplot(aes(y = lat, x = long, group = group)) + geom_polygon(aes(fill = compliance_threshold), size = 0.1, color = 'black') + coord_equal(ratio = 1.3) + scale_fill_manual(name='Reduction in\nPark Movements',
                                                                                                                                                                             values = c('#ffcfbe', '#39d943'), drop=FALSE) 
                                                                                                                                                                             
#workplace                                                                                                                                                                                                                                                                                 
workplace_sp_points = fortify(workplace_sp, region = "id")
workplace_sp_fort = dplyr::left_join(workplace_sp_points, workplace_sp@data, by="id")

p6 = workplace_sp_fort %>% 
  mutate(compliance_threshold = ifelse(sqrt(value_perc^2)  >= threshold_level, paste0('Over ', sqrt(threshold_level^2), '% compliance'), paste0('Under ', sqrt(threshold_level^2), '% compliance'))) %>% 
  mutate(compliance_threshold = factor(compliance_threshold, levels = c(paste0('Under ', sqrt(threshold_level^2), '% compliance'), paste0('Over ', sqrt(threshold_level^2), '% compliance')))) %>% 
  ggplot(aes(y = lat, x = long, group = group)) + geom_polygon(aes(fill = compliance_threshold), size = 0.1, color = 'black') + coord_equal(ratio = 1.3) + scale_fill_manual(name='Reduction in\nWorkplace Movements',
                                                                                                                                                                               values = c('#ffcfbe', '#39d943'), drop=FALSE)

#patchwork

(p1 | p2 | p3) / 
  (p4 | p5 | p6)
