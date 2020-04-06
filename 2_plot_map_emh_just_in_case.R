#2_plot_maps
# clean_restart = function(leave = NULL){
#   rm(list=setdiff(ls(envir = .GlobalEnv), leave), envir = .GlobalEnv)
#   invisible(.rs.restartR())
# }
# 
# clean_restart()
#source('1_build_map.R')
load('mobility_data_maps.RData')
library(tidyverse)

#Static ggplot
#themes
ggplot = function(...) ggplot2::ggplot(...) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) #+
#  # guides(fill=guide_legend(ncol=1))

date_val %>% format('%d-%B-%Y') -> date_of_pull

plot_title = paste('\nUK Mobility')

#ggplot2 - annoying scales
#Groceries and Pharmacy
gro_pharm_sp_points = fortify(gro_pharm_sp, region = "id")
gro_pharm_sp_fort = dplyr::left_join(gro_pharm_sp_points, gro_pharm_sp@data, by="id")

gro_pharm_sp_fort %>% 
  ggplot(aes(y = lat, x = long, group = group)) + 
  geom_polygon(aes(fill = value), size = 0.1, color = 'black') + coord_equal(ratio = 1.3) + 
  scale_fill_gradientn(colours = c("#0f7000", "#56d300", "white", "red"), 
                                                                                                                                                                   values=c(0, 0.20, 0.25, 0.30, 0.75, 1),
                                                                                                                                                                     name='Reduction in\nPharmacy/Groceries Movements', 
                                                                                                                                                                    labels = scales::percent(0.50*-1.5:1.5), breaks = 0.50*-1.5:1.5,
                                                                                                                                                                    limits = c(-1, 1)) + 
                                                                                                                                                                    ggtitle(plot_title, subtitle = date_of_pull)
                                                                                                                                                                    


#Residential
resid_sp_points = fortify(resid_sp, region = "id")
resid_sp_fort = dplyr::left_join(resid_sp_points, resid_sp@data, by="id")

resid_sp_fort %>% 
  ggplot(aes(y = lat, x = long, group = group)) + 
  geom_polygon(aes(fill = value), size = 0.1, color = 'black') + 
  coord_equal(ratio = 1.3)  + 
  scale_fill_gradientn(colours = c("red", "white", "#56d300","#0f7000"), 
                                                                                                                                                                  values=c(0, 0.20, 0.60, 0.70, 0.80, 1),
                                                                                                                                                                  name='Increase in\nResidential Movements', 
                                                                                                                                                                  labels = scales::percent(0.50*-1.5:1.5), breaks = 0.50*-1.5:1.5,
                                                                                                                                                                  limits = c(-1, 1)) + 
                                                                                                                                                                    ggtitle(plot_title, subtitle = date_of_pull)


#Retail
retail_sp_points = fortify(retail_sp, region = "id")
retail_sp_fort = dplyr::left_join(retail_sp_points, retail_sp@data, by="id")

retail_sp_fort %>% 
  ggplot(aes(y = lat, x = long, group = group)) + geom_polygon(aes(fill = value), size = 0.1, color = 'black')  + scale_fill_gradientn(colours = c("#0f7000", "#56d300", "white", "red"), 
                                                                                                                                       values=c(0, 0.20, 0.25, 0.30, 0.75, 1),
                                                                                                                                       name='Reduction in\nRetail Movements', 
                                                                                                                                       labels = scales::percent(0.50*-1.5:1.5), breaks = 0.50*-1.5:1.5,
                                                                                                                                       limits = c(-1, 1)) + 
                                                                                                                                       ggtitle(plot_title, subtitle = date_of_pull)

#Transit
transit_sp_points = fortify(transit_sp, region = "id")
transit_sp_fort = dplyr::left_join(transit_sp_points, transit_sp@data, by="id")

transit_sp_fort %>% 
  ggplot(aes(y = lat, x = long, group = group)) + geom_polygon(aes(fill = value), size = 0.1, color = 'black') + coord_equal(ratio = 1.3) + scale_fill_gradientn(colours = c("#0f7000", "#56d300", "white", "red"), 
                                                                                                                                                                 values=c(0, 0.05, 0.15, 0.25, 0.75, 1),
                                                                                                                                                                 name='Reduction in\nTransit Movements', 
                                                                                                                                                                 labels = scales::percent(0.50*-1.5:1.5), breaks = 0.50*-1.5:1.5,
                                                                                                                                                                 limits = c(-1, 1)) + 
                                                                                                                                                                 ggtitle(plot_title, subtitle = date_of_pull)
#Parks
parks_sp_points = fortify(parks_sp, region = "id")
parks_sp_fort = dplyr::left_join(parks_sp_points, parks_sp@data, by="id")

parks_sp_fort %>% 
  ggplot(aes(y = lat, x = long, group = group)) + geom_polygon(aes(fill = value), size = 0.1, color = 'black') + coord_equal(ratio = 1.3) + scale_fill_gradientn(colours = c("#0f7000", "#56d300", "white", "red"), 
                                                                                                                                                                 values=c(0, 0.05, 0.15, 0.25, 0.60, 1),
                                                                                                                                                                 name='Reduction in\nPark Movements', 
                                                                                                                                                                 labels = scales::percent(0.50*-1.5:1.5), breaks = 0.50*-1.5:1.5,
                                                                                                                                                                 limits = c(-1, 1)) + 
                                                                                                                                                                 ggtitle(plot_title, subtitle = date_of_pull)

#Workplace
workplace_sp_points = fortify(workplace_sp, region = "id")
workplace_sp_fort = dplyr::left_join(workplace_sp_points, workplace_sp@data, by="id")

workplace_sp_fort %>% 
  ggplot(aes(y = lat, x = long, group = group)) + geom_polygon(aes(fill = value), size = 0.1, color = 'black') + coord_equal(ratio = 1.3) + scale_fill_gradientn(colours = c("#0f7000", "#56d300", "white", "red"), 
                                                                                                                                                                 values=c(0, 0.15, 0.20, 0.25, 0.75, 1),
                                                                                                                                                                 name='Reduction in\nWorkplace Movements', 
                                                                                                                                                                 labels = scales::percent(0.50*-1.5:1.5), breaks = 0.50*-1.5:1.5,
                                                                                                                                                                 limits = c(-1, 1)) + 
                                                                                                                                                                 ggtitle(plot_title, subtitle = date_of_pull)


