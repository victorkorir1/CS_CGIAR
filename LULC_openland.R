#* Land Use Land Cover change
#* 
#* 
#* Author:: Victor Korir
####################################################################################
library(sf)
library(terra)
library(tidyverse)
library(raster)
library(tmap)
library(OpenLand)
library(geodata)


AOI <- st_read("D:/OneDrive - CGIAR/SA_Team/korir/LULC/igad_cluster_1_1/igad_cluster_1_1.shp")
AOI_buffer <- st_buffer(AOI, 0.44996400287491034)
plot(AOI)

#ESA_CCI Land Cover
CCI_1995 <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1995-v2.0.7cds.nc")
CCI_2004 <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2004-v2.0.7cds.nc")
CCI_2013 <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2013-v2.0.7cds.nc")
CCI_2007 <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2007-v2.0.7cds.nc")
CCI_2012 <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2012-v2.0.7cds.nc")
CCI_2017 <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/C3S-LC-L4-LCCS-Map-300m-P1Y-2017-v2.1.1.nc")
CCI_2022 <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc")



#Cropping and masking to the AOI
CCI_1995 <- CCI_1995 %>% terra::crop(., st_bbox(AOI)) %>% terra::mask(., sf::st_as_sf(AOI))
CCI_2004 <- CCI_2004 %>% terra::crop(., st_bbox(AOI)) %>% terra::mask(., sf::st_as_sf(AOI))
CCI_2013 <- CCI_2013 %>% terra::crop(., st_bbox(AOI)) %>% terra::mask(., sf::st_as_sf(AOI))
CCI_2007 <- CCI_2007 %>% terra::crop(., st_bbox(AOI)) %>% terra::mask(., sf::st_as_sf(AOI))
CCI_2012 <- CCI_2012 %>% terra::crop(., st_bbox(AOI)) %>% terra::mask(., sf::st_as_sf(AOI))
CCI_2017 <- CCI_2017 %>% terra::crop(., st_bbox(AOI)) %>% terra::mask(., sf::st_as_sf(AOI))
CCI_2022 <- CCI_2022 %>% terra::crop(., st_bbox(AOI)) %>% terra::mask(., sf::st_as_sf(AOI))


#Defining reclass matrix based on the following
#*1-cropland, 2- irrigated cropland, 3-cropland/tree cover, 4-tree cover, 5- grassland,
#*6-shrubland,7-Flooded shrub/herbaceous cover, 8- urban areas, 9- water, 10- bareland


Reclass_mat <- matrix(c(10, 1, 11, 1, 20, 2, 30, 3, 40, 3, 60, 4, 61, 4, 50, 4, 
                        62, 4,100, 4,110, 5, 122, 6, 120, 6, 130, 5, 150, 6, 151,
                        6, 152,6, 153,6, 160,4,170,4, 180, 7, 190, 8, 200, 10,201, 10, 202, 10, 210, 9),
                      ncol = 2,
                      byrow = TRUE)
#Reclassifying

LULC1995_reclass <- terra::classify(CCI_1995, Reclass_mat)
LULC2004_reclass <- terra::classify(CCI_2004, Reclass_mat)
LULC2013_reclass <- terra::classify(CCI_2013, Reclass_mat)
LULC2022_reclass <- terra::classify(CCI_2022, Reclass_mat)


kara1_class <- c(LULC1995_reclass$lccs_class, LULC2004_reclass$lccs_class, LULC2013_reclass$lccs_class, 
                 LULC2022_reclass$lccs_class)

##############################################################################


#Loading data(Brick required by Openland package) 
lcc_kara1_cl <- brick(kara1_class)

names(lcc_kara1_cl) <- c('LULC_1995', 'LULC_2004', 'LULC_2013','LULC_2022')

#Contingency table



cont_table <- OpenLand::contingencyTable(lcc_kara1_cl, 300)
cont_table$tb_legend$categoryName <-as.character(cont_table$tb_legend$categoryName)

#cont_table$tb_legend$categoryName[cont_table$tb_legend$categoryValue ==1 ,] <- 'Cropland'
cont_table$tb_legend$color <- c("#33a02c", "#b2df8a", "#CAFF70", "#ff7f00",
                                "#fdbf6f", "#fb9a99", "#436EEE",
                                "#6a3d9a", "#1f78b4", "#e31a1c")

#cont_table$tb_legend <- left_join(cont_table$tb_legend, labels, by= c('categoryValue' = 'V1'))
 
#cont_table$tb_legend <- select(cont_table$tb_legend, -categoryName)
#names(cont_table$tb_legend) <- c("categoryValue" , "color" ,"categoryName" )

labels <- read.csv('D:/OneDrive - CGIAR/SA_Team/korir/LULC/karamoja_classes.csv', header = F)
cont_table$tb_legend$categoryName <- labels$V2

karaSL <- intensityAnalysis(dataset = cont_table, category_n ='Grassland' , category_m ='Bareland', area_km2 = T )

#Interval level plot
interval<-plot(karaSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
     leg_curv = c(x = 2/10, y = 3/10))
#Gain area
gainarea<-plot(karaSL$category_lvlGain,
     labels = c(leftlabel = bquote("Gain Area (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))
#Loss area
lossarea<-plot(karaSL$category_lvlLoss,
     labels = c(leftlabel = bquote("Loss Area (" ~ km^2 ~ ")"),
                rightlabel = "Loss Intensity (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))

netgrossplot <-netgrossplot(dataset = cont_table$lulc_Onestep,
             legendtable = cont_table$tb_legend,
             xlab = "LUC Category",
             ylab = bquote("Area (" ~ km^2 ~ ")"),
             changesLabel = c(GC = "Gross changes", NG = "Net Gain", NL = "Net Loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"))

chordDiagramLand(dataset = cont_table$lulc_Onestep,
                 legendtable = cont_table$tb_legend)

barplotLand(dataset = cont_table$lulc_Multistep, 
            legendtable = cont_table$tb_legend,
            xlab = "Year",
            ylab = bquote("Area (" ~ km^2~ ")"),
            area_km2 = TRUE)


ken_setl <- st_read('D:/OneDrive - CGIAR/SA_Team/korir/LULC/hotosm_ken_populated_places_points_shp.shp')%>% st_set_crs(st_crs(AOI)) %>% st_intersection(., AOI) %>% filter(., place == 'town')
uga_setl <- st_read('D:/OneDrive - CGIAR/SA_Team/korir/LULC/hotosm_uga_populated_places_points_shp.shp')%>% st_set_crs(st_crs(AOI))%>% st_intersection(., AOI) %>% filter(., place == 'town')
ssd_setl <- st_read('D:/OneDrive - CGIAR/SA_Team/korir/LULC/SSD_PopulatedAreas_Dataset/ssd_pppls_ocha_20221216.shp' )%>% st_set_crs(st_crs(AOI))%>% st_intersection(., AOI) 

#Country boundaries
eth_b <- st_read('D:/OneDrive - CGIAR/SA_Team/korir/LULC/Country_boundaries/eth_adm_csa_bofedb_2021_shp/eth_admbnda_adm0_csa_bofedb_itos_2021.shp' )%>% st_set_crs(st_crs(AOI)) %>% dplyr::select(c('ADM0_EN', 'geometry'))
ken_b <- st_read("D:/OneDrive - CGIAR/SA_Team/korir/LULC/Country_boundaries/ken_adm_iebc_20191031_shp/ken_admbnda_adm0_iebc_20191031.shp")%>% st_set_crs(st_crs(AOI)) %>% dplyr::select(c('ADM0_EN', 'geometry'))
uga_b <- st_read("D:/OneDrive - CGIAR/SA_Team/korir/LULC/Country_boundaries/uga_admbnda_ubos_20200824_shp/uga_admbnda_adm0_ubos_20200824.shp")%>% st_set_crs(st_crs(AOI)) %>% dplyr::select(c('ADM0_EN', 'geometry'))
ssd_b <- st_read("D:/OneDrive - CGIAR/SA_Team/korir/LULC/Country_boundaries/ssd_admbnda_imwg_nbs_20230829_shp/ssd_admbnda_imwg_nbs_20230829_SHP/ssd_admbnda_adm0_imwg_nbs_20230829.shp")%>% st_set_crs(st_crs(AOI)) %>% st_difference(., ken_b) %>% dplyr::select(c('ADM0_EN', 'geometry'))

gha <- st_simplify(rbind(eth_b,ken_b,uga_b,ssd_b))


ssd <- geodata::gadm('SSD', level = 0, path = tempdir())
uga <- geodata::gadm('UGA', level = 0, path = tempdir())
ken <- geodata::gadm('KEN', level = 0, path = tempdir())
eth <- geodata::gadm('ETH', level = 0, path = tempdir())
gha <- st_as_sf(rbind(ssd, uga, ken, eth))

################Conf-clim geojsons######################################################
eth_gj <- st_read("Z:/1.Data/Palmira/CSO/data/ETH/_results/clim_conflict_ips_overlays.geojson") %>% st_set_crs(st_crs(AOI))
ken_gj <- st_read("Z:/1.Data/Palmira/CSO/data/KEN/_results/clim_conflict_ips_overlays.geojson")%>% st_set_crs(st_crs(AOI))
ssd_gj <- st_read("Z:/1.Data/Palmira/CSO/data/SSD/_results/clim_conflict_ips_overlays.geojson")
uga_gj <- st_read("Z:/1.Data/Palmira/CSO/data/UGA/_results/clim_conflict_ips_overlays.geojson")

Ken_high <- ken_gj[which(ken_gj$conflict_clust_short_label=='High'),] %>%  st_intersection(., AOI)
Uga_high <- uga_gj[which(uga_gj$conflict_clust_short_label=='High') ,] %>%  st_intersection(., AOI)
Ken_moderate <- ken_gj[which(ken_gj$conflict_clust_short_label=='Moderate'),]%>%  st_intersection(., AOI)
Uga_moderate <- uga_gj[which(uga_gj$conflict_clust_short_label=='Moderate') ,] %>%  st_intersection(., AOI)

############################ Temperature and Precipiation ##########################
basetemp <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ftas_1991_2020.tif") %>% terra::mask(., AOI)
futuretemp <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ftas_2024_2050.tif")%>% terra::mask(., AOI)
basepr <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/pr_1991_2020.tif")%>% terra::mask(., AOI)
futurepr <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/pr_2024_2050.tif")%>% terra::mask(., AOI)

temp_dif <- futuretemp-basetemp
pr_dif <- futurepr-basetemp


#Plotting the changes per pixel, o - no change, 1- single change, 2 - double change
testacc <- acc_changes(lcc_kara1_cl)
acc_map <- tmap::tm_shape(testacc[[1]]) +
  tmap::tm_raster(
    style = "cat",
    labels = c(
      paste0(testacc[[2]]$PxValue[1], " Change", " (", round(testacc[[2]]$Percent[1], 2), "%", ")"),
      paste0(testacc[[2]]$PxValue[2], " Change", " (", round(testacc[[2]]$Percent[2], 2), "%", ")"),
      paste0(testacc[[2]]$PxValue[3], " Changes", " (", round(testacc[[2]]$Percent[3], 2), "%", ")"),
      paste0(testacc[[2]]$PxValue[4], " Changes", " (", round(testacc[[2]]$Percent[4], 2), "%", ")")
    ),
    palette = c("lightgrey", "#FFD700","#ff7f00",'darkred'),
    title = "Changes in the interval \n1995 - 2022"
  ) +  tm_shape(AOI)+tm_borders(col = 'black', lwd = 0.5)+
  tm_shape(gha)+tm_borders(col = 'black', lwd = 2)+tm_text('COUNTRY', remove.overlap = F)+
  tm_shape(uga_setl)+tm_text('name', remove.overlap = T, print.tiny = T)+
  tm_shape(ken_setl)+tm_text('name', remove.overlap = T, print.tiny = T)+
  tm_shape(Ken_high)+ tm_borders(col = "red")+
  tm_shape(Uga_high)+ tm_borders(col = "red")+
  tm_shape(Ken_moderate)+ tm_borders(col = "purple")+
  tm_shape(Uga_moderate)+ tm_borders(col = "purple")+
 
  tmap::tm_legend(
    position = c(0.01, 0.02),
    legend.title.size = 1.2,
    legend.title.fontface = "bold",
    legend.text.size = 0.8
  ) +
  tmap::tm_compass(type = "arrow",
                   position = c("right", "top"),
                   size = 3) +
  tmap::tm_scale_bar(
    breaks = c(seq(0, 40, 10)),
    position = c(0.76, 0.001),
    text.size = 0.6
  ) +
  tmap::tm_graticules(
    n.x = 6,
    n.y = 6,
    lines = FALSE,
    #alpha = 0.1
    labels.rot = c(0, 90)
  ) +
  tmap::tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.02))

tmap_save(acc_map,'acc_map.png', device = png, dpi = 300)

#Land cover maps



LULC_map <- tmap::tm_shape(LULC1995_reclass$lccs_class) +
  tmap::tm_raster(
    style = "cat",
    labels = as.character(labels$V2),
    palette = c("#33a02c", "#b2df8a", "#CAFF70", "#ff7f00",
                "#fdbf6f", "#fb9a99", "#436EEE",
                "#6a3d9a", "#1f78b4", "#e31a1c"),
    title = "LULC 1995"
  ) + tm_shape(AOI)+tm_borders(col = 'black', lwd = 2)+
  tm_shape(gha)+tm_borders(col = 'black', lwd = 2)+tm_text('COUNTRY', remove.overlap = F)+
  tm_shape(uga_setl)+tm_text('name', remove.overlap = T, print.tiny = T)+
  tm_shape(ken_setl)+tm_text('name', remove.overlap = T, print.tiny = T)+
  
  tmap::tm_legend(
    position = c(0.01, 0.01),
    legend.title.size = 1.2,
    legend.title.fontface = "bold",
    legend.text.size = 0.8
  ) +
  tmap::tm_compass(type = "arrow",
                   position = c("right", "top"),
                   size = 3) +
  tmap::tm_scale_bar(
    breaks = c(seq(0, 40, 10)),
    position = c(0.76, 0.001),
    text.size = 0.6
  ) +
  
  tmap::tm_graticules(
    n.x = 6,
    n.y = 6,
    lines = FALSE,
    #alpha = 0.1
    labels.rot = c(0, 90)
  ) +
  tmap::tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.02))

#clim plots
temp_map <- tmap::tm_shape(pr_dif) +
  tmap::tm_raster(
    style = "cont",
    title = "Precipitation Variation(mm/day)"
  ) +  tm_shape(AOI)+tm_borders(col = 'black', lwd = 0.5)+
  tm_shape(gha)+tm_borders(col = 'black', lwd = 2)+tm_text('COUNTRY', remove.overlap = F)+
  tm_shape(uga_setl)+tm_text('name', remove.overlap = T, print.tiny = T)+
  tm_shape(ken_setl)+tm_text('name', remove.overlap = T, print.tiny = T)+
  tm_shape(Ken_high)+ tm_borders(col = "red")+
  tm_shape(Uga_high)+ tm_borders(col = "red")+
  tm_shape(Ken_moderate)+ tm_borders(col = "purple")+
  tm_shape(Uga_moderate)+ tm_borders(col = "purple")+
  
  tmap::tm_legend(
    position = c(0.01, 0.02),
    legend.title.size = 1.2,
    legend.title.fontface = "bold",
    legend.text.size = 0.8
  ) +
  tmap::tm_compass(type = "arrow",
                   position = c("right", "top"),
                   size = 3) +
  tmap::tm_scale_bar(
    breaks = c(seq(0, 40, 10)),
    position = c(0.76, 0.001),
    text.size = 0.6
  ) +
  tmap::tm_graticules(
    n.x = 6,
    n.y = 6,
    lines = FALSE,
    #alpha = 0.1
    labels.rot = c(0, 90)
  ) +
  tmap::tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.02))
tmap_save(temp_map)
        