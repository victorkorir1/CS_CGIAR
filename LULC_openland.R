#* Land Use Land Cover classification
#* 
#* 
#* Author:: Victor Korir
####################################################################################
AOI <- st_read("D:/OneDrive - CGIAR/SA_Team/korir/LULC/igad_cluster_1_1/igad_cluster_1_1.shp")
plot(AOI)

#ESA_CCI Land Cover
CCI_1992 <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992-v2.0.7.tif")
CCI_2000 <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7.tif")
CCI_2010 <- terra::rast("D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010-v2.0.7.tif")



#Cropping to the AOI
CCI_1992 <- terra::crop(CCI_1992, st_bbox(AOI))
CCI_2010 <- terra::crop(CCI_2010, st_bbox(AOI))
CCI_2000 <- terra::crop(CCI_2000, st_bbox(AOI))
#Masking to the AOI
CCI_1992 <- terra::mask(CCI_1992, sf::st_as_sf(AOI))
CCI_2010 <- terra::mask(CCI_2010, sf::st_as_sf(AOI))
CCI_2000 <- terra::mask(CCI_2000, sf::st_as_sf(AOI))

#Defining reclass matrix based on the following
#*1-cropland, 2- irrigated cropland, 3-cropland/tree cover, 4-tree cover, 5- grassland,
#*6-shrubland,7-Flooded shrub/herbaceous cover, 8- urban areas, 9- water, 10- bareland


Reclass_mat <- matrix(c(10, 1, 11, 1, 20, 2, 30, 3, 40, 3, 60, 4, 61, 4, 50, 4, 
                        62, 4,100, 4,110, 5, 122, 6, 120, 6, 130, 5, 150, 6, 151,
                        6, 152,6, 153,6, 160,4,170,4, 180, 7, 190, 8, 200, 10,201, 10, 202, 10, 210, 9),
                      ncol = 2,
                      byrow = TRUE)
#Reclassifying

LULC1992_reclass <- terra::classify(CCI_1992, Reclass_mat)
LULC2000_reclass <- terra::classify(CCI_2000, Reclass_mat)
LULC2010_reclass <- terra::classify(CCI_2010, Reclass_mat)

Kara1_stack_unclass <- c(CCI_1992, CCI_2000, CCI_2010)
kara1_class <- c(LULC1992_reclass, LULC2000_reclass, LULC2010_reclass)

##############################################################################
library(OpenLand)

#Loading data 
lcc_kara1_cl <- brick(kara1_class)
lcc_kara1 <- brick('D:/OneDrive - CGIAR/SA_Team/korir/LULC/ESA_CCI/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992-2010.tif')
names(lcc_kara1) <- c('LULC_1992', 'LULC_2000', 'LULC_2010')
names(lcc_kara1_cl) <- c('LULC_1992', 'LULC_2000', 'LULC_2010')

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

karaSL <- intensityAnalysis(dataset = cont_table,
                            category_n = "Grassland", category_m = "Cropland")

#Interval level plot
plot(karaSL$interval_lvl,
     labels = c(leftlabel = "Interval Change Area (%)",
                rightlabel = "Annual Change Area (%)"),
     marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
     leg_curv = c(x = 2/10, y = 3/10))
#Gain area
plot(karaSL$category_lvlGain,
     labels = c(leftlabel = bquote("Gain Area (" ~ km^2 ~ ")"),
                rightlabel = "Intensity Gain (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))
#Loass area
plot(karaSL$category_lvlLoss,
     labels = c(leftlabel = bquote("Loss Area (" ~ km^2 ~ ")"),
                rightlabel = "Loss Intensity (%)"),
     marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
     leg_curv = c(x = 5/10, y = 5/10))

netgrossplot(dataset = cont_table$lulc_Onestep,
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


#Plotting the changes per pixel, o - no change, 1- single change, 2 - double change
testacc <- acc_changes(lcc_kara1_cl)
acc_map <- tmap::tm_shape(testacc[[1]]) +
  tmap::tm_raster(
    style = "cat",
    labels = c(
      paste0(testacc[[2]]$PxValue[1], " Change", " (", round(testacc[[2]]$Percent[1], 2), "%", ")"),
      paste0(testacc[[2]]$PxValue[2], " Change", " (", round(testacc[[2]]$Percent[2], 2), "%", ")"),
      paste0(testacc[[2]]$PxValue[3], " Changes", " (", round(testacc[[2]]$Percent[3], 2), "%", ")")
    ),
    palette = c("#757575", "#FFD700", "#CD0000"),
    title = "Changes in the interval \n1992 - 2010"
  ) +
  tmap::tm_legend(
    position = c(0.01, 0.2),
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
  tmap::tm_credits(
    paste0(
      "Case of Study site",
      "\nAccumulate changes from 2002 to 2014",
      "\nData create with OpenLand package",
      "\nLULC derived from Embrapa Pantanal, Instituto SOS Pantanal, and WWF-Brasil 2015."
    ),
    size = 0.7,
    position = c(0.01, -0, 01)
  ) +
  tmap::tm_graticules(
    n.x = 6,
    n.y = 6,
    lines = FALSE,
    #alpha = 0.1
    labels.rot = c(0, 90)
  ) +
  tmap::tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.02))

#Land cover maps

LULC_map <- tmap::tm_shape(LULC2010_reclass) +
  tmap::tm_raster(
    style = "cat",
    labels = as.character(labels$V2),
    palette = c("#33a02c", "#b2df8a", "#CAFF70", "#ff7f00",
                "#fdbf6f", "#fb9a99", "#436EEE",
                "#6a3d9a", "#1f78b4", "#e31a1c"),
    title = "LULC Classes 2010"
  ) + 
  tmap::tm_legend(
    position = c(0.75, 0.33),
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


        