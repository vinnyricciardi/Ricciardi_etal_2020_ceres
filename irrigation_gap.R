library(dplyr)
library(ggplot2)
library(rworldmap)
library(ggalt)
library(rgdal)
library(raster)
library(ncdf4)
library(lattice)
library(boot)
library(reshape2)
library(rasterVis)
# library(ggthemes)
library(tidyr)
# library(ggthemr)  #devtools::install_github('cttobin/ggthemr')
library(egg)
library(reldist)



###############################
# Read in data
###############################

# Mekonnen Hoekstra 2016 (blue water scarcity):
# https://advances.sciencemag.org/content/2/2/e1500323.full

b <- raster('data/WS_blue_monthly_rasters/WSbl_monthly_30m/no_of_months/w001001.adf')

# GPCC (Global Precipitation Climate Center) Total Full V2018 (0.5x0.5):
# https://www.esrl.noaa.gov/psd/data/gridded/data.gpcc.html

# Set to 1996-2005 to match Mekonnen Hoekstra 2016.

g <- brick('data/precipitation/precip.mon.total.v2018.nc')
g <- g[[which(getZ(g) >= as.Date('1996-01-01') & getZ(g) <= as.Date('2005-12-31'))]]

# Take median rainfall values.

g <- calc(g, median)
g <- rotate(g)

# Get the HDI data for the most recent year (2017):
# http://hdr.undp.org/en/content/human-development-indices-indicators-2018-statistical-update
# https://datadryad.org/resource/doi:10.5061/dryad.dk1j0

# h <- raster('data/hdi/hdi_2017.tif')

# tmp code - test per capita gdp
tmp_pop <- data.table::fread('data/gdp_pc/pop_ssp1.csv')
tmp_gdp <- data.table::fread('data/gdp_pc/gdp_ssp1.csv')
tmp_pcg <- merge(tmp_pop, tmp_gdp, by = c('gID', 'px', 'py'))
tmp_pcg$pcgdp <- tmp_pcg$g1_2010 / tmp_pcg$p1_2010
tmp_pcg <- tmp_pcg %>% select('gID', 'pcgdp') %>%
  mutate(pcgdp = ifelse(is.na(pcgdp), 0, pcgdp))

shp <- readOGR('data/gdp_pc/grid/grid.shp')
shp <- merge(shp, tmp_pcg, by = 'gID')

## Set up a raster "template" to use in rasterize()
ext <-  extent(-180,180,-90,90)
xy <- abs(apply(as.matrix(bbox(ext)), 1, diff))
n <- 2  # 1/n = degree resolution
r <- raster(ext, ncol = xy[1]*n, nrow = xy[2]*n)

## Rasterize the shapefile
h <- rasterize(shp, r, 'pcgdp')
crs(h) <- crs(shp)
# plot(h)
#tmp code complete


# Get the World Bank 2019 LMIC classifications. Subset to the LMICs:
# https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups

lmic <- read.csv('data/lmic/country_regions_economies.csv')

  
# Farm size from Mehrabi et al. 2020 (Forthcoming, see our manuscript for up-to-date citation).

fs <- list.files(path = 'data/farm_size_active/', 
                 pattern = 'tif$', 
                 full.names = T)
fs <- stack(fs)
fs <- brick(fs)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fs <- calc(fs, getmode)
  
# Irrigation from Salmon et al. 2015. Where class 1 is rainfed, class 2 is irrigated, and class 3 is paddy.
# https://sgst.wr.usgs.gov/gfsad30/GRIPC/GRIPCmap/mosaic/
# https://www.sciencedirect.com/science/article/pii/S0303243415000240

irr <- raster('data/irrigation/GRIPC/GRIPC_irri_area.tif')
irr_paddy <- raster('data/irrigation/GRIPC/gripc_paddy_area.tif')
irr_rain <- raster('data/irrigation/GRIPC/gripc_rainfed_area.tif')
  
# Make a raster of the world country data from the rworldmap package.

# world  <- getMap(resolution = 'low')
# lookup <- data.frame(
#   ISO3 = as.character(world$ISO3),
#   ADMIN = world$ADMIN,
#   country = as.character(world$ADMIN))
#
# raster.world <- raster(res = c(0.0833282, 0.0833282))
# raster.world <- raster(res = c(0.5, 0.5))
# extent(raster.world) <- extent(world)
# world.raster <- rasterize(as(world, 'SpatialPolygons'), 
#                           raster.world,
#                           field = world[, 'ADMIN'],
#                           fun   = 'first')
# world.rast <- writeRaster(world.raster, 
#                           'data/precipitation/worldrast.tif', 
#                           format    = 'GTiff', 
#                           overwrite = TRUE)

world  <- raster('data/precipitation/worldrast.tif')

  
###############################
# Preprocess data
###############################

# Match CRS, spatial extents, and geographic center across datasets.
# Use the blue water scarcity raster as a base dataset and set all datatsets' projections and extents.

###########
# Convert to Equal Area
###########

# Here we set each dataset to have an equal area (Ekert IV) at a 8.4 km\textsuperscript{2} resolution.

rast.list.ngb <- list(fs, b, world)
rast.list.bil <- list(g, h, irr, irr_paddy, irr_rain)

resValue <- 8439
rast.list.ngb.eq <- lapply(rast.list.ngb, function(x)
projectRaster(x,
              res    = c(resValue, resValue),
              crs    = '+proj=eck4 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0',
              method = 'ngb',
              over   = T))
names(rast.list.ngb.eq[[1]]) <- 'fs'
names(rast.list.ngb.eq[[2]]) <- 'w001001'
names(rast.list.ngb.eq[[3]]) <- 'worldrast'

rast.list.bil.eq <- lapply(rast.list.bil, function(x)
projectRaster(x, 
              res    = c(resValue, resValue),
              crs    = '+proj=eck4 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0',
              method = 'bilinear',
              over   = T))
names(rast.list.bil.eq[[1]]) <- 'layer'
names(rast.list.bil.eq[[2]]) <- 'hdi_2017'
names(rast.list.bil.eq[[3]]) <- 'GRIPC_irri_area'
names(rast.list.bil.eq[[4]]) <- 'gripc_paddy_area '
names(rast.list.bil.eq[[5]]) <- 'gripc_rainfed_area'

rast.list <- c(rast.list.ngb.eq, rast.list.bil.eq)

###########
# Set Extents
###########

# Next, we need to make sure that all the rasters have the same spatial extents.

ex1 <- extent(rast.list[[1]])

# Extend
rast.list.ex <- lapply(rast.list,    
                     function(x) extend(x, ex1))
# Crop
rast.list.c  <- lapply(rast.list.ex, 
                     function(x) crop(x, ex1))
# Force equal extent
rast.list.et <- lapply(rast.list.c,  
                     function(x) setExtent(x, ex1,
                                           keepres = TRUE))

###########
# Create Stack and Dataframe
###########

# Now that all the layers' resolution, CRS, and spatial extent are equal, we can create a raster stack and proceed with the analysis.

rast.all <- stack(rast.list.et)
  
# Check all plots.

par(mfrow = c(3,4))
plot(rast.all)

# Convert stack into a dataframe.

df <- raster::extract(rast.all, 1:ncell(rast.all), df = T)


###############################
# Final dataframe
###############################

tmp <- df  # useful to have diff name for df here to debug below
  
# Convert country ID to country name
require(rworldmap)
world  <- getMap(resolution = 'low')
lookup <- as.data.frame(
  cbind(
    as.character(world@data[,'ISO3']),
    world@data[,'ADMIN'],
    as.character(world@data[,'ADMIN'])))

matched     <- lookup[match(tmp$world, lookup$V2), ]
tmp$ISO3    <- matched[[1]]
tmp$country <- matched[[3]]

# Add dummy variable for if country is a LMIC.
tmp <- merge(tmp, lmic, by.x = 'ISO3', by.y = 'Code', all.x = T)


# Define water scarcity as at least 1 month blue water scarcity
# or less than 250 mm rain per year (note, varibale in months, hence 20.83)
tmp <- tmp %>%
  mutate(waterScarce = ifelse((w001001 >= 1) | (layer <= 20.8333), 
                              'Water Scarce', 'Not Water Scarce'))

# Define irrigation as irrigation, non-irrigation as rainfed + paddy
# Convert m2 to ha

tmp <- tmp %>%
  mutate(#GRIPC_irri_area = GRIPC_irri_area,
         gripc_rainfed_area = gripc_rainfed_area,
         GRIPC_irri_area = rowSums(cbind(GRIPC_irri_area, gripc_paddy_area), na.rm = T),
         # gripc_rainfed_area = rowSums(cbind(gripc_rainfed_area, gripc_paddy_area), na.rm = T),
         tot_area = rowSums(cbind(GRIPC_irri_area, gripc_rainfed_area), na.rm = T),
         tot_area = ifelse(tot_area == 0 | is.na(tot_area), NA, tot_area))

# Calculate number of farms in cell are irrigated
tmp <- tmp %>%
  mutate(GRIPC_irri_farms = GRIPC_irri_area / fs,
         gripc_rainfed_farms = gripc_rainfed_area / fs,
         tot_farms = rowSums(cbind(GRIPC_irri_farms, gripc_rainfed_farms), na.rm = T),
         tot_farms = ifelse(tot_area == 0 | is.na(tot_area), NA, tot_farms))


# Define small-scale farms as the smallest 40\% of farms in each country,
# located in LMICs, and located in areas with lthe lowest 40% GDP per capita.
tmp <- tmp %>%
  group_by(ISO3) %>%
  mutate(fs_check = sum(!is.na(fs))) %>% 
  mutate(ISO3_usable = ifelse(fs_check > 2, as.character(ISO3), NA)) %>%
  group_by(ISO3_usable) %>%
  mutate(fs_agArea = ifelse(!is.na(tot_area) | tot_area == 0, fs, NA),  # assurance measure
         fs_threshold = wtd.quantile(fs, .4, na.rm = T, weight = tot_farms),
         fs_binary = ifelse(fs <= fs_threshold, 1,
                     ifelse(fs > fs_threshold, 0, NA)),
         hdi_agArea = ifelse(!is.na(fs_binary), hdi_2017, NA), # ensures only ag area
         hdi_threshold = wtd.quantile(hdi_agArea, .4, na.rm = T, weight = tot_farms),
         hdi_binary = ifelse(hdi_agArea <= hdi_threshold, 1,
                      ifelse(hdi_agArea > hdi_threshold, 0, NA)),
         criteria_sum = rowSums(cbind(fs_binary, hdi_binary), na.rm = T),
         criteria_sum = ifelse(min(fs, na.rm = T) == fs_threshold,  # special case
                               criteria_sum + 1, criteria_sum),
         criteria_sum = ifelse(min(hdi_agArea, na.rm = T) == hdi_threshold,  # special case
                               criteria_sum + 1, criteria_sum),
         size = ifelse(criteria_sum >= 2, 'Small-scale', 'Not small-scale'),
         size = ifelse(is.na(fs_agArea), NA, size),
         size = ifelse(Income.group %in% c('Low income',
                                           'Lower middle income',
                                           'Upper middle income'),
                       size, NA))


# percent of farms and ag area per water scarce condition per country
tmp1 <- tmp %>%
  group_by(waterScarce, ISO3_usable, country, Income.group, size) %>%
  summarise(t_irr_fs = sum(GRIPC_irri_farms, na.rm = T),
            t_irr_ag = sum(GRIPC_irri_area, na.rm = T),
            p_irr_fs = 100 * sum(GRIPC_irri_farms, na.rm = T) / sum(tot_farms, na.rm = T),
            p_irr_ag = 100 * sum(GRIPC_irri_area, na.rm = T) / sum(tot_area, na.rm = T),
            tot_fs = sum(tot_farms, na.rm = T),
            tot_ag = sum(tot_area, na.rm = T))

# global statistics
tmp2 <- tmp %>%
  group_by(waterScarce, size) %>%
  summarise(p_irr_fs = 100 * sum(GRIPC_irri_farms, na.rm = T) / sum(tot_farms, na.rm = T),
            p_irr_ag = 100 * sum(GRIPC_irri_area, na.rm = T) / sum(tot_area, na.rm = T),
            tot_fs = sum(tot_farms, na.rm = T),
            tot_ag = sum(tot_area, na.rm = T)) %>%
  na.omit()

# orig, rainpaddy, irripaddy
# sensitivity test between allocating paddy to rainfed or irrigated area
orig$allocation <- 'none'
rainpaddy$allocation <- 'rainfed'
irripaddy$allocation <- 'irrigation'
df_si <- rbind(as.data.frame(orig), 
               as.data.frame(rainpaddy), 
               as.data.frame(irripaddy))

ggplot(df_si, aes(waterScarce, p_irr_fs, fill = size)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~allocation)

# save for viz
write.csv(tmp1, '../fulltext_analysis/input/water_stressed_fs_country.csv')
write.csv(tmp2, '../fulltext_analysis/input/water_stressed_fs_all.csv')
















# Place global layer into a raster for plotting water scarce regions with the small-scale farm binary definition. Colors represent HDI level.
# 
# tmp_small_ws <- tmp %>% 
#   mutate(rasterValue = ifelse(size == 'Small-scale', 1, NA))
#          # rasterValue = ifelse((w001001 >= 1) | (layer < 20.8333),
#                               # hdi_2017, NA))
# 
# 
# final_raster <- function(dat) {
#   
#   rast_final <- rast.all@layers[[1]]
#   dat <- dat[order(dat$ID), ]
#   final_vector <- as.vector(dat$rasterValue)
#   rast_final <- setValues(rast_final, final_vector)
#   
#   return(rast_final)
# }
# 
# r_v <- as.data.frame(final_raster(
#   tmp_small_ws %>%
#     mutate(rasterValue = ifelse(Income.group %in% c('Low income',
#                                                      'Lower middle income'), 
#                                 rasterValue, NA))), xy = T) %>%
#   filter(!is.na(fs)) %>%
#   mutate(fs = ifelse(fs <= .549, 'Low HDI',
#                      ifelse(fs > .549 &
#                               fs <= .699, 'Medium HDI',
#                             ifelse(fs > .699 &
#                                      fs <= .799, 'High HDI',
#                                    ifelse(fs > .799, 'Very High HDI', NA)))),
#          fs = factor(fs, c('Low HDI', 'Medium HDI', 
#                            'High HDI', 'Very High HDI')))
#   
# 
# 
# r_w_hic <- as.data.frame(final_raster(
#   tmp_small_ws %>% 
#     mutate(rasterValue = ifelse(!is.na(ISO3), 1, NA),
#            rasterValue = ifelse(!Income.group %in% c('Low income',
#                                                      'Lower middle income'), 
#                                 rasterValue, NA),
#            rasterValue = ifelse(!is.na(fs_binary), rasterValue, NA))),
#   xy = T) %>%
#   filter(!is.na(fs))
# 
# r_w_countries <- as.data.frame(final_raster(
#   tmp_small_ws %>% 
#     mutate(rasterValue = ifelse(!is.na(ISO3), 1, NA))),
#   xy = T) %>%
#   filter(!is.na(fs))
# 
# 
# ggthemr('light')
# tg <- theme_get()
# 
# dd <- data.frame(
#   y = c(
#     'Very High HDI',
#     'High HDI',
#     'Medium HDI',
#     'Low HDI'
#   ),
#   LMIC = paste('lmic', 1:4),
#   HIC = paste('hic', 1:4)
# )
# dd <- melt(dd, id.vars = 'y')
# dd$y <- factor(dd$y, 
#                levels = 
#                  c('Very High HDI',
#                    'High HDI',
#                    'Medium HDI',
#                    'Low HDI'))
# 
# p_l <- ggplot() +
#   geom_tile(aes(as.factor(variable), 
#                 fill = value, y),
#             data = dd) +
#   theme_minimal() +
#   theme(panel.grid = element_blank(),
#         axis.text.y = element_text(hjust = 0)) +
#   scale_fill_manual(
#     values = c(
#       alpha(c(
#         swatch()[1],
#         swatch()[2],
#         swatch()[3],
#         swatch()[5]
#       ), .6),
#       swatch()[1],
#       swatch()[2],
#       swatch()[3],
#       swatch()[5]),
#     breaks = c(paste('lmic', 1:4),paste('hic', 1:4))) +
#   xlab('') +
#   ylab('') +
#   guides(fill = F) +
#   scale_x_discrete(position = 'top') 
#   
# 
# ggplot() +
#   geom_tile(aes(x, y),
#             fill = 'grey90',
#             show.legend = F,
#             data = r_v) +
#   geom_tile(aes(x, y,
#                 fill = fs),
#             show.legend = T,
#             data = r_v) +
#   labs(fill = element_blank()) +
#   coord_equal() +
#   # theme_map() +
#   theme(panel.background = element_rect(fill = 'transparent'), # bg of the panel
#         plot.background = element_rect(fill = 'transparent', color = NA), # bg of the plot
#         panel.grid.major = element_blank(), # get rid of major grid
#         panel.grid.minor = element_blank()) +  # get rid of minor grid 
#   # scale_fill_manual(
#   #   values = c('Very High HDI' = swatch()[1],
#   #              'High HDI' = swatch()[2],
#   #              'Medium HDI' = swatch()[3],
#   #              'Low HDI'  = swatch()[5]),
#   #   breaks = c('Very High HDI', 
#   #              'High HDI', 
#   #              'Medium HDI', 
#   #              'Low HDI'),
#   #   na.value = 'transparent') 
#   guides(fill = F)
# 
# 
# # p + 
# #   annotation_custom(
# #     ggplotGrob(p_l), 
# #     xmin = -1.9e+07, xmax = -1.e+07, ymin = -9e+06, ymax = -2e+06
# #   )
# 
# ggsave('../fulltext_analysis/output/fig_highres_map.png')
