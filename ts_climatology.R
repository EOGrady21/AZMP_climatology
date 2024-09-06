# Update Scotian Shelf TS climatology
library(librarian)
shelf(tidyverse, ncdf4, ggplot2, R.matlab, sf)

# read in Chantelle's nc file
cl_files <- list.files('data/netCDFclimatology/', pattern = '1991to2020', full.names = TRUE)

cldf <- list()
for (i in 1:length(cl_files)) {
  cldf[[i]] <- list()
  cldat <- nc_open(cl_files[i])
  names(cldf)[[i]] <- str_split_i(cldat$filename, '/',  -1)
  cldf[[i]][['sea_water_temperature']] <- ncvar_get(cldat, 'sea_water_temperature')
  cldf[[i]][['sea_water_temperature_standard_deviation']] <- ncvar_get(cldat, 'sea_water_temperature_standard_deviation')
  cldf[[i]][['sea_water_practical_salinity']] <- ncvar_get(cldat, 'sea_water_practical_salinity')
  cldf[[i]][['sea_water_practical_salinity_standard_deviation']] <- ncvar_get(cldat, 'sea_water_practical_salinity_standard_deviation')
  cldf[[i]][['sea_water_sigma_theta']] <- ncvar_get(cldat, 'sea_water_sigma_theta')
  cldf[[i]][['sea_water_sigma_theta_standard_deviation']] <- ncvar_get(cldat, 'sea_water_sigma_theta_standard_deviation')
  nc_close(cldat)
}

cldat <- nc_open(cl_files[1])
cl_depths <- ncvar_get(cldat, 'depth')
nc_close(cldat)

cl_boxes <- str_split_i(names(cldf), '_', 3)
names(cldf) <- cl_boxes
# reformat


# read in Petrie matlab files
pdat <- readMat('C:/Users/ogradye/Documents/local_QC/IML_QC_MATLAB/AZMP/Rosette/B_stage3_Q_BIO_chl2.mat')
test35 <- data.frame(pdat$test35[[3]])

# reformat


# compare on map


# read in boxes
boxdat <- read.csv('data/Climate_ScotiaFundy_Polygons.csv')
boxdat <- boxdat %>%
  group_by(Area)

box_poly <- st_as_sf(boxdat, coords = c("Longitude", "Latitude"), crs = 4326)
# Create polygon
df_poly <- box_poly %>%
  group_by(Area) %>%
  summarise(geometry = st_combine(geometry), do_union = FALSE) %>%
  st_cast("POLYGON")

# coastline object
data(coastlineWorldFine)
clwf <- as.data.frame(coastlineWorldFine@data)

boxdat14 <- read.csv("Gordana/2014_boxes.csv")
boxdat14 <- boxdat14 %>%
  group_by(box)

cl_boxdat <- boxdat %>%
  filter(Area %in% names(cldf))

ggplot() +
  geom_polygon(data = cl_boxdat,
               aes(x = Longitude, y = Latitude, fill = as.factor(Area)),
               colour = 'white')+
  geom_polygon(data = boxdat14,
               aes(x = longitude, y = latitude, group = box),
               colour = 'black', fill = NA, linewidth = 2)+
  geom_polygon(data = clwf,
               aes(x = longitude, y = latitude),
               colour = 'grey')+
  scale_x_continuous(limits = c(-71, -48),
                     expand = c(0,0))+
  scale_y_continuous(limits = c(37, 48),
                     expand = c(0,0)) +
  labs(fill = 'Box')+
  theme_classic()


# Does not have appropriate spatial coverage

# Try CASTS2023 ----


