# convert CASTS_2023 nc files to TS climatology for IML QC
# Emily O'Grady 
# July 2024
library(librarian)
shelf(ncdf4, tidyverse, tidync, sf, ocedata, ggplot2)

# test visualization
nc <- nc_open('data/CASTS_2023/1950.nc')

position <- data.frame(Latitude = ncvar_get(nc, 'latitude'),
                       Longitude = ncvar_get(nc, 'longitude'))

ggplot() +
  geom_point(data = position,
             aes(x = Longitude, y = Latitude))+
  geom_polygon(data = boxdat14,
               aes(x = longitude, y = latitude, group = box),
               colour = 'darkblue', fill = NA, linewidth = 2)+
  geom_polygon(data = clwf,
               aes(x = longitude, y = latitude),
               colour = 'grey')+
  scale_x_continuous(limits = c(-71, -48),
                     expand = c(0,0))+
  scale_y_continuous(limits = c(37, 48),
                     expand = c(0,0)) +
  labs(fill = 'Box')+
  theme_classic()

data("coastlineWorldFine")
clwf <- as.data.frame(coastlineWorldFine@data)

# Step 1 - Read in all nc files ----
# 1991 - 2020
ncfiles <- list.files('data/CASTS_2023/', full.names = TRUE)
ncfiles <- ncfiles[80:109]

# sink('data/CASTS_data/TS_climatology_filtering.log')
for (i in 1:length(ncfiles)){
  
nc <- nc_open(ncfiles[i])
cat('Opened ', ncfiles[i], '...\n')
# Fix NaNs

# Step 2 - Filter by location ----
# Gordana's boxes
boxdat14 <- read.csv("Gordana/2014_boxes.csv")
boxdat14 <- boxdat14 %>%
  group_by(box)
df_sf <- st_as_sf(boxdat14, coords = c("longitude", "latitude"), crs = 4326)

# Create polygons
df_poly <- df_sf %>%
  group_by(box) %>%
  summarise(geometry = st_combine(geometry), do_union = FALSE) %>%
  st_cast("POLYGON")

latdat <- ncvar_get(nc, 'latitude')
londat <- ncvar_get(nc, 'longitude')

data_l <- data.frame(latdat, londat)

df_l_sf <- st_as_sf(data_l, coords = c("londat", "latdat"), crs = 4326)

# Check if points  are within the boxes
within_boxes <- st_within(df_l_sf, df_poly)
# 
# nc_f <- tidync(ncfiles[1]) %>%
#   hyper_array() 

boxindex <- unlist(lapply(within_boxes, length))
saldat <- ncvar_get(nc, varid = 'salinity')
tempdat <- ncvar_get(nc, varid = 'temperature')
timedat <- as.POSIXct(ncvar_get(nc, varid = 'time'), origin = '1900-01-01')
depthdat <- ncvar_get(nc, varid = 'level')
# TODO create ID column to help with filtering?

cat('     Data outside boxes removed! [', length(boxindex[boxindex == 0]), '] \n')

# filter into boxes
saldat <- saldat[, which(boxindex != 0)]
tempdat <- tempdat[, which(boxindex != 0)]
timedat <- timedat[which(boxindex != 0)]
boxindexf <- boxindex[which(boxindex != 0)]
latdat <- latdat[which(boxindex != 0)]
londat <- londat[which(boxindex != 0)]


# coastal point removal

coastfilt <- clwf %>%
  filter(longitude > -71 & longitude < -48) %>%
  filter(latitude > 37 & latitude < 48)

# Convert your dataframe to a spatial object
df_sf <- st_as_sf(coastfilt, coords = c("longitude", "latitude"), crs = 4326)

coastline <- st_transform(df_sf, crs = 32620) 

# Create buffer around coastline
buffer <- st_buffer(coastline, dist = 5000) # 5km buffer

onebuff <- st_union(buffer)

data_l <- data.frame(latitude = latdat, longitude = londat)
points <- st_as_sf(data_l, coords = c("longitude", "latitude"), crs = 4326)
points <- st_transform(points, 32620)
intersects <- st_intersects(points, onebuff, sparse = FALSE)

cat('    Coastal points removed! [', length(intersects[intersects == TRUE]), '] \n')

# filter coastal points
saldat <- saldat[, which(intersects == FALSE)]
tempdat <- tempdat[, which(intersects == FALSE)]
timedat <- timedat[which(intersects == FALSE)]
boxindexf <- boxindexf[which(intersects == FALSE)]
latdat <- latdat[which(intersects == FALSE)]
londat <- londat[which(intersects == FALSE)]

position <- data.frame(Longitude = londat, Latitude = latdat)

allpos <- data.frame(Latitude = ncvar_get(nc, 'latitude'),
                     Longitude = ncvar_get(nc, 'longitude'))


pp <- ggplot() +
  geom_point(data = allpos,
             aes(x = Longitude, y = Latitude), colour = 'red')+
  geom_point(data = position,
             aes(x = Longitude, y = Latitude))+
  geom_polygon(data = boxdat14,
               aes(x = longitude, y = latitude, group = box),
               colour = 'darkblue', fill = NA, linewidth = 2)+
  geom_polygon(data = clwf,
               aes(x = longitude, y = latitude),
               colour = 'grey')+
  scale_x_continuous(limits = c(-71, -48),
                     expand = c(0,0))+
  scale_y_continuous(limits = c(37, 48),
                     expand = c(0,0)) +
  labs(fill = 'Box')+
  theme_classic()+
  ggtitle(ncfiles[i])


data <- list('salinity' = saldat,
             'temperature' = tempdat,
             'time' = timedat,
             'box' = boxindexf,
             'latitude' = latdat,
             'longitude' = londat,
             'level' = depthdat
             )

year <- format(data$time[1], format = '%Y')
save(data, file = file.path('data/CASTS_data', paste0(year, '.RData')))
ggsave(pp, filename = paste0(year, '_mapPlot.png'), path = 'data/CASTS_data/plots')

cat('    Data saved! [', paste0(year, '.RData'), '] \n')

nc_close(nc)
remove(nc)
cat('\n\n\n')
}
# sink()

# Step 3 - Combine into IML QC format ----

data_files <- list.files('data/CASTS_data', pattern = 'RData', full.names = TRUE)
alldata <- list()

for (i in 1:length(data_files)){
  load(data_files[i])
  year <- format(data$time[1], '%Y')
  alldata[[i]] <- data
  names(alldata)[[i]] <- year
  remove(data)
}

# TODO
# combine all years
# Group by Gordana boxes
# group by month
# group by depth bin

# Step 4 - Export to MATLAB -----


