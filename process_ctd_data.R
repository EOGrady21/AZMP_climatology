# load in archive ctd data (from 01_readData.R)
library(librarian)
shelf(oce, tidyverse, sf)

load('ctd_1.rda')
ctd1 <- ctd
load('ctd_2.rda')
ctd2 <- ctd
load('ctd_3.rda')
ctd3 <- ctd
load('ctd_4.rda')
ctd4 <- ctd
remove(ctd)

allctd <- c(ctd1, ctd2, ctd3, ctd4)
remove(ctd1, ctd2, ctd3, ctd4)

# check which casts have oxygen


# load in climatology boxes
boxdat <- read.csv("Gordana/2014_boxes.csv")

# Convert your dataframe to a spatial object
df_sf <- st_as_sf(boxdat, coords = c("longitude", "latitude"), crs = 4326)

# Create polygons
# Assuming each box consists of 4 points in order
df_poly <- df_sf %>%
  group_by(box) %>%
  summarise(geometry = st_combine(geometry), do_union = FALSE) %>%
  st_cast("POLYGON")

# loop through ctd files
sink('ctdprocessing.log')
for (i in 1:length(allctd)) {
  ctd <- allctd[[i]]
  cat('Reading', ctd@metadata$filename, '...\n')
  
  ctd_pts <- data.frame( lat = ctd@metadata$latitude,
                         lon = ctd@metadata$longitude)
  if(is.na(ctd_pts$lat)| is.na(ctd_pts$lon)) {
    cat('Position data missing from CTD file! Attempting to find it... \n')
    latname <- grep(names(ctd@metadata$header$EVENT_HEADER), pattern = 'lat', ignore.case = TRUE, value = TRUE)
    lonname <- grep(names(ctd@metadata$header$EVENT_HEADER), pattern = 'lon', ignore.case = TRUE, value = TRUE)
    ctd_pts <- data.frame(lat = as.numeric(ctd@metadata$header$EVENT_HEADER[[latname[1]]]),
                          lon = as.numeric(ctd@metadata$header$EVENT_HEADER[[lonname[1]]]))
    if(is.na(ctd_pts$lat)| is.na(ctd_pts$lon)) {
      cat('Could not recover position data, skipping file!')
    }
  }
  
  df_l_sf <- st_as_sf(ctd_pts, coords = c("lon", "lat"), crs = 4326)
  
  # Check if points  are within the boxes
  within_boxes <- as.data.frame(st_within(df_l_sf, df_poly))
  boxnum <- within_boxes$col.id
  allctd[[i]] <- oceSetMetadata(ctd, name = 'climatologyBox', value = boxnum)
  cat('Profile within box', boxnum, '! \n \n')
}
sink()

# split casts by box

# split by month

# check number of points in each section

# calculate stats in depth bins


