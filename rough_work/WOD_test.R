# read in NOAA WOA tS climatology products
boxdat14 <- read.csv("Gordana/2014_boxes.csv")
boxdat14 <- boxdat14 %>%
  group_by(box)
data(coastlineWorldFine)
clwf <- as.data.frame(coastlineWorldFine@data)

t_files <- list.files('data/NOAA_temperature_climatology/', full.names = TRUE)
s_files <- list.files('data/NOAA_salinity_climatology/', full.names = TRUE)


tdat <- read.csv(t_files[1], skip = 1)
names(tdat) <- c('latitude', 'longitude', 'X0', names(tdat)[4:59])



ggplot() +
  geom_point(data = tdat, aes(x = longitude, y = latitude), colour = 'purple')+
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
