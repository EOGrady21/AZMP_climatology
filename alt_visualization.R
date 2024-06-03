# Visualize issues with oxygen data
library(ggplot2)
library(tidyverse)
library(oce)
library(ocedata)

data("coastlineWorldFine")
clwf <- as.data.frame(coastlineWorldFine@data)
col_vec = c('grey', 'black', 'yellow', 'orange', 'red', 'purple', 'blue', 'springgreen', 'cyan', 'magenta')
names(col_vec) <- c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')


data <- read.csv('data/azmp_99_24_O2.csv')

ggplot(data)+
  geom_point(aes(x = EVENT_START, y = DATA_VALUE, colour = METHOD))

ggplot(data) +
  geom_point(aes(x = EVENT_START,
                 y = DATA_VALUE,
                 colour = DATA_CENTER,
                 shape = METHOD))

winkler_data <- data %>%
  filter(METHOD %in% c('O2_Winkler', 'O2_Winkler_Auto')) %>%
  filter(DATA_CENTER %in% c('BIO', 'BIO_INREVIEW')) %>%
  filter(DATA_VALUE < 60)

ggplot(winkler_data) +
  geom_point(aes(x = EVENT_START,
                 y = DATA_VALUE,
                 colour = DATA_CENTER))

# save filter winkler data
write.csv(winkler_data, file = 'azmp_99_24_O2_filtered.csv', row.names = FALSE)
# Check on nitrite data

data <- read.csv('data/extractions/azmp_99_24_nitrite.csv')

ggplot(data)+
  geom_point(aes(x = as.Date(EVENT_START, '%d-%b-%y'),
                 y = DATA_VALUE,
                 colour = METHOD))+
  scale_x_date(date_breaks = "1 year", date_labels = '%Y')+
  labs(x = 'Date', y = 'Data Value')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data)+
  geom_point(aes(x = as.Date(EVENT_START, '%d-%b-%y'),
                 y = DATA_VALUE,
                 colour = DATA_CENTER))+
  scale_x_date(date_breaks = "1 year", date_labels = '%Y')+
  labs(x = 'Date', y = 'Data Value')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

 ggplot(data) +
  geom_polygon(data = clwf, aes(x = longitude, y = latitude)) +
  #geom_polygon(data = polygons, aes(x = Longitude, y = Latitude, group = Area), color = "red", fill = NA)+
  geom_point(aes(x = HEADER_START_LON, y = HEADER_START_LAT, colour = DATA_VALUE)) +
  scale_x_continuous(limits = c(-71, -48))+
  scale_y_continuous(limits = c(37, 48)) +
  theme_classic()+
  ggtitle("Nitrite spatial coverage")
 
 ggplot(data) +
   geom_polygon(data = clwf, aes(x = longitude, y = latitude)) +
   geom_polygon(data = polygons, aes(x = Longitude, y = Latitude, group = Area), color = "red", fill = NA)+
   geom_point(aes(x = HEADER_START_LON, y = HEADER_START_LAT, alpha = DATA_VALUE)) +
   scale_x_continuous(limits = c(-71, -48))+
   scale_y_continuous(limits = c(37, 48)) +
   theme_classic()+
   ggtitle("Nitrite spatial coverage")
 
unique(data$NAME[data$DATA_VALUE > 5])

# bbmp21 <- data[data$NAME == 'BCD2021667',]
# 
# ggplot(bbmp21) +
#   geom_point(aes(x = DATA_VALUE, y = HEADER_START_DEPTH, colour = as.factor(DATA_QC_CODE)))+
#   scale_y_reverse()+
#   scale_color_manual(values = col_vec)+
#   labs(x = 'Data Value', y = 'Depth', colour = 'QC Flag') +
#   scale_x_continuous(position = 'top')+
#   ggtitle(paste('Nitrite profile summary with quality control'))
# 
bbmp19 <- data[data$NAME == 'BCD2019667',]

ggplot(bbmp19) +
  geom_point(aes(x = DATA_VALUE, y = HEADER_START_DEPTH, colour = as.factor(DATA_QC_CODE)))+
  scale_y_reverse()+
  scale_color_manual(values = col_vec)+
  labs(x = 'Data Value', y = 'Depth', colour = 'QC Flag') +
  scale_x_continuous(position = 'top')+
  ggtitle(paste('Nitrite profile summary with quality control'))

bbmp17 <- data[data$NAME == 'BCD2017667',]

ggplot(bbmp17) +
  geom_point(aes(x = DATA_VALUE, y = HEADER_START_DEPTH, colour = as.factor(DATA_QC_CODE)))+
  scale_y_reverse()+
  scale_color_manual(values = col_vec)+
  labs(x = 'Data Value', y = 'Depth', colour = 'QC Flag') +
  scale_x_continuous(position = 'top')+
  ggtitle(paste('Nitrite profile summary with quality control'))

ggplot(bbmp17) +
  geom_point(aes(x = as.Date(EVENT_START, '%d-%b-%y'),
                 y = DATA_VALUE,
                 colour = as.factor(ceiling(HEADER_START_DEPTH/5) *5)))+
  scale_x_date(date_breaks = "1 month", date_labels = '%b')+
  scale_colour_discrete(name = "depth bin")+
  labs(x = 'Date', y = 'Data Value')


bbmp2004 <- data[data$NAME == 'BCD2004667',]

ggplot(bbmp2004) +
  geom_point(aes(x = DATA_VALUE, y = HEADER_START_DEPTH, colour = as.factor(DATA_QC_CODE)))+
  scale_y_reverse()+
  scale_color_manual(values = col_vec)+
  labs(x = 'Data Value', y = 'Depth', colour = 'QC Flag') +
  scale_x_continuous(position = 'top')+
  ggtitle(paste('Nitrite profile summary with quality control'))

ggplot(bbmp2004) +
  geom_point(aes(x = as.Date(EVENT_START, '%d-%b-%y'),
                 y = DATA_VALUE,
                 colour = as.factor(ceiling(HEADER_START_DEPTH/5) *5)))+
  scale_x_date(date_breaks = "1 month", date_labels = '%b')+
  scale_colour_discrete(name = "depth bin")+
  labs(x = 'Date', y = 'Data Value')+
  ggtitle('BBMP 2004 - Nitrite')


# remove weirdo salinity at 2000 ----

data <- read.csv('data/extractions/azmp_99_24_salinity.csv')

range(data$DATA_VALUE)

data[data$DATA_VALUE == 1999, ]


# check on CAR2021221

car21 <- read.table('c:/Users/ogradye/Documents/local_QC/data/2021/CAR2021221/18CR21221.tsv', header = TRUE, sep = '\t')


# check on salinity ranges

finalsalinity <- read.csv('data/filtered_data/SS_Salinity_final_2024-05-30.csv')
unique(finalsalinity$NAME[finalsalinity$DATA_VALUE < 25])
lowsal <- finalsalinity %>%
  filter(DATA_VALUE < 25)

ggplot(lowsal)+
  geom_point(aes(x = DATA_VALUE, y = HEADER_START_DEPTH, colour = NAME))+
  scale_y_reverse()

ggplot(lowsal) +
  geom_polygon(data = clwf, aes(x = longitude, y = latitude), colour = 'grey', fill = NA) +
  #geom_polygon(data = polygons, aes(x = Longitude, y = Latitude, group = Area), color = "red", fill = NA)+
  geom_point(aes(x = HEADER_START_LON, y = HEADER_START_LAT, shape = as.factor(DATA_QC_CODE), colour = DATA_VALUE)) +
  #scale_color_manual(values = col_vec)+
  scale_x_continuous(limits = c(-71, -48))+
  scale_y_continuous(limits = c(37, 48)) +
  theme_classic()+
  ggtitle("Low salinity values")

# gather all profile data from events with salinity <25

finalsalinity <- finalsalinity %>%
  mutate(DIS_SAMPLE_KEY_VALUE = paste0(DESCRIPTOR, '_', COLLECTOR_EVENT_ID, '_', COLLECTOR_SAMPLE_ID))

lowsal_keys <- str_split(unique(finalsalinity$DIS_SAMPLE_KEY_VALUE[finalsalinity$DATA_VALUE < 25]), '_')

lowsal_p <- list()
for (i in 1:length(lowsal_keys)) {
  missionname <- lowsal_keys[[i]][[1]]
  eventnum <- lowsal_keys[[i]][[2]]
  
  lowsal_p[[i]] <- finalsalinity[which(finalsalinity$DESCRIPTOR == missionname & finalsalinity$COLLECTOR_EVENT_ID == eventnum),]
  
  pp <- ggplot(lowsal_p[[i]])+
    geom_point(aes(x = DATA_VALUE, y = HEADER_START_DEPTH))+
    scale_y_reverse() +
    ggtitle(paste0('Low salinity detected in profile: ', missionname, ' event ', eventnum))
  
  print(pp)
}

lowsalpdf <- do.call(rbind, lowsal_p)


ggplot()