



###############################
#   ROUGH WORKING SCRIPT ----
###############################



























# check for all holes in 2024 climatology
library(tidyverse)
library(ggplot2)
library(sf)

# read in Gordana's data
data14 <- read.csv('Gordana/AZMP_climatology_stats_2014.csv')

# read in stats from 2024
data24 <- read.csv('AZMP_climatology_2024-06-03.csv')

# read in boxes
boxdat <- read.csv("Gordana/2014_boxes.csv")
boxdat <- boxdat %>%
  group_by(box)

box_poly <- st_as_sf(boxdat, coords = c("longitude", "latitude"), crs = 4326)
# Create polygon
df_poly <- box_poly %>%
  group_by(box) %>%
  summarise(geometry = st_combine(geometry), do_union = FALSE) %>%
  st_cast("POLYGON")

missing_data <- data24 %>%
  dplyr::filter(n <= 1)

data_fns <- list.files(path = 'data/filtered_data', pattern = 'csv', full.names = TRUE)

data <- list()
for (i in 1:length(data_fns)) {
  data[[i]] <- read_csv(data_fns[i])
  param_name <- unique(data[[i]]$PARAMETER_NAME)
  names(data)[i] <- param_name
}

amon_data <- data$Ammonia

amon_data <- amon_data %>%
  dplyr::mutate(MONTH = format(as.Date(amon_data$EVENT_START, '%d-%b-%y'), '%m')) 

data_l <- amon_data %>%
  select(NAME, EVENT_MIN_LAT, EVENT_MIN_LON, COLLECTOR_EVENT_ID, COLLECTOR_SAMPLE_ID)

df_l_sf <- st_as_sf(data_l, coords = c("EVENT_MIN_LON", "EVENT_MIN_LAT"), crs = 4326)

# Check which box each point is inside
within_boxes <- as.data.frame(st_within(df_l_sf, df_poly))

amon_data <- amon_data %>%
  mutate(BOX = within_boxes$col.id )


# make sure that there is no bug removing the data

d1 <- amon_data %>%
  dplyr::filter(BOX == 4)%>%
  dplyr::filter(MONTH == '02') %>%
  dplyr::filter(HEADER_START_DEPTH < 10)
# RESULT: There is actually only one data point here, from P5. Could check that QC is not filtering out good data from this chunk but unlikely


# look at distribution of missing pieces
dict_var <- data.frame(variable = unique(data24$variable))
dict_var$code <- c('amon', 'cphl', 'ntrz', 'ntri', 'doxy', 'phae', 'phos', 'psal', 'slca')


data14 <- data14 %>%
  left_join(dict_var, by = c("variable" = "code")) %>%
  select(-variable) %>%
  rename('variable' = 'variable.y')


ggplot()+
  geom_point(data = data14, aes(x = month, y = box, shape = as.factor(depth_bin)), colour = 'blue', alpha = 0.1, size = 4)+
  geom_point(data = data24, aes(x = MONTH, y = BOX, shape = as.factor(DEPTH_BIN)), colour = 'red')+
  facet_wrap(facets = 'variable')

# plot by variable and facet by bin

for (i in 1:length(unique(data24$variable))) {
  var <- unique(data24$variable)[i]

  vardat <- data24[data24$variable == var,]
  
  p <- ggplot()+
    geom_tile(data = vardat, aes(x = MONTH, y = BOX, fill = n))+
    facet_wrap(facets = 'DEPTH_BIN')+
    ggtitle(var)
  
  print(p)
  
}

for (i in 1:length(unique(data14$variable))) {
  var <- unique(data14$variable)[i]
  
  vardat <- data14[data14$variable == var,]
  
  p <- ggplot()+
    geom_tile(data = vardat, aes(x = month, y = box, fill = no))+
    facet_wrap(facets = 'depth_bin')+
    ggtitle(var)
  
  print(p)
  
}

# merge data14 and data24 dataframes into new dataframe, with dat14 filling in missing data in data24
# warning: is this weak because it is fillinng holes in columns individually instead of identifying 
# missing row records and filling them all together?
data_m <- data24 %>%
  full_join(data14, by = c('variable',
                           'MONTH' = 'month',
                           'BOX' = 'box',
                           'DEPTH_BIN' = 'depth_bin')) %>%
  mutate(source = ifelse(is.na(n), 'data14', 'data24')) %>%
  mutate(mval_m = ifelse(is.na(mval.x), mval.y, mval.x)) %>%
  mutate(n_m = ifelse(is.na(n), no, n)) %>%
  mutate(min_val_m = ifelse(is.na(min_val), min, min_val)) %>%
  mutate(max_val_m = ifelse(is.na(max_val), max, max_val)) %>%
  mutate(sd_m = ifelse(is.na(sd), std, sd)) %>%
  mutate(min_depth_m = ifelse(is.na(min_depth.x), min_depth.y, min_depth.x)) %>%
  mutate(max_depth_m = ifelse(is.na(max_depth.x), max_depth.y, max_depth.x)) %>%
  select(c(variable, MONTH, BOX, DEPTH_BIN, mval_m, n_m, min_val_m, max_val_m, sd_m, min_depth_m, max_depth_m, source))
  

# geom_tile plot of data_m with month on x axis, box on y axis, facet by depth_bin, fill with n, and shade by source
# loop for each variable
for (i in 1:length(unique(data_m$variable))) {
  var <- unique(data_m$variable)[i]
  
  vardat <- data_m[data_m$variable == var,]
  vardat <- vardat %>%
    mutate(month_name = month.name[vardat$MONTH])
  
  vardat$month_name <- forcats::fct_relevel(vardat$month_name, month.name)
  
p <- ggplot()+
  geom_tile(data = vardat, 
            aes(x = month_name, y = BOX, fill = n_m, colour = source),
            linewidth = 2)+
  # angle x axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  # force integer values on y axis
  scale_y_continuous(breaks = seq(1, 10, 1))+
  ggtitle(paste( var))+
  scale_fill_continuous('Number \n of points', low = 'grey', high = 'black')+
  scale_color_manual('Source',
                     labels = c('data14' = '2014', 'data24' = '2024'),
                     values = c('data14' = 'blue', 'data24' = 'red'))+
  labs(x = 'Month', y = 'Box')+
  theme(panel.background = element_rect(fill = 'white'),
        axis.text = element_text(size = 15),
        title = element_text(size = 25))

ggsave(paste0('plots/merged_data_coverage_', var, '.png'), p, height = 12, width = 12)


}

# export data_m to csv
write.csv(data_m, 'data/merged_climatology.csv', row.names = FALSE)


# loop to plot each variable
# ggplot2 profile plot of data_m with depth on y axis and data value on x axis, display as boxes coloured by source
# use geom_rect to display boxes from min to max depth
xlimmin <- 0

# add season to data_m

# group seasons
seasons <- data.frame('season' = c(rep('spring', 3),
                                   rep('summer', 3),
                                   rep('fall', 3),
                                   rep('winter', 3)),
                      'month' = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2))


data_m <- data_m %>%
  right_join(seasons, c('MONTH' = 'month'))
data_m$season <- forcats::fct_relevel(data_m$season,  c("spring", "summer", "fall", "winter"))

for (i in 1:length(unique(data_m$variable))) {
  var <- unique(data_m$variable)[i]
  
  vardat <- data_m[data_m$variable == var,]
  vardat <- vardat %>%
    mutate(month_name = month.name[vardat$MONTH])
  
  vardat$month_name <- forcats::fct_relevel(vardat$month_name, month.name)
  
  
  p <- ggplot()+
    geom_rect(data = vardat,
              aes(xmin = min_val_m,
                  xmax = max_val_m,
                  ymin = min_depth_m,
                  ymax = max_depth_m))+
    scale_y_reverse()+
    facet_wrap(~season)+
    ggtitle(paste( var))+
    coord_cartesian(ylim = c(200, 0), xlim = c(xlimmin, NA))+
    theme_classic()+
    labs(x = 'Value', y = 'Depth [m]')+
    #scale_fill_manual(values = c('data14' = 'blue', 'data24' = 'red'))+
    theme(strip.text = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size = 25))

    
  
  ggsave(paste0('plots/merged_data_profile_', var, '.png'), p, height = 12, width = 12)
  
}

# plot data_m boxplot of data values for each variable
# includes percentage of each month filled with Gordana's data

for (i in 1:length(unique(data_m$variable))) {
  var <- unique(data_m$variable)[i]
  
  vardat <- data_m[data_m$variable == var,]
  
  vardat <- vardat %>%
    mutate(month_name = month.name[vardat$MONTH]) %>%
    group_by(month_name) %>%
    mutate(percentage_data14 = round(sum(source == 'data14')/nrow(vardat)*100, digits = 2))
  
  vardat$month_name <- forcats::fct_relevel(vardat$month_name, month.name)
  
  p <- ggplot()+
    geom_boxplot(data = vardat, aes(x = month_name, y = mval_m))+
    ggtitle(paste('Merged data:', var), subtitle = 'Blue text shows percentage of 2014 data included in each month')+
    theme_classic()+
    labs(x = 'Month', y = 'Value')+
    #scale_fill_manual(values = c('data14' = 'blue', 'data24' = 'red'))+
  # add percentage of data14 in each month
  geom_text(data = vardat,
            aes(x = month_name,
                y = max(vardat$mval_m, na.rm = TRUE), 
                label = vardat$percentage_data14,
               ),
            vjust = -0.5, 
            colour = 'blue')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 15),
          axis.text = element_text(size = 15),
          title = element_text(size = 25))
  
  
  ggsave(paste0('plots/merged_data_boxplot_', var, '.png'), p, height = 12, width = 12)
  
}

# for each variable in data24 and data14 plot a time series of mean values (y axis) by month on the x axis
# colour lines by source

for (i in 1:length(unique(data14$variable))) {
  var <- unique(data14$variable)[i]
  
  vardat14 <- data14[data14$variable == var,]
  
  vardat14 <- vardat14 %>%
    mutate(month_name = month.name[vardat14$month]) %>%
    group_by(month_name) %>%
    summarise(mean_val = mean(mval, na.rm = TRUE))
  
  vardat14$month_name <- forcats::fct_relevel(vardat14$month_name, month.name)
  
  vardat24 <- data24[data24$variable == var,]
  
  vardat24 <- vardat24 %>%
    mutate(month_name = month.name[vardat24$MONTH]) %>%
    group_by(month_name) %>%
    summarise(mean_val = mean(mval, na.rm = TRUE))
  
  vardat24$month_name <- forcats::fct_relevel(vardat24$month_name, month.name)
  
  
  p <- ggplot()+
    geom_point(data = vardat24, aes(x = month_name, y = mean_val), colour = 'red')+
    geom_line(data = vardat24, aes(x = month_name, y = mean_val, group = 1), colour = 'red')+
    geom_point(data = vardat14, aes(x = month_name, y = mean_val), colour = 'blue')+
    geom_line(data = vardat14, aes(x = month_name, y = mean_val, group = 1), colour = 'blue')+
    ggtitle(paste('2014 vs 2024:', var), subtitle = 'Red: 2024, Blue: 2014')+
    theme_classic()+
    labs(x = 'Month', y = 'Mean Value')+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 15),
          title = element_text(size = 25))
    
  
  ggsave(paste0('plots/2014_2024_timeseries_', var, '.png'), p, height = 12, width = 12)
  
}



