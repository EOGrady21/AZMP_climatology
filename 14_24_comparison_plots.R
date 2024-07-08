# presentation style plots

library(ocedata)
library(ggplot2)
library(tidyverse)
library(cmocean)
library(sf)
library(scales)


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

# coastline object
data(coastlineWorldFine)
clwf <- as.data.frame(coastlineWorldFine@data)

depth_bins <- data.frame(bin = c(1, 2, 3, 4), min_depth = c(0, 10, 30, 100), max_depth = c(10, 30, 100, 5000))

yaxis_order <- rev(c('1-1', '1-2', '1-3', '1-4',
                     '2-1', '2-2', '2-3', '2-4',
                     '3-1', '3-2', '3-3', '3-4',
                     '4-1', '4-2', '4-3', '4-4',
                     '5-1', '5-2', '5-3', '5-4',
                     '6-1', '6-2', '6-3', '6-4',
                     '7-1', '7-2', '7-3', '7-4',
                     '8-1', '8-2', '8-3', '8-4',
                     '9-1', '9-2', '9-3', '9-4'))

# read in Gordana's data
data14 <- read.csv('Gordana/AZMP_climatology_stats_2014.csv')

# read in stats from 2024
data24 <- read.csv('AZMP_climatology_2024-06-03.csv')

dict_var <- data.frame(variable = unique(data24$variable))
dict_var$code <- c('amon', 'cphl', 'ntrz', 'ntri', 'doxy', 'phae', 'phos', 'psal', 'slca')

# group seasons
seasons <- data.frame('season' = c(rep('spring', 3),
                                   rep('summer', 3),
                                   rep('fall', 3),
                                   rep('winter', 3)),
                      'month' = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2))

data14 <- data14 %>%
  right_join(seasons)
data24 <- data24 %>%
  right_join(seasons, c('MONTH' = 'month'))

# find actual max compared value min/max or 3std whichever is greater

data14 <- data14 %>%
  mutate(min_sd = (data14$mval - 3*data14$std)) %>%
  mutate(max_sd = (data14$mval + 3*data14$std)) %>%
  mutate(min_c = case_when(
    min_sd < min ~ min_sd,
    min < min_sd ~ min
  )) %>%
  mutate(max_c = case_when(
    max_sd > max ~ max_sd,
    max > max_sd ~ max
  ))

data24 <- data24 %>%
  mutate(min_sd = (data24$mval - 3*data24$sd)) %>%
  mutate(max_sd = (data24$mval + 3*data24$sd)) %>%
  mutate(min_c = case_when(
    min_sd < min_val ~ min_sd,
    min_val < min_sd ~ min_val
  )) %>%
  mutate(max_c = case_when(
    max_sd > max_val ~ max_sd,
    max_val > max_sd ~ max_val
  ))


# TODO check ammonia, nitrite and o2 missing depth bins in box 1, offsetting plot?

### heat map of number of points in 2024 ----

for(var in unique(data24$variable)) {
  plot_dat <- data24 %>%
    filter(variable == var) %>%
    mutate(box_bin = paste0(BOX, '-', DEPTH_BIN)) 
  
  
  pp <- ggplot(plot_dat) +
    geom_tile(aes(x = MONTH,
                  y = factor(box_bin, levels = yaxis_order),
                  fill = n))+
    geom_hline(yintercept = seq(0.5, 36.5, by = 4), size = 2, color = "black") +
    labs(x = 'Month', y = 'Box-Bin', fill = 'N')+
    scale_fill_gradient2(mid = 'pink', high = 'darkred', na.value = 'grey')+
    coord_cartesian(expand = FALSE)+
    scale_x_continuous(breaks = c(1:12))+
    scale_y_discrete(labels = yaxis_order)+
    #theme(axis.text = element_text(size = 15),
    #    title = element_text(size = 25))+
    ggtitle('Number of points in climatology', subtitle = var)+
    theme_classic()+
    theme(panel.background = element_rect(fill = 'grey'))
  
ggsave(paste0('plots/2024_numberofpoints_', var, '.png'), pp, height = 12, width = 12)

}

##heat map of number of points in 2014 ----


# 2014 number of data points on a heat map

for(var in unique(data14$variable)) {
  plot_dat <- data14 %>%
    filter(variable == var) %>%
    mutate(box_bin = paste0(box, '-', depth_bin)) %>%
    mutate(no = case_when(
      no == 0 ~ NA,
      .default = no
    ))
  
  
  pp <- ggplot(plot_dat) +
    geom_tile(aes(x = month,
                  y = factor(box_bin, levels = yaxis_order),
                  fill = no))+
    geom_hline(yintercept = seq(0.5, 36.5, by = 4), size = 2, color = "black") +
    labs(x = 'Month', y = 'Box-Bin', fill = 'N')+
    theme_classic()+
    scale_fill_gradient2(low = 'white', mid = 'pink', high = 'darkred', na.value = 'grey')+
    coord_cartesian(expand = FALSE)+
    scale_x_continuous(breaks = c(1:12))+
    scale_y_discrete(labels = yaxis_order)+
    #theme(axis.text = element_text(size = 15),
    #    title = element_text(size = 25))+
    ggtitle('Number of points in climatology', subtitle = var)
  
  ggsave(paste0('plots/2014_numberofpoints_', var, '.png'), pp, width = 12, height = 12)
}


### Plot 1 (timeseries) ----
for(var in 1:9){
var14 <- dict_var$code[var]
var24 <- dict_var$variable[var]

for (boxnum in 1:9) {
#boxnum <- 1
  #for (binnum in 1:4){
#binnum <- 1
sdat24 <- data24 %>%
  filter(variable == var24) %>%
  filter(BOX == boxnum)# %>%
  #filter(DEPTH_BIN == binnum)

sdat14 <- data14 %>%
  filter(variable == var14) %>%
  filter(box == boxnum)#%>%
  #filter(depth_bin == binnum)

# join data
alldat <- full_join(sdat24, sdat14,
          suffix = c('24', '14'),
          by = c('DEPTH_BIN' = 'depth_bin',
                 'MONTH' = 'month',
                 'BOX' = 'box')) %>%
  arrange(MONTH)

# TODO fix ordering of month data

pp <- ggplot(alldat)+
  geom_path(
             aes(x = MONTH, y = mval24, group = DEPTH_BIN),
            colour = 'pink', linewidth = 3)+
  geom_errorbar( aes(x = MONTH, ymin = min_c24, ymax = max_c24),
                colour = 'pink',linewidth = 3, alpha = 0.5)+
  geom_path(
             aes(x = MONTH, y = mval14, group = DEPTH_BIN),
             colour = 'lightblue', linewidth = 1.5)+
  geom_errorbar(aes(x = MONTH, ymin = min_c14, ymax = max_c14),
                colour = 'lightblue', linewidth = 1.25, alpha = 0.5)+
  facet_grid(rows = vars(DEPTH_BIN))+
  labs(x = 'Month', y = 'Value', colour = 'Depth Bin')+
  scale_x_continuous(breaks = seq(1:12)) +
  coord_cartesian(ylim = c(0, NA))+
  theme_classic()+
  ggtitle(paste(var24, "climatology values: Box", boxnum),
          subtitle = 'Blue: 2014, Pink: 2024')+
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 25))

ggsave(paste0('plots/timeseries_box', boxnum, '_', var24, '.png'), pp,
       width = 12, height = 10)

#}
}

}

# plot 2 (profile) ---- 

for (var in 1:9){
#var <- 9
var14 <- dict_var$code[var]
var24 <- dict_var$variable[var]

for (boxnum in 1:9){
#boxnum <- 3
  for (sind in 1:4){
seasonval <- unique(seasons$season)[sind]
sdat24 <- data24 %>%
  filter(variable == var24) %>%
  filter(BOX == boxnum) %>%
  filter(season == seasonval) %>%
  group_by(DEPTH_BIN) %>%
  summarize(min_c = min(min_c, na.rm = TRUE),
            max_c = max(max_c, na.rm = TRUE),
            ) %>%
  right_join(., depth_bins, c('DEPTH_BIN' = 'bin'))

sdat14 <- data14 %>%
  filter(variable == var14) %>%
  filter(box == boxnum)%>%
  filter(season == seasonval)%>%
  group_by(depth_bin) %>%
  summarize(min_c = min(min_c, na.rm = TRUE),
            max_c = max(max_c, na.rm = TRUE),
  ) %>%
  right_join(., depth_bins, c('depth_bin' = 'bin'))

xlimmin <- 0
if (var24 == 'Salinity') {
  xlimmin <- floor(min(sdat24$min_c, na.rm = TRUE))
}
pp <- ggplot()+
  geom_rect(data = sdat14,
            aes(xmin = min_c, xmax = max_c,
                ymin = min_depth, ymax = max_depth),
            fill = NA,
            alpha = 0.5, 
            colour = 'lightblue',
            linetype = 'dashed',
            linewidth = 2)+
  geom_rect(data = sdat24,
            aes(xmin = min_c, xmax = max_c,
                ymin = min_depth, ymax = max_depth),
            fill = 'pink',
            alpha = 0.5,
            colour = 'black',
            linetype = 'solid',
            linewidth = 2)+
  scale_y_reverse()+
  coord_cartesian(ylim = c(200, 0), xlim = c(xlimmin, NA))+
  theme_classic()+
  labs(x = 'Value', y = 'Depth [m]')+
  ggtitle(paste(var24, "climatology profile: Box", boxnum,',', seasonval),
          subtitle = 'Pink: 2024, Blue-Dash: 2014')+
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 25))

ggsave(paste0('plots/profile_box', boxnum, '_', seasonval, '_', var24, '.png'), pp,
       width = 10, height = 12)
}
}
}

# plot 3 (anomaly) ----
for (var in 1:9) {
#var <- 2
var14 <- dict_var$code[var]
var24 <- dict_var$variable[var]


sdat24 <- data24 %>%
  filter(variable == var24)

sdat14 <- data14 %>%
  filter(variable == var14)

anomdat <- full_join(sdat24, sdat14,
                     suffix = c('24', '14'),
                     by = c('DEPTH_BIN' = 'depth_bin',
                            'MONTH' = 'month',
                            'BOX' = 'box')) %>%
  select('DEPTH_BIN', 'MONTH', 'BOX', 'variable24', 'variable14', 'mval24', 'mval14', 'std', 'sd') %>%
  mutate(anomaly = mval24 - mval14) %>%
  #mutate(st_anomaly = anomaly/sd) %>%
  mutate(n_anomaly = rescale(anomaly, c(0,1))) %>%
  #mutate(n_st_anomaly = rescale(st_anomaly, c(0,1)))%>%
  mutate(box_bin = paste0(BOX, '-', DEPTH_BIN))

#TODO: try normalizing by box to see if the colour scale is easier to read?

# anomdat <- full_join(sdat24, sdat14, 
#                      suffix = c('24', '14'),
#                      by = c('DEPTH_BIN' = 'depth_bin', 
#                             'MONTH' = 'month',
#                             'BOX' = 'box')) %>%
#   select('DEPTH_BIN', 'MONTH', 'BOX', 'variable24', 'variable14', 'max_c14', 'max_c24', 'std', 'sd') %>%
#   mutate(anomaly = max_c24 - max_c14) %>%
#   mutate(st_anomaly = anomaly/sd) %>%
#   mutate(n_anomaly = rescale(anomdat$st_anomaly)) %>%
#   mutate(box_bin = paste0(BOX, '-', DEPTH_BIN))

pp <- ggplot(anomdat) +
  geom_tile(aes(x = MONTH,
                y = factor(box_bin, levels = yaxis_order),
                fill = n_anomaly))+
  geom_hline(yintercept = seq(0.5, 36.5, by = 4), size = 2, color = "black") +
  labs(title = paste(var24, ": Normalized Anomaly Heatmap"),
       x = "Month",
       y = "Box-Bin",
       fill = 'Anomaly') +
  theme_classic()+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_discrete(labels = yaxis_order)+
  #scale_fill_cmocean(name = 'balance', )+
  scale_fill_gradient2(low = 'darkblue', high = 'darkred', mid = 'white', midpoint = 0.5)+
  coord_cartesian(expand = FALSE)+
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 25))

ggsave(paste0('plots/anomaly_', var24, '.png'), pp,
       width = 12, height = 12)

}

# plot 4 (map anomaly) ----
# depth integrated anomaly plot on map with boxes

for (var in 1:9) {
  #var <- 2
  var14 <- dict_var$code[var]
  var24 <- dict_var$variable[var]
  
  for (sind in 1:4){
    seasonval <- unique(seasons$season)[sind]
  
  sdat24 <- data24 %>%
    filter(variable == var24) %>%
    filter(season == seasonval)
  
  sdat14 <- data14 %>%
    filter(variable == var14)%>%
    filter(season == seasonval)
  if (nrow(sdat14) != 0){
boxanom <- full_join(sdat24, sdat14,
                     suffix = c('24', '14'),
                     by = c('DEPTH_BIN' = 'depth_bin',
                            'MONTH' = 'month',
                            'BOX' = 'box')) %>%
  select('DEPTH_BIN', 'MONTH', 'BOX', 'variable24', 'variable14', 'mval24', 'mval14', 'std', 'sd') %>%
  group_by(BOX) %>%
  summarize(mval24 = mean(mval24, na.rm = TRUE),
            mval14 = mean(mval14, na.rm = TRUE)) %>%
  mutate(anomaly = mval24-mval14) %>%
  mutate(n_anomaly = rescale(anomaly, c(0,1))) %>%
  select(BOX, anomaly, n_anomaly)

boxdatanom <- full_join(boxdat, boxanom, by = c('box' = 'BOX'))

pp <- ggplot()+
  geom_polygon(data = boxdatanom[boxdatanom$box == 1,],
               aes(x = longitude, y = latitude, fill = anomaly),
               colour = 'black')+
  geom_polygon(data = boxdatanom[boxdatanom$box == 2,],
               aes(x = longitude, y = latitude, fill = anomaly),
               colour = 'black')+
  geom_polygon(data = boxdatanom[boxdatanom$box == 3,],
               aes(x = longitude, y = latitude, fill = anomaly),
               colour = 'black')+
  geom_polygon(data = boxdatanom[boxdatanom$box == 4,],
               aes(x = longitude, y = latitude, fill = anomaly),
               colour = 'black')+
  geom_polygon(data = boxdatanom[boxdatanom$box == 5,],
               aes(x = longitude, y = latitude, fill = anomaly),
               colour = 'black')+
  geom_polygon(data = boxdatanom[boxdatanom$box == 6,],
               aes(x = longitude, y = latitude, fill = anomaly),
               colour = 'black')+
  geom_polygon(data = boxdatanom[boxdatanom$box == 7,],
               aes(x = longitude, y = latitude, fill = anomaly),
               colour = 'black')+
  geom_polygon(data = boxdatanom[boxdatanom$box == 8,],
               aes(x = longitude, y = latitude, fill = anomaly),
               colour = 'black')+
  geom_polygon(data = boxdatanom[boxdatanom$box == 9,],
               aes(x = longitude, y = latitude, fill = anomaly),
               colour = 'black')+
  geom_polygon(data = clwf,
               aes(x = longitude, y = latitude),
               colour = 'grey')+
  scale_fill_gradient2(high = 'darkred', low = 'darkblue', mid = 'white', midpoint = 0)+
  #scale_fill_manual(values = color_palette)+
  scale_x_continuous(limits = c(-71, -48),
                     expand = c(0,0))+
  scale_y_continuous(limits = c(37, 48),
                     expand = c(0,0)) +
  labs(fill = 'Anomaly')+
  theme_classic()+
  ggtitle(paste('Anomaly map:', var24, '[', seasonval, ']'))+
  theme(legend.key.size = unit(2, 'cm'),
        title = element_text(size = 25),
        legend.text = element_text(size = 15))

ggsave(paste0('plots/anomalymap_', var24, '_', seasonval, '.png'), pp,
       width = 12, height = 12)
}
  }
}

# plot 5 (profile anomaly) ----
# profile anomaly for each box facet by season


for (boxnum in 1:9) {
  for (sind in 1:4){
    seasonval <- unique(seasons$season)[sind]
    
    sdat24 <- data24 %>%
      filter(BOX == boxnum) %>%
      filter(season == seasonval)
    
    sdat14 <- data14 %>%
      filter(box == boxnum)%>%
      filter(season == seasonval) 
    for (i in 1:length(sdat14$variable)){ 
      sdat14$variable[i] <- dict_var$variable[dict_var$code == sdat14$variable[i]]
    }

      boxbinanom <- full_join(sdat24, sdat14,
                           suffix = c('24', '14'),
                           by = c('DEPTH_BIN' = 'depth_bin',
                                  'MONTH' = 'month',
                                  'BOX' = 'box',
                                  'variable' = 'variable',
                                  'season' = 'season')) %>%
        filter(!is.na(mval14)) %>%
        select('DEPTH_BIN', 'MONTH', 'BOX', 'variable', 'mval24', 'mval14', 'std', 'sd') %>%
        group_by(BOX, DEPTH_BIN, variable) %>%
        summarize(mval24 = mean(mval24, na.rm = TRUE),
                  mval14 = mean(mval14, na.rm = TRUE)) %>%
        mutate(anomaly = mval24-mval14) %>%
        mutate(n_anomaly = rescale(anomaly, c(0,1))) 
      
      boxbinanom <- right_join(boxbinanom, depth_bins, by  = c('DEPTH_BIN' = 'bin'))
      
      pp <- ggplot()+
        geom_tile(data = boxbinanom, 
                  aes(x = variable, y = DEPTH_BIN, fill = anomaly)) +
        scale_y_reverse()+
        scale_fill_gradient2(high = 'darkred', low = 'darkblue', mid = 'white', midpoint = 0)+
        labs(x = 'Variable Name', y = 'Depth Bin', fill = 'Anomaly')+
        coord_cartesian(expand = FALSE)+
        ggtitle(paste('Anomaly profiles: Box', boxnum, '[', seasonval, ']'))+
        theme(axis.text = element_text(size = 15),
              title = element_text(size = 25),
              legend.key.size = unit(1.5, 'cm'),
              panel.background = 'darkgrey')+
        theme_classic()+
        geom_vline(xintercept = 1.5, linetype="solid", linewidth = 2, color = "black")+
        geom_vline(xintercept = 2.5, linetype="solid", linewidth = 2, color = "black")+
        geom_vline(xintercept = 3.5, linetype="solid", linewidth = 2, color = "black")
      
      ggsave(paste0('plots/anomalyprofile_Box', boxnum, '_', seasonval, '.png'), pp,
             width = 10, height = 12)
    }}


### plot 6 (profile means) ----

for (boxnum in 1:9) {
  for (var in 1:9) {
    var14 <- dict_var$code[var]
    var24 <- dict_var$variable[var]
    
    
    sdat24 <- data24 %>%
      filter(variable == var24) %>%
      filter(BOX == boxnum) %>%
      rename('month' = 'MONTH')
    
    sdat14 <- data14 %>%
      filter(variable == var14) %>%
      filter(box == boxnum)
    
    if (nrow(sdat14) != 0) {
      
      pp <- ggplot()+
        geom_path(data = sdat14,
                  aes(x = mval, y = min_depth),
                  linewidth = 2,
                  linetype = 'dashed',
                  alpha = 0.5,
                  colour = 'lightgreen')+
        geom_path(data = sdat24, 
                  aes(x = mval, y = min_depth),
                  linewidth = 2, alpha = 0.5,
                  colour = 'darkgreen')+
        scale_y_reverse()+
        facet_wrap('month')+
        labs(x = 'Value', y = 'Depth', col = 'Month')+
        theme_classic()+
        theme(panel.background = element_rect(fill = 'white'),
              axis.text = element_text(size = 15),
              title = element_text(size = 25))+
        ggtitle(paste('Mean climatology profiles:', var24, '[ Box', boxnum, ']'), subtitle = 'Solid: 2024 , Dashed: 2014')
      
      ggsave(paste0('plots/meanprofile_box', boxnum, '_', var14, '.png'), pp,
             width = 16, height = 16)
    }
  }
  
}

# plot 7 ----


#for (boxnum in 1:9) {
  for (var in 1:9) {
    var14 <- dict_var$code[var]
    var24 <- dict_var$variable[var]
    
    
    sdat24 <- data24 %>%
      filter(variable == var24)
    
    sdat14 <- data14 %>%
      filter(variable == var14)
    if (nrow(sdat14) != 0) {
    
    for (i in 1:length(sdat14$variable)){ 
      sdat14$variable[i] <- dict_var$variable[dict_var$code == sdat14$variable[i]]
    }
    
    alldat <- full_join(sdat24, sdat14,
                            suffix = c('24', '14'),
                            by = c('DEPTH_BIN' = 'depth_bin',
                                   'MONTH' = 'month',
                                   'BOX' = 'box',
                                   'variable' = 'variable',
                                   'season' = 'season')) %>%
      filter(!is.na(mval14)) %>%
      select('DEPTH_BIN', 'MONTH', 'BOX', 'variable', 'mval24', 'mval14', 'std', 'sd', 'min_c14', 'max_c14', 'min_c24', 'max_c24') 
    
    alldat_l_m <- pivot_longer(alldat,
                             cols = c('mval14', 'mval24'),
                             names_to = 'year',
                             values_to = 'mval',
                             names_prefix = 'mval') %>%
      select('DEPTH_BIN', 'MONTH', 'BOX', 'variable', 'year', 'mval')
    
    all_dat_l_min <- pivot_longer(alldat,
                                  cols = c('min_c14', 'min_c24'),
                                  names_to = 'year',
                                  values_to = 'min',
                                  names_prefix = 'min_c') %>%
      select('DEPTH_BIN', 'MONTH', 'BOX', 'variable', 'year', 'min')
    
    all_dat_l_max <- pivot_longer(alldat,
                                  cols = c('max_c14', 'max_c24'),
                                  names_to = 'year',
                                  values_to = 'max',
                                  names_prefix = 'max_c') %>%
      select('DEPTH_BIN', 'MONTH', 'BOX', 'variable', 'year', 'max')
    
    alldat_l <- alldat_l_m %>%
      left_join(all_dat_l_min, by = c('DEPTH_BIN', 'MONTH', 'BOX', 'variable', 'year')) %>%
      left_join(all_dat_l_max, by = c('DEPTH_BIN', 'MONTH', 'BOX', 'variable', 'year')) %>%
      mutate(year = as.factor(paste0('20',year)))
    
    
      
      # pp <- ggplot(data = alldat_l, aes(x = as.factor(MONTH), y = mval))+
      #   geom_boxplot(aes(fill = year))+
      #   labs(x = 'Month', y = 'Mean Value') +
      #   ggtitle(paste(var14))+
      #   facet_wrap(facets = 'BOX')
       
      alldat_l_summarized <- alldat_l %>%
        group_by(MONTH, year, BOX) %>%
        summarize(
          mval = mean(mval),
          min = min(min),
          max = max(max)
        )
    } else{
      # if var is only in 24
      alldat_l_summarized <- sdat24 %>%
        mutate(year = as.factor('2024')) %>%
        group_by(MONTH, year, BOX) %>%
        summarize(
          mval = mean(mval, na.rm = TRUE),
          min = min(min_c, na.rm = TRUE),
          max = max(max_c, na.rm = TRUE)
        )
    }
      
    p_labeller <- function(variable,value){
      return(paste('Box', value))
    }
    
      # Create the plot
      pp <- ggplot(data = alldat_l_summarized,
                   aes(x = as.factor(MONTH), y = mval)) +
        geom_boxplot(aes(ymin = min,
                         ymax = max,
                         lower = min,
                         upper = max,
                         middle = mval,
                         fill = year),
                     stat = "identity") +
        labs(x = 'Month', y = 'Value', fill = '') +
        ggtitle(paste(var24)) +
        facet_wrap(facets = 'BOX', labeller = p_labeller)+
        theme_classic()+
        theme(axis.text = element_text(size = 15), 
              title = element_text(size = 25), 
              legend.text = element_text(size = 15),
              strip.text = element_text(size = 20))+
        coord_cartesian(ylim = c(0, NA))
      
      ggsave(paste0('plots/boxplot_', var24, '.png'), pp,
              width = 16, height = 16)
    
    
  }
  
#}
