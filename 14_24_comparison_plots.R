# presentation style plots

library(ocedata)
library(ggplot2)
library(tidyverse)
library(cmocean)


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




### Plot 1 ----
var <- 9
var14 <- dict_var$code[var]
var24 <- dict_var$variable[var]

boxnum <- 2
binnum <- 1
sdat24 <- data24 %>%
  filter(variable == var24) %>%
  filter(BOX == boxnum) %>%
  filter(DEPTH_BIN == binnum)

sdat14 <- data14 %>%
  filter(variable == var14) %>%
  filter(box == boxnum)%>%
  filter(depth_bin == binnum)

ggplot()+
  geom_path(data = sdat24, 
             aes(x = MONTH, y = mval, group = DEPTH_BIN),
            colour = 'pink', linewidth = 3)+
  geom_errorbar(data = sdat24, aes(x = MONTH, ymin = min_c, ymax = max_c),
                colour = 'pink',linewidth = 2, alpha = 0.5)+
  geom_path(data = sdat14,
             aes(x = month, y = mval, group = 'depth_bin'),
             colour = 'lightblue', linewidth = 3)+
  geom_errorbar(data = sdat14, aes(x = month, ymin = min_c, ymax = max_c),
                colour = 'lightblue', linewidth = 1.25, alpha = 0.5)+
  labs(x = 'Month', y = 'Value', colour = 'Depth Bin')+
  scale_x_continuous(breaks = seq(1:12)) +
  coord_cartesian(ylim = c(0, NA))+
  theme_classic()+
  ggtitle(paste(var24, "climatology values: Box", boxnum, ', Depth bin', binnum),
          subtitle = 'Blue: 2014, Pink: 2024')+
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 25))


# plot 2 ---- 

var <- 9
var14 <- dict_var$code[var]
var24 <- dict_var$variable[var]

boxnum <- 3
seasonval <- 'fall'
sdat24 <- data24 %>%
  filter(variable == var24) %>%
  filter(BOX == boxnum) %>%
  filter(season == seasonval) %>%
  group_by(DEPTH_BIN) %>%
  summarize(min_c = min(min_c),
            max_c = max(max_c),
            ) %>%
  right_join(., depth_bins, c('DEPTH_BIN' = 'bin'))

sdat14 <- data14 %>%
  filter(variable == var14) %>%
  filter(box == boxnum)%>%
  filter(season == seasonval)%>%
  group_by(depth_bin) %>%
  summarize(min_c = min(min_c),
            max_c = max(max_c),
  ) %>%
  right_join(., depth_bins, c('depth_bin' = 'bin'))

ggplot()+
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
  coord_cartesian(ylim = c(200, 0), xlim = c(0, NA))+
  theme_classic()+
  labs(x = 'Value', y = 'Depth [m]')+
  ggtitle(paste(var24, "climatology profile: Box", boxnum,',', seasonval),
          subtitle = 'Pink: 2024, Blue-Dash: 2014')+
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 25))


# plot 3 ----
var <- 7
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
  select('DEPTH_BIN', 'MONTH', 'BOX', 'variable24', 'variable14', 'mval24', 'mval14') %>%
  mutate(anomaly = mval24 - mval14) %>%
  mutate(norm_anomaly = scale(anomaly)) %>%
  mutate(box_bin = paste0(BOX, '-', DEPTH_BIN))

yaxis_order <- rev(c('1-1', '1-2', '1-3', '1-4',
                 '2-1', '2-2', '2-3', '2-4',
                 '3-1', '3-2', '3-3', '3-4',
                 '4-1', '4-2', '4-3', '4-4',
                 '5-1', '5-2', '5-3', '5-4',
                 '6-1', '6-2', '6-3', '6-4',
                 '7-1', '7-2', '7-3', '7-4',
                 '8-1', '8-2', '8-3', '8-4',
                 '9-1', '9-2', '9-3', '9-4'))


ggplot(anomdat) +
  geom_tile(aes(x = MONTH,
                y = factor(box_bin, levels = yaxis_order),
                fill = norm_anomaly))+
  geom_hline(yintercept = seq(0.5, 36.5, by = 4), size = 2, color = "black") +
  labs(title = paste(var24, ": Normalized Anomaly Heatmap"),
       x = "Month",
       y = "Box-Bin",
       fill = 'Normalized \n Anomaly') +
  theme_classic()+
  scale_x_continuous(breaks = c(1:12))+
  scale_y_discrete(labels = yaxis_order)+
  scale_fill_cmocean(name = 'balance')+
  coord_cartesian(expand = FALSE)+
  theme(axis.text = element_text(size = 15),
        title = element_text(size = 25))
