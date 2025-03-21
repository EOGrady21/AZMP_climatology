



###############################
#   ROUGH WORKING SCRIPT ----
###############################


























# more plots

library(tidyverse)

data_m <- read_csv('data/final_climatology/merged_climatology.csv')

depth_bin_labels <- c('0-10m', '10-30m', '30-100m', '100m-bottom')
depth_bin_labels <- setNames(depth_bin_labels, c('1', '2', '3', '4'))
# make timeseries plots for each variable, box and depth bin

for (var in unique(data_m$variable)) {
  
  for(box in unique(data_m$BOX)) {
    filtdat <- data_m %>%
      filter(variable == var & BOX == box)
    
    # plot time series box plot and facet by depth bin
    p <- ggplot(filtdat) +
      geom_point(aes(x = MONTH, y = mval_m), size = 3, pch = 21, col = 'darkblue')+
      geom_text(aes(label = n_m, x = MONTH, y = max_val_m), vjust = -.5, hjust = 1.5, colour = 'blue', size = 5)+
      #geom_path(aes(x = MONTH, y = mval_m, group = DEPTH_BIN))+
      geom_errorbar(aes(x = MONTH, ymin = min_val_m, ymax = max_val_m))+
      facet_wrap(~DEPTH_BIN, ncol = 1, labeller = labeller(DEPTH_BIN = depth_bin_labels))+
      scale_x_continuous(breaks = 1:12)+
      # add facet labels
      labs(title = paste('Timeseries of ', var, ' for box ', box),
           x = 'Month', y = 'Value')+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 15),
            title = element_text(size = 25), 
            strip.text = element_text(size = 15)
            )+
      scale_y_continuous(limits = c(0, 1.5*max(filtdat$max_val_m, na.rm = TRUE)))
    ggsave(paste0('plots/box_timeseries_', var, '_box_', box, '.png'), p, height = 12, width = 14)

  }
}

# plot time series of Ammonia mean values

for (var in unique(data_m$variable)) {

vardat <- data_m %>%
  filter(variable == var)

vardat <- vardat %>%
  mutate(month_name = month.name[vardat$MONTH]) %>%
  group_by(month_name) %>%
  summarise(mean_val = mean(mval_m, na.rm = TRUE))

vardat$month_name <- forcats::fct_relevel(vardat$month_name, month.name)


p <- ggplot()+
  geom_point(data = vardat, aes(x = month_name, y = mean_val), colour = 'red')+
  geom_line(data = vardat, aes(x = month_name, y = mean_val, group = 1), colour = 'red')+
  ggtitle(paste('Timeseries of ', var), subtitle = 'Scotian Shelf Climatology')+
  theme_classic()+
  labs(x = 'Month', y = 'Mean Value')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 15),
        title = element_text(size = 25))

ggsave(paste0('plots/Ammonia/timeseries_', var,'.png'), p, height = 12, width = 14)

}

# plot ammonia data profile by season for all boxes


# show some spatial data distribution for css, wss, ess

regions <- data.frame(region = c('wss', 'wss', 'wss', 'wss',
                                 'css', 'css',
                                 'ess', 'ess', 'ess'), 
                      box = c(1, 2, 3, 4, 
                              5, 6,
                              7, 8, 9
                              ))
data_m <- data_m %>%
  right_join(regions, c('BOX' = 'box'))
data_m$region <- forcats::fct_relevel(data_m$region,  c("wss", "css", "ess"))


for (i in 1:length(unique(data_m$variable))) {
  var <- unique(data_m$variable)[i]
  
  vardat <- data_m[data_m$variable == var,]
  
  vardat <- vardat %>%
    mutate(month_name = month.name[vardat$MONTH]) %>%
    group_by(month_name) 
  
  vardat$month_name <- forcats::fct_relevel(vardat$month_name, month.name)
  
  
  p <- ggplot()+
    geom_boxplot(data = vardat, aes(x = month_name, y = max_val_m))+
    #geom_line(data = vardat, aes(x = month_name, y = max_val_m, group = 1), colour = 'red')+
    ggtitle(paste(var), subtitle = 'Scotian Shelf Climatology')+
    theme_classic()+
    labs(x = 'Month', y = 'Max Value')+
    theme(strip.text = element_text(size = 15),
      axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 15),
          title = element_text(size = 25),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1))+
    facet_wrap(~region)
  
  
  ggsave(paste0('plots/regional_boxplots_', var, '.png'), p, height = 12, width = 16)
  
}
