# update oxygen climatology based on notes from researchers
# Emily O'Grady April 2025

# During the presentation of this climatology product it was commented by Dr.
# Catherine Johnson that the top two oxygen bins could be combined, since the
# coverage is so poor and resulting in narrow ranges in the intermediate depth
# bin. This adjustment should help make the range more reasonable and the QC
# testing should have a more realistic tolerance for variation that still
# aligns with expectations of climatologically average oxygen profiles.

# This code reformats the oxygen data from the merged climatology file produced
# in final_climatology_calculation.rmd and then exports it to the same format
# which will be translated into matlab for use in the IML QC system

# Load libraries
library(tidyverse)


# Load data
# The merged climatology file produced in final_climatology_calculation.rmd
data <- read.csv('data/final_climatology/merged_climatology.csv', header = TRUE)

oxygen_data <- data %>%
  filter(variable == 'O2')

data <- data %>%
  filter(variable != 'O2')

# Combine the top two oxygen bins
# maintain 4 bin data structure for easy compatibility with other vars in matlab
oxygen_data_m <- oxygen_data %>%
  mutate(db_12 = ifelse(DEPTH_BIN == 1, 12, DEPTH_BIN)) %>%
  mutate(db_12 = ifelse(DEPTH_BIN == 2, 12, db_12)) %>%
  group_by(MONTH, BOX, db_12) %>%
  mutate(mval_m = ifelse(db_12 == 12, mean(mval_m), mval_m)) %>%
  mutate(n_m = ifelse(db_12 == 12, sum(n_m), n_m)) %>%
  mutate(min_val_m = ifelse(db_12 == 12, min(min_val_m), min_val_m)) %>%
  mutate(max_val_m = ifelse(db_12 == 12, max(max_val_m), max_val_m)) %>%
  mutate(sd_m = ifelse(db_12 == 12, mean(sd_m), sd_m)) %>%
  mutate(min_depth_m = ifelse(db_12 == 12, min(min_depth_m), min_depth_m)) %>%
  mutate(max_depth_m = ifelse(db_12 == 12, max(max_depth_m), max_depth_m)) %>%
  ungroup() %>%
  select(-db_12)


# plot to check

ggplot(oxygen_data_m) +
  geom_point(aes(y = min_depth_m, x = mval_m)) +
  geom_errorbar(aes(y = min_depth_m, xmin = min_val_m, xmax = max_val_m)) +
  facet_wrap(~MONTH)+
  scale_y_reverse()
  

# export oxygen data back to merged climatology
data_f <- rbind(data, oxygen_data_m)
write.csv(data_f, 'data/final_climatology/merged_climatology.csv', row.names = FALSE)
