library(tidyverse)

data_fns <- list.files('data/filtered_data', pattern = '.csv', full.names = TRUE)

for (i in 1:length(data_fns)) {
data <- read.csv(data_fns[i])

param_name <- unique(data$PARAMETER_NAME)

# temporal coverage by year/month ----
yearMonth <- data %>%
  mutate(., year = format(as.Date(data$EVENT_START, '%d-%b-%y'), '%Y')) %>%
  mutate(., month = format(as.Date(data$EVENT_START, '%d-%b-%y'), '%b')) %>%
  group_by(year, month) %>%
  summarise(N = n())

# order months in reverse calendar order
yearMonth$month <- factor(yearMonth$month,
                          levels = c("Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan"))

p_temp <- ggplot(yearMonth, aes(year, month)) +
  geom_tile(aes(fill = N), colour = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Year", y = "Month", fill = "Number of \n Data Points") +
  theme_minimal()+
  ggtitle(paste(param_name, "temporal coverage"), subtitle = paste("Total points: ", sum(yearMonth$N)))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p_temp, filename = paste('plots/', param_name, '_temporal_coverage.png'))

}
