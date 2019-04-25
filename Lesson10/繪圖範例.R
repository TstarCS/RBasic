library(tidyverse)
library(magrittr)

# Date <- Sys.Date()
Date <- as.Date('2019-03-20')

df <- 
  read_csv(file = 
  paste0('~/df/CUST_', Date,'.csv'))

df %<>% mutate(Hour = substr(CTACT_STRT_TSTMP,12,13))

df2 <- 
  df %>%
  group_by(Hour)%>%
  summarise(counts=n())%>%
  arrange(Hour)

plot <- 
  df2 %>%
  ggplot(aes(x = Hour, y = counts))+
  geom_line(stat = "identity", group = 1)+
  labs(x = "Hour", y = "counts", 
       title = "Counts per hour")
plot

ggsave(filename = paste0(Date,'.jpeg'), 
       plot = plot,
       device = 'jpeg',
       path = '~/plot')
