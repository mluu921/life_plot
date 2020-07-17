library(tidyverse)
library(hrbrthemes)
library(paletteer)

df <- tibble(
  date = seq(mdy('9/1/1987'), floor_date(Sys.Date(), 'month'), 'month')
) %>%
  mutate(
    month = month(date),
    year = year(date)
  )

df <- tibble(date = seq(mdy('9/1/1987'), Sys.Date(), '1 month')) %>%
  mutate(month = month(date),
         year = year(date))


temp <- df %>%
  mutate(
    era = case_when(
      date %in% mdy('9/1/1987'):mdy("9/1/1991") ~ 'Childhood',
      date %in% mdy('10/1/1991'):mdy('6/1/2005') ~ 'K-12 Grade School',
      date %in% mdy('7/1/2005'):mdy('12/1/2009') ~ 'BSc in Biological Sciences',
      date %in% mdy('1/1/2010'):mdy('7/1/2013') ~ 'Pre Graduate Work',
      date %in% mdy('8/1/2013'):mdy('6/1/2015') ~ 'MPH in Biostatistics & Epidemiology',
      date %in% mdy('7/1/2015'):mdy('8/1/2016') ~ 'CHLA - Data Analyst',
      date %in% mdy('9/1/2016'):Sys.Date() ~ 'CSHS - Biostatistician',
      TRUE ~ 'Time Remaining'
    )
  ) %>%
  mutate(era = factor(
    era,
    levels = c(
      'Childhood',
      'K-12 Grade School',
      'BSc in Biological Sciences',
      'Pre Graduate Work',
      'MPH in Biostatistics & Epidemiology',
      'CHLA - Data Analyst',
      'CSHS - Biostatistician',
      'Time Remaining'
    )
  ))


plot <- temp %>%
  group_by(era) %>%
  count() %>%
  ungroup() %>%
  ggplot(.) + 
  geom_waffle(aes(fill = era, values = n), n_rows = 12, flip = T, alpha = .5, color = 'white') + 
  scale_fill_paletteer_d("ggsci::category10_d3", guide = guide_legend(reverse = T)) +
  theme_void() +
  coord_equal() +
  theme_enhance_waffle() +
  theme(legend.title = element_blank(), legend.position = 'none')

plot + 
  scale_y_continuous(breaks = 0:50) + 
  annotate('text', x = 6.5, y = 2.5, label = 'Childhood', hjust = 'center', fontface = 'bold') + 
  annotate('text', x = 6.5, y = 11.5, label = 'K-12 Grade School', hjust = 'Center', fontface = 'bold') +
  annotate('text', x = 6.5, y = 21, label = 'BSc in Biological Sciences', hjust = 'Center', fontface = 'bold') +
  annotate('text', x = 6.5, y = 25, label = 'Pre Graduate Work', hjust = 'Center', fontface = 'bold') +
  annotate('text', x = 6.5, y = 27.5, label = 'MPH in Biostatistics & Epidemiology', hjust = 'Center', fontface = 'bold') +
  annotate('text', x = 6.5, y = 29, label = 'CHLA - Data Analyst', hjust = 'Center', fontface = 'bold') +
  annotate('text', x = 6.5, y = 31.5, label = 'CSHS - Biostatistician', hjust = 'Center', fontface = 'bold') 














