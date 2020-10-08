library(tidyverse)
library(lubridate)
library(ggsci)

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


plot_data <- df %>%
  mutate(
    era = case_when(
      date %in% mdy('9/1/1987'):mdy("9/1/1991") ~ 'Childhood',
      date %in% mdy('10/1/1991'):mdy('6/1/2005') ~ 'K-12 Grade School',
      date %in% mdy('7/1/2005'):mdy('12/1/2009') ~ 'BSc in Biological Sciences',
      date %in% mdy('1/1/2010'):mdy('7/1/2013') ~ 'Pre Graduate Work',
      date %in% mdy('8/1/2013'):mdy('6/1/2015') ~ 'MPH in Biostatistics & Epidemiology',
      date %in% mdy('7/1/2015'):mdy('8/1/2016') ~ 'Data Analyst',
      date %in% mdy('9/1/2016'):Sys.Date() ~ 'Biostatistician'
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
      'Data Analyst',
      'Biostatistician'
    )
  ))

base_plot <- ggplot(plot_data, aes(y = month, x = year)) + 
  geom_tile(color = 'white', aes(fill = era), size = 1) + 
  scale_y_continuous(breaks = -6:18, limits = c(-6, 18)) +
  scale_x_continuous(breaks = 1980:2020) +
  labs(y = 'Month', x = 'Year') + 
  theme_light() + 
  theme(legend.position = 'bottom') + 
  scale_fill_d3()
  

## annotate the definition of 1 square = 1 month
plot <- base_plot +
  geom_curve(
    x = 1987,
    y = 12,
    xend = 1986,
    yend = 14,
    curvature = -.4,
    arrow = arrow(length = unit(0.01, "npc"), ends = 'first'),
    color = 'black'
  ) + 
  annotate(
    'text',
    x = 1985,
    y = 15,
    hjust = 0,
    label = '1 square = 1 month',
    fontface = 'bold.italic'
  )

## annotate eras with labels

### set colors 
pallete_colors <- pal_d3("category10")(10)

## set size
annotation_size <- 5

plot <- plot + 
  annotate(
    'text',
    x = 1989,
    y = -1,
    label = 'Childhood',
    fontface = 'bold.italic',
    color = pallete_colors[[1]],
    size = annotation_size
  )  +
  annotate(
    'text',
    x = 1998,
    y = -1,
    label = 'K-12 Grade School',
    fontface = 'bold.italic',
    color = pallete_colors[[2]],
    size = annotation_size
  ) +
  annotate(
    'text',
    x = 2007.5,
    y = -1,
    label = 'BSc in Biological Sciences',
    fontface = 'bold.italic',
    color = pallete_colors[[3]],
    size = annotation_size
  ) +
  annotate(
    'text',
    x = 2011,
    y = 14,
    label = 'Pre Graduate Employment',
    fontface = 'bold.italic',
    color = pallete_colors[[4]],
    size = annotation_size
  ) +
  annotate(
    'text',
    x = 2013,
    y = -3,
    label = 'MPH in Biostatistics & Epidemiology',
    fontface = 'bold.italic',
    color = pallete_colors[[5]],
    size = annotation_size
  ) +
  annotate(
    'text',
    x = 2013,
    y = 16,
    label = 'Data Analyst',
    fontface = 'bold.italic',
    color = pallete_colors[[6]],
    size = annotation_size
  ) +
  annotate(
    'text',
    x = 2018.5,
    y = -1,
    label = 'Biostatistician',
    fontface = 'bold.italic',
    color = pallete_colors[[7]],
    size = annotation_size
  ) 


## add additional curve segments for labels

plot <- plot + 
  geom_curve(
    x = 1989,
    y = 1,
    xend = 1989,
    yend = -.5,
    curvature = .2,
    arrow = arrow(length = unit(0.01, 'npc'), ends = 'first'),
    color = 'black'
  ) +
  geom_curve(
    x = 1998,
    y = 1,
    xend = 1998,
    yend = -.5,
    curvature = .2,
    arrow = arrow(length = unit(0.01, 'npc'), ends = 'first'),
    color = 'black'
  ) +
  geom_curve(
    x = 2007,
    y = 1,
    xend = 2007,
    yend = -.5,
    curvature = -.2,
    arrow = arrow(length = unit(0.01, 'npc'), ends = 'first'),
    color = 'black'
  ) +
  geom_curve(
    x = 2011,
    y = 12,
    xend = 2011,
    yend = 13.5,
    curvature = -.2,
    arrow = arrow(length = unit(0.01, 'npc'), ends = 'first'),
    color = 'black'
  ) +
  geom_curve(
    x = 2015,
    y = 12,
    xend =  2015,
    yend = 16,
    arrow = arrow(length = unit(0.01, 'npc'), ends = 'first'),
    color = 'black',
    curvature = .5
  ) +
  geom_curve(
    x = 2014,
    y = 1,
    xend =  2014,
    yend = -2.5,
    arrow = arrow(length = unit(0.01, 'npc'), ends = 'first'),
    curvature = -0.2,
    color = 'black'
  ) +
  geom_curve(
    x = 2018,
    y = 1,
    xend =  2018,
    yend = -0.5,
    arrow = arrow(length = unit(0.01, 'npc'), ends = 'first'),
    curvature = -0.2,
    color = 'black'
  ) 

## let's add a label for 1 column equals 1 year of age 

plot <- plot + 
  annotate(
    'text',
    x = 1985,
    y = 6,
    label = '1 year',
    angle = 90,
    size = 7,
    fontface = 'bold.italic',
    color = 'black'
  ) + 
  annotate(
    'text',
    x = 1988,
    y = 13,
    label = 'age',
    size = 5,
    fontface = 'bold.italic',
    color = 'black'
  ) +
  geom_segment(
    x = 1988.75,
    y = 13,
    xend = 1993,
    yend = 13,
    arrow = arrow(ends = 'last', length = unit(.01, units = 'npc')),
    color = 'black'
  ) +
  geom_segment(
    x = 1985,
    xend = 1985,
    y = 8,
    yend = 12,
    color = 'black'
  ) +
  geom_segment(
    x = 1985,
    xend = 1985,
    y = 1,
    yend = 4,
    color = 'black'
  ) +
  geom_segment(
    x = 1984.5,
    xend = 1985.5,
    y = 12,
    yend = 12,
    color = 'black'
  ) +
  geom_segment(
    x = 1984.5,
    xend = 1985.5,
    y = 1,
    yend = 1,
    color = 'black'
  ) 

## lets add a title
plot <- plot + 
  annotate(
    'text',
    x = 1987,
    y = -5,
    label = 'My life in Months.....',
    size = 25,
    hjust = 0,
    fontface = 'bold.italic'
  )

plot <- plot +
  theme_void() +
  theme(
    legend.position = 'none'
  )

plot

ggsave('mylifeinmonths.png', height = 7, width = 15, dpi = 600)


