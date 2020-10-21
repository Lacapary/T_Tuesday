Week 43
================

Libraries and settings

``` r
library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(ggtext)
library(geofacet)
loadfonts(device = "win")
```

# Load the weekly Data

Dowload the weekly data and make available in the `tt` object.

``` r
data <- tidytuesdayR::tt_load("2020-10-20")
beer_awards<-data$beer_awards
glimpse(beer_awards)
```

# Visualize

Using the dataset to create unique visualization.

``` r
p <- beer_awards %>%
  mutate(state = toupper(state)) %>%
  group_by(state, medal) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
# time to plot
 ggplot( aes("", freq, fill = medal)) +
  geom_col(width = 1) +
  scale_fill_manual(values = c("#DB8A11", "#F5D20E", "#8F8F8F")) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal()  +
  facet_geo(~ state, grid = "us_state_grid2", label = "name") +
  labs(
    title = "Brewing excellence",
    subtitle = "Proportion of medals of Great American Beer Festival awards earned per state since 1987",
    caption = "Source:  Great American Beer Festival \nTidyTuesday Project - week 43 \nVisualization: @Lacapary",
    x = NULL,
    y = "Proportion of medals"
  ) +
  
  theme(
    legend.position = "none",
    text = element_text(family = "Candara", color = "#00688B"),
    plot.title = element_text(
      family = "Candara",
      size = 20,
      hjust = .5,
      face = "bold"
    ),
    plot.subtitle = element_text(
      family = "Candara",
      size = 11,
      hjust = .5
    ),
    plot.caption = element_text(family = "Candara",
                                size = 10),
    plot.caption.position = "plot",
    plot.margin = margin(1, 1, 1, 1),
    axis.ticks.y = element_blank(),
    strip.text.x = element_text(
      size = 6,
      family = "Candara",
      color = "#00688B"
    ),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 6,
      color = "#00688B",
      family = "Candara"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(color = "#F0EDE7", fill = "#F0EDE7")
  )
```

    ## `summarise()` regrouping output by 'state' (override with `.groups` argument)

![](README_figs/README-unnamed-chunk-3-1.png)<!-- -->
