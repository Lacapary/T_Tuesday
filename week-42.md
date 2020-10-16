Week 42
================

Libraries and settings

``` r
library(tidyverse)
library(tidytuesdayR)
library(rcartocolor)
library(extrafont)
library(gganimate)
library(ggtext)
library(here)
loadfonts(device = "win")
```

# Load the weekly Data

Dowload the weekly data and make available in the `tt` object.

``` r
data <- tidytuesdayR::tt_load("2020-10-13")

datasaurus<-data$datasaurus
glimpse(datasaurus)

colours <- c(carto_pal(12, "Prism"), "#000000")
```

# Visualize

Using your processed dataset, create your unique visualization.

``` r
datasaurus<-datasaurus %>%
  group_by(dataset) %>%
  summarise(mean_x = mean(x),
            mean_y = mean(y),
            sd_x   =   sd(x),
            sd_y   =   sd(y),
            coefficient  = cor(x, y, method = "pearson")) %>%
  left_join(datasaurus)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Joining, by = "dataset"

``` r
p<-datasaurus %>%   ggplot(aes(x = x, y = y)) +
  #set same limits for x and y coords
  #coord_equal(clip = "off") +
  geom_point(aes(colour = dataset, group = 1L),
             size = 3,
             alpha = 0.6) +
   scale_color_manual(values = colours, guide = "none") +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 100, by = 20)) +
  geom_text(
    aes(
      x = 60,
      y = 136,
      label = paste("mean (x):", round(mean_x, 4)),
      family = "Century Gothic"
    ),
    hjust = 0.5,
    size = 4.5,
    color = "grey70"
  ) +
  geom_text(
    aes(
      x = 60,
      y = 132,
      label = paste("mean (y):", round(mean_y, 4)),
      family = "Century Gothic"
    ),
    hjust = 0.5,
    size = 4.5,
    color = "grey70"
  ) +
  geom_text(
    aes(
      x = 60,
      y = 128,
      label = paste("standard deviation (x):", round(sd_x, 4)),
      family = "Century Gothic"
    ),
    hjust = 0.5,
    size = 4.5,
    color = "grey70"
  ) +
  geom_text(
    aes(
      x = 60,
      y = 124,
      label = paste("standard deviation (y):", round(sd_y, 4)),
      family = "Century Gothic"
    ),
    hjust = 0.5,
    size = 4.5,
    color = "grey70"
  ) +
  geom_text(
    aes(
      x = 60,
      y = 120,
      label = paste("correlation", round(coefficient, 4)),
      family = "Century Gothic"
    ),
    hjust = 0.5,
    size = 4.5,
    color = "grey70"
  ) +
  labs(title = "The Importance of Data Visualisation",
       subtitle = "<b style='font-size:24pt;'>Case Study: The DataSaurus Dozen</b><br><br>The DataSaurus Dozen highlights the importance of visualising data; while summary statistics (means, standard deviations and correlation measures) for various datasets can be the same, their distributions can be significantly varied. ",
       caption = " Source: Francis Anscombe, Alberto Cairo, Justin Matejka & George Fitzmaurice  \nTidyTuesday Project - week 42 \nVisualization: @Lacapary") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Century Gothic", 
                                  size = 42,
                                  color = "#E836F5",
                                  hjust = 0.5,
                                  margin = margin(15, 0, 40, 0)),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(family = "Century Gothic", 
                                     size = 12,
                                     color = "#FCDBF4", 
                                     halign = 0.5,
                                     lineheight = 1.2,
                                     margin = margin(10, 0, 0, 0)),
        plot.caption = element_text(family = "Century Gothic",
                                    size = 12,
                                    colour = "#FCDBF4",
                                    lineheight = 1.2,
                                    hjust = 0.5,
                                    margin = margin(40, 0, 20, 0)),
        plot.caption.position = "plot",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(90, 70, 90, 70),
        plot.background = element_rect(color = "grey27", fill = "grey27")) +
  transition_states(dataset, 10, 3) + 
  ease_aes('cubic-in-out') 
```

# Plot

![](README_figs/README-unnamed-chunk-3-1.gif)<!-- -->
