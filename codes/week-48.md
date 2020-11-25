Week 48
================

Libraries and settings

``` r
library(tidyverse)
library(tidytuesdayR)
library(scales)
library(sysfonts)
library(showtext)
library(ggtext)
```

# Load the weekly Data

Dowload the weekly data and make available in the `tt` object.

``` r
#data <- tidytuesdayR::tt_load("2020-11-24")
hike<-readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))
```

# Data

Using the dataset to create unique visualization.

``` r
library(measurements)
hike<-hike %>%
  separate(location,c("region","location"),sep="--") %>% 
  separate(length,c("length","type"),sep ="\\,") %>% 
  separate(length,c("length","measure"),sep =" ",convert =TRUE) %>% 
  unnest(features) %>% 
  mutate(type=if_else(is.na(type),"other",type) %>%trimws() ,
         length=conv_unit(length, "mile", "km"),
         gain=conv_unit(gain %>% as.numeric(),"ft","m"),
         highpoint=as.numeric(highpoint),
         rating=as.numeric(rating),
         region=trimws(region))

hike_avg<-hike %>%group_by(region) %>%
  mutate(avg=mean(rating),
         length_mean = mean(length) ) %>% 
  ungroup() %>% 
  mutate(region = factor(region))
```

# Theme

``` r
font_add_google("Nunito", "Nunito")
font_add_google("Roboto Condensed", "Roboto Condensed")
font_add_google("Special Elite", "Special Elite")
showtext_auto()
theme_set(hrbrthemes::theme_ft_rc(base_family = "Nunito",
                                  base_size = 15))
```

# Create plot

``` r
p<-hike_avg %>%
  ggplot(aes(length_mean, rating)) +
  geom_jitter(aes(size = gain,
                 color = region),
             alpha = 0.5,
             width = 0.2) +
  nord::scale_color_nord(
    palette = "red_mountain"
  ) +
  #coord_flip()+
 guides(size = guide_legend(order = 1))+
   labs(
    title = "Washington State Trails",
    subtitle = " ",
    caption = "Data: Washington Trails Association (www.wta.org) \nTidyTuesday Project - week 48 \nVisualization: @Lacapary",
    y ="User rating (out of 5)",
    x = "Average length of trail \n (km) ",
    color = "",
    size = "Gain in elevation (m.a.s.l)"
   ) +
  theme(
    panel.background = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "horizontal",
    plot.caption = element_text(
      family = "Special Elite",
      size = 10,
      color = "grey80",
      margin = margin(5, 0, 20, 0)
    ),
    plot.margin = margin(1, 2, 1, 2, "cm")
  )

p 
```

![](README_figs/README-Washington_State-1.png)<!-- -->

``` r
#ggsave(plot =p,"README_figs/Washington_trail.png", dpi = 300)
```
