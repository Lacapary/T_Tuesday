Week 45
================

Libraries and settings

``` r
library(tidyverse)
library(tidytuesdayR)
library(sysfonts)
library(showtext)
library(ggpubr)
library(scales)
```

# Load the weekly Data

Dowload the weekly data and make available in the `tt` object.

``` r
data <- tidytuesdayR::tt_load("2020-11-03")
ikea<-data$ikea
glimpse(ikea)

ikea<- ikea %>% 
  mutate(usd = price*0.27)

grouped_cat <- ikea %>% 
  group_by(category) %>% 
  summarize(average_cost = mean(price), n()) %>% 
  arrange(desc(average_cost))
```

# Visualize

Using the dataset to create unique visualization.

``` r
font_add_google(name = "Quicksand", family = "Quicksand")
showtext_auto()

p<-ikea %>% ggerrorplot(
  x = "category",
  y = "usd",
  desc_stat = "mean_sd",
  #color = "category",
  color = "#0051ba",
  add = "violin",
  #color violin
  add.params = list(color = "#ffda1a", fill = "#ffda1a"),
  orientation = "horizontal",
  xlab = "",
  legend = "none",
  #reordering bu group category
  order = rev(grouped_cat$category)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "#0051ba",
    size = 0.4
  ) +
  labs(
    x = NULL,
    y = "Mean Price (in USD)",
    title = "IKEA Prices in Saudi Arabian",
    subtitle = "Average price comparison based on furniture categories: ",
    caption = "1 USD ~ 3.7 SAR n\
        Data source: Kaggle n\
        TidyTuesday Project - week 45 \n Visualization: @Lacapary"
  ) + 
  theme(
    text = element_text(family =  "Quicksand", color = "#0051ba"),
    axis.line = element_line(colour = "#0051ba"),
    axis.ticks =element_line(colour = "#0051ba"),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(color = "#0051ba"),
    axis.text.x = element_text(size = 12, face = "bold", color = "#0051ba"),
    axis.text.y = element_text(size = 10, face = "bold", color = "#0051ba")
  ) +
  scale_y_continuous(labels = scales::dollar_format())
```

![](README_figs/README-unnamed-chunk-3-1.png)<!-- -->
