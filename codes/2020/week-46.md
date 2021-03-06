Week 46
================

Libraries and settings

``` r
library(tidyverse)
library(tidytuesdayR)
library(scales)
library(sysfonts)
library(showtext)
library(ggtext)
theme_set(theme_bw())
```

# Load the weekly Data

Dowload the weekly data and make available in the `tt` object.

``` r
data <- tidytuesdayR::tt_load("2020-11-10")
mobile<-data$mobile %>% 
  rename(subs=mobile_subs) %>% 
  mutate(type="Mobile")
landline<-data$landline %>% 
    rename(subs=landline_subs) %>% 
  mutate(type="Landline")


df <- bind_rows(mobile,landline) %>% 
  rename(country=entity) %>% 
 filter(!is.na(subs)) %>%
  arrange(continent) %>% 
  group_by(continent,type,year) %>% 
  summarise(sum_sub=sum(subs)) %>% 
   mutate(subs=if_else(type =="Mobile",sum_sub,sum_sub*-1)) 
```

# Visualize

Using the dataset to create unique visualization.

``` r
font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()


p<-df %>% ggplot(aes(x = year, y = subs, fill = type)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = 0, color = "white") +
  scale_fill_manual(
    name = "Phone Type",
    labels = c("Landline", "Mobile"),
    values = c("#5D478B", "#8968CD")
  ) +
  labs(
    x = "Year",
    y = "Phone Subs (per 100 People)",
    fill = "",
    title = "Phone Subscriptions over Time",
    subtitle = "Comparison of Landline vs. Mobile Phone Subscriptions \n (per 100 people) by continent from 1990 to 2017",
    caption = "Data source: OurWorldInData.org \n TidyTuesday Project - week 46 \n Visualization: @Lacapary"
  ) +
  facet_wrap(~ continent, scales = "free") +
  scale_x_continuous(breaks = seq(from = 1990, to = 2017, by = 5)) +
  theme(
    text = element_text(family = "Montserrat"),
    strip.background = element_blank(),
    strip.text = element_textbox(
      size = 12,
      color = "#115C8AD3" ,
      fill = "#8FB7CFB3",
      box.color = "#607A8AB3",
      halign = 0.5,
      linetype = 1,
      r = unit(5, "pt"),
      width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0),
      margin = margin(3, 3, 3, 3)
    ),
    panel.grid = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.ticks.y = element_blank(),
    axis.text.x =  element_text(size = 6),
    axis.text.y =  element_text(size = 8),
    plot.title = element_text(size = 20, hjust = .5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.background = element_blank(),
    legend.key = element_blank(),
    aspect.ratio = 0.5
  )
```

![](README_figs/README-phone_line-1.png)<!-- -->
