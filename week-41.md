Week 41
================

Libraries and settings

``` r
library(tidyverse)
library(tidytuesdayR)
library(extrafont)
```

# Load the weekly Data

Dowload the weekly data and make available in the `tt` object.

``` r
data <- tidytuesdayR::tt_load("2020-10-06")
tournament<-data$tournament

tournament <- tournament %>% 
  mutate(
    tournament_level = case_when(
      tourney_finish == "1st" ~ 7,
      tourney_finish == "2nd" ~ 6,
      tourney_finish == "RSF" ~ 5,
      tourney_finish == "RF" ~ 4,
      tourney_finish == "NSF" ~ 3,
      tourney_finish == "N2nd" ~ 2,
      tourney_finish == "Champ" ~ 1
    ),
    seed_group = case_when(
      seed %in% c('1', '2', '3') ~ '1-3',
      seed %in% c('4', '5', '6') ~ '4-6',
      seed %in% c('7', '8', '9') ~ '7-9',
      seed %in% c('10', '11', '12') ~ '10-12',
      seed %in% c('13', '14', '15', '16') ~ '13-16'
    ),
    seed_group = factor(seed_group, 
                        levels = c('1-3','4-6','7-9','10-12','13-16'),
                        ordered = TRUE)
  )


rect_1 <- tibble(
  x1 = c(0.4),
  x2 = c(1.6),
  y1 = c(0, 4.5, 6.5, 8.5), 
  y2 = c(3.5, 5.5, 7.5, 9.5)
)
```

# Visualize

Using your processed dataset, create your unique visualization.

``` r
colors <- c('#a72167ff', '#1a2857ff', '#6ab2e2ff', 
            '#fdc52fff', '#17a85aff')

plot<-tournament %>% 
  filter(!is.na(seed_group )) %>% 
  ggplot() + 
  geom_rect(data = rect_1, aes(xmin = x1, xmax = x2,
                               ymin = y1, ymax = y2), 
            fill = "azure4") +
  geom_point(aes(x = 1, y = tournament_level + 2, color = seed_group), 
             position = position_jitter(width = 0.5, height = 0.25),
             size = 1) + 
  labs(title = "MARCH MADNESS",
       subtitle = "In the women's NCAA tournament, the top three \nseeded teams dominate the final rounds.",
       caption = "Source: FiveThirtyEight  \nTidyTuesday Project - week 41 \nVisualization: @Lacapary") + 
  scale_color_manual(name = "Seed", values = colors) + 
  #circular
  coord_polar(start = pi) +
  guides(color = guide_legend(ncol = 1)) + 
  theme_void() +
  theme(
    plot.title = element_text(size = 20, hjust = .4, face = "bold"),
    plot.subtitle = element_text(size = 11, hjust = .4),
    plot.margin = margin(0, 0, 0, 0)
    )
```

# Plot

![](README_figs/README-unnamed-chunk-3-1.png)<!-- -->
