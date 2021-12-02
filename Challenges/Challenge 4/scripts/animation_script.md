```
# Animation ----

animation <- ggplot(data = uk_map) +    # create animation object
  map_uk +    # use UK map
  geom_point(data = microplastics_reduced,    # create geom points 
             aes(y = dec_lat_retrieve, x = dec_long_retrieve, 
                 size = sum_micro_m3,    # set point size according amount of plastic (mm/m3) 
                 fill = sum_micro_m3),    # set point fill colour according amount of plastic (mm/m3)
             alpha = 0.5,    # set transparency  
             shape = 21) +    # set shape 
  scale_fill_continuous(name = "Amount of plastic (mm/m3)", type = "viridis", guide = "legend") +    # set colourblind friendly colour palette  
  guides(color = guide_legend(), size = guide_legend()) +    # combine legend for geom point colour and size
  map_theme() +    # add map theme
  labs(x = "Longitude (Â°)",
       y = "Latitude (Â°)",
       size = 'Amount of plastic (mm/m3)',
       title = "Cummulative spatial distribution of plastics around Scotland in:",   # set labels 
       subtitle = "{current_frame}", 
       size.subtitle = 34) +
  theme(plot.subtitle = element_text(size = 36)) +
  theme(legend.position = c(0.16, 0.35), legend.title = element_text(size = 21)) +    # change legend position and size
  transition_manual(year, cumulative = TRUE)    # set animation transition according to year, set it to be cumulative (accumulate data points across the years)

animate(animation, duration = 7, fps = 20, width = 1000, height = 800, renderer = gifski_renderer())    # animate plot

# anim_save("outputs/animation.gif")    # save animation GIF 
```
