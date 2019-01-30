library(psychdata)
library(gapminder)
library(ggplot2)
library(gganimate)
library(tidyr)
library(stringr)
library(dplyr)

psychdata
gapminder

plot <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
               geom_point(alpha = 0.7, show.legend = FALSE) +
               scale_colour_manual(values = country_colors) +
               scale_size(range = c(2, 12)) +
               scale_x_log10()


animation <- plot +
             # Here comes the gganimate specific bits
             labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
             transition_time(year) +
             ease_aes('linear') +
             # shadow_wake(wake_length = .5, alpha = FALSE) +
             shadow_mark(alpha = .3, size = .5)

animation

# psychdata

psychdata_long <- psychdata %>% 
  gather(key = measure_time, value = value, -c(id, group)) %>%
  separate(measure_time, into = c("measure", "time"), sep = "_") %>% 
  separate(time, into = c("time_point", "time_num"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(time_point = case_when(time_point == "s" ~ "session",
                                time_point == "fu" ~ "follow-up"),
         time_num = as.integer(time_num),
         group = factor(group)) %>% 
  filter(time_point == "session")


psychdata_long %>% 
  ggplot(aes(x = time_num,
             y = value,
             shape = group,
             colour = group)) +
  # Set group colours to colour-blind friendly colours
  scale_color_viridis_d(option = "D") +
  # Add point for mean, dodge so the groups dont overlap
  # Add lines between means, dodge so the groups dont overlap
  stat_summary(fun.y = mean, geom = "line", aes(group = group), position = position_dodge(width = 0.5), size = 1, alpha = 1) +
  transition_reveal(time_num) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .5) , size = 3, alpha = 1) +
  transition_reveal(time_num) +
  geom_point(aes(group = seq_along(time_num)), alpha  = 0.3, size = .8, position = position_dodge(width = 0.5)) +
  transition_reveal(time_num) +
  # Change name of labels on x axis
  labs(x = "Session", y = "Measure", colour = NULL, shape = NULL) +
  # Make graph look APAish
  theme_classic() +
  theme(text = element_text(size = 12)) +
  theme(legend.position = "top")
