library(psychdata)
library(gapminder)
library(ggplot2)
library(gganimate)
library(tidyr)
library(stringr)
library(dplyr)
library(suddengains)

psychdata
gapminder

# this is example from github gganimate
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

# this is me trying to do some animation with  psychdata

# transform to long data
psychdata_long <- psychdata %>% 
  gather(key = measure_time, value = value, -c(id, group)) %>%
  separate(measure_time, into = c("measure", "time"), sep = "_") %>% 
  separate(time, into = c("time_point", "time_num"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(time_point = case_when(time_point == "s" ~ "session",
                                time_point == "fu" ~ "follow-up"),
         time_num = as.integer(time_num),
         group = factor(group),
         group = case_when(group == 0 ~ "Group 1",
                           group == 1 ~ "Group 2")) %>% 
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
  theme(text = element_text(size = 14)) +
  theme(legend.position = "top") +
  geom_hline(yintercept = 20, colour = "green", size = 1) + 
  annotate("text", min(psychdata_long$time_num), x = 1, y = 20, vjust = 2, label = "Clinical cut-off") +
  anim_save("psychdata_anim_01.gif")

  
sgdata_long <- sgdata %>% 
    gather(key = measure_time, value = value, -c(id)) %>%
    separate(measure_time, into = c("measure", "time"), sep = "_") %>% 
    separate(time, into = c("time_point", "time_num"), sep = "(?<=[A-Za-z])(?=[0-9])") %>%
    mutate(time_point = case_when(time_point == "s" ~ "session",
                                  time_point == "fu" ~ "follow-up"),
           time_num = as.integer(time_num)
           # group = factor(group),
           # group = case_when(group == 0 ~ "Group 1",
                             # group == 1 ~ "Group 2")
           ) %>% 
    filter(time_point == "session")


  
sgdata_long %>% 
    ggplot(aes(x = time_num,
               y = value)) +
    scale_color_viridis_d(option = "D") +
    stat_summary(fun.y = mean, geom = "line", aes(group = 1), position = position_dodge(width = 0.5), size = 1, alpha = 1) +
      transition_reveal(time_num) +
    stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .5) , size = 3, alpha = 1) +
      transition_reveal(time_num) +
    geom_point(aes(group = seq_along(time_num)), alpha  = 0.3, size = .8, position = position_dodge(width = 0.5)) +
      transition_reveal(time_num) +
    labs(x = "Session", y = "Measure", colour = NULL, shape = NULL) +
    theme_classic() +
    theme(text = element_text(size = 14)) +
    theme(legend.position = "top") +
    geom_hline(yintercept = 40, colour = "red1", size = 1) +
      annotate("text", x = 0, y = 40, vjust = -1, hjust = 0, colour = "red4", size = 5, label = "Extreme") +
      annotate("text", x = 0, y = 40, vjust = 1.5, hjust = 0, colour = "red1", size = 5, label = "Severe") +
    geom_hline(yintercept = 30, colour = "orangered1", size = 1) +
      annotate("text", x = 0, y = 30, vjust = 1.5, hjust = 0, colour = "orangered1", size = 5, label = "Moderate") +
    geom_hline(yintercept = 16, colour = "orange1", size = 1) +
      annotate("text", x = 0, y = 16, vjust = 1.5, hjust = 0, colour = "orange1", size = 5,  label = "Mild") +
    geom_hline(yintercept = 10, colour = "green", size = 1) +
      annotate("text", x = 0, y = 10, vjust = 1.5, hjust = 0, colour = "green", size = 5, label = "Clinical cut-off")
    anim_save("sgdata_anim_01.gif")
  