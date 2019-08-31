library(tidyverse)
library(lubridate)
library(gganimate)
library(suddengains)

data <- sgdata %>% 
  gather(key = "variable", value = "value", -(id)) %>% 
  separate(variable, into = c("measure", "session")) %>% 
  mutate(session = str_extract(session, "\\d+"),
         session_chr = as.character(session),
         session_fct = factor(session, labels = 0:12)) %>% 
  filter(measure == "bdi")

# only go from year 2001 forward
g1 <- ggplot(data = data, aes(x = session_fct, y = value, color = value, group = id)) +
  geom_line(size = 1.05) +
  geom_point(size = 4, shape = 21, fill = "white", stroke = 2) +
  scale_color_viridis_c(option = "B", end = 0.85, name = "Symptom severity") +
  geom_text(hjust = 1, data = .%>% filter(session == 0), nudge_x = -0.25, aes(label = id), size = 5) +
  geom_text(hjust = 0, data = .%>% filter(session == 12), nudge_x = 0.25, aes(label = id), size = 5) +
  theme_gray(base_size = 18) +
  theme(legend.position = "top") +
  theme(plot.caption = element_text(hjust = 0,size = rel(0.5))) +
  labs(subtitle = "{closest_state}", y = "Symptoms", x = "Session")

g1

tickers <- c("MORTGAGE30US", 
             "HOUSTNSA",
             "HOUST1FNSA", 
             "HSN1FNSA",
             "COMPUTNSA")

names <- c("30-year Mortgage",
           "Housing Starts",
           "1-unit Housing Starts",
           "New Home Sales",
           "Housing Completions"
)

df_names <- data.frame(symbol=tickers,name=names)
month.abb

# load data ----
df <- tidyquant::tq_get(tickers,
                        get = "economic.data",
                        from = "1960-01-01") %>% 
  mutate(month = month(date), 
         monthf = factor(month, labels=month.abb),
         year = year(date),
         yearc = as.character(year)) %>%
  left_join(df_names,by = "symbol")


df_starts1 <- filter(df, symbol == "HOUST1FNSA")

# only go from year 2001 forward
ggplot(data = filter(df_starts1, year > 2000), aes(x = month, y = price, color = price)) +
             geom_line(size = 1.05) +
             geom_point(size = 4, shape = 21, fill = "white",stroke = 2) +
             scale_color_viridis_c(option = "B", end = 0.85, name = "Symptom severity") +
             geom_text(hjust = 1, data = .%>% filter(month == 1), nudge_x = -0.25, aes(label = yearc), size = 5) +
             geom_text(hjust = 0, data = .%>% filter(month == 12), nudge_x = 0.25, aes(label = yearc), size = 5) +
             scale_x_continuous(limits = c(0,13), breaks = 1:12, labels = month.abb)+
             theme_gray(base_size = 18) +
             theme(legend.position = "top") +
             theme(plot.caption = element_text(hjust=0,size = rel(0.5))) +
             labs(subtitle = "{closest_state}", y = "Symptoms", x = "Session"
                  # , title = "U.S. single-family housing starts by year",
                  # caption = "@lenkiefer Source: U.S. Bureau of the Census and U.S. Department of Housing and Urban Development,\nPrivately Owned Housing Starts: 1-Unit Structures [HOUST1FNSA],\nretrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/HOUST1FNSA, March 3, 2019"
                  )

anim <- g1 + 
  transition_states(session_fct) + 
  shadow_mark(alpha = 0.25)

animate(anim, end_pause = 25, nframes = 125, width = 1000, height = 800)

# anim_save(file = "PATH_TO_YOUR_DIRECTORY/starts1_line_anim_mar2019_v4.gif", 
          # animation = last_animation())
