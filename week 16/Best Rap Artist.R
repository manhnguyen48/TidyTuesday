
library(tidyverse)
library(hrbrthemes)


setwd("D:/Projects/Tidy Tuesday/wc 140420")



# Reading in data ---------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2020, week = 16)



polls <- tuesdata$polls


rankings <- tuesdata$rankings %>%
  arrange(year) %>%
  mutate(track = as.character(glue::glue("{artist}\n{title}")),
         to_annotate = case_when(points > 50 ~ T,
                                 title %in% c("U.N.I.T.Y.",
                                              "Doo Wop (That Thing)") ~ T,
                                 T ~ F))



# Making plots ------------------------------------------------------------


rankings %>%
  ggplot(aes(x = year, y = points, color = gender, label = track)) +
  geom_hline(yintercept = 32, colour = "white") +
  annotate("text", x = 2017, y = 35,
           label = "95% of songs under this line",
           colour = "white") +
  geom_point(position = "jitter", size = 3, alpha = 0.8) +
  geom_text(data = filter(rankings, to_annotate),
            nudge_y = -5,
            show.legend = F, size = 3) +
  scale_color_manual(values = list("male" = "#0571b0",
                                   "female" = "#ca0020",
                                   "mixed" = "white")) +
  scale_y_continuous(breaks = seq(0, 175, 25)) +
  scale_x_continuous(breaks = seq(1979, 2019, 5)) +
  labs(x = "Year", y = "Critics points",
       title = "The early 90s produced the best rap songs according to critics",
       caption = "Critic rating: Each critic voted for five songs, ranking them from one (favourite) to five (5th favourite).\nBBC Music awarded 10 points for first ranked track, eight points for second ranked track, and so on down to two points for fifth place. The song with the most points won.",
       color = "Artist's gender") +
  hrbrthemes::theme_modern_rc(axis_text_size = 11,
                              axis_title_size = 12,
                              axis_title_face = "bold",
                              caption_size = 11,
                              plot_title_size = 20) +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.ticks = element_blank())


ggsave(filename = "./Output.png", width = 12.8, height= 8, dpi = 300)

