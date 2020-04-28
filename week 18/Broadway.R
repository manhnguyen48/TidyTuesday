library(tidyverse)
library(hrbrthemes)


options(tibble.print_max = 50, tibble.print_min = 30)


# Reading in data ---------------------------------------------------------

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')



# Processing data ---------------------------------------------------------

cpi_data <- cpi %>%
  mutate(year_month = glue::glue("{lubridate::year(year_month)}-{lubridate::month(year_month)}"))

adjusted_data <- grosses %>%
  #Adjust by inflation
  mutate(year_month = glue::glue("{lubridate::year(week_ending)}-{lubridate::month(week_ending)}")) %>%
  left_join(cpi_data, by = "year_month") %>%
  mutate_at(vars(weekly_gross, weekly_gross_overall, avg_ticket_price),
            ~(.x / cpi)*100)




# Plotting ----------------------------------------------------------------

#Total gross over time

total_gross <- adjusted_data %>%
  select(week_ending, weekly_gross_overall) %>%
  unique() %>%
  ggplot(aes(x = week_ending, y = weekly_gross_overall)) +
  geom_point(color = "grey", alpha = 0.3) +
  geom_smooth(method = "gam", color = "red") +
  stat_decomp(frequency = 52, type = "multiplicative",
              color = "steelblue") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks= "4 years",
               labels = scales::date_format(format = "%Y")) +
  labs(title = "Total Broadway weekly grosses have increased over time",
       subtitle = "Raw data is adjusted for CPI (grey points), then seasonally adjusted (blue lines), then fitted general additive model trends (red line) ",
       caption = "Data source: Playbill. Graphics by Ethan Nguyen @bananadata48\nAdjusted for CPI denominated by 1983. All items less food and energy in U.S. city average, all urban consumers, seasonally adjusted.",
       x = "",
       y = "USD") +
  hrbrthemes::theme_modern_rc(plot_title_size = 30, axis_text_size = 12, caption_size = 13)
total_gross

ggsave(total_gross, filename = "Total gross.jpeg", width = 18, height = 12)



#Percentage change

change_data <- adjusted_data %>%
  group_by(week_ending) %>%
  summarise(`Total grosses` = sum(weekly_gross),
            `Seat sales` = sum(seats_sold),
            `Median price` = median(avg_ticket_price, na.rm = T)) %>%
  arrange(week_ending) %>%
  mutate_if(is.numeric, ~100*(.x / .x[1L])) %>%
  pivot_longer(cols = -week_ending, names_to = "types") %>%
  ggplot(aes(x = week_ending, y = value, color = types)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  scale_y_continuous(n.breaks = 10) +
  scale_x_date(date_breaks= "5 years",
               labels = scales::date_format(format = "%Y")) +
  scale_colour_manual(values = list("Total grosses" = "#66c2a5",
                                    "Seat sales" = "#fc8d62",
                                    "Median price" = "#8da0cb")) +
  labs(title = "Weekly gross growth seems to be driven by both increases in price and sales",
       subtitle = "Although more by sales than price",
       caption = "Data source: Playbill. Graphics by Ethan Nguyen @bananadata48\nData is normalised so that first week is 100",
       x = "",
       y = "",
       colour = "") +
  hrbrthemes::theme_modern_rc(plot_title_size = 30, axis_text_size = 12, caption_size = 13, base_size = 12) +
  theme(legend.position = "top", legend.justification = "left")
change_data

ggsave(change_data, filename =  "Drivers of growth.jpeg", width = 18, height= 12)


#Quality of shows

performances <- adjusted_data %>%
  group_by(week_ending) %>%
  summarise(`Median utilisation` = median(pct_capacity, na.rm = T),
            `Performances` = sum(performances)) %>%
  mutate(`Median weekly utilisation (logit)` = log(`Median utilisation`  / ( 1 - `Median utilisation`  )) ) %>%
  select( - `Median utilisation`) %>%
  arrange(week_ending) %>%
  mutate_if(is.numeric, ~100*(.x / .x[1L])) %>%
  pivot_longer(cols = -week_ending, names_to = "types") %>%
  ggplot(aes(x = week_ending, y = value, colour = types)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(n.breaks = 10) +
  scale_x_date(date_breaks= "5 years",
               labels = scales::date_format(format = "%Y")) +
  labs(title = "Sales seems to be driven more by utilisation rather than number of shows",
       subtitle = "Higher utilisation might be attributed to better quality shows and/or better marketing",
       caption = "Data source: Playbill. Graphics by Ethan Nguyen @bananadata48\nUtilisation is transformed using log odds. Data is normalised so that first week is 100",
       x = "",
       y = "",
       colour = "") +
  hrbrthemes::theme_modern_rc(plot_title_size = 30, axis_text_size = 12, caption_size = 13, base_size = 12) +
  theme(legend.position = "top", legend.justification = "left")

performances

ggsave(performances, filename = "Drivers of sales.jpeg", width = 18, height = 12)



top_shows <- adjusted_data %>%
  group_by(show) %>%
  summarise(total_revenue = sum(weekly_gross)) %>%
  arrange(-total_revenue) %>%
  head(20) %>%
  mutate(show = fct_reorder(as_factor(show), total_revenue)) %>%
  ggplot(aes(x = show, y = total_revenue)) +
  geom_bar(stat="identity") +
  coord_flip(expand = F) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 8e+08), n.breaks = 8) +
  labs(title = "Top shows of all time by grossing",
       x = "", y = "USD",
       caption = "Data source: Playbill. Graphics by Ethan Nguyen @bananadata48") +
  hrbrthemes::theme_modern_rc(plot_title_size = 30, axis_text_size = 12, caption_size = 13)

ggsave(top_shows, filename = "Top shows.jpeg", width = 15, height = 15)


