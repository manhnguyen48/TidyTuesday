library(tidyverse)
library(hrbrthemes)
library(ggrepel)
library(glue)


# Reading in data --------------------------------------------------------


gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')




# Exploratory -------------------------------------------------------------

#Most violated articles

gdpr_violations %>%
  count(article_violated, sort = T) %>%
  mutate(proportion = n/ sum(n))


gdpr_violations %>%
  group_by(controller) %>%
  summarise_at(vars(price), lst(mean, median, sum, max)) %>%
  arrange(-sum) %>%

# Plots -------------------------------------------------------------------


#Which country has the most fines

gdpr_country <- gdpr_violations %>%
  filter(!(name %in% c("Croatia", "Lithuania", "Malta")))  %>%
  filter(price != 0) %>%
  mutate(name = fct_reorder(as_factor(name), price, .fun = "median"),
         details = glue("{controller}\n€{scales::comma(price)}"))

gdpr_country %>%
  ggplot(aes(x = name, y = price)) +
  geom_boxplot(coef = 0, outlier.shape = NA, fill = "steelblue", colour = "white") +
  geom_point(data = filter(gdpr_country, price >= 5e6),
             colour = "red") +
  geom_text_repel(data = filter(gdpr_country, price >= 5e6),
             aes(label = details),
             colour = "white") +
  geom_hline(yintercept = 12950, colour = "red") +
  annotate("text", x = "Netherlands", y = 7000,
           label = "Median fine\n€12,950",
           colour = "white") +
  coord_flip() +
  scale_y_log10(labels = scales::comma) +
  labs(title = "How EU countries are handing out GDPR fines in 2019",
       caption = "Source: privacyaffairs.com/gdpr-fines/\nGraphics by Ethan Nguyen @bananadata48",
       subtitle = "In 2019, EU authorities collectively fined 250 cases with total of over 150 million euros\nBoxplots show marks at 25 percentile, median, and 75 percentile of fines",
       x = "",
       y = "Price of fine in Euros (log scale)") +
  hrbrthemes::theme_modern_rc(axis_text_size = 13,
                              axis_title_size = 13,
                              plot_title_size = 20)




ggsave("./Fines by country.jpeg", width = 12, height = 14, dpi =300)

