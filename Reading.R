library("httr")
library("readxl")
library(janitor)
library(tidyverse)
library(lubridate)
library(geofacet)
library(ggtext)

GET("https://query.data.world/s/hyolmkmzururu54acof2v5yijykn3i", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf, skip = 1) %>%
  clean_names()

reading <- 
  df %>%
  separate(year_month,
           into = c("year", "month"),
           sep = "M") %>%
  mutate(
   date = str_c(year, month, "01", sep = "-"),
   date = ymd(date),
   country = factor(country),
   country = fct_recode(country,
                        "EU" = "EU27_2020 - European Union - 27 countries (from 2020)",
                        "Czech Republic" = "Czechia")
  ) %>%
  select(category, country, index_2015_100, date)

library(RColorBrewer)

scales::show_col(brewer.pal(2, "Dark2"))

reading %>%
  pivot_wider(names_from = category, 
              values_from = index_2015_100) %>%
  clean_names() %>%
  mutate(
    real_books = books/all_items_hicp,
    real_books = real_books - 1
  ) %>%
  filter(date == as.Date("2015-04-01") | date == as.Date("2020-08-01")) %>%
  select(country, real_books, date) %>%
  pivot_wider(names_from = date, values_from = real_books) %>%
  mutate(change = (`2020-08-01` - `2015-04-01`)*100) %>%
  ggplot()+
  geom_segment(aes(x = 0, xend = 1, y = `2015-04-01`, yend = `2020-08-01`, colour = change > 0), show.legend = F, size = 1, arrow = arrow(length = unit(0.2, "cm")))+
  geom_text(aes(x = 1.2, y = `2020-08-01` - 0.3, label = paste0(round(change, 1), "%"), colour = change > 0),
            show.legend = F, family = "IBM Plex Mono Light", size = 4)+
  scale_color_manual(values = c("#1B9E77", "#7570B3"))+
  labs(
    title = "**Have real book prices in Europe <b style = 'color:#7570B3'>increased</b> or <b style = 'color:#1B9E77'>decreased</b> since 2015?**",
    subtitle = "There seems to be no consistent pattern across countries",
    caption = "data: Eurostat | visualisation: @beeboileau"
  )+
  theme_void(base_family = "IBM Plex Mono Light", 
             base_size = 17)+
  theme(
    plot.title = element_markdown(margin = margin(10,0,10,0), family = "Helvetica"),
    plot.subtitle = element_text(margin = margin(0,0,30,0), family = "Helvetica"),
    plot.caption = element_text(family = "Helvetica"),
    plot.margin = margin(20,20,20,20),
    plot.background = element_rect(fill = "#F0EFEB",
                                   color = "#F0EFEB"),
    panel.border = element_rect(colour = alpha("black", 0.2), fill = alpha("white", 0))
  )+
  facet_geo(~country, grid = "eu_grid1", label = "code")+
  coord_cartesian(clip = "off", expand = F, xlim = c(-1, 2), ylim = c(-1.1, 1))

ggsave("books.png", height = 10, width = 14)
