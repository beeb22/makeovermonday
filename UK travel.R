library(RColorBrewer)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(patchwork)
library(httr)
library(readxl)
library(janitor)
library(zoo)

#import data

GET("https://query.data.world/s/yykg26ag7mbw5gqvy37mauy4xbh72u", write_disk(tf <- tempfile(fileext = ".xls")))
visits <- read_excel(tf)
visits <- visits %>%
  slice(-c(1:7)) %>%
  clean_names()
visits <- visits %>%
  select(
    year = title,
    journeys = uk_visits_abroad_all_visits_thousands_sa
  )

#clean data
visits_year <- subset(visits, str_length(year) == 4)
visits_quarter <- subset(visits, str_length(year) == 7)
visits_month <- subset(visits, str_length(year) > 7)
visits_month$journeys <- as.numeric(visits_month$journeys)
visits_quarter$journeys <- as.numeric(visits_quarter$journeys)
visits_year$journeys <- as.numeric(visits_year$journeys)
visits_year$year <- as.numeric(visits_year$year)
visits_quarter$year <- yq(visits_quarter$year)
visits_month$year <- ymd(visits_month$year, truncated = 1)

visits_quarter <- visits_quarter %>%
  mutate(
    change = journeys - lag(journeys),
    change_percent = (journeys - lag(journeys))/journeys
  )
visits_month <- visits_month %>%
  mutate(
    change = journeys - lag(journeys),
    change_percent = (journeys - lag(journeys))/journeys
  )
visits_year <- visits_year %>%
  mutate(
    change = journeys - lag(journeys),
    change_percent = (journeys - lag(journeys))/journeys
  )

#create tb to bind
tb <- tibble(year = date("2020-01-01"), journeys = NA, change = NA, change_percent = NA)

#find percent change from Jan-Mar 2019 to Jan-Mar 2020
dates_2019 <- visits_month[visits_month$year %in% c(as.Date("2019-01-01"):as.Date("2019-03-01")), ]
dates_2020 <- visits_month[visits_month$year %in% c(as.Date("2020-01-01"):as.Date("2020-03-01")), ]
tb$change <- sum(dates_2020$journeys) - sum(dates_2019$journeys)
tb$change_percent <- (sum(dates_2020$journeys) - sum(dates_2019$journeys))/sum(dates_2019$journeys)
visits_year <- rbind(visits_year, tb)

visits_year$year <- as.Date(paste(visits_year$year, 1, 1, sep = "-"))
visits_year

p_month <- 
  visits_month %>%
  slice(-c(1:12)) %>%
  ggplot()+
  geom_line(aes(year, journeys, col = journeys), show.legend = F)+
  scale_color_continuous(low = "#A1D99B", high = "#006D2C")+
  geom_rect(aes(xmin = as.Date(as.yearqtr("1990 Q3")), xmax = as.Date(as.yearqtr("1991 Q3")),
                ymin = -Inf, ymax = Inf),
            alpha = 0.01, fill = "gray83")+
  geom_rect(aes(xmin = as.Date(as.yearqtr("2008 Q2")), xmax = as.Date(as.yearqtr("2009 Q2")),
                ymin = -Inf, ymax = Inf),
            alpha = 0.01, fill = "gray83")+
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2020-03-01"), 
                ymin = -Inf, ymax = Inf),
            alpha = 0.01, fill = "gray83")+
  scale_y_continuous(labels = scales::comma)+
  labs(
    subtitle = "UK visits abroad each quarter",
    y = "visits",
    x = "year"
  )+
  theme_minimal()+
  theme(
    plot.subtitle = element_text(family = "NYTFranklin Light", vjust = 5, size = rel(0.05), margin = margin(20, 0,0,0)),
    plot.margin = margin(20,20,20,20),
    axis.text = element_text(family = "NYTFranklin Light"),
    axis.title = element_text(family = "NYTFranklin Light")
  )

p_yearchange <- 
  ggplot(visits_year)+
  geom_col(aes(year, change_percent), fill = "#006D2C", alpha = 0.5, width = 100,
           show.legend = F)+
  geom_rect(aes(xmin = as.Date(as.yearqtr("1990 Q3")), xmax = as.Date(as.yearqtr("1991 Q3")),
                ymin = -Inf, ymax = Inf),
            alpha = 0.01, fill = "gray83")+
  geom_rect(aes(xmin = as.Date(as.yearqtr("2008 Q2")), xmax = as.Date(as.yearqtr("2009 Q2")),
                ymin = -Inf, ymax = Inf),
            alpha = 0.01, fill = "gray83")+
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2020-03-01"), 
                ymin = -Inf, ymax = Inf),
            alpha = 0.01, fill = "gray83")+
  labs(
    subtitle = "Year-on-year change in UK visits abroad",
    y = "visits (%)",
    x = "year",
    caption = "source: ONS\nauthor: @beeboileau\nnote: change from 2019-2020 based on first three months"
  )+
  theme_minimal()+
  theme(
    plot.subtitle = element_text(family = "NYTFranklin Light"),
    plot.caption = element_text(family = "NYTFranklin Light"),
    plot.margin = margin(20,20,20,20),
    axis.text = element_text(family = "NYTFranklin Light"),
    axis.title = element_text(family = "NYTFranklin Light")
  )

#use patchwork to plot together!
UK_travel <- p_month/p_yearchange + 
  plot_annotation(
  title = "UK visits abroad",
  subtitle = "UK recessions are shaded grey: they map closely onto temporary drops in visits abroad"
) &
  theme(
    plot.title = element_text(family = "NYTFranklin Light", size = rel(3), margin = margin(20,0,10,0)),
    plot.subtitle = element_text(family = "NYTFranklin Light", size = rel(1.5))
  )
UK_travel
ggsave("UK_travel.png", width = 20, height = 12)

