library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(countrycode)
library(ggtext)
library(glue)
library(here)
library(extrafont)



#import data
womeninpower <- read.csv("https://query.data.world/s/64twvxt24wxeybhy3uyxzgqyae54ua", header=TRUE, stringsAsFactors=FALSE)

#tidy data
womeninpower <- womeninpower %>%
  rename(country_name = Country.Name,
         country_code = Country.Code,
         year = Year,
         women_prop = Proportion.of.seats.held.by.women.in.national.parliaments....) %>%
  as_tibble() %>%
  na.omit(women_prop)

womeninpower$country_name <- as_factor(womeninpower$country_name)

#find top countries 1997-2019
topwoman <- 
  function(x) {
    top1 <- womeninpower %>%
      filter(year == x) %>%
      arrange(desc(women_prop)) %>%
      slice(1)
    return(top1)
  }

top1_19972019 <- mapply(
  topwoman,
  1997:2019,
  SIMPLIFY = F
) %>%
  bind_rows()


#find percent change, average percent, by year
percentchange <- womeninpower %>%
  group_by(year) %>%
  summarise(
    women_prop = mean(women_prop)
  ) %>%
  mutate(
    pct_diff = ((women_prop - lag(women_prop))/lag(women_prop))*100
  )

#get graphin' 
ggplot(womeninpower)+
# adding dotted lines
  geom_segment(aes(x = 0, xend = 0.6+0.7, y = year, yend = year), linetype = "dotted", size = 0.2)+
#adding labels for 'year'
  geom_richtext(data = womeninpower %>%
                  group_by(year) %>%
                  summarise(year = mean(year)) %>%
                  arrange(desc(year)),
                aes(x = 0, y = year, label = year), 
                label.color = NA, 
                label.padding = unit(0.1, "lines"), 
                family = "NYTFranklin Light", 
                size = 7)+
#best country for female rep
  geom_richtext(data = top1_19972019, aes(x = 0.1, y = year, label = country_name),
                label.color = NA, 
                label.padding = unit(0.1, "lines"), 
                family = "NYTFranklin Light", 
                size = 6)+
#av. proportion of women
  geom_richtext(data = percentchange, aes(x = 0.2, y = year, label = glue("{round(women_prop, 2)}%")), 
                label.color = NA, 
                label.padding = unit(0.1, "lines"), 
                family = "NYTFranklin Light", 
                size = 6)+
  geom_segment(data = percentchange, 
               aes(x = 0.24, xend = (women_prop/2.5) + 0.23, y = year, yend = year),
               size = 10, alpha = 0.5, col = "darkgrey")+
#av. change per year
  geom_richtext(data = percentchange, aes(x = 0.4, y = year, label = ifelse(is.na(pct_diff), "", glue("{round(pct_diff, 2)}%"))), 
                label.color = NA, 
                label.padding = unit(0.1, "lines"), 
                family = "NYTFranklin Light", 
                size = 6,
                col = "darkgreen")+
  geom_segment(data = percentchange, 
               aes(x = 0.44, xend = (pct_diff/100) + 0.43, y = year, yend = year),
               size = 10, alpha = 0.5, col = "darkgrey")+
#adding points for proportions
  geom_jitter(aes(x = (women_prop/1.5 + 0.6), y = year, col = women_prop), show.legend = F)+
#adding proportion labels
  annotate("text", x = c(0.6, 0.6+0.33, 0.6+0.66), y = 2019.5, label = c("0", "50%", "100%"), hjust = c(0, 0.5, 1), family = "NYTFranklin Light", size = 3)+
#adding a line for 50%
  annotate("segment", x = 0.6+0.33, xend = 0.6+0.33, y = 1997, yend = 2019, size = 0.3, alpha = 0.5)+
#adding labels
  annotate("text", x = c(0, 0.1, 0.2, 0.4, 0.6+0.33), y = 2020, label = toupper(c("year", "country with \nmost women", "average %", "% change", "distribution")), hjust = 0.5, family = "NYTFranklin Light", size = 3.5) +
#adding labels
  labs(
    title = toupper("proportion of women in national parliaments around the world 1997-2019"),
    caption = toupper("data: world bank; graphic: bee boileau")
  )+
#changing fill of points
  scale_colour_gradient(low = "red", high = "green", name = "proportion of women")+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.2, vjust = -2, size = 27, family = "NYTFranklin Light"),
    plot.caption = element_text(family = "NYTFranklin Light", hjust = 0.9, vjust = 20),
    aspect.ratio = 0.8
  ) 
ggsave("women-in-power.png", width = 14, height = 9, units = "in")

