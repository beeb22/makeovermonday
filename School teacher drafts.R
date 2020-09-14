library(tidyverse)
library(scales)

teacherpay <- read.csv("https://query.data.world/s/igjaw6zicuiprbldnqx6nrrl64kudx", header=TRUE, stringsAsFactors=FALSE)

teacherpay_clean <-
  teacherpay %>%
  select(
    time_period,
    gender,
    grade,
    average_mean
  ) %>%
  mutate(average_mean = as.numeric(average_mean)) %>%
  filter(gender != "Unclassified",
         gender != "Total",
         grade != "Total") %>%
  as_tibble() %>%
  mutate(grade = factor(grade, levels = c("Head teachers",
                                          "Other Leadership teachers",
                                          "Classroom teachers")))

teacherpay_clean$time_period <- gsub("(\\d{4})(\\d{2})$","\\1-\\2",teacherpay_clean$time_period)

ggplot(data = teacherpay_clean,
       aes(x = time_period,
             y = average_mean,
             col = gender,
             group = gender))+
  geom_line(show.legend = F)+
  geom_point(show.legend = F,
             alpha = 0.5)+
  geom_label(data = subset(teacherpay_linegraph, 
                                 time_period == "2010-11"),
                   aes(label = gender),
                   label.size = 0,
                   fill = alpha("white", 0.2),
             show.legend = F,
             nudge_y = c(500,2200,1000,
                         500,2200,1000),
             hjust = 0,
             family = "Playfair Display",
             size = rel(4))+
  scale_color_brewer(palette = "Set1")+
  labs(
    title = "The gender pay gap remains very real in teaching",
    subtitle = "Average pay for English state-school teachers, 2011-2020",
    caption = "data: School Workforce Census | visualisation: @beeboileau"
  )+
  scale_y_continuous(labels = dollar_format(prefix = "£"))+
  theme_minimal(base_family = "IBM Plex Mono Light",
                base_size = 20)+
  facet_wrap(~grade, ncol = 1, scales = "free_y")+
  theme(
    plot.title = element_text(family = "Playfair Display"),
    plot.subtitle = element_text(family = "Playfair Display"),
    plot.caption = element_text(family = "Playfair Display",
                                size = rel(0.5)),
    axis.title = element_blank(),
    axis.text = element_text(size = rel(0.5)),
    strip.text = element_text(hjust = 0, family = "Playfair Display"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line(linetype = "dotted"),
    panel.spacing = unit(2, "lines")
  )


ggsave("teacherpay.png", height = 10, width = 10)
