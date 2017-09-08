library(tidyverse)
library(scales)

johbor <- select(ufc, winner, date) %>%
  filter(winner == "Demetrious Johnson" | winner == "Ray Borg") %>%
  arrange(winner, date)

johbor$ufc.win = sequence(rle(as.character(johbor$winner))$lengths) 

ggplot(data = johbor, aes(x = date, y = ufc.win, color = winner)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(1, 15)) +
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Cumulative UFC wins (Demetrious Johnosn, Ray Borg)",
       subtitle = "Through September 6, 2017")
