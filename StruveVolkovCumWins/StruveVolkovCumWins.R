ufc <- read.csv("UFCMaster.csv")

strvol <- select(ufc, winner, date) %>%
  filter(winner == "Alexander Volkov" | winner == "Stefan Struve") %>%
  arrange(winner, date)

strvol$ufc.win = sequence(rle(as.character(strvol$winner))$lengths) 

ggplot(data = strvol, aes(x = date, y = ufc.win, color = winner)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(1, 12)) +
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 12)) +
  labs(title = "Cumulative UFC wins (Stefan Struve, Alexander Volkov)",
       subtitle = "Through August 31, 2017")
