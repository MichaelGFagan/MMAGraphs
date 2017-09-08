struve <- read_csv("struve.csv")
struve$result <- factor(struve$result)

ggplot(struve, aes(x= index, y = reach.dif, fill = result)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 18)) +
  scale_y_continuous(breaks = seq(0, 12)) +
  ylab("Reach difference") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom") +
  labs(title = "Stefan Struve UFC career reach advantage")
