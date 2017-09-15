penguins <- read.csv("Penguins.csv")

penguins$year <- factor(penguins$year)

ggplot(penguins, aes(x = year, y = points, fill = stanley.cup)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_continuous(low = "black", high = "gold") +
  ylab("Points") +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  labs(title = "Pittsburgh Penguins points in PPG Paints Arena era",
       subtitle = "Gold: Stanley Cup win")
