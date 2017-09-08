# Graphing Demetrious Johnson's UFC title reign vs. other UFC champs during
# same period

title.defense <- read.csv("UFCTitleDefense.csv")

title.defense$fighter <- factor(title.defense$fighter, 
                                levels = title.defense$fighter[order(title.defense$streak)])

ggplot(title.defense, aes(x = fighter, y = streak)) +
  geom_bar(stat = "identity", fill = "gold3") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 10)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank()) +
  labs(title = "Most consecutive UFC/WEC/Strikeforce title defenses")
