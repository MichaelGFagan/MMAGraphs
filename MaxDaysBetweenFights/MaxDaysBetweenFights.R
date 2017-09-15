

melt.ufc <- select(ufc, date, winner, loser) %>%
  melt(id = "date") %>%
  rename(result = variable, fighter = value) %>%
  arrange(date)

fighters <- unique(melt.ufc$fighter)

MaxDaysBetweenFights <- function(name, df){
  df <- filter(df, fighter == name)
  max(as.numeric(diff(df$date)))
}

max.days <- as.vector(sapply(fighters, MaxDaysBetweenFights, melt.ufc))

fighter.max.days <- data.frame(fighters, max.days)
fighter.max.days <- filter(fighter.max.days, is.finite(max.days) == TRUE)
fighter.max.days <- arrange(fighter.max.days, desc(max.days))

# highlight david branch
branch.max.days <- head(fighter.max.days, n = 20)
branch.max.days$branch <- ifelse(branch.max.days$fighters == "David Branch", 1, 0)

ggplot(branch.max.days, aes(x = reorder(fighters, max.days), 
                            y = max.days, fill = branch)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_continuous(low = "darkblue", high = "red") +
  theme_bw() +
  xlab("Days") +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  labs(title = "Most time between UFC fights",
       subtitle = "Since UFC 28")
