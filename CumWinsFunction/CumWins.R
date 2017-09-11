CumWins <- function(fighter1, fighter2){

  wins <- select(ufc, winner, date) %>%
    filter(winner == fighter1 | winner == fighter2) %>%
    arrange(winner, date)

  wins$ufc.win = sequence(rle(as.character(wins$winner))$lengths)

  ggplot(data = wins, aes(x = date, y = ufc.win, color = winner)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = seq(1, max(wins$ufc.win))) +
    scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y")) +
    theme_bw() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position="bottom",
          plot.title = element_text(size = 12),
          panel.grid.minor = element_blank()) +
    labs(title = paste0("Cumulative UFC wins (", fighter1, ", ", fighter2, ")"),
         subtitle = paste0("Through ", format(Sys.Date(), format = "%B %d, %Y")))
}