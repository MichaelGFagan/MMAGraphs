# Graphing distance traveled to UFC Rotterdam

rotterdam <- read_csv("RotterdamDist.csv")

coords <- geocode(rotterdam$fights.out.of)
rotterdam$lon <- coords$lon
rotterdam$lat <- coords$lat

rotterdam$dist <- distHaversine(c(rotterdam$lon, rotterdam$lat), c(4.4777, 51.9244))

dist <- distHaversine(as.matrix(coords), c(4.4777, 51.9244))

rotterdam$dist <- dist
rotterdam$dist.mi <- rotterdam$dist * 0.000621371


ggplot(rotterdam, aes(x = reorder(fighter, dist.mi), y = dist.mi, fill = dist.mi)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank()) +
  ylab("Distance (in miles)") +
  labs(title = "'Fights out of' distance to Rotterdam",
       subtitle = "Source: UFC.com profiles") +
  scale_fill_continuous(low = "blue", high = "red")
