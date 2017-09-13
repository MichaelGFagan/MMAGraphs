# Graphing distance traveled to UFC events

library(tidyverse)
library(ggmap)
library(geosphere)

UFCDist <- function(filename, city){
  distances <- read_csv(filename)
  
  city.coord <- geocode(city)

  coords <- geocode(distances$fights.out.of)
  distances$lon <- coords$lon
  distances$lat <- coords$lat

  dist <- distHaversine(as.matrix(coords), c(city.coord$lon, city.coord$lat))

  distances$dist <- dist
  distances$dist.mi <- distances$dist * 0.000621371


  ggplot(distances, aes(x = reorder(fighter, dist.mi), y = dist.mi, fill = dist.mi)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    coord_flip() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank()) +
    ylab("Distance (in miles)") +
    labs(title = paste0("'Fights out of' distance to ", city),
         subtitle = "Source: UFC.com profiles") +
    scale_fill_continuous(low = "blue", high = "red")
}