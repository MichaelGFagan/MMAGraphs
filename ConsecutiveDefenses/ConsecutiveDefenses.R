# Graphing top UFC consecutive title defenses

titlereigns <- read.csv("UFCTitleReigns.csv")
titlereigns$reign.start <- as.Date(titlereigns$reign.start, "%m/%d/%y")
titlereigns$reign.end   <- as.Date(titlereigns$reign.end  , "%m/%d/%y")

timeline(df = titlereigns, label.col = names(titlereigns)[3], 
         group.col = names(titlereigns)[1],
         start.col = names(titlereigns)[4], 
         end.col = names(titlereigns)[5],
         text.size = 6,
         text.color = "white") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(title = "UFC title reigns 9/22/12 to present",
       subtitle = "Key: @ - dos Santos, * - Jones, & - Holloway, ! - de Randamie, $ - Cyborg, ~ - Esparza")
