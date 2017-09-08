# First 20-something graphs at @MMAGraphs on Twitter. Not organized well!

ufc <- read.csv("UFCMaster.csv")
unique(ufc$Event, ufc$Date)

UFCProcess <- function(df){
  require(lubridate)
  df$fight.id <- as.character(df$fight.id)
  df$round <- as.factor(df$round)
  df$time <- as.character(df$time)
  df$weight.class.id <- as.factor(df$weight.class.id)
  df$date <- as.Date(df$date, "%m/%d/%y")
  df$year <- year(df$date)
  df$month <- month(df$date)
  df$day <- day(df$date)
  df$minute <- as.integer(substr(df$time, 1, 1))
  df$second <- as.integer(substr(df$time, 3, 4))
  df$round.seconds <- df$minute * 60 + df$second
  df$fight.seconds <- (as.integer(df$round) - 1) * 300 + df$round.seconds
  df$title.fight <- as.factor(df$title.fight)
  df$five.rounds <- as.factor(df$five.rounds)
  df$gender <- as.factor(ifelse(df$weight.class.id == 24 | 
                                df$weight.class.id == 25 |
                                df$weight.class.id == 32 | 
                                df$weight.class.id == 88, "Female", "Male"))
  return(df)
}

ufc <- UFCProcess(ufc)

vector <- rep(0, nrow(ufc))
events <- unique(ufc$Event)
events

FightsOnCard <- function(event){
  vector <- as.numeric(ufc$Event == event)
  return(sum(vector))
}

events.fights <- sapply(events, FUN = FightsOnCard)
events.fights

events.fights.total <- sapply(events.fights, FUN = seq)
ufc$matchOrder <- unlist(events.fights.total, recursive = TRUE, use.names = TRUE)
event.dates <- ufc[c("event", "date")]
event.dates <- unique(event.dates)
event.dates[duplicated(event.dates$date), ]

ufc$date <- as.Date(ufc$date, "%m/%d/%y")
ufc$year <- year(ufc$date)
ufc$month <- month(ufc$date)
ufc$day <- day(ufc$date)

months <- ifelse(ufc$month > 9, as.character(ufc$month), paste0(0, ufc$month))
days <- ifelse(ufc$day > 9, as.character(ufc$day), paste0(0, ufc$day))
fight.order <- ifelse(ufc$fight.order > 9, as.character(ufc$fight.order), paste0(0, ufc$fight.order))
ufc.fight.id <- paste0("UFC", ufc$year, months, days, fight.order)
ufc$fight.id <- ufc.fight.id
write.csv(ufc, file = "UFCtemp.csv")
ufc <- read.csv("UFCtemp.csv")

ufc$minute <- as.integer(substr(ufc$time, 1, 1))
ufc$second <- as.integer(substr(ufc$time, 3, 4))
ufc$round.seconds <- ufc$minute * 60 + ufc$second
ufc$fight.seconds <- (ufc$round - 1) * 300 + ufc$round.seconds
ufc$weight.class.id <- as.factor(ufc$weight.class.id)

ufc %>%
  filter(., as.integer(weight.class.id) < 9) %>%
  group_by(weight.class.id) %>%
  summarize(
    average.time = mean(fight.seconds)
  ) %>%
  ggplot(., aes(weight.class.id, average.time, fill = weight.class.id)) +
  ylab("Average Fight Time (Seconds)") +
  scale_x_discrete(labels=c("1" = "Heavyweight", "2" = "Light Heavyweight", "3" = "Middleweight",
                            "4" = "Middleweight", "5" = "Lightweight", "6" = "Featherweight",
                            "7" = "Bantamweight", "8" = "Flyweight")) +
  coord_flip() +
  geom_col() +
  theme(legend.position = "none", axis.title.y = element_blank())

  

ufc$title.fight <- 0
ufc$five.rounds <- 0

filter(ufc, weight.class.id == 14)

ufc$title.fight[ufc$winner == "Brock Lesnar" & ufc$event == "UFC 100"] <- 1
ufc <- read.csv("UFCtemp.csv")

ufc <- read.csv("MasterUFCJuly.csv")


# percentage of male/female fights over time
ufc %>%
  filter(year > 2011) %>%
  group_by(year) %>%
  summarize(
    n = length(year),
    male = sum(gender == "Male"),
    male_pct = male / n,
    female_pct = 1 - male_pct
  ) %>%
  ggplot(.) +
    geom_line(aes(x = year, y = male_pct), color = "blue") +
    geom_line(aes(x = year, y = female_pct), color = "orange")

# graph of ufc fights by gender frequency
ufc %>%
  filter(year > 2011) %>%
  ggplot(aes(x = factor(year), fill = gender)) +
    geom_bar(position = "fill") +
    ylab("Frequency") +
    xlab("Year") +
    labs(title = "Frequency of UFC fights by gender", 
         subtitle = "Thru July 11, 2007") +
    theme(legend.title = element_blank()) +
    theme_minimal()

# number of headlining bouts per weight class
ufc %>%
  filter(fight.order == 1, year > 2012) %>%
  ggplot(aes(x = weight.class.id, fill = gender)) +
    geom_bar() +
    theme_minimal() + 
    scale_x_discrete(labels=c("1" = "HW", "2" = "LHW", "3" = "MW", "4" = "WW",
                              "5" = "LW", "6" = "FW", "7" = "BW", "8" = "FL",
                              "24" = "FW", "25" = "BW", "32" = "SW",
                              "88" = "CW", "11" = "CW")) +
    theme(axis.text.x = element_text(angle=45),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.title = element_blank()) +
    labs(title = "Number of headlining bouts per weight class (UFC)", 
         subtitle = "2013 - July 12, 2017")


# graph of a weight class' percentage of headlining bouts in the ufc since 2013
ufc %>%
  filter(year > 2012, (as.integer(weight.class.id) < 9 |
                       as.integer(weight.class.id) == 13 |
                       as.integer(weight.class.id) == 15)) %>%
  group_by(weight.class.id, gender) %>%
  summarize(
    n = length(weight.class.id),
    head = sum(fight.order == 1),
    pct.head = head / n
  ) %>%
  ggplot(aes(x = weight.class.id, y = pct.head, fill = gender)) +
    geom_col() +
    theme_minimal() + 
    scale_x_discrete(labels=c("1" = "HW", "2" = "LHW", "3" = "MW", "4" = "WW",
                              "5" = "LW", "6" = "FW", "7" = "BW", "8" = "FL",
                              "24" = "FW", "25" = "BW", "32" = "SW",
                              "88" = "CW", "11" = "CW")) +
    theme(axis.text.x = element_text(angle=45),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.title = element_blank()) +
    labs(title = "Percentage of weight class bouts that have headlined UFC event", 
         subtitle = "2013 - July 13, 2017")

NumberOfFighters <- function(start.year = 1990, end.year = 9999, df = ufc){
  xdf = filter(df, year > start.year & year < end.year)
  winner <- xdf %>%
    select(fighter = winner, year = year) %>%
    group_by(fighter, year) %>%
    summarize(
      wins = n(),
      losses = 0
    )
  loser <- xdf %>%
    select(fighter = loser, year = year) %>%
    group_by(fighter, year) %>%
    summarize(
      wins = 0,
      losses = n()
    )
  total <- full_join(winner, loser, by = c("fighter", "year"))
  total[is.na(total)] <- 0
  total <- select(total, fighter, year, wins = wins.x, losses = losses.y)
  total$fights <- total$wins + total$losses
  total <- arrange(total, fighter)
  total
}

test <- NumberOfFighters()

test %>% filter(year > 2000 & year < 2017) %>%
  group_by(year) %>%
  summarize(
    fighters = n()
  ) %>%
  ggplot(aes(x = factor(year), y = fighters)) +
    geom_col() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45),
          axis.title.x = element_blank()) +
    labs(title = "Number of fighters to appear in the UFC",
         y = "Fighters")

unique(factor(ufc$method))
    
# Finish rate in UFC year-by-year  
filter(ufc, method != "DQ" & method != "No Contest", year > 2000) %>%
  mutate(finish = ifelse(method == "KO/TKO" | method == "Submission" |
                         method == "TKO - Doctor's Stoppage", 1, 0)) %>%
  group_by(year) %>%
  summarise(
    fights = n(),
    finishes = sum(finish),
    finish.rate = finishes / fights
  ) %>%
  ggplot(aes(x = factor(year), y = finish.rate, group = 1)) +
    geom_line() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust = -0.00005),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(6.5, 5.5, 10.5, 5.5), "points")) +
    labs(title = "UFC stoppage rate",
         subtitle = "Through July 17, 2017") +
   scale_y_continuous(limits = c(0, 1), labels = scales::percent)

# Percentage difference in UFC finish rate year by year
filter(ufc, method != "DQ" & method != "No Contest", year > 2000) %>%
  mutate(finish = ifelse(method == "KO/TKO" | method == "Submission" |
                         method == "TKO - Doctor's Stoppage", 1, 0)) %>%
  group_by(year) %>%
  summarise(
    fights = n(),
    finishes = sum(finish),
    finish.rate = finishes / fights
  ) %>%
  mutate(rate.diff = finish.rate / lag(finish.rate, 1) - 1) %>%
  na.omit() %>%
  ggplot(aes(x = factor(year), y = rate.diff, group = 1)) +
    geom_line() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust = -0.00005),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(6.5, 5.5, 10.5, 5.5), "points")) +
    labs(title = "UFC stoppage rate, % difference relative to prior year",
         subtitle = "Through July 17, 2017") +
    scale_y_continuous(labels = scales::percent)
      
# UFC finishing rates by weight class
filter(ufc, method != "DQ" & method != "No Contest", year > 2000) %>%
  mutate(finish = ifelse(method == "KO/TKO" | method == "Submission" |
                           method == "TKO - Doctor's Stoppage", 1, 0),
         weight.group = ifelse(as.integer(weight.class.id) %in% 1:4, 1, 
                               ifelse(as.integer(weight.class.id) %in% 5:8, 2,
                                      ifelse(as.integer(weight.class.id) %in% 12:15, 3,
                                             0)))) %>%
  filter(weight.group != 0) %>%
  group_by(year, weight.group) %>%
  summarise(
    fights = n(),
    finishes = sum(finish),
    finish.rate = finishes / fights
  ) %>%
  ggplot(aes(x = factor(year), y = finish.rate, group = factor(weight.group),
             color = factor(weight.group))) +
  geom_line() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust = -0.00005),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(6.5, 5.5, 10.5, 5.5), "points")) +
  labs(title = "UFC stoppage rate",
       subtitle = "Through July 17, 2017") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

filter(ufc, method != "DQ" & method != "No Contest", year > 2000,
       weight.class.id != 11, weight.class.id != 22,
       weight.class.id != 24, weight.class.id != 26, 
       weight.class.id != 88) %>%
  mutate(finish = ifelse(method == "KO/TKO" | method == "Submission" |
                           method == "TKO - Doctor's Stoppage", 1, 0)) %>%
  group_by(year, weight.class) %>%
  summarise(
    fights = n(),
    finishes = sum(finish),
    finish.rate = finishes / fights
  ) %>%
  ggplot(aes(x = factor(year), y = finish.rate, group = 1)) +
    geom_line() +
    facet_wrap(~weight.class) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust = -0.00005),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(6.5, 5.5, 10.5, 5.5), "points")) +
    labs(title = "UFC stoppage rate by weight class",
         subtitle = "Through July 18, 2017") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent)


# Rate of KO/TKO vs. Submission vs. Decision
method.df <- select(ufc, method, year) %>%
  filter(method != "DQ" & method != "No Contest", year > 2000) %>%
  mutate(finish = as.factor(ifelse(method == "KO/TKO" | 
                            method == "TKO - Doctor's Stoppage", "KO/TKO",
                            ifelse(method == "Submission", "Submission", 
                                   "Decision")))) %>%
  group_by(year, finish) %>%
  summarize(
    count = n()
  )

tko <- filter(method.df, finish == "KO/TKO")$count
sub <- filter(method.df, finish == "Submission")$count
dec <- filter(method.df, finish == "Decision")$count

finishes <- as.data.frame(matrix(c(2001:2017, tko, sub, dec), nrow = 17, ncol = 4))
colnames(finishes) <- c("year", "tko", "sub", "dec")
finishes$tko.pct <- with(finishes, tko / (tko + sub + dec))
finishes$sub.prt <- with(finishes, sub / (tko + sub + dec))
finishes$dec.pct <- with(finishes, dec / (tko + sub + dec))
finishes
with(finishes, tko.pct + sub.pct + dec.pct)
ggplot(data = finishes) +
  geom_line(aes(x = factor(year), y = tko.pct, group = 1, color = "red")) +
  geom_line(aes(x = factor(year), y = sub.pct, group = 1, color = "blue")) +
  geom_line(aes(x = factor(year), y = dec.pct, group = 1, color = "darkgreen")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) + 
  theme(axis.text.x = element_text(angle=45),
        axis.title.y = element_blank()) +
  xlab("Year") +
  labs(title = "Rate of fight-ending methods in UFC",
       subtitle = "2001 - July 19, 2017",
       color = "Method") +
  scale_color_manual(labels = c("Submission", "Decision", "KO/TKO"),
                     values = c("blue", "darkgreen", "red"))

# bar chart of UFC headlining weight class

filter(ufc, 
       weight.class != "Men's Catchweight" & 
       weight.class != "Super Heavyweight" &
       weight.class != "Old Middleweight" &
       weight.class != "Women's Featherweight" &
       weight.class != "Women's Flyweight" &
       weight.class != "Women's Catchweight") %>%
  droplevels() %>%
  filter(substring(as.character(event), 1, 10) == "UFC on Fox",
         fight.order == 1) %>%
  ggplot(aes(x = weight.class, fill = gender)) + geom_bar() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 15),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank()) +
  scale_x_discrete(drop = FALSE) +
    labs(title = "UFC on Fox main events by weight class",
         subtitle = "UFC on Fox 1 through 24")

# Length of UFC on Fox main cards
filter(ufc, substring(as.character(event), 1, 10) == "UFC on Fox", 
       fight.order < 5, date > "2012-02-01") %>%
  select(event, fight.seconds) %>%
  group_by(event) %>%
  summarize(
    main.card.total = sum(fight.seconds) / 60
  ) %>%
  ggplot() +
    geom_histogram(aes(x = main.card.total), binwidth = 5,
                   color = "black", fill = "gray") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 12)) + 
  xlab ("Fight time (in minutes)") +
  ylab("Count") +
  labs(title = "Combined fight length of UFC on Fox main cards",
        subtitle = "UFC on Fox 3 through 24") +
  scale_x_continuous(breaks = seq(20, 70, 5))

# Jon Jones winning LHW title fights
filter(ufc, weight.class.id == 2, title.fight == 1) %>%
  mutate(jon.jones = ifelse(winner == "Jon Jones", 1, 0)) %>%
  ggplot(aes(x = factor(1), fill = factor(jon.jones))) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    scale_fill_manual(labels = c("Not Jon Jones", "Jon Jones"), 
                      values = c("orange", "black")) + 
    theme(legend.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    labs(title = "Winners in UFC light heavyweight title fights",
         subtitle = "Includes UFC 31 to UFC on Fox 25")


# Finish types in UFC title fights (Jon Jones vs. others)
filter(ufc, weight.class.id == 2, title.fight == 1) %>%
  mutate(jon.jones = as.factor(ifelse(winner == "Jon Jones", 1, 0))) %>%
  mutate(finish = as.factor(ifelse(method == "KO/TKO" | 
                                     method == "TKO - Doctor's Stoppage", "KO/TKO",
                                   ifelse(method == "Submission", "Submission", 
                                          "Decision")))) %>%
  ggplot(aes(x = finish, fill = jon.jones)) +
  geom_bar() +
  scale_fill_manual(labels = c("Not Jon Jones", "Jon Jones"), 
                    values = c("orange", "black")) + 
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position="bottom") +
  labs(title = "Finishes in UFC light heavyweight title fights",
       subtitle = "Includes UFC 31 to UFC on Fox 25")

(ugh <- c(8, 7, 16, 6, 5, 15, 4, 14, 3, 2, 13, 12, 1, seq(11, 1)))

# Plot of Jones and Cormier UFC wins
select(ufc, winner, date) %>%
  filter(winner == "Jon Jones" | winner == "Daniel Cormier") %>%
  mutate(ufc.win = ugh) %>%
  arrange(date)
  ggplot(aes(x = date, y = ufc.win, color = winner)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = seq(1, 16)) +
    scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y")) +
    theme_bw() +
    theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 12)) +
    labs(title = "Cumulative UFC wins (Jon Jones, Daniel Cormier)",
         subtitle = "Through July 27, 2017")

lawcer <- select(ufc, winner, date) %>%
  filter(winner == "Robbie Lawler" | winner == "Donald Cerrone") %>%
  arrange(winner, date)

lawcer$ufc.win = sequence(rle(as.character(lawcer$winner))$lengths) 
  
ggplot(data = lawcer, aes(x = date, y = ufc.win, color = winner)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(1, 20)) +
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 12)) +
  labs(title = "Cumulative UFC wins (Robbie Lawler, Donald Cerrone)",
       subtitle = "Through July 27, 2017")

woomai <- select(ufc, winner, date) %>%
  filter(winner == "Tyron Woodley" | winner == "Demian Maia") %>%
  arrange(winner, date)

woomai$ufc.win = sequence(rle(as.character(woomai$winner))$lengths) 
    
ggplot(data = woomai, aes(x = date, y = ufc.win, color = winner)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(1, 20)) +
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 12)) +
  labs(title = "Cumulative UFC wins (Tyron Woodley, Demian Maia)",
       subtitle = "Through July 28, 2017")


### Most UFC title wins

filter(ufc, title.fight == 1) %>%
  select(winner) %>%
  group_by(winner) %>%
  summarise(
    title.wins = n()
  ) %>%
  arrange(desc(title.wins)) %>%
  filter(title.wins > 5) %>%
  ggplot(aes(x = reorder(winner, title.wins), y = title.wins)) + 
    geom_bar(stat = "identity", fill = "goldenrod3") +
  coord_flip() +
  scale_y_continuous(breaks = seq(1, 12)) +
  ylab("Wins") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(title = "Wins in UFC title fights",
       subtitle = "UFC 31 through UFC 214")
# Most finishes in UFC title fights
filter(ufc, title.fight == 1, method == "KO/TKO" | method == "Submission" |
                              method == "TKO - Doctor's Stoppage") %>%
  select(winner) %>%
  group_by(winner) %>%
  summarise(
    title.wins = n()
  ) %>%
  arrange(desc(title.wins)) %>%
  filter(title.wins > 3) %>%
  ggplot(aes(x = reorder(winner, title.wins), y = title.wins)) + 
  geom_bar(stat = "identity", fill = "goldenrod3") +
  coord_flip() +
  scale_y_continuous(breaks = seq(1, 12)) +
  ylab("Wins") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(title = "Finishes in UFC title fights",
       subtitle = "UFC 28 through UFC 214")
  
# Rashad Evans position on card

rashad <- filter(ufc, winner == "Rashad Evans" | loser == "Rashad Evans") %>%
  select(winner, loser, fight.order)

rashad$fight <- 21:1
rashad$fight.order = 1 - rashad$fight.order

ggplot(rashad, aes(x = fight, y = fight.order)) + 
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Career fight number") +
  ylab("Fights away from main event") +
  labs(title = "Rashad Evans' position on UFC cards")

test.df <- filter(ufc, title.fight == 1, method == "KO/TKO" | method == "Submission" |
                    method == "TKO - Doctor's Stoppage") %>%
  select(winner) %>%
  group_by(winner) %>%
  summarise(
    title.wins = n()
  ) %>%
  arrange(desc(title.wins))
print(tbl_df(test.df), n=54)

# Evans, Alvey cumulative UFC wins
evaalv <- select(ufc, winner, date) %>%
  filter(winner == "Rashad Evans" | winner == "Sam Alvey") %>%
  arrange(winner, date)

evaalv$ufc.win = sequence(rle(as.character(evaalv$winner))$lengths) 

ggplot(data = evaalv, aes(x = date, y = ufc.win, color = winner)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(1, 20)) +
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position="bottom",
        plot.title = element_text(size = 12)) +
  labs(title = "Cumulative UFC wins (Rashad Evans, Sam Alvey)",
       subtitle = "Through August 3, 2017")

# Most UFC wins in Mexico
mexico <- filter(ufc, date == "2014-11-15" | date == "2015-06-13" | date == "2015-11-21" |
            date == "2016-11-05")

NumberOfFights <- function(df = ufc){
  winner <- df %>%
    select(fighter = winner) %>%
    group_by(fighter) %>%
    summarize(
      wins = n(),
      losses = 0
    )
  loser <- df %>%
    select(fighter = loser) %>%
    group_by(fighter) %>%
    summarize(
      wins = 0,
      losses = n()
    )
  total <- full_join(winner, loser, by = "fighter")
  total[is.na(total)] <- 0
  total <- select(total, fighter, wins = wins.x, losses = losses.y)
  total$fights <- total$wins + total$losses
  total <- arrange(total, desc(fights), desc(wins))
  total
}

mexico.test <- NumberOfFights(mexico)
mexico.test$fighter <- factor(mexico.test$fighter)
mexico.test$fighter <- reorder(mexico.test$fighter, mexico.test$wins)
mexico.test <- filter(mexico.test, wins > 1)
img <- readPNG("MexicoFlag2.png")

ggplot(mexico.test, aes(x = fighter, y = wins)) +
  geom_bar(stat = "identity", fill = "green4", alpha = 0.8) + 
  theme_bw() +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank()) +
  labs(title = "Most UFC wins in Mexico")

# mean fights per card by year
ufc %>%
  group_by(event, year) %>%
  summarize(
    fights.on.card = max(fight.order)
  ) %>%
  group_by(year) %>%
  summarize(
    avg.num.fights = mean(fights.on.card)
  ) %>%
  ggplot(aes(x = year, y = avg.num.fights)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = seq(0, 13)) +
    coord_cartesian(ylim = c(0, 13)) +
    scale_x_continuous(breaks = seq(2000, 2017)) +
    theme_bw() +
    ylab("Fights") +
    labs(title = "Average number of fights per UFC card",
         subtitle = "UFC 28 through UFC Fight Night 114") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle=90),
          panel.grid.minor = element_blank())

# average length of fight cards 
ufc %>%
  group_by(event, year) %>%
  summarize(
    card.minutes = sum(fight.seconds) / 60
  ) %>%
  group_by(year) %>%
  summarize(
    avg.card.minutes = mean(card.minutes)
  ) %>%
  ggplot(aes(x = year, y = avg.card.minutes)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = seq(0, 140, 20)) +
    coord_cartesian(ylim = c(0, 140)) +
    scale_x_continuous(breaks = seq(2000, 2017)) +
    theme_bw() +
    ylab("Minutes") +
    labs(title = "Average total fight length of UFC cards",
         subtitle = "UFC 28 through UFC Fight Night 114") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90),
          panel.grid.minor = element_blank())


# event types
ufc.events$type <- ifelse(grepl("\\<on Fox\\>", unique(ufc$event)), 
                          "Network",
                          ifelse(grepl("UFC [[:digit:]]", unique(ufc$event)), "PPV", 
                                 "Cable"))
ufc.events$type <- ifelse(grepl("UFC 70|UFC 89|UFC 95|UFC 105|UFC 120|UFC 138", ufc.events$event),
                          "Cable", 
                          ifelse(grepl("UFC 37.5", ufc.events$event), "Taped", ufc.events$type))

# heatmap of events per month
ufc.events.month <- filter(ufc, year < 2017, year > 2000) %>%
  distinct(event, month, year) %>%
  group_by(month, year) %>%
  summarize(
    num.events = n()
  ) %>%
  arrange(year, month) %>%
  print(n = 162)

blank <- data.frame(rep(1:12, times = 16), rep(2001:2016, each = 12), 0)
colnames(blank) <- c("month", "year", "num.events")

ufc.events.heatmap <- left_join(blank, ufc.events.month, by = c("month", "year"))
ufc.events.heatmap$num.events <-
  ifelse(is.na(ufc.events.heatmap$num.events.y), ufc.events.heatmap$num.events.x,
         ufc.events.heatmap$num.events.y)

ggplot(ufc.events.heatmap, aes(y = year, x = month)) +
  geom_tile(aes(fill = num.events)) +
  scale_fill_gradient(name = "# of events",
                      low = "blue4", high = "firebrick2") +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1, 12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(breaks = seq(2001, 2016)) +
  theme(axis.text.x = element_text(angle=90),
        axis.title = element_blank()) +
  labs(title = "Number of UFC events by month")

ggplot(ufc.events.heatmap, aes(y = year, x = month)) +
  geom_tile(aes(fill = factor(num.events))) +
  scale_fill_discrete(name = "# of events") +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1, 12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(breaks = seq(2001, 2016)) +
  theme(axis.text.x = element_text(angle=90),
        axis.title = element_blank()) +
  labs(title = "Number of UFC events by month")


# heatmap of fights per month

ufc.fights.month <- filter(ufc, year < 2017, year > 2000) %>%
  group_by(event, month, year) %>%
  summarize(
    num.fights = max(fight.order)
  ) %>%
  group_by(month, year) %>%
  summarize(
    num.fights.month = sum(num.fights)
  ) %>%
  arrange(year, month)

blank2 <- data.frame(rep(1:12, times = 16), rep(2001:2016, each = 12), 0)
colnames(blank2) <- c("month", "year", "num.fights.month")

ufc.fights.month.heatmap <- left_join(blank2, ufc.fights.month, by = c("month", "year"))
ufc.fights.month.heatmap$num.fights.month <-
  ifelse(is.na(ufc.fights.month.heatmap$num.fights.month.y), 
         ufc.fights.month.heatmap$num.fights.month.x, 
         ufc.fights.month.heatmap$num.fights.month.y)

ggplot(ufc.fights.month.heatmap, aes(y = year, x = month)) +
  geom_tile(aes(fill = num.fights.month)) +
  scale_fill_gradient(name = "# of fights",
                      low = "yellow2", high = "red2") +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1, 12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(breaks = seq(2001, 2016)) +
  theme(axis.text.x = element_text(angle=90),
        axis.title = element_blank()) +
  labs(title = "Number of UFC fights by month")

ggplot(ufc.fights.month.heatmap, aes(y = num.fights.month, x = month)) +
  geom_line(aes(color = factor(year))) +
  theme_bw() + 
  scale_x_continuous(breaks = seq(1, 12), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle=90),
        axis.title = element_blank()) +
  labs(title = "Number of UFC fights by month")

ggplot(ufc.fights.month.heatmap, aes(x = year, y = num.fights.month)) +
  geom_line(aes(color = factor(month))) +
  theme_bw() + 
  scale_x_continuous(breaks = seq(2001, 2016)) +
  theme(axis.text.x = element_text(angle=90),
        axis.title = element_blank()) +
  labs(title = "Number of UFC fights by month")
