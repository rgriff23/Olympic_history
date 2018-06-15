################
# PREPARATIONS #
################

# load packages
library("tidyverse")
library("gridExtra")

# load data 
data <- read_csv("~/Documents/GitHub/Olympic_history/data/athlete_events.csv",
                 col_types = cols(
                   ID = col_character(),
                   Name = col_character(),
                   Sex = col_factor(levels = c("M","F")),
                   Age =  col_integer(),
                   Height = col_double(),
                   Weight = col_double(),
                   Team = col_character(),
                   NOC = col_character(),
                   Games = col_character(),
                   Year = col_integer(),
                   Season = col_factor(levels = c("Summer","Winter")),
                   City = col_character(),
                   Sport = col_character(),
                   Event = col_character(),
                   Medal = col_factor(levels = c("Gold","Silver","Bronze"))
                 )
)

# Subset to Art Competitions and variables of interest
art <- data %>% 
  filter(Sport == "Art Competitions") %>%
  select(Name, Sex, Age, Team, NOC, Year, City, Event, Medal)

# View art competition data
art %>% print(n = 5, width = Inf)

# View unique Year/City arranged by Year
art %>% select(Year, City) %>% unique %>% arrange(Year)

#########################
# PLOT TRENDS OVER TIME #
#########################

# View unique Year/City arranged by Year
art %>% select(Year, City) %>% unique %>% arrange(Year)

# Count Events, Nations, and Artists each year
counts <- art %>% filter(Team != "Unknown") %>%
  group_by(Year) %>%
  summarize(
    Events = length(unique(Event)),
    Nations = length(unique(Team)),
    Artists = length(unique(Name))
  )

# Create plots
p1 <- ggplot(counts, aes(x=Year, y=Events)) +
  geom_point(size=2) +
  geom_line() + xlab("")
p2 <- ggplot(counts, aes(x=Year, y=Nations)) +
  geom_point(size=2) +
  geom_line() + xlab("")
p3 <- ggplot(counts, aes(x=Year, y=Artists)) +
  geom_point(size=2) +
  geom_line()
grid.arrange(p1, p2, p3, ncol=1)

#########################
# WOMEN'S PARTICIPATION #
#########################

# subset to female artists
art_women <- art %>% filter(Sex == "F") %>% select(-Sex, -Team) 

# percentage women in art competitions
(art_women$Name %>% unique %>% length)/
  (art$Name %>% unique %>% length)

# percentage women in all competitions
data_1912_1948 <- data %>% filter(Year %in% unique(art$Year))
(data_1912_1948 %>% filter(Sex == "F") %>% select(Name) %>% unique %>% nrow)/
  (data_1912_1948$Name %>% unique %>% length)

# Percentage of medalists who were women
(art_women %>% filter(!is.na(Medal)) %>% nrow)/
  (art %>% filter(!is.na(Medal)) %>% nrow)

#############
# MEDALISTS #
#############

# Count number of medals awarded to each Team
medal_counts <- art %>% filter(!is.na(Medal))%>%
  group_by(Team, Medal) %>%
  summarize(Count=length(Medal)) 

# Order Team by total medal count
lev <- medal_counts %>%
  group_by(Team) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(Team)
medal_counts$Team <- factor(medal_counts$Team, levels=lev$Team)

# Plot
ggplot(medal_counts, aes(x=Team, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Historical medal counts from Art Competitions") +
  theme(plot.title = element_text(hjust = 0.5))

########################
# MEDALS AT NAZI GAMES #
########################

# Count number of medals awarded to each Team at Nazi Olympics
medal_counts2 <- art %>% filter(Year==1936, !is.na(Medal))%>%
  group_by(Team, Medal) %>%
  summarize(Count=length(Medal)) 

# Order Team by total medal count
lev2 <- medal_counts2 %>%
  group_by(Team) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(Team)
medal_counts2$Team <- factor(medal_counts2$Team, levels=lev2$Team)

# Plot
ggplot(medal_counts2, aes(x=Team, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Nazi domination of Art Competitions at the 1936 Olympics") +
  theme(plot.title = element_text(hjust = 0.5))

#######
# END #
#######