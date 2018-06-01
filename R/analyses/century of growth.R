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

# Table counting number of athletes, nations, & events
# excluding the Art Competitions
counts <- data %>% filter(Sport != "Art Competitions") %>%
  group_by(Year, Season) %>%
  summarize(
    Athletes = length(unique(ID)),
    Nations = length(unique(NOC)),
    Events = length(unique(Event))
  )

# Plot
p1 <- ggplot(counts, aes(x=Year, y=Athletes, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("darkorange","darkblue")) +
  xlab("") +  
  annotate("text", x=c(1932,1956,1976,1980),
           y=c(2000,2750,6800,4700),
           label=c("L.A. 1932","Melbourne 1956","Montreal 1976","Moscow 1980"),
           size=3) +
  annotate("text",x=c(1916,1942),y=c(10000,10000),
           label=c("WWI","WWII"), size=4, color="red") +
  geom_segment(mapping=aes(x=1914,y=8000,xend=1918,yend=8000),color="red", size=2) +
  geom_segment(mapping=aes(x=1939,y=8000,xend=1945,yend=8000),color="red", size=2)
p2 <- ggplot(counts, aes(x=Year, y=Nations, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("darkorange","darkblue")) +
  xlab("") +  
  annotate("text", x=c(1932,1976,1980),
           y=c(60,105,70),
           label=c("L.A. 1932","Montreal 1976","Moscow 1980"),
           size=3)
p3 <- ggplot(counts, aes(x=Year, y=Events, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("darkorange","darkblue"))
grid.arrange(p1, p2, p3, ncol=1)