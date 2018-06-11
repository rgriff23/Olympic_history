################
# PREPARATIONS #
################

# load packages
library("tidyverse")
library("plotly")

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

# Exclude art competitions
data <- data %>% filter(Sport != "Art Competitions")

# Add BMI column
data$BMI <- data$Weight/(data$Height/100)^2

# Recode year of Winter Games after 1992 to match the next Summer Games
original <- c(1994,1998,2002,2006,2010,2014)
new <- c(1996,2000,2004,2008,2012,2016)
for (i in 1:length(original)) {
  data$Year <- gsub(original[i], new[i], data$Year)
}
data$Year <- as.integer(data$Year)
rm(original, new, i)

#####################
# DATA COMPLETENESS #
#####################

# Check data availability with interactive plot
g1 <- data %>% group_by(Year, Sex) %>%
  summarize(Present = length(unique(ID[which(!is.na(Height) & !is.na(Weight))])),
            Total = length(unique(ID))) %>%
  mutate(Proportion = Present/Total) %>%
  ggplot(aes(x=Year, y=Proportion, color=Sex)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=c("darkblue","red"))  +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Height/Weight data completeness from each Olympiad")
plotly(g1)

# Remove missing Height/Weight data and limit to years from 1960 onward
data <- data %>% filter(!is.na(Height), !is.na(Weight), Year > 1959)

############
# BOXPLOTS #
############

# Height over time
data %>% ggplot(aes(x=as.factor(Year), y=Height, fill=Sex)) +
  geom_boxplot(alpha=0.75) +
  xlab("Olympiad Year") + ylab("Height (cm)") +
  scale_fill_manual(values=c("blue","red"))

# Weight over time
data %>% ggplot(aes(x=as.factor(Year), y=Weight, fill=Sex)) +
  geom_boxplot(alpha=0.75) +
  xlab("Olympiad Year") + ylab("Weight (kg)") +
  scale_fill_manual(values=c("blue","red"))

######################################
# ANALYZE TRENDS IN DIFFERENT EVENTS #
######################################

# Identify events present in all 15 Games
events <- data[data$Year==1960,"Event"] %>% unique %>% .$Event # 177 in 1960
years <- data$Year %>% unique %>% sort %>% tail(-1)
for (i in 1:length(years)) {
  nxt <- data[data$Year==years[i],"Event"] %>% unique %>% .$Event
  events <- intersect(events, nxt)
}

# Subset data to only these events
data <- data %>% filter(Event %in% events)

# Get list of sports matching events
sports_events <- data %>% select(Sport, Event) %>% unique

# Eliminate wrestling, weightlifting, and boxing
sports_events <- sports_events %>% 
  filter(!Sport %in% c("Wrestling","Weightlifting","Boxing","Equestrianism")) %>%
  filter(!Event %in% c("Figure Skating Mixed Pairs")) %>%
  arrange(Sport)

# Add column for men/women/mixed
sports_events$Sex <- ifelse(grepl("Women",sports_events$Event),"Women","Men")

# Loop through events and fit regressions
s.height <- s.weight <- c()
for (i in 1:nrow(sports_events)) {
  temp <- data %>% filter(Event == sports_events$Event[i])
  lm.height <- lm(Height ~ Year, data=temp)
  lm.weight <- lm(Weight ~ Year, data=temp)
  s.height[i] <- lm.height$coefficients["Year"]
  s.weight[i] <- lm.weight$coefficients["Year"]
}
slopes <- tibble(Sport = sports_events$Sport, 
                 Event = sports_events$Event,
                 Sex = sports_events$Sex,
                 Height = s.height,
                 Weight = s.weight)

# Multiple slopes by 56 since 56 years passed between 1960 to 2016
slopes$Height <- round(slopes$Height*56,1)
slopes$Weight <- round(slopes$Weight*56,1)

# Plot regression slopes of weight ~ height for men
g2.m <- ggplot(slopes[slopes$Sex=="Men",], aes(x=Height, y=Weight, color=Sport, label=Event)) +
  geom_point(alpha=0.75) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(title="Temporal trends in men's size in different events",
       x="Height (cm)",
       y="Weight (kg)")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none")
ggplotly(g2.m)

# Plot regression slopes of weight ~ height for women
g2.f <- ggplot(slopes[slopes$Sex=="Women",], aes(x=Height, y=Weight, color=Sport, label=Event)) +
  geom_point(alpha=0.75) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(title="Temporal trends in women's size in different events",
       x="Height (cm)",
       y="Weight (kg)")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none")
ggplotly(g2.f)

#######
# END #
#######