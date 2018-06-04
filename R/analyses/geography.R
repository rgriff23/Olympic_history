################
# PREPARATIONS #
################

# load packages
library("tidyverse")

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

###########################
# GET HOST CITY LOCATIONS #
###########################

# Sorted table of Games (Year, Season, City)
#games <- data %>% 
#  select(Year,Season,City) %>% 
#  unique %>% 
#  arrange(Year)

# Set API key and geocode (don't include actual key here for security reasons)
#register_google(key = "my key")
#locations <- mutate_geocode(games, City)

###################
# MAP HOST CITIES #
###################

# Load packages
library("maps")

# Read locations data (originally obtained using the code above)
loc <- read.csv("~/Documents/GitHub/Olympic_history/data/host_city_locations.csv")

# Add color coding for Olympic eras
loc$Color <- c(rep("yellow",16),rep("red",22),rep("blue",14))
loc$pch <- ifelse(loc$Season=="Winter", 8, 16)

# Map
map("world", fill=TRUE, col="gray", bg="white", border=NA, ylim=c(-60, 90), mar=c(0,0,0,0))
points(x=loc$lon, y=loc$lat, col=alpha(loc$Color, 0.7), pch=loc$pch, cex=0.4)
legend("bottomleft", bty="n", pch=c(19,8,19,19,19),
       legend=c("Summer Games","Winter Games","Pre-Cold War","Cold War","Post-Cold War"),
       col=c("gray30","gray30","yellow","red","blue"))

################################
# MAP WHERE ATHLETES COME FROM #
################################

# Read NOC-region map data
noc <- read_csv("~/Documents/GitHub/Olympic_history/data/noc_regions.csv",
                col_types = cols(
                  NOC = col_character(),
                  region = col_character()
                ))
noc <- noc %>% dplyr::select(-notes)

# Add regions to data and remove missing points
data_regions <- data %>% 
  left_join(noc,by="NOC") %>%
  filter(!is.na(region))
nrow(data) - nrow(data_regions) # 370 removed

# Subset to Games of interest and count athletes from each country
amsterdam <- data_regions %>% 
  filter(Games == "1928 Summer") %>%
  group_by(region) %>%
  summarize(Amsterdam = length(unique(ID)))
munich <- data_regions %>% 
  filter(Games == "1972 Summer") %>%
  group_by(region) %>%
  summarize(Munich = length(unique(ID)))
rio <- data_regions %>% 
  filter(Games == "2016 Summer") %>%
  group_by(region) %>%
  summarize(Rio = length(unique(ID)))

# Create data for mapping
world <- map_data("world")
mapdat <- tibble(region=unique(world$region))
mapdat <- mapdat %>% 
  left_join(amsterdam, by="region") %>%
  left_join(munich, by="region") %>%
  left_join(rio, by="region")
mapdat$Amsterdam[is.na(mapdat$Amsterdam)] <- 0
mapdat$Munich[is.na(mapdat$Munich)] <- 0
mapdat$Rio[is.na(mapdat$Rio)] <- 0
mapdat <- as.data.frame(mapdat)
world <- left_join(world, mapdat, by="region")

# Plot: Amsterdam 1928
ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Amsterdam)) +
  labs(title = "Amsterdam 1928",
       x = NULL, y=NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "navy"),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient(low="white",high="red")

# Plot: Munich 1972
ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Munich)) +
  labs(title = "Munich 1972",
       x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "navy"),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient2(low = "white", high = "red")

# Plot:  Rio 2016
ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Rio)) +
  labs(title = "Rio 2016",
       x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "navy"),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient2(low="white",high = "red")

#######
# END #
#######