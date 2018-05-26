##########################
# Load packages and data #
##########################

# Load packages
library("XML")
library("tidyverse")
library("stringi")

# Load data (takes a few seconds)
load('C:/Users/Randi Griffin/Documents/GitHub/Olympic_history/scrapings.Rdata')

##################################################################
# Check and parse infobox (extract name, gender, height, weight) #
##################################################################

# How long is it?
n_athletes <- length(infobox) # 135584

# How many null entries?
infobox %>% lapply(is.null) %>% unlist %>% sum # none

###
## Check completeness of variables of interest and
## create tibble 'info' to save info from each athlete in rows
###

# Name
infobox %>% lapply(function(x) grepl("Full name", x)) %>% unlist %>% sum # 100% complete
# The first column of the tibble contains the full, unedited name of each athlete
info <- lapply(infobox, function(x) strsplit(x[[1]], ": ")[[1]][2]) %>% unlist %>% tibble(Name = .)
info$Name %>% is.null %>% sum # check: no NULL entries

# Gender
gender_check <- infobox %>% lapply(function(x) grepl("Gender", x)) %>% lapply(sum) 
gender_check %>% unique # Problem: some infoboxes have 0 or 2 entries for gender
which(gender_check==0) # 17 have 0... checking these pages reveals missing data 
which(gender_check==2) # 2 have 2... checking these pages reveals athletes named "Genders"
# To avoid athletes named "Genders", include the colon when grepping: "Gender:"
# The missing data can all be inferred from either the athlete's name or event.
# Of the athletes missing data, all are men except for entries: 30521, 42623, 47015, 81716.
# These athletes are Pam Dukes, Cynthia Green, Carol Ann Heavey, and Moosaka.
# Classify as female if it says "Female" in the "Gender:" line, male otherwise
# This will misclassify the females with no gender data as male, so fix those manually
info <- infobox %>% 
  lapply(function(x) {
    x <- x[grep("Gender:",x)] 
    if (length(x) == 0) sex <- "M" else sex <- grepl("Female",x) %>% ifelse("F","M")
    return(sex)
  }) %>%
  unlist %>% add_column(info, Sex = .)
info$Sex[c(30521, 42623, 47015, 81716)] <- "F"
info$Sex <- info$Sex %>% factor # convert to factor
info$Sex %>% is.null %>% sum # check: no NULL entries
info$Sex %>% unique # check: only M and F 

# Height (cm)
n_athletes - (infobox %>% lapply(function(x) grepl("Height:", x)) %>% lapply(sum) %>% unlist %>% sum) # 33923 missing
infobox %>% lapply(function(x) grepl("Height:", x)) %>% lapply(sum) %>% unique # either 0 or 1 per infobox
# Pull numeric height in cm from each infobox that has a "Height:" line
info <- infobox %>% 
  lapply(function(x) {
    x <- x[grep("Height:",x)] 
    if (length(x) > 0) {
      x <- gsub(".*\\((.*)\\).*", "\\1", x)
      height <- as.numeric(gsub(" cm", "", x))
    } else height <- NA
    return(height)
  }) %>%
  unlist %>% add_column(info, Height = .)
info$Height %>% is.na %>% sum # 33923 NA 
hist(info$Height) # looks good

# Weight (kg)
n_athletes - (infobox %>% lapply(function(x) grepl("Weight:", x)) %>% lapply(sum) %>% unlist %>% sum) # 34,892 missing
infobox %>% lapply(function(x) grepl("Weight:", x)) %>% lapply(sum) %>% unique # either 0 or 1 per infobox
# Pull numeric weight in kg from each infobox that has a "Weight:" line
# This one is trickier than Height because weight is displayed in various ways:
  # Weight: xx lbs (xx kg)
  # Weight: xx kg
  # Weight: xx-xx kg
  # Weight: xx, xx kg
  # Weight: xx, xx, xx kg
# When multiple values are provided, I will average them
info <- infobox %>% 
  lapply(function(x) {
    x <- x[grep("Weight:",x)] 
    if (length(x) > 0) {
      
      if (length(grep("lbs", x)) == 1) {
        x <- gsub(".*\\((.*)\\).*", "\\1", x)
        weight <- as.numeric(gsub(" kg", "", x))
      } else {
        x <- strsplit(x, " ")[[1]]
        if (length(x) == 3) {
          weight <- strsplit(x[[2]],"-")[[1]] %>% as.numeric %>% mean
        } else if (length(x) == 4) {
          weight <- c(gsub(",","",x[2]),x[3]) %>% as.numeric %>% mean
        } else if (length(x) == 5) {
          weight <- c(gsub(",","",x[2:3]),x[4]) %>% as.numeric %>% mean
        } else weight <- -1
      }
    } else weight <- NA
    return(weight)
  }) %>%
  unlist %>% add_column(info, Weight = .)
# Got one warning: NAs introduced by coercion
info$Weight %>% is.na %>% sum # 34,893 NA... there is one extra NA, this must be the warning
info$Weight[16087] <- 77.5 # found problem: no space after comma, so script didn't work
info$Weight %>% is.na %>% sum # check: 34,892 NA 
hist(info$Weight) # looks good

# Some summary information
n_athletes - info$Name %>% unique %>% length # 831 repeat names
n_athletes - info$Sex %>% is.na %>% `!`(.) %>% sum # 0 missing sex data
n_athletes - info$Height %>% is.na %>% `!`(.) %>% sum # 33923 missing height data
n_athletes - info$Weight %>% is.na %>% `!`(.) %>% sum # 34892 missing weight data
sum(((!is.na(info$Weight)) + (!is.na(info$Height))) == 2) # 99593 have both height and weight data

# Save work at this point
#saveRDS(info, file="C:/Users/Randi Griffin/Documents/GitHub/Olympic_history/wrangled_infobox.rds")

# Clean up workspace
rm(list=c("gender_check","infobox"))

#######################
# Check results_table #
#######################

# Are the tables consistent across athletes?
results_table %>% lapply(ncol) %>% unique # 10 columns in every table (if not null)
results_table %>% lapply(names) %>% unique # same names in every table (if not null)

# How many null entries? 
results_table %>% lapply(is.null) %>% unlist %>% sum # 13 NULL entries
nulls <- which(results_table %>% lapply(is.null) %>% unlist) # these should be dropped

# Drop NULL entries from both info and results
info <- info[-nulls,]
results_table <- results_table[-nulls]
length(results_table) # new number of athletes: 135571

# Keep columns of interest (drop 'Rank' and empty final column)
keep <- c("Games", "Age", "City", "Sport", "Event", "Team", "NOC", "Medal")
results_table <- lapply(results_table, function (x) {x[,keep]})

# Clean up workspace
rm(list=c("n_athletes","nulls","keep"))

############################
# Combine info and results #
############################

# Join info and results_table
info$ID <- as.character(1:nrow(info))
results_table <- setNames(results_table, info$ID)
data <- data.table::rbindlist(results_table, use.names=TRUE, idcol="ID")
data <- right_join(info, data, by="ID")

# Split Olympic year and season
data$Games <- data$Games %>% as.character
data$Year <- data$Games %>% gsub(" [A-z]*", "", .) %>% as.numeric
data$Season <- data$Games %>% gsub("[0-9]* ", "", .)

# Reorder the variables
data <- data[,c("ID","Name","Sex","Age","Height","Weight","Team","NOC","Games","Year","Season","City","Sport","Event","Medal")]

# Clean up workspace
rm(list=c("info","results_table"))

######################################
# Look for weird stuff and fine tune #
######################################

# Check for weird values
data$Sex %>% unique # M and F
data$Age %>% unique # convert this to integer type
data$Height %>% range(na.rm=TRUE)
data$Weight %>% range(na.rm=TRUE) # smallest person was 55 lbs??
data$Team %>% unique # 1184 levels - this may not be a useful variable
data$NOC %>% unique # 230 levels
data$Games %>% unique # 52 levels
data$Year %>% unique # 35 levels
data$Season %>% unique # there is an 'Equestrian' value (also in Games variable)
# These are for the 1956 Games in Stockholm, in which equestrian was held 5 months earlier
# And in a different city than the rest of the Summer Games, which were in Melbourne
# I will replace these values with 'Summer'
data$City %>% unique # 42 levels - bad text encoding should be fixed
data$Sport %>% unique # 66 levels
data$Event %>% unique # 679 levels - bad text encoding should be fixed
data$Medal %>% unique # replace "" with NA

# Name and Team variables contain a lot of non-ascii characters which I will remove
data$Name <- data$Name %>% iconv("UTF-8","ASCII", sub="")
data$Team <- data$Team %>% iconv("UTF-8","ASCII", sub="")

# Convert Age variable to integer type
data$Age <- data$Age %>% parse_integer

# Change 'Equestrian' to 'Summer' in Games and Season variables
data$Games[data$Games == "1956 Equestrian"] <- "1956 Summer"
data$Season[data$Season == "Equestrian"] <- "Summer"
data$Season <- data$Season %>% as.factor

# Replace "" with NA in Medal variable
data$Medal[data$Medal == ""] <- NA

# Fix text encoding for cities
cities <- data$City %>% unique
problem_cities <- cities[c(24,25,33)]
correct_cities <- c("Mexico City","Munich","Montreal")
data$City <- data$City %>% as.character
data$City[data$City == problem_cities[1]]  <- correct_cities[1]
data$City[data$City == problem_cities[2]]  <- correct_cities[2]
data$City[data$City == problem_cities[3]]  <- correct_cities[3]
data$City <- data$City %>% as.factor

# Fix text encoding for events
events <- data$Event %>% unique
problem_events <- events[c(10,13,53,56,57,66,103,132,158,170,172,205,209,223,237,
                           252,276,277,288,380,400,408,416,449,455,466,470,494,
                           495,528,534,540,563,566,579,619,625,655,664,677)]
correct_events <- c("Men's 4 x 10 kilometres Relay", "Women's 4 x 100 metres Relay",
                    "Men's 4 x 100 metres Medley Relay","Men's epee, Individual",
                    "Men's epee, Team","Men's 4 x 100 metres Relay",
                    "Men's 4 x 200 metres Freestyle Relay","Men's 4 x 400 metres Relay",
                    "Women's 4 x 100 metres Medley Relay","Women's epee, Individual",
                    "Men's 4 x 100 metres Freestyle Relay","Men's 4 x 7.5 kilometres Relay",
                    "Women's 4 x 100 metres Freestyle Relay","Women's 4 x 400 metres Relay",
                    "Mixed 2 x 6 kilometres and 2 x 7.5 kilometres Relay","Women's 3 x 5 kilometres Relay",
                    "Women's 3 x 7.5 kilometres Relay","Women's 4 x 7.5 kilometres Relay",
                    "Men's 4 x 50 Yard Freestyle Relay","Women's 4 x 200 metres Freestyle Relay",
                    "Women's 4 x 6 kilometres Relay","Men's 333 metres Time Trial",
                    "Women's 4 x 5 kilometres Relay","Mixed 40 metres",
                    "Men's Kayak Relay 4 x 500 metres","Men's epee, Masters, Individual",
                    "Women's epee, Team","Men's 1/4 mile",
                    "Men's 1/2 mile","Mixed 0.5-1 Ton",
                    "Men's epee, Masters and Amateurs, Individual","Men's 4 x 250 metres Freestyle Relay",
                    "Men's Au Cordon Dore, 50 metres","Mixed 30 metres",
                    "Men's 1/3 mile","Mixed 0-0.5 Ton",
                    "Men's Dueling Pistol Au Vise 20 metres","Men's Sur La Perche a La Herse",
                    "Men's Sur La Perche a La Pyramide","Men's Au Cordon Dore, 33 metres")
length(problem_events) == length(correct_events)
data$Event <- data$Event %>% as.character
for (i in 1:length(problem_events)) {
  data$Event[data$Event == problem_events[i]] <- correct_events[i]
}
data$Event <- data$Event %>% as.factor

# Since some event names can correspond to different sports (e.g., "Men's 100 meters" can be many sports)
# The Event column should include the name of the sport as well
data$Event <- paste(data$Sport, data$Event)
data$Event <- data$Event %>% as.factor

# Clean up workspace
rm(list=c("i","problem_cities","problem_events","correct_cities","correct_events","events","cities"))

#############
# Save data #
#############

# Write table
write.csv(data, "C:/Users/Randi Griffin/Documents/GitHub/Olympic_history/data/athlete_events.csv", row.names=FALSE)

#######
# END #
#######