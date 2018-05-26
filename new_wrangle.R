##########################
# Load packages and data #
##########################

# Load packages
library("XML")
library("tidyverse")

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

#################################
# Check and parse results_table #
#################################

# Are the tables consistent across athletes?
results_table %>% lapply(ncol) %>% unique # 10 columns in every table
results_table %>% lapply(names) %>% unique # same names in every table

# How many null entries? 
which(results_table %>% lapply(nrow) %>% is.null)
results_table %>% lapply(is.null) %>% unlist %>% sum # 13 NULL entries
nulls <- which(results_table %>% lapply(is.null) %>% unlist) # these should be dropped

# Drop NULL entries from both info and results
info <- info[-nulls,]
results_table <- results_table[-nulls]
nrow(info) # 135571
length(results_table) # 135571

# Keep columns of interest
keep <- c("Games", "Age", "City", "Sport", "Event", "Team", "NOC", "Medal")
results <- lapply(results_table, function (x) {x[,keep]})

############################
# Combine info and results #
############################

# Loop through athletes to combine info and results (takes )
r <- results %>% lapply(nrow) %>% unlist %>% sum # 271116
data <- data.frame()
next_line <- 1
for (i in 1:nrow(info)) {
  n <- nrow(results[[i]])
  end_line <- next_line + n - 1
  data[next_line:end_line,] <- cbind(info[i,], results[[i]])
  next_line <- end_line + 1
  print(i)
  flush.console()
  }

# Split Olympic year and season
data$Games <- data$Games %>% as.character
data <- mutate(data, Year = strsplit(Games, " ")[[1]][1])
data <- mutate(data, Season = strsplit(Games, " ")[[1]][2])

############################################################################################################
# Write data
############################################################################################################

# Write table
write.csv(data, "C:/Users/Randi Griffin/Desktop/athlete_events.csv", row.names=FALSE, quote=FALSE)

#######
# END #
#######