##########################
# Load packages and data #
##########################

# Load packages
library("RCurl")
library("XML")
library("data.table") 
library("tidyverse")

# Load data
load('C:/Users/Randi Griffin/Documents/GitHub/Olympic_history/scrapings.Rdata')

##############################################################################
# Check and parse infobox (extract name, gender, height, weight, birthplace) #
##############################################################################

# How many null entries?
infobox %>% lapply(is.null) %>% unlist %>% sum

# Name
infobox %>% lapply(function(x) grepl("Full name", x)) %>% unlist %>% sum # completeness
name <- xpathSApply(data_xml, '//*[@id="info_box"]/h1', xmlValue)

# Gender
infobox %>% lapply(function(x) grepl("Gender", x)) %>% unlist %>% sum # completeness
sex <- ifelse(length(grep("Female", infobox))==0, "M", "F")

# Height (cm)
infobox %>% lapply(function(x) grepl("Height", x)) %>% unlist %>% sum # completeness
# use regular expression to 1. extract what lies between the parentheses and 2. extract the number

# Weight (kg)
infobox %>% lapply(function(x) grepl("Weight", x)) %>% unlist %>% sum # completeness
# use regular expression to 1. extract what lies between the parentheses and 2. extract the number

# Birthplace (country)
infobox %>% lapply(function(x) grepl("Born", x)) %>% unlist %>% sum # completeness
born <- infobox[[i]][grepl("Born", infobox[[i]])]
birthplace <- ifelse(length(born)==1, substring(tail(strsplit(infobox[born], ',')[[1]], 1), 2), NA)





if (!is.null(tab)) {
  athlete_data[[i]] <- data.table(Name=name, Sex=sex, Birthplace=birthplace, tab)
} else {athlete_data[[i]] <- NA}

#################################
# Check and parse results_table #
#################################

# Are the tables consistent across athletes?
results_table %>% lapply(ncol) %>% unique
results_table %>% lapply(names) %>% unique

# How many null entries? 
results_table %>% lapply(is.null) %>% unlist %>% sum



# Parse results_table

##########################################
# Combine infobox and results_table data #
##########################################

# Check data
length(athlete_data) # should be 135584 athletes
unique(unlist(lapply(athlete_data, ncol))) # should be 12 columns for all athletes (NAs excluded)
complete_cases <- unlist(lapply(athlete_data, is.data.frame))
missing_cases <- individual_links[!complete_cases] # visit these links and see what's up
sum(complete_cases) # how many athletes with data?

# Combine results into a single table for all athletes
athlete_data <- athlete_data[complete_cases] # drop cases with missing data
athlete_dataframe <- rbindlist(athlete_data, use.names=TRUE)

######################
# Clean up variables #
######################




















#############################################################
# Get list of countries and check if they match birthplaces # 
#############################################################

# Countries
countries <- getURL("http://www.sports-reference.com/olympics/countries/")
countries <- htmlParse(countries, asText=TRUE)
countries <- readHTMLTable(countries)
countries <- countries$countries$Country
countries <- countries[-which(countries %in% c("Summer Games", "Country", "Unknown"))]
countries <- as.character(countries)
for (i in 1:length(countries)) {
  if (i != 163) {countries[i] <- substr(countries[i], 3, nchar(countries[i]))}
}

# The following athletes' place of birth is not in an Olympic country 
# Some are missing data, but others may just be born in non-countries
unique(data$Birthplace)[!(unique(data$Birthplace) %in% countries)]

########################
# Check easy variables #
########################

# check sex
table(data$Sex, useNA="always") # good
# check age
table(data$Age, useNA="always") # good
# check Olympics city
table(data$City, useNA="always") # good
# check sport
table(data$Sport, useNA="always") # good
# check NOC
table(data$NOC, useNA="always") # good
# check medal
table(data$Medal, useNA="always") # good
# check year
table(data$Year, useNA="always") # some are missing, not sure why
# check season
table(data$Season, useNA="always") # good
# Names are messy due to foreign characters, but I'm not going to bother with this now

############################################################################################################
# Write data
############################################################################################################

# Write table
#write.csv(data, "C:/Users/Randi Griffin/Desktop/athletes_clean2.csv", row.names=FALSE, quote=FALSE)

#######
# END #
#######