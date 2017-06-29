##########################
# Load packages and data #
##########################

# Load packages
library(RCurl)
library(XML)
library(data.table) 
library(plyr)

# Load data
#data <- read.csv('C:/Users/Randi Griffin/Desktop/athletes_clean.csv', header=TRUE, stringsAsFactors = FALSE)

# Load data
load('C:/Users/Randi Griffin/Desktop/start-here.Rdata')

##################################################################
# Get list of sports and countries from www.sports-reference.com #
##################################################################

# Sports
sports <- getURL("http://www.sports-reference.com/olympics/sports/")
sports <- htmlParse(sports, asText=TRUE)
sports <- readHTMLTable(sports)
sports <- sports$sports$Sport

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

######################
# Birthplace and NOC #
######################

# The following athletes' place of birth is not in an Olympic country 
# Some are missing data, but others may just be born in non-countries
unique(data$Birthplace)[!(unique(data$Birthplace) %in% countries)]

############################################################################################################
# Write data
############################################################################################################

# Write table
write.csv(data, "C:/Users/Randi Griffin/Desktop/athletes_clean2.csv", row.names=FALSE, quote=FALSE)

############################################################################################################
# END
############################################################################################################
