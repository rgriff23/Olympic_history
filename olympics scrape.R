
# Sep 30, 2016

# Install and load RCurl and XML
install.packages("RCurl")
install.packages("XML")
install.packages("data.table")
library(RCurl)
library(XML)
library(data.table) 


# Identify relevant nodes in the athlete directory
athlete_url <- getURL("http://www.sports-reference.com/olympics/athletes/")
athlete_xml <- htmlParse(athlete_url, asText=TRUE)
athlete_paths <- xpathSApply(athlete_xml, '//td/a', xmlGetAttr, 'href')
athlete_links <- paste('http://www.sports-reference.com/', athlete_paths, sep="")
length(athlete_links) # 452 pages

# Visit athlete directory pages and identify individual athlete pages
individual_links <- c()
system.time(for (i in 1:length(athlete_links)) {
  individual_url <- getURL(athlete_links[i])
  individual_xml <- htmlParse(individual_url, asText=TRUE)
  individual_paths <- xpathSApply(individual_xml, '//*[(@id = "page_content")]//a', xmlGetAttr, 'href')
  individual_links_new <- paste('http://www.sports-reference.com/', individual_paths, sep="")
  individual_links <- c(individual_links, individual_links_new)
  print(i)
}) # took about ~7.5 minutes
length(individual_links)  # 128415

# Extract data table for each individual athlete page into a list (takes ~1 sec per athlete)
athlete_data <- list()
for (i in 1:128415) {
  data_url <- getURL(individual_links[i])
  data_xml <- htmlParse(data_url, asText=TRUE)
  tab <- readHTMLTable(data_xml)$results[-10]
  name <- xpathSApply(data_xml, '//*[@id="info_box"]/h1', xmlValue)
  infobox <- strsplit(xpathSApply(data_xml, '//*[@id="info_box"]/p', xmlValue), '\n')[[1]]
  sex <- ifelse(length(grep("Female", infobox))==0, "M", "F")
  born <- grep("Born:", infobox)
  birthplace <- ifelse(length(born)==1, substring(tail(strsplit(infobox[born], ',')[[1]], 1), 2), NA)
  if (!is.null(tab)) {
    athlete_data[[i]] <- data.table(Name=name, Sex=sex, Birthplace=birthplace, tab)
  } else {athlete_data[[i]] <- NA}
  print(i)
}

# Check data
length(athlete_data) # should be 128415 athletes
unique(unlist(lapply(athlete_data, ncol))) # should be 12 columns for all athletes (NAs excluded)
complete_cases <- unlist(lapply(athlete_data, is.data.frame))
missing_cases <- individual_links[!complete_cases] # visit these links and see what's up
sum(complete_cases) # how many athletes with data?

# Combine results into a single table for all athletes
athlete_data <- athlete_data[complete_cases] # drop cases with missing data
athlete_dataframe <- rbindlist(athlete_data, use.names=TRUE)

# Write table
write.csv(athlete_dataframe, "C:/Users/Randi Griffin/Desktop/athletes.csv", row.names=FALSE, quote=FALSE)


