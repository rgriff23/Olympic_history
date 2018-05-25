# May 2018, second scrape

#################
# Load packages #
#################

library("RCurl")
library("XML")
library("tidyverse")

######################################
# Get nodes in the athlete directory #
######################################

# Get list of sub-pages in athlete directory (each contains list of individual athlete pages)
athlete_directory <- getURL("https://www.sports-reference.com/olympics/athletes/") %>%
  htmlParse(asText=TRUE) %>%
  xpathSApply('//td/a', xmlGetAttr, 'href') %>%
  paste('https://www.sports-reference.com/', ., sep="")

# Check number of sub-pages 
length(athlete_directory) # 453 pages

#########################################################
# Visit each node and identify individual athlete pages #
#########################################################

# Initialize vector to store links
individual_links <- c() 

system.time( # ~3.5 minutes
  for (i in 1:length(athlete_directory)) {
    
    # parse athlete directory sub-page to get all links
    new <- getURL(athlete_directory[i]) %>%
      htmlParse(asText=TRUE) %>%
      xpathSApply('//*[(@id = "page_content")]//a', xmlGetAttr, 'href') %>%
      paste('http://www.sports-reference.com/', ., sep="")
    
    # update vector of athlete pages
    individual_links <- c(individual_links, new) 
    
    # track progress in console
    print(i) 
    flush.console() # avoid output buffering
  }
) 

# Check number of individual links (athletes)
length(individual_links)  # 135584

###################################
# Get data from each athlete page #
###################################

# Initialize lists to store scraped data
infobox <- results_table <- vector("list", length(individual_links))

# Loop through links and extract data 
system.time( 
  for (i in 1:135584) {
    
    # get html (wait a minute and try again if it times out and throws and error)
    html <- try(getURL(individual_links[i], .opts=curlOptions(followlocation=TRUE)), silent=TRUE)
    if(class(html) == "try-error") {
      Sys.sleep(60)
      html <- getURL(individual_links[i], .opts=curlOptions(followlocation=TRUE))
    }
    html <- htmlParse(html, asText=TRUE)
    
    # save 'infobox'
    infobox[[i]] <- xpathSApply(html, '//*[@id="info_box"]/p', xmlValue) %>%
    strsplit('\n') %>% .[[1]]
    
    # save 'results table'
    results_table[[i]] <- readHTMLTable(html) %>% .$results
    
    # track progress in console
    print(i)
    flush.console() 
  }
)
# 95560.75/135548 = 0.705 sec/page
# total run time: 26.54 hours

##############
# Save Rdata #
##############

save(individual_links, infobox, results_table, file="C:/Users/Randi Griffin/Documents/GitHub/Olympic_history/scrapings.Rdata")

#######
# END #
#######