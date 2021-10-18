message("Loading Packages...")
# Load Packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(hms)
})

# Helper function to look into detail of arrest and extract date of birth, was arrested, and officer name
get_detail <- function(link){
  
  link <- read_html(link)
  
  tibble(dob = link %>% html_element("dd:nth-child(6)") %>% html_text(),
         jailed = link %>% html_element("dd:nth-child(8)") %>% html_text(),
         officer = link %>% html_element("dd:nth-child(14)") %>% html_text())
  
} 

message("Scraping Charges...")
# Scrape arrests/charges and append to a table for future analysis
read_html("https://www.iowa-city.org/IcgovApps/Police/ArrestBlotter") %>% 
  html_element("table") %>% 
  html_table() %>% 
  rename_all(~tolower(.x) %>% str_replace_all(" ", "_")) %>% 
  bind_cols(tibble(details = read_html("https://www.iowa-city.org/IcgovApps/Police/ArrestBlotter") %>% 
                     html_elements(".body-content a") %>% 
                     html_attr("href"))) %>% 
  mutate(details = paste0("https://www.iowa-city.org", details),
         details = map(details, get_detail)) %>% 
  unnest(details) %>% 
  mutate(offense_date = parse_hms(offense_date, "%m/%d/%Y %h:%m:%s %p")) %>% 
  write_csv("Data/charge_history.csv", append = TRUE)

message("Scraping Activity...")
# Scrape overall activity and append to a table for future analysis
read_html("https://www.iowa-city.org/IcgovApps/police/activitylog") %>% 
  html_element("table") %>% 
  html_table() %>% 
  rename_all(~tolower(.x) %>% str_replace(" ", "_")) %>% 
  write_csv("Data/police_activity.csv", append = TRUE)

suppressMessages({
  charge_hist <- read_csv("Data/charge_history.csv") %>% 
    distinct() %>% 
    mutate(offense_date = parse_hms(offense_date, "%m/%d/%Y %h:%m:%s %p")) %>% 
    arrange(offense_date)
  
  police_activity <- read_csv("Data/police_activity.csv") %>% 
    distinct() %>% 
    mutate(offense_date = parse_hms(offense_date, "%m/%d/%Y %h:%m:%s %p"))  %>% 
    arrange(offense_date)
  
  write_csv(charge_hist, "Data/police_activity.csv")
  write_csv(police_activity, "Data/police_activity.csv")
})