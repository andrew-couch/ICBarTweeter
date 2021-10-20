message("Loading Packages...")
# Load Packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
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
  write_csv("Data/charge_history.csv", append = TRUE)


suppressMessages({
  charge_hist <- read_csv("Data/charge_history.csv") 
  
  charge_hist <- charge_hist %>% 
    distinct() %>% 
    arrange(offense_date)
  
  write_csv(charge_hist, "Data/police_activity.csv")
})