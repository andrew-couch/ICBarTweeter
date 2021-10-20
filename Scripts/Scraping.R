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

read_html("https://www.iowa-city.org/IcgovApps/Police/ActivityLog") %>% 
  html_element(".body-content") %>% 
  html_table() %>% 
  rename_with(~tolower(.x) %>% str_replace_all(" ", "_")) %>% 
  mutate(link = paste0("https://www.iowa-city.org/IcgovApps/Police/Details?dispatchNumber=", dispatch_number)) %>% 
  # filter(activity == "BAR CHECK") %>% 
  mutate(link = map(link, read_html),
         dispatch_time = map_chr(link, ~html_element(.x, "dd:nth-child(4)") %>% html_text()),
         activity_medium = map_chr(link, ~html_element(.x, "dd:nth-child(8)") %>% html_text()),
         location = map_chr(link, ~html_element(.x, "dd:nth-child(12)") %>% html_text()),
         location_detail = map_chr(link, ~html_element(.x, "dd:nth-child(14)") %>% html_text())) %>% 
  select(-link) %>% 
  separate(dispatch_time, c("date", "time"), sep = " ", extra = "merge") %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  write_csv("Data/police_activity.csv", append = TRUE)

suppressMessages({
  charge_hist <- read_csv("Data/charge_history.csv") 
  activity_hist <- read_csv("Data/police_activity.csv") 
  
  charge_hist <- charge_hist %>% 
    distinct() %>% 
    arrange(offense_date)
  
  activity_hist <- activity_hist %>% 
    distinct() %>% 
    arrange(date)
  
  write_csv(charge_hist, "Data/police_activity.csv")
  write_csv(activity_hist, "Data/police_activity.csv")
})