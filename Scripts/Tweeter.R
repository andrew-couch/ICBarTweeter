suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
  library(rtweet)
})

past_tweets <- read_csv("Data/tweeted_activity.csv")

activity <- read_html("https://www.iowa-city.org/IcgovApps/Police/ActivityLog") %>% 
  html_element(".body-content") %>% 
  html_table() %>% 
  rename_with(~tolower(.x) %>% str_replace_all(" ", "_")) %>% 
  mutate(link = paste0("https://www.iowa-city.org/IcgovApps/Police/Details?dispatchNumber=", dispatch_number)) %>% 
  filter(activity == "BAR CHECK") %>% 
  # filter(!(dispatch_number %in% past_tweets$dispatch_number)) %>% 
  mutate(link = map(link, read_html),
         dispatch_time = map_chr(link, ~html_element(.x, "dd:nth-child(4)") %>% html_text()),
         activity_medium = map_chr(link, ~html_element(.x, "dd:nth-child(8)") %>% html_text()),
         location = map_chr(link, ~html_element(.x, "dd:nth-child(12)") %>% html_text()),
         location_detail = map_chr(link, ~html_element(.x, "dd:nth-child(14)") %>% html_text())) %>% 
  select(-link) %>% 
  separate(dispatch_time, c("date", "time"), sep = " ", extra = "merge") %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

if(nrow(activity) == 0){
  message("No Bar Raids")
} else {
  activity
  write_csv(activity, file = "Data/tweeted_activity.csv", append = TRUE)
  # Tweet activity
  # create_token(
  #   app = "Iowa City Tweeter",  
  #   consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  #   consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  #   access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  #   access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
  # )
  
  # activity %>% 
  #   walk2(location, dispatch_time, .f = function(location, time){
  #     post_tweet(status = paste0(str_to_title(location), " was just raided at ", time))
  #   })
}