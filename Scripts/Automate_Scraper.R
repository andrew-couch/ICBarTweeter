library(taskscheduleR)

taskscheduler_create(taskname = "bar_scraper",
                     schedule = "DAILY",
                     startdate = "07/18/2021",
                     starttime = "12:00",
                     rscript = "E:/School/R Work/ICBarTweeter/Scripts/Scraping.R")

taskscheduler_delete("bar_scraper")
