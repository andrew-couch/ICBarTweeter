library(taskscheduleR)

taskscheduler_create(taskname = "bar_scraper",
                     schedule = "DAILY",
                     startdate = "03/20/2021",
                     starttime = "12:00",
                     rscript = "E:/School/R Work/ICBarTweeter/Scripts/Scraping.Rmd")

#taskscheduler_delete("bar_scraper")
