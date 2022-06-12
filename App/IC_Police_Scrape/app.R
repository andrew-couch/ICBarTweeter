library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

charges <- read_csv("https://raw.githubusercontent.com/andrew-couch/ICBarTweeter/main/Data/charge_history.csv") %>% filter(date <= Sys.Date())
activity <- read_csv("https://raw.githubusercontent.com/andrew-couch/ICBarTweeter/main/Data/police_activity.csv") %>% filter(date <= Sys.Date())
bar_directory <- read_csv("https://raw.githubusercontent.com/andrew-couch/ICBarTweeter/main/Data/Bar%20Directory.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Iowa City Police Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Summary"),
      menuItem("Data", tabName = "Data") 
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Summary",
              fluidRow(valueBoxOutput("weekly_charges", width = 3), valueBoxOutput("weekly_activity", width = 3), 
                       valueBoxOutput("weekly_jailed", width = 3), valueBoxOutput("weekly_alcohol", width = 3)),
              fluidRow(box(plotOutput("charges_history")), box(plotOutput("activity_history")),
                       box(plotOutput("top_ten_charges")), box(plotOutput("top_ten_bars")))),
      tabItem(
        tabName = "Data",
        fluidRow(
          tabBox(title = "", id = "tabset1", width = 12,
                 tabPanel("Charges", dataTableOutput("charges_tbl")),
                 tabPanel("Activity", dataTableOutput("activity_tbl")))
        )
      )
    )
  )
)


server <- function(input, output) { 
  
  output$charges_history <- renderPlot({
    charges %>% 
      select(case_number, date, charges) %>% 
      separate_rows(charges, sep = ";") %>% 
      distinct() %>% 
      count(date) %>% 
      ggplot(aes(x = date, y = n)) + 
      geom_line() + 
      labs(title = "Charges Scraped")
  })
  
  output$activity_history <- renderPlot({
    activity %>% 
      count(date) %>% 
      ggplot(aes(x = date, y = n)) + 
      geom_line() + 
      labs(title = "Activity Scraped")
  })
  
  output$weekly_charges <- renderValueBox({
    weekly_charge_value <- charges %>% 
      filter(date >= Sys.Date() - 7) %>% 
      select(case_number, date, charges) %>% 
      separate_rows(charges, sep = ";") %>% 
      distinct() %>% 
      nrow()
    valueBox(value = weekly_charge_value, subtitle = "Weekly Charges", icon = icon("balance-scale"), color = "yellow") 
  })
  
  output$weekly_activity <- renderValueBox({
    weekly_acitivity_value <- activity %>% 
      filter(date >= Sys.Date() - 7) %>% 
      nrow()
    valueBox(value = weekly_acitivity_value, subtitle = "Weekly Activity", icon = icon("walking"), color = "green")
  })
  
  output$weekly_jailed <- renderValueBox({
    weekly_jailed_value <- charges %>% 
      select(case_number, date, charges, jailed) %>% 
      separate_rows(charges, sep = ";") %>% 
      distinct() %>% 
      filter(date >= Sys.Date() - 7 & jailed == "Y") %>% 
      nrow()
    
    valueBox(value = weekly_jailed_value, subtitle = "Weekly Jailed", icon = icon("gavel"), color = "red")
  })
  
  output$weekly_alcohol <- renderValueBox({
    weekly_alcohol_value <- charges %>% 
      separate_rows(charges, sep = ";") %>% 
      mutate(charges = str_trim(charges) %>% str_squish() %>% tolower()) %>% 
      distinct() %>% 
      filter(charges %in% c(
        "owi", "public intoxication", "license-possess ficticious dl/id",
        "possess alcohol under legal age (paula)", "owi- 2nd offense",
        "possess open container alcohol in public", "owi- 3rd and subsequent",
        "persons under legal age in licensed establishment-1st", "in a bar after 10 pm while underage"
      )) %>% 
      filter(date >= Sys.Date() - 7) %>% 
      nrow()
    
    valueBox(value = weekly_alcohol_value, subtitle = "Weekly Alcohol Charges", icon = icon("beer"), color = "blue")
  })
  
  output$top_ten_charges <- renderPlot({
    charges %>% 
      select(case_number, date, charges) %>% 
      separate_rows(charges, sep = ";") %>% 
      distinct() %>% 
      count(charges, sort = T) %>% 
      slice_max(n, n = 10) %>% 
      mutate(charges = reorder(charges, n)) %>% 
      ggplot(aes(x = n, y = charges, fill = charges)) + 
      geom_col(show.legend = FALSE) + 
      labs(x = "Freq", y = "",
           title = "Top 10 Charges")
  })
  
  output$top_ten_bars <- renderPlot({
    charges %>% 
      separate_rows(charges, sep = ";") %>% 
      mutate(charges = str_trim(charges) %>% str_squish() %>% tolower(),
             location = tolower(location) %>% str_trim() %>% str_squish()) %>% 
      distinct() %>% 
      select(case_number, location) %>% 
      inner_join(
        bar_directory %>% 
          mutate(across(.cols = everything(), .fns = ~str_squish(.x) %>% tolower())) %>% 
          rename_with(tolower) %>%
          mutate(bar = names) %>% 
          pivot_longer(-bar) %>% 
          select(value, bar),
        by = c("location" = "value")
      ) %>% 
      count(bar, sort = T) %>% 
      mutate(bar = reorder(bar, n)) %>% 
      ggplot(aes(x = n, y = bar, fill = bar)) + 
      geom_col(show.legend = FALSE) +
      labs(y = "", x = "Charges", title = "Bar Charges")
  })
  
  output$charges_tbl <- renderDataTable({
    charges %>% 
      separate_rows(charges, sep = ";") %>% 
      separate(name, c("last", "firstmiddle"), sep = ",", extra = "merge") %>% 
      mutate(charges = str_trim(charges) %>% str_squish() %>% tolower(),
             location = tolower(location) %>% str_trim() %>% str_squish(),
             firstmiddle = replace_na(firstmiddle, " "),
             name = paste0(firstmiddle, " ", last) %>% str_squish() %>% str_to_title()) %>% 
      distinct() %>% 
      arrange(desc(date), desc(time)) %>% 
      select(case_number, date, time, name, dob, charges, dob, jailed, officer)
  })
  
  output$activity_tbl <- renderDataTable({
    activity %>% 
      arrange(desc(date), desc(time)) %>% 
      select(dispatch_number, date, time, activity, activity_medium, disposition, details_text, address, location) 
      
  })
}

shinyApp(ui = ui, server = server)
