library(shiny)
library(tidyverse)

dic <- left_join(read.csv("dat/Daily_Inmates_In_Custody.csv"), read.csv("dat/charge features.csv"), by = c('TOP_CHARGE' = 'Code'))
todays.pop <- nrow(dic)

ui <- fluidPage(
  titlePanel("Can You Get to 3,300?"),
  sidebarLayout(
    sidebarPanel(
      selectInput('race', "Race", c("Asian", "Black", "Indian", "Other", "Unknown", "White"), multiple = T, selected = c("Asian", "Black", "Indian", "Other", "Unknown", "White")),
      selectInput('sex', "Sex", c("Male", "Female"), multiple = T, selected = c("Male", "Female")),
      sliderInput('age', "Age", min = 0, max = 100, c(0, 100)),
      selectInput('fmv', "Offense", c("Felon", "Misdemeanor", "Violation"), multiple = T, selected = c("Felon", "Misdemeanor", "Violation")),
      selectInput('type', "Type", c("Narcotics", "Sex Offense", "Violent", "Weapons", "Vehicle", "Misc", "Property"), multiple = T, selected = c("Narcotics", "Sex Offense", "Violent", "Weapons", "Vehicle", "Misc", "Property")),
      selectInput('custody', "Custody Level", c("Minimum", "Medium", "Maximum"), multiple = T, selected = c("Minimum", "Medium", "Maximum")),
      checkboxInput('srg', "Include Prison Gang Members", value = T),
      checkboxInput('bradh', "Include Mental Health Evaluated", value = T),
      actionButton("update", "Update")
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)

server <- function(input, output, session) {
  
  dic.summary <- eventReactive(input$update, {
    new.length <- dic %>%
      filter(AGE %in% input$age[1]:input$age[2]) %>%
      nrow()
    
    data.frame(a = factor(c("Today's Population", "Your version", "3,300"),
                          levels = c("Today's Population", "Your version", "3,300")),
               b = c(todays.pop, new.length, 3300))
  })
  
  output$hist <- renderPlot({
    ggplot(dic.summary(), aes(x=a, y=b)) +
      geom_col(fill = "#55A5DA") + 
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() + 
      theme(panel.grid = element_blank())
  }, res = 96)
}

shinyApp(ui, server)