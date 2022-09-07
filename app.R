library(shiny)
library(tidyverse)

dic <- left_join(read.csv("dat/Daily_Inmates_In_Custody.csv"), read.csv("dat/charge features.csv"), by = c('TOP_CHARGE' = 'Code')) %>%
  mutate(RACE = recode(RACE, "O" = "Other", "B" = "Black", "W" = "White", "I" = "Indian", "A" = "Asian", "U" = "Unknown"),
         GENDER = recode(GENDER, "M" = "Male", "F" = "Female"),
         FMV = ifelse(is.na(FMV), "None", recode(FMV, "F" = "Felony", "M" = "Misdemeanor", "V" = "Violation")),
         Type = ifelse(is.na(Type), "None", Type),
         CUSTODY_LEVEL = recode(CUSTODY_LEVEL, "MIN" = "Minimum", "MED" = "Medium", "MAX" = "Maximum"))
todays.pop <- nrow(dic)

ui <- fluidPage(
  titlePanel("Can You Get to 3,300?"),
  sidebarLayout(
    sidebarPanel(
      selectInput('race', "Race", c("Asian", "Black", "Indian", "Other", "Unknown", "White"), multiple = T, selected = c("Asian", "Black", "Indian", "Other", "Unknown", "White")),
      selectInput('sex', "Sex", c("Male", "Female"), multiple = T, selected = c("Male", "Female")),
      sliderInput('age', "Age", min = 0, max = 100, c(0, 100)),
      selectInput('fmv', "Offense", c("Felony", "Misdemeanor", "Violation"), multiple = T, selected = c("Felony", "Misdemeanor", "Violation")),
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
  
  dic.summary <- eventReactive(input$update, ignoreNULL = F, {
    new.length <- dic %>%
      filter(RACE %in% c(input$race, ""),
             GENDER %in% c(input$sex, ""),
             AGE %in% input$age[1]:input$age[2],
             FMV %in% c(input$fmv, "None"),
             Type %in% c(input$type, "None"),
             CUSTODY_LEVEL %in% c(input$custody, "")
             ) %>%
      nrow()
    
    data.frame(version = factor(c("Today's Population", "Your version", "3,300"),
                          levels = c("Today's Population", "Your version", "3,300")),
               count = c(todays.pop, new.length, 3300))
  })
  
  output$hist <- renderPlot({
    ggplot(dic.summary(), aes(x=version, y=count)) +
      geom_col(fill = "#55A5DA") + 
      geom_text(aes(label = scales::comma(count), y = count + 120)) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            axis.title = element_blank()) 
  }, res = 96)
}

shinyApp(ui, server)