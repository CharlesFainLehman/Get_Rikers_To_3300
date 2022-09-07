library(shiny)
library(tidyr)
library(ggplot2)

dic <- read.csv("https://raw.githubusercontent.com/CharlesFainLehman/Rikers-DIC/main/dat/via_github/DOC_Inmates_InCustody_Daily_20220905.csv")
todays.pop <- nrow(dic)

ui <- fluidPage(
  titlePanel("Can You Get to 3,300?"),
  sidebarLayout(
    sidebarPanel(
      selectInput('custody', "Custody Level", c("Minimum", "Medium", "Maximum"), multiple = T, selected = c("Minimum", "Medium", "Maximum")),
      selectInput('race', "Race", c("Asian", "Black", "Indian", "Other", "Unknown", "White"), multiple = T, selected = c("Asian", "Black", "Indian", "Other", "Unknown", "White")),
      selectInput('sex', "Sex", c("Male", "Female"), multiple = T, selected = c("Male", "Female")),
      sliderInput('age', "Age", min = 0, max = 100, c(0, 100)),
      checkboxInput('srg', "Include Prison Gang Members", value = T),
      checkboxInput('bradh', "Include Mental Health Evaluated", value = T)
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)

#break out by different inputs
server <- function(input, output, session) {
  dic.summary <- data.frame(a = c("Your version", "3300"), b = c(nrow(dic), 3300))
  
  output$hist <- renderPlot({
    ggplot(dic.summary, aes(x=a, y=b)) +
      geom_col() + 
      theme_minimal()
  }, res = 96)
}

shinyApp(ui, server)