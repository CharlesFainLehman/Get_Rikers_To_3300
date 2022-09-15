library(shiny)
library(dplyr)
library(ggplot2)

#there's a trailing ' before all the codes
#so that excel doesn't screw it up every time I open it
#Yes, I know, a mess
charges <- read.csv("dat/charge features.csv") %>% mutate(Code = gsub("'", "", Code))
dic <- left_join(read.csv("dat/Daily_Inmates_In_Custody.csv"), charges, by = c('TOP_CHARGE' = 'Code')) %>%
  mutate(RACE = recode(RACE, "O" = "Other", "B" = "Black", "W" = "White", "I" = "Indian", "A" = "Asian", "U" = "Unknown"),
         GENDER = recode(GENDER, "M" = "Male", "F" = "Female"),
         AGE = ifelse(is.na(AGE), -1, AGE),
         INMATE_STATUS_CODE = recode(INMATE_STATUS_CODE, "CS" = "City Sentenced", "CSP" = "City Sentenced", "DE" = "Pretrial Detainee", "DEP" = "Pretrial Criminal Parole", "DNS" = "State Sentenced", "DPV" = "Pretrial Technical Parole", "SCO"= "State Sentenced", "SSR" = "State Sentenced"),
         FMV = ifelse(is.na(FMV), "None", recode(FMV, "F" = "Felony", "M" = "Misdemeanor", "V" = "Violation")),
         Type = as.character(Type),
         Type = ifelse(is.na(Type), "None", as.character(Type)),
         CUSTODY_LEVEL = recode(CUSTODY_LEVEL, "MIN" = "Minimum", "MED" = "Medium", "MAX" = "Maximum"))
todays.pop <- nrow(dic)

print("Unique Types in Charges: ")
pull(charges, Type) %>% unique() %>% print()
print("Unique Types in dic: ")
pull(dic, Type) %>% unique() %>% print()

ui <- fluidPage(
  titlePanel("Can You Get to 3,300?"),
  fluidRow(
    column(3, 
           wellPanel(
             selectInput("sentence", "Sentence", c("City Sentenced", "Pretrial Detainee", "Pretrial Criminal Parole", "Pretrial Technical Parole", "State Sentenced"), multiple = T, selected = c("City Sentenced", "Pretrial Detainee", "Pretrial Criminal Parole", "Pretrial Technical Parole", "State Sentenced")),
             selectInput('fmv', "", c("Felony", "Misdemeanor", "Violation"), multiple = T, selected = c("Felony", "Misdemeanor", "Violation")),
             selectInput('offense', "Offense", c("Narcotics", "Sex Offense", "Violent", "Weapons", "Vehicle", "Misc", "Property"), multiple = T, selected = c("Narcotics", "Sex Offense", "Violent", "Weapons", "Vehicle", "Misc", "Property")),
             selectInput('custody', "Custody Level", c("Minimum", "Medium", "Maximum"), multiple = T, selected = c("Minimum", "Medium", "Maximum"))
             )
           ),
    column(6,
      plotOutput("hist")
    ),
    column(3, 
           wellPanel(
             selectInput('race', "Race", c("Asian", "Black", "Indian", "Other", "Unknown", "White"), multiple = T, selected = c("Asian", "Black", "Indian", "Other", "Unknown", "White")),
             selectInput('sex', "Sex", c("Male", "Female"), multiple = T, selected = c("Male", "Female")),
             sliderInput('age', "Age", min = 0, max = 150, c(0, 150)),
             checkboxInput('srg', "Include Prison Gang Members", value = T),
             checkboxInput('bradh', "Include Mental Health Evaluated", value = T),
           )
    )
    )
  )

server <- function(input, output, session) {
  
  dic.summary <- reactive({

    new.length <- dic %>%
      filter(RACE %in% c(input$race, ""),
             GENDER %in% c(input$sex, ""),
             AGE %in% c(input$age[1]:input$age[2], -1),
             INMATE_STATUS_CODE %in% c(input$sentence, ""),
             FMV %in% c(input$fmv, "None"),
             Type %in% c(input$offense, "None"),
             CUSTODY_LEVEL %in% c(input$custody, ""),
             if(input$srg == F) {SRG_FLG != "Y"} else {SRG_FLG %in% c("Y", "N", "")},
             if(input$bradh == F) {BRADH != "Y"} else {BRADH %in% c("Y", "N", "")}
             ) %>%
      nrow()
    
    data.frame(version = factor(c("Today's Population", "Your version", "3,300"),
                          levels = c("Today's Population", "Your version", "3,300")),
               count = c(todays.pop, new.length, 3300))
  })
  
  output$hist <- renderPlot({
    ggplot(dic.summary(), aes(x=version, y=count)) +
      geom_col(fill = "#55A5DA") + 
      geom_text(aes(label = scales::comma(count, accuracy = 1), y = count + 120)) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            axis.title = element_blank()) 
  }, res = 96)
}

shinyApp(ui, server)