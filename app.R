library(shiny)
library(dplyr)
library(ggplot2)

#there's a trailing ' before all the codes
#so that excel doesn't screw it up every time I open it
#Yes, I know, a mess
charges <- read.csv("dat/charge features.csv") %>% mutate(Code = gsub("'", "", Code))
dic <- read.csv("dat/Daily_Inmates_In_Custody.csv") %>% 
  rename_with(tolower) %>% #uh sometimes DOC changes whether the variables are upper or lower case? Dumb!
  left_join(charges, by = c('top_charge' = 'Code')) %>%
  mutate(race = recode(race, "O" = "Other", "B" = "Black", "W" = "White", "I" = "Indian", "A" = "Asian", "U" = "Unknown"),
         gender = recode(gender, "M" = "Male", "F" = "Female"),
         age = as.numeric(age),
         age = ifelse(is.na(age), -1, age),
         inmate_status_code = recode(inmate_status_code, "CS" = "City Sentenced", "CSP" = "City Sentenced", "DE" = "Pretrial Detainee", "DEP" = "Pretrial Criminal Parole", "DNS" = "State Sentenced", "DPV" = "Pretrial Technical Parole", "SCO"= "State Sentenced", "SSR" = "State Sentenced"),
         FMV = as.character(FMV),
         FMV = ifelse(is.na(FMV), "None", recode(FMV, "F" = "Felony", "M" = "Misdemeanor", "V" = "Violation")),
         Type = as.character(Type),
         Type = ifelse(is.na(Type), "None", Type),
         custody_level = recode(custody_level, "MIN" = "Minimum", "MED" = "Medium", "MAX" = "Maximum"))
todays.pop <- nrow(dic)

ui <- fluidPage(
    column(12, 
           plotOutput("hist"),
           hr(),
           h4("Filters:"),
           tabsetPanel(
               tabPanel("Charge",
                        br(),
                 checkboxGroupInput('fmv', "Charge Severity", c("Felony", "Misdemeanor", "Violation"), inline = T,   selected = c("Felony", "Misdemeanor", "Violation")),
                 checkboxGroupInput('offense', "Charge Type", c("Narcotics", "Sex Offense", "Violent", "Weapons", "Vehicle", "Misc", "Property"), inline = T, selected = c("Narcotics", "Sex Offense", "Violent", "Weapons", "Vehicle", "Misc", "Property"))
                 ),
               tabPanel("Detention", 
                        br(),
                 checkboxGroupInput("sentence", "Detention Status", c("City Sentenced", "Pretrial Detainee",   "Pretrial Criminal Parole", "Pretrial Technical Parole", "State Sentenced"), inline = T,   selected = c("City Sentenced", "Pretrial Detainee", "Pretrial Criminal Parole", "Pretrial Technical Parole", "State Sentenced")),
                 checkboxGroupInput('custody', "Custody Level", c("Minimum", "Medium", "Maximum"), inline = T, selected = c("Minimum", "Medium", "Maximum"))
               ),
               tabPanel("Demographics", 
                        br(),
                 checkboxGroupInput('race', "Race", c("Asian", "Black", "Indian", "Other", "Unknown", "White"), selected = c("Asian", "Black", "Indian", "Other", "Unknown", "White"), inline = T),
                 checkboxGroupInput('sex', "Sex", c("Male", "Female"), selected = c("Male", "Female"), inline = T),
                 sliderInput('age', "Age", min = 0, max = 150, c(0, 150))
                 ),
               tabPanel("Misc.", 
                        br(),
                 checkboxInput('bradh', "Mental Health Evaluated", value = T),
                 checkboxInput('srg', "Prison Gang Members", value = T) 
               )
           ),
           hr(),
           tags$em("Notes: Data reflects yesterday's population, are updated daily. Data do not distinguish Hispanic ethnicity.")
    )
)

server <- function(input, output, session) {
  
  dic.summary <- reactive({

    new.length <- dic %>%
      filter(race %in% c(input$race, ""),
             gender %in% c(input$sex, ""),
             age %in% c(input$age[1]:input$age[2], -1),
             inmate_status_code %in% c(input$sentence, ""),
             FMV %in% c(input$fmv, "None"),
             Type %in% c(input$offense, "None"),
             custody_level %in% c(input$custody, ""),
             if(input$srg == F) {srg_flg != "Y"} else {srg_flg %in% c("Y", "N", "")},
             if(input$bradh == F) {bradh != "Y"} else {bradh %in% c("Y", "N", "")}
             ) %>%
      nrow()
    
    data.frame(version = factor(c("Today's population", "Your version", "3,300"),
                          levels = c("Today's population", "Your version", "3,300")),
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

shinyApp(ui, server, options = list(port = 5649))