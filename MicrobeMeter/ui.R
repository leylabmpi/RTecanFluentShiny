# Shiny UI
library(shiny)



#-- UI --#
shinyUI(fluidPage(
  pageWithSidebar(
    titlePanel("MicrobeMeter"),
    sidebarPanel(width = 3, 
                 selectInput('time_unit',
                             label = 'Time unit for the x-axis',
                             choices = c('hours' = 'hours',
                                         'minutes' = 'minutes',
                                         'seconds' = 'seconds'),
                             selected = 'minutes'),
                 numericInput('window_size',
                              label = 'Window size of moving average (use "0" if no window)',
                              value = 0,
                              min = 0,
                              max = 999)
    ),
    mainPanel(
      h4("Convert table between labware plate format and a long table format"),
      tabsetPanel(type = "tabs", 
                  tabPanel("Input", 
                           textAreaInput("input_text", 
                                         "Paste your MicrobeMeter output table into this box (tab-delimited)", 
                                         width = "1000px",
                                         height = "300px")
                           ),
                  tabPanel("Calculated turbidity", 
                           DT::dataTableOutput('turbidity_tbl')),
                  tabPanel("Example input", 
                           DT::dataTableOutput('ex_data'))
      )
    )
  )
))

