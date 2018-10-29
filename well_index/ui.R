# Shiny UI
library(shiny)



#-- UI --#
shinyUI(fluidPage(
  pageWithSidebar(
    titlePanel("Well ID (eg., 'A1' or 'C5') <--> Well number (eg., '1' or '10')"),
    sidebarPanel(width = 3, 
                 selectInput('conv_direction',
                             label = 'WellID --> Well# or Well# --> WellID',
                             choices = c('WellID --> Well#' = 'WellID --> Well#',
                                         'Well# --> WellID' = 'Well# --> WellID'),
                             selected = 'WellID --> Well#'),
                 selectInput('plate_type',
                             label = 'Plate type',
                             choices = c('96-well' = '96-well',
                                         '384-well' = '384-well'),
                             selected = '96-well')
    ),
    mainPanel(
      h4("Convert between wellID and well#"),
      h4("Note: well numbering is column-wise"),
      tabsetPanel(type = "tabs", 
                  tabPanel("Input", 
                           textAreaInput("input_text", 
                                         "Paste your data into this box (tab-delimited)", 
                                         "A1\nA2\nC3\nE5", 
                                         width = "1000px",
                                         height = "300px")
                  ),
                  tabPanel("Output table", 
                           DT::dataTableOutput('tbl'))
      )
    )
  )
))

