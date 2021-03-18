# Shiny UI
library(shiny)


#-- UI --#
shinyUI(fluidPage(
  pageWithSidebar(
    titlePanel("Plate-table <--> Long-table"),
    sidebarPanel(width = 3, 
                 h4('Input'),
                 selectInput('plate_or_long',
                             label = 'Plate or long table format?',
                             choices = c('Plate' = 'Plate',
                                         'Long' = 'Long'),
                             selected = 'Plate'),
                 h4('Output'),
                 selectInput('plate_type',
                             label = "Which plate type?",
                             choices = c('96-well' = '96-well',
                                           '384-well' = '384-well'),
                             selected = '96-well'),
                 checkboxInput('add_well_ID',
                               label = "Add well ID?",
                               value = FALSE)
                ),
    mainPanel(
      h4("Convert table between labware plate format and a long table format"),
      tabsetPanel(type = "tabs", 
                  tabPanel("Input", 
                           textAreaInput("input_text", 
                                         "Paste your data into this box (tab-delimited)", 
                                         "1	9	17\n2	10	18\n3	11	19\n4	12	20\n5	13	21\n6	14	22\n7	15	23\n8	16	24", 
                                         width = "1000px",
                                         height = "300px")
                           ),
                  tabPanel("Output table", 
                           DT::dataTableOutput('tbl'))
      )
    )
  )
))

