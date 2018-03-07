# Shiny UI
library(shiny)



#-- UI --#
shinyUI(fluidPage(
  pageWithSidebar(
    titlePanel("Convert well index"),
    sidebarPanel( width = 4,    
      fileInput("sample_file", 
                label = "A table listing samples. One sample per row. (Excel or CSV)"),
      textInput('sheet_name', 
                label = 'Sheet name (if excel)?',
                value = 'Sheet1'),
      textInput('column_idx',
                label = 'Column name/number to convert',
                value = 'TECAN_sample_target_position'),
      selectInput('plate_type',
                  label = "Labware plate type (# of wells)",
                  choices = c('96-well' = '96-well',
                              '384-well' = '384-well'),
                  selected = '96-well')
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Converted", DT::dataTableOutput('conv_table'))
      )
    )
  )
))
