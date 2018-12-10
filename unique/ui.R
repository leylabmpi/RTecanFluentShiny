# Shiny UI
library(shiny)



#-- UI --#
shinyUI(fluidPage(
  pageWithSidebar(
    titlePanel("Uniques"),
    sidebarPanel(width = 3, 
                 numericInput('unique_col',
                              label='Column number for selecting uniques',
                              value=1, 
                              max=1)
                ),
    mainPanel(
      h4("Get just unique values"),
      tabsetPanel(type = "tabs", 
                  tabPanel("Input", 
                           textAreaInput("input_text", 
                                         "Paste your data into this box (tab-delimited)", 
                                         "#Example data\nA	9	17\nA	10	18\nB	11	19\nC	1	9\n", 
                                         width = "1000px",
                                         height = "300px")
                           ),
                  tabPanel("Uniques", verbatimTextOutput('raw_txt')),
                  tabPanel("Output table", DT::dataTableOutput('tbl'))
      )
    )
  )
))

