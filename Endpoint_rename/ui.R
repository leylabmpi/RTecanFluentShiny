# Shiny UI
library(shiny)


#-- UI --#
shinyUI(fluidPage(
  pageWithSidebar(
    titlePanel("qPCR Endpoint rename"),
    sidebarPanel( width = 3,      
      fileInput("endpoint_file", 
                label = "An 'Endpoint' table exported from the BioRad qPCR software (Excel, csv, or txt)",
                multiple = TRUE,
                placeholder='No file(s) selected'),
      textInput('sheet_name_endpoint', 
                label = 'Sheet name (if excel)?',
                value = 'SYBR'),
      hr(),
      fileInput("map_file", 
                label = "The mapping file containing the locations of each sample (Excel, csv, or txt)"),
      textInput('sheet_name_map', 
                label = 'Sheet name (if excel)?',
                value = 'Sheet1'),
      hr(),
      numericInput("RFU_cutoff",
                   label = "RFU cutoff for calling successful PCRs",
                   value = 1000)
      # need to add method for selecting 'pass' wells & 'PCR blank' wells
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Raw Endpoint", DT::dataTableOutput('endpoint_dt')),
                  tabPanel("Raw Mapping", DT::dataTableOutput('map_dt')),
                  tabPanel("Joined table", DT::dataTableOutput('end_map_dt'))
      )
    )
  )
))
