# Shiny UI
library(shiny)



#-- UI --#
shinyUI(fluidPage(
  pageWithSidebar(
    titlePanel("Barcodes"),
    sidebarPanel( width = 3,  
      selectInput('barcode_type',
                  label = "Which barcodes?",
                  choices = c('Nextera N7-S5 (HiSeq)' = 'N7-S5_HiSeq',
                              'Nextera N7-S5 (MiSeq)' = 'N7-S5_MiSeq',
                              'Caporaso 515FB' = '515FB'),
                  selected = 'S5-N7'),
      h5('NOTE: the S5 barcode is reverse-complemented for MiSeq versus HiSeq'),
      hr(),
      fileInput("sample_file", 
                label = "A table listing samples. One sample per row. (Excel or CSV)"),
      textInput('sheet_name', 
                label = 'Sheet name (if excel)?',
                value = 'Sheet1'),
      hr(),
      numericInput("barcode_start",
                   label = "Starting sample barcode",
                   value = 1)
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Barcodes", DT::dataTableOutput('raw_tbl')),
                  tabPanel("Pairwise barcodes", DT::dataTableOutput('pw_loc_barcodes_tbl')),
                  tabPanel("Samples", DT::dataTableOutput('samples_tbl')),
                  tabPanel("Samples + barcodes", DT::dataTableOutput('sample_barcodes_tbl')),
                  tabPanel("Example samples input", DT::dataTableOutput('ex_samples_tbl'))
      )
    )
  )
))
