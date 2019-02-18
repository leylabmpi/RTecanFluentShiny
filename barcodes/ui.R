# Shiny UI
library(shiny)



#-- UI --#
shinyUI(fluidPage(
  pageWithSidebar(
    titlePanel("Barcodes"),
    sidebarPanel( width = 3,
      h4('View NGS barcodes & map them to samples'),
      br(),
      selectInput('barcode_type',
                  label = "Which barcodes?",
                  choices = c('Nextera N7-S5 (HiSeq)' = 'N7-S5_HiSeq',
                              'Nextera N7-S5 (MiSeq)' = 'N7-S5_MiSeq',
                              'Caporaso 515FB' = '515FB'),
                  selected = 'S5-N7'),
      h5('NOTE: the S5 barcode is reverse-complemented for MiSeq versus HiSeq'),
      hr(),
      numericInput("barcode_start",
                   label = "Starting sample barcode",
                   value = 1)
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Barcodes", DT::dataTableOutput('raw_tbl')),
                  tabPanel("Pairwise barcodes", DT::dataTableOutput('pw_loc_barcodes_tbl')),
                  tabPanel("Samples", 
                    h4('Paste in a tab-delimited table of your samples if you would like to match barcodes to it'),
                    h4('If you want to assign NEW barcodes to your samples, include the following columns:'),
                    tags$ul(
                      tags$li('"Sample" = the unique IDs of your samples')
                      ),
                    h4('If you need to get barcode sequences based on primer_plate_IDs + primer_well_IDs, include the following columns:'),
                    tags$ul(
                      tags$li('"Sample" = the unique IDs of your samples'),
                      tags$li('"TECAN_primer_labware_name" = the primer labware name (this MUST match the plate name in the barcodes table)'),
                      tags$li('"TECAN_primer_target_position" = the target position of the primer')
                    ),
                    textAreaInput("Samples", 
                                  "Paste your Samples table here", 
                                  "", 
                                  width = "900px",
                                  height = "500px")
                  ),
                  tabPanel("Samples + barcodes", DT::dataTableOutput('sample_barcodes_tbl')),
                  tabPanel("Example 'Samples' input", DT::dataTableOutput('ex_samples_tbl'))
      )
    )
  )
))
