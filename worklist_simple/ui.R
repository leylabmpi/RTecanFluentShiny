# Shiny UI
library(shiny)


#-- UI --#
shinyUI(fluidPage(
  fluidPage(
    titlePanel("Worklist (simple)"),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Description", 
                           fluidRow(
                             column(12,
                                    h5('This app provides guidelines for creating the input table for the "Worklist-simple" TECAN robot method'),
                                    br(),
                                    h5('The input table should be a csv (comma-delimited) file, where the first 5 columns are:'),
                                    tags$ul(
                                      tags$li('"Source Labware Label"'),
                                      tags$li('"Source Position"'),
                                      tags$li('"Destination Labware Label"'),
                                      tags$li('"Destination Position"'),
                                      tags$li('"Volume"')
                                      ),
                                    br(),
                                    h5('Possible labware IDs:'),
                                    tags$ul(
                                      tags$li('"96 Well Eppendorf TwinTec PCR[XXX]"'),
                                      tags$ul(
                                        tags$li('[XXX] can be up to [006]')
                                      ),
                                      tags$li('"384 Well Biorad PCR[XXX]"'),
                                      tags$ul(
                                        tags$li('[XXX] can be up to [003]')
                                      ),
                                      tags$li('"100ml[001]"'),
                                      tags$li('"25ml[001]"'),
                                      tags$li('"Eppendorf1p5ml[XXX]"'),
                                      tags$ul(
                                        tags$li('[XXX] can be up to [048]')
                                      ),
                                      tags$li('"Eppendorf2p0ml[XXX]"'),
                                      tags$ul(
                                        tags$li('[XXX] can be up to [048]')
                                      ),
                                      tags$li('"Eppendorf5ml[XXX]"'),
                                      tags$ul(
                                        tags$li('[XXX] can be up to [008]')
                                      ),
                                      tags$li('"G10ml_Falcon[XXX]"'),
                                      tags$ul(
                                        tags$li('[XXX] can be up to [012]')
                                      )
                                    ),
                                    br(),
                                    h5('Notes:'),
                                    tags$ul(
                                      tags$li('96-well plates and 384-well plates must be placed on metal adapters')
                                    )
                                      
                             )
                           )
                  ),
                  tabPanel('Example: many-to-one', 'Pooling samples from multiple plates into 1 Eppendorf tube', br(), br(), 
                           DT::dataTableOutput('example1_tbl')),
                  tabPanel('Example: one-to-one', 'Combining samples from multiple plates into 1 plate', br(), br(), 
                           DT::dataTableOutput('example2_tbl'))
      )
    )
  )
))
