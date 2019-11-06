# Shiny UI
library(shiny)

#-- shiny --#
shinyUI(
  navbarPage("PCR success/fail",
    tabPanel("Raw data",
          tabsetPanel(
            tabPanel("PCR plate 1",
              textAreaInput("pcr_plate_1", 
                            "Paste the endRFU table here (tab-delimited)", 
                            "", 
                            width = "700px",
                            height = "500px")
            ),
            tabPanel("PCR plate 2",
                     textAreaInput("pcr_plate_2", 
                                   "Paste the endRFU table here (tab-delimited)", 
                                   "", 
                                   width = "700px",
                                   height = "500px")
            ),
            tabPanel("PCR plate 3",
                     textAreaInput("pcr_plate_3", 
                                   "Paste the endRFU table here (tab-delimited)", 
                                   "", 
                                   width = "700px",
                                   height = "500px")
            ),
            tabPanel("Combined table", DT::dataTableOutput('raw_endRFU'))
          )
    ),
    tabPanel("Filtering", 
      sidebarLayout(
        sidebarPanel(
           h5("Either manually set an endRFU cutoff or determine from negative controls"),
           numericInput("endRFU_cutoff",
                        label = "Manual cutoff for min. endRFU to consider a 'success'",
                        value = 40), 
           textInput("neg_ctrl_regex",
                     label = "Search pattern to ID negative control PCRs", 
                     value = "plate[0-9]_blank"),
           numericInput("neg_ctrl_std",
                        label = "Setting min. endRFU: number of stdevs above mean endRFU for neg. controls",
                        value = "2"),
           numericInput("min_num_success",
                        label = "Min. number of successful PCR replicates",
                        value = 2), 
           hr(),
           textInput("labware_type",
                     label = "PCR labware type", 
                     value = "96 Well Eppendorf TwinTec PCR"),
           width=3
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Negative controls", 
                     verbatimTextOutput("min_RFU_txt"),
                     hr(),
                     DT::dataTableOutput('raw_neg_cntl')),
            tabPanel("Filtered table", 
                     verbatimTextOutput("num_suc_txt"),
                     verbatimTextOutput("num_suc_samp_txt"),
                     hr(),
                     DT::dataTableOutput('filt_endRFU'))
          )
        )
      )
    )
  )
)
