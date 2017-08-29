# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
source("../utils/io.R")
source("../utils/format.R")

#   algorithm
#   ---------
#   * load
#     * Endpoint file
#     * Mapping file from map2robot
#   * join tables by location
#     * For endpoint file: convert "Well" to location
#     * inner_join(endpoint, mapping)
#   * call PCR success/failure/pass based on RFU values
#     * Place in "Call" column
#     * User defined: "pass" (failures to be sequenced)
#     * Optional: user-defined End RFU cutoff
#   * output
#     * write an updated Endpoint file w/ samples & successes/failures/passes
#       * also, locations are edited
#     * write a (dereplicated) truncated mapping file of PCR failures
#       * this can be used for subsequent PCR attempts

#' Joining endpoint & mapping tables.
#' Joining based on 'location' 
join_end_map = function(df_end, df_map){
  # checking for needed columns
  if(is.null(df_end$Well)){
    stop('Cannot find "Well" column in Endpoint file')
  }
  if(is.null(df_map$TECAN_dest_location)){
    stop('Cannot find "TECAN_dest_location" in mapping file')
  }
  if(is.null(df_map$"#SampleID")){
    stop('Cannot find "#SampleID" in mapping file')
  }
  
  # determe plate type
  if(all(c('A9', 'H13') %in% df_end$Well)){
    well_index = well96_index()
  } else {
    well_index = well384_index()
  }
  df_end$Location = well_index[gsub('0([0-9])$', '\\1', df_end$Well)]
  
  # temp renaming endpoint columns to avoid naming collision
  colnames(df_end) = gsub('^', 'QPCR_ENDPOINT_', colnames(df_end))
  
  # filtering mapping file
  df_end = df_end %>%
    left_join(df_map, c('QPCR_ENDPOINT_Location'='TECAN_dest_location'))
  df_end$QPCR_ENDPOINT_Sample = df_end$"#SampleID"
  return(df_end)
}

#-- server --#
shinyServer(function(input, output, session) {
  # loading endpoint file
  endpoint_tbl = eventReactive(input$endpoint_file,{
    infile = rename_tmp_file(input$endpoint_file)
    read_tbl(infile, sheet_name=input$sheet_name_endpoint)
  })

  # loading mapping file
  map_tbl = eventReactive(input$map_file,{
    infile = rename_tmp_file(input$map_file)
    read_tbl(infile, sheet_name=input$sheet_name_map)
  })
  
  # joining tables
  end_map_tbl = reactive({
    join_end_map(endpoint_tbl(), map_tbl())
  })
  
  
  
  #--- Rendering ---#
  
  # rendering table of raw endpoints
  output$endpoint_dt = DT::renderDataTable(
    endpoint_tbl(),
    extensions = c('Buttons'),
    options = list(
      pageLength = 50,
      dom = 'Brt',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  # rendering raw mapping file
  output$map_dt = DT::renderDataTable(
    map_tbl(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 40,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  # rendering joined map-endpoint file
  output$end_map_dt = DT::renderDataTable(
    end_map_tbl(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 40,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )

})

