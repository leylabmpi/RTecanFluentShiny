# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
source("../utils/io.R")
source("../utils/format.R")


samples_convert = function(df, column_idx, plate_type){
  column_idx = column_idx %>% as.character
  df = df %>% as.data.frame
  if(class(df[,column_idx]) == 'character'){
    f = well2index
  } else{
    f = index2well
  }
  df[,column_idx] = sapply(df[,column_idx], f, plate_type=plate_type)
  return(df)
}


#-- server --#
shinyServer(function(input, output, session) {
  
  # loading samples file
  samples = eventReactive(input$sample_file,{
    infile = rename_tmp_file(input$sample_file)
    read_tbl(infile, sheet_name=input$sheet_name)
  })
  
  # making pairwise barcodes with position info
  samples_converted = reactive({
    samples_convert(samples(), 
                    column_idx=input$column_idx,
                    plate_type=input$plate_type)
  })
  
  #--- Rendering ---#
  # rendering example samples input
  output$conv_table = DT::renderDataTable(
    samples_converted(),
    extensions = c('Buttons'),
    options = list(
      pageLength = 10000,
      dom = 'Brt',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
})

