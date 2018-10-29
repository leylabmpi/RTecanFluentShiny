# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
source("../utils/io.R")
source("../utils/format.R")


well_convert = function(x, conv_dir, plate_type){
  x = read.delim(text=x, sep='\t', header=FALSE)[,1]
  x = as.character(x)
  if(conv_dir == 'well2index'){
    y = well2index(x, plate_type)
  } else
  if(conv_dir == 'index2well'){
    y = index2well(x, plate_type)
  } else{
    y = x 
  }
  df = data.frame(converted = y)
  rownames(df) = x
  return(df)
}


#-- server --#
shinyServer(function(input, output, session) {
  tbl = reactive({
    x = NULL
    if(input$conv_direction == 'WellID --> Well#'){
      x = well_convert(input$input_text, 'well2index', plate_type=input$plate_type)
    } else if(input$conv_direction == 'Well# --> WellID'){
      x = well_convert(input$input_text, 'index2well', plate_type=input$plate_type)
    } 
    return(x)
  })
  
  output$tbl = DT::renderDataTable(
    tbl(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 96,
      lengthMenu = c(48, 96, 384, 1536),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
})