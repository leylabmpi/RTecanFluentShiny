# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
source("../utils/io.R")

#' loading example input table file for Step2 PCR
load_ex_data_file = function(F){
  print(F)
  df = read.delim(F, sep=',')
  return(df)
}

#-- server --#
shinyServer(function(input, output, session) {

  
  # example data tableread
  output$example1_tbl = DT::renderDataTable(
    load_ex_data_file('./data/example1.csv'),
    extensions = c('Buttons'),
    rownames = FALSE,
    options = list(
      pageLength = 40,
      dom = 'Brt',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  output$example2_tbl = DT::renderDataTable(
    load_ex_data_file('./data/example2.csv'),
    extensions = c('Buttons'),
    rownames = FALSE,
    options = list(
      pageLength = 40,
      dom = 'Brt',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )

})

