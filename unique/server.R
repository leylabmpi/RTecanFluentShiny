# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
source("../utils/io.R")
source("../utils/format.R")

uniques = function(x, unique_col=1){
  if(is.null(x)){
    return(NULL)
  }
  unique_col = paste0('V', unique_col)
  y = read.delim(text=x, sep='\t', header=FALSE, comment.char='#') %>%
      dplyr::distinct_(unique_col, .keep_all=TRUE)
  return(y)
}

#-- server --#
shinyServer(function(input, output, session) {
  tbl = reactive({
    uniques(input$input_text, unique_col=input$unique_col)
  })
  
  output$tbl = DT::renderDataTable(
    tbl(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 20,
      lengthMenu = c(20, 100, 500, 5000),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  output$raw_txt = renderText(paste(tbl()[,input$unique_col],
                                    collapse='\n'))
})

