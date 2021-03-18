# Shiny server
library(shiny)
library(tidyr)
source("../utils/io.R")
source("../utils/format.R")

plate2long = function(x, plate_type = '96-well', add_well_ID=TRUE){
  y = read.delim(text=x, sep='\t', header=FALSE)
  y = as.data.frame(as.vector(as.matrix(y)))
  colnames(y) = c('long_format')
  if(add_well_ID == TRUE){
    if(plate_type == '96-well'){
      y$well = index2well(well96_index()[1:nrow(y)])
    } else 
    if(plate_type == '384-well'){
      y$well = index2well(well384_index()[1:nrow(y)])
    }
    y = y[,c('well', 'long_format')]
  }
  return(y)
}

long2plate = function(x, plate_type = '96-well'){
  y = read.delim(text=x, sep='\t', header=FALSE)
  n_rows_per_column = ifelse(plate_type == '96-well', 8, 16)
  y = y %>%
    unite_('plate_format', colnames(.), sep=';')
  
  n_col = ceiling(nrow(y) / n_rows_per_column)
  y = as.vector(as.matrix(y))
  y = matrix(y, nrow=n_rows_per_column, ncol=n_col)
  y = as.data.frame(y)
  colnames(y) = as.character(1:n_col)
  rownames(y) = LETTERS[1:n_rows_per_column]
  
  return(y)
}

#-- server --#
shinyServer(function(input, output, session) {
  tbl = reactive({
    x = NULL
    if(input$plate_or_long == 'Plate'){
      x = plate2long(input$input_text, input$plate_type, input$add_well_ID)
    } else if(input$plate_or_long == 'Long'){
      x = long2plate(input$input_text, plate_type=input$plate_type)
    } 
    return(x)
  })
  
  output$tbl = DT::renderDataTable(
    tbl(),
    filter = 'bottom',
    extensions = c('Buttons'),
    rownames = FALSE,
    options = list(
      pageLength = 384,
      lengthMenu = c(48, 96, 384, 1536),
      dom = 'Blfrtip',
      buttons = list(
        list(extend = "copy", title = NULL), 
        'csv', 
        list(extend = 'excel', title = NULL),
        'pdf', 
        'print'
      )
    )
  )
})

