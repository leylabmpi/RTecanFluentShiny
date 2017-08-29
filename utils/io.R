library(readxl)

# i/o utility functions 
#' remaming temp file produced by fileInput (adding back file extension)
#' select_input_obj = object generated from shiny::selectInput()
rename_tmp_file = function(select_input_obj){
  file_ext = gsub('.+(\\.[^.])$', '\\1', select_input_obj$name) 
  new_file = paste0(select_input_obj$datapath, file_ext)
  file.copy(select_input_obj$datapath, new_file, overwrite=TRUE)
  return(new_file)
}


#' reading in table (excel or csv)
read_tbl = function(file_name, sheet_name='Sheet1', header=TRUE, 
                    check.names=FALSE, comment.char='~', ...){
  if(endsWith(file_name, '.txt')){
    df = read.table(file_name, sep='\t', header=header, 
                    check.names=check.names, 
                    comment.char=comment.char, ...)
  } else
  if(endsWith(file_name, '.csv')){
    df = read.table(file_name, sep=',', header=header, 
                    check.names=check.names, 
                    comment.char=comment.char, ...)
  } else
  if(endsWith(file_name, '.xls') | endsWith(file_name, 'xlsx')){
    df = read_excel(file_name, sheet=sheet_name, ...)
  } else{
    stop(paste0('Cannot determine format of file: ', file_name))
  }
  return(df)
}
