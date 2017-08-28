# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
source("../utils/io.R")
source("../utils/format.R")

#' long table of well IDs
wells = function(){
  cols = as.character(1:12)
  rows = LETTERS[1:8]
  df = apply(expand.grid(rows, cols), 1, paste, sep='', collapse='')
  df = matrix(as.vector(df), nrow=8, ncol=12) %>% data.frame
  colnames(df) = 1:ncol(df)
  df$row_ID = 1:nrow(df)
  df = df %>%
    gather(col_ID, location, -row_ID) %>%
    mutate(col_ID = col_ID %>% as.Num,
           row_ID = row_ID %>% as.Num) 
  # converting well (location) to numeric
  ## column-wise
  df %>%
    arrange(col_ID, row_ID) %>%
    mutate(location = 1:nrow(.))
}


#' adding plate & well location information (96-well)
add_location = function(barcodes){
  # skip adding location info if already present
  if('location' %in% colnames(barcodes)){
    barcodes = barcodes %>%
      mutate(location = well2index(barcodes$location)) %>%
      arrange(location)
    return(barcodes)
  }
  # data.frame of well locations
  df_wells = wells()
  
  # forward
  fp = barcodes %>% 
    filter(tolower(direction) == 'forward')
  ## adding well information
  if(nrow(fp) > 0){
    fp$row_ID = rep(seq(1,8), 20)[1:nrow(fp)]
    fp = fp %>%
      left_join(df_wells, c('row_ID')) %>%
      dplyr::select(-row_ID, -col_ID) %>%
      arrange(location)
  } 
  
  # reverse
  rp = barcodes %>%
    filter(tolower(direction) == 'reverse')
  ## adding well information
  if(nrow(rp) > 0){
    rp$col_ID = rep(seq(1,12), 20)[1:nrow(rp)]
    rp = rp %>%
      left_join(df_wells, c('col_ID')) %>%
      dplyr::select(-row_ID, -col_ID) %>%
      arrange(location)
  } 
  
  # return
  if(nrow(fp) > 0 & nrow(rp) > 0){
    barcodes = rbind(fp, rp) 
  } else 
  if(nrow(fp) > 0){
    barcodes = fp
  } else
  if(nrow(rp) > 0){
    barcodes = rp
  } else {
    barcodes = NULL
  }
  return(barcodes)
}

#' making pairwise table of all barcodes
#' Input: data.frame of barcodes
#' Output: table listing barcodes & plate locations
pairwise_barcodes = function(barcodes, join_id = NULL){
  # parsing by barcode
  barcodes = barcodes %>%
    dplyr::select(-sequence)
  fp = barcodes %>% 
    filter(tolower(direction) == 'forward') %>%
    dplyr::select(-direction) 
  rp = barcodes %>% 
    filter(tolower(direction) == 'reverse') %>%
    dplyr::select(-direction) 
  
  # joining 
  if(is.null(join_id)){
    df = full_join(fp %>% mutate(JOIN_ID = 1), 
                   rp %>% mutate(JOIN_ID = 1), 
                   c(join_id), suffix = c('_F', '_R')) %>%
        df %>% dplyr::select(-JOIN_ID)
  } else {
    df = full_join(fp, rp, c(join_id), suffix = c('_F', '_R'))
  }
  
  # spliting location info
  n_dir = barcodes$direction %>% unique %>% length
  if(n_dir == 1){
  } else
  if(n_dir == 2){
    df = df %>% 
      mutate(location_F = location,
            location_R = location) %>%
      dplyr::select(-location) 
    ## column order
    df = cbind(df %>% dplyr::select(-name_F, -index_F,
                                    -labware_F, -location_F,
                                    -name_R, -index_R,
                                    -labware_R, -location_R),
               df %>% dplyr::select(name_F, index_F,
                                    labware_F, location_F,
                                    name_R, index_R,
                                    labware_R, location_R))
  } else {
    stop('Too many barcode directions!')
  }
           
  # arranging & formatting 
  df = df %>%
    arrange(labware_F, labware_R) 
  
  # renaming columns
  colnames(df) = gsub('^', 'TECAN_primer_', colnames(df))
  
  # fitlering if columns are all NAs
  df = df[,colSums(is.na(df)) < nrow(df)]
  
  # renaming columns (if only forward primers left)
  if(!any(startsWith(colnames(df), 'TECAN_') & 
          endsWith(colnames(df), '_R'))){
    colnames(df) = gsub('(TECAN.+)_F$', '\\1', colnames(df))
  }
  
  # return
  return(df)
}

#' adding barcodes + location information to sample file
add_barcodes = function(samples, pw_barcodes, barcode_start=1){
  # filtering
  n_samples = nrow(samples)
  barcode_end = barcode_start + n_samples - 1
  idx = rep(1:nrow(pw_barcodes), 2)[barcode_start:barcode_end]
  pw_barcodes = pw_barcodes[idx,]
  
  if(nrow(samples) != nrow(pw_barcodes)){
    stop('Cannot join samples & barcodes')
  }
  
  # joining
  cbind(samples, pw_barcodes)
}

#-- server --#
shinyServer(function(input, output, session) {
  # loading barcodes
  raw = eventReactive(input$barcode_type, {
    read_excel('./data/barcodes.xlsx', 
               sheet=input$barcode_type)
  })
  
  # loading samples file
  samples = eventReactive(input$sample_file,{
    infile = rename_tmp_file(input$sample_file)
    read_tbl(infile, sheet_name=input$sheet_name)
  })
  
  # making pairwise barcodes with location info
  pw_loc_barcodes = reactive({
    pairwise_barcodes(add_location(raw()), 
                      join_id='location')
  })
  
  sample_barcodes = reactive({
    add_barcodes(samples(), 
                 pw_loc_barcodes(),
                 input$barcode_start)
  })
  
  
  #--- tables/plots ---#
  # rendering barcodes (raw)
  output$raw_tbl = DT::renderDataTable(
    raw(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 48,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # rendering samples table
  output$samples_tbl = DT::renderDataTable(
    samples(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 50,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # rendering pairwise barcodes with location info
  output$pw_loc_barcodes_tbl = DT::renderDataTable(
    pw_loc_barcodes(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 48,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # rendering samples + barcodes
  output$sample_barcodes_tbl = DT::renderDataTable(
    sample_barcodes(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 48,
      dom = 'Blfrtip',
      buttons = c('colvis', 'copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
})

