# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
source("../utils/io.R")
source("../utils/format.R")

#' example data
example_samples = function(){
  read_excel('./data/samples1.xlsx', sheet='Sheet1')
}

#' long table of well IDs
wells = function(){
  cols = as.character(1:12)
  rows = LETTERS[1:8]
  df = apply(expand.grid(rows, cols), 1, paste, sep='', collapse='')
  df = matrix(as.vector(df), nrow=8, ncol=12) %>% data.frame
  colnames(df) = 1:ncol(df)
  df$row_ID = 1:nrow(df)
  df = df %>%
    gather(col_ID, position, -row_ID) %>%
    mutate(col_ID = col_ID %>% as.Num,
           row_ID = row_ID %>% as.Num) 
  # converting well (location) to numeric
  ## column-wise
  df %>%
    arrange(col_ID, row_ID) %>%
    mutate(position = 1:nrow(.))
}


#' adding plate & well location information (96-well)
add_position = function(barcodes){
  # skip adding position info if already present
  if('position' %in% colnames(barcodes)){
    barcodes = barcodes %>%
      mutate(position = well2index(barcodes$position)) %>%
      arrange(position)
    return(barcodes)
  }
  # data.frame of well positions
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
      arrange(labware)
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
      arrange(labware)
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
#' Output: table listing barcodes & plate positions
pairwise_barcodes = function(barcodes, barcode_type, join_id = NULL){
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
  
  # spliting positions info
  n_dir = barcodes$direction %>% unique %>% length
  if(n_dir == 1){
  } else
  if(n_dir == 2){
    ## column order
    df = cbind(df %>% dplyr::select(-name_F, -index_F,
                                    -labware_F,
                                    -name_R, -index_R,
                                    -labware_R, -position),
               df %>% dplyr::select(name_F, index_F, labware_F, 
                                    name_R, index_R, labware_R, position)) %>%
      arrange(labware_F, labware_R, position) %>%
      mutate(labware_name = mapply(paste0, labware_F, labware_R) %>% as.factor %>% as.numeric,
             barcode_type = gsub('_(Mi|Hi)Seq$', '', barcode_type)) %>%
      unite(labware_name, barcode_type, labware_name)
  } else {
    stop('Too many barcode directions!')
  }
  
  # renaming columns
  colnames(df) = gsub('^', 'TECAN_primer_', colnames(df))
  
  # filtering if columns are all NAs
  df = df[,colSums(is.na(df)) < nrow(df)]
  
  # renaming columns (if only forward primers left)
  if(!any(startsWith(colnames(df), 'TECAN_') & 
          endsWith(colnames(df), '_R'))){
    colnames(df) = gsub('(TECAN.+)_F$', '\\1', colnames(df))
  }
  # renaming
  df = df %>%
    rename('TECAN_primer_target_position' = TECAN_primer_position) %>%
    mutate(primer_plate_well_ID = index2well(TECAN_primer_target_position))
  
  # return
  return(df)
}

#' adding barcodes + positions information to sample file
append_barcodes = function(samples, pw_barcodes, barcode_start=1){
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

#' main flow control for adding barcodes to samples table
add_barcodes = function(samples, pw_barcodes, barcode_start=1){
  if(is.null(samples) | is.null(pw_barcodes)){
    return(NULL)
  }
  cols = c('TECAN_primer_labware_name', 'TECAN_primer_target_position')
  pw_barcodes$TECAN_primer_labware_F = NULL
  pw_barcodes$TECAN_primer_labware_R = NULL
  if(all(cols %in% colnames(samples))){
    df = inner_join(samples, pw_barcodes, cols)
  } else {
    df = append_barcodes(samples, pw_barcodes, barcode_start)
  }
  return(df)
}

#' Loading concentration table (plate reader output)
read_samples = function(txt, header=TRUE){
  # read table from pasted-in
  if(is.null(txt) | nchar(txt) == 0){
    return(NULL)
  }
  df = read.delim(text=txt, sep='\t', header=header)
  
  ## checking for required columns
  req_cols = c('Sample')
  for(x in req_cols){
    if(!x %in% colnames(df)){
      print(sprintf('%s not in header', x))
    }
  }
  return(df)
}

#-- server --#
shinyServer(function(input, output, session) {
  # loading barcodes
  raw = eventReactive(input$barcode_type, {
    read_excel('./data/barcodes.xlsx', 
               sheet=input$barcode_type)
  })
  
  # loading samples file
  samples = reactive({
    read_samples(input$Samples)
  })
  
  # making pairwise barcodes with position info
  pw_loc_barcodes = reactive({
    pairwise_barcodes(add_position(raw()), 
                      join_id='position',
                      barcode_type=input$barcode_type)
  })
  
  sample_barcodes = reactive({
    add_barcodes(samples(), 
                 pw_loc_barcodes(),
                 input$barcode_start)
  })
  
  
  #--- Rendering ---#
  
  # rendering barcodes (raw)
  output$raw_tbl = DT::renderDataTable(
    raw(),
    rownames= FALSE,
    filter = 'top',
    extensions = c('Buttons'),
    options = list(
      pageLength = 48,
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
  
  # rendering samples table
  output$samples_tbl = DT::renderDataTable(
    samples(),
    rownames= FALSE,
    filter = 'top',
    extensions = c('Buttons'),
    options = list(
      pageLength = 48,
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
  
  # rendering pairwise barcodes with location info
  output$pw_loc_barcodes_tbl = DT::renderDataTable(
    pw_loc_barcodes(),
    rownames= FALSE,
    filter = 'top',
    extensions = c('Buttons'),
    options = list(
      pageLength = 48,
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
  
  # rendering samples + barcodes
  output$sample_barcodes_tbl = DT::renderDataTable(
    sample_barcodes(),
    rownames= FALSE,
    filter = 'top',
    extensions = c('Buttons'),
    options = list(
      pageLength = 48,
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
  
  # rendering example samples input
  output$ex_samples_tbl = DT::renderDataTable(
    example_samples(),
    rownames= FALSE,
    extensions = c('Buttons'),
    options = list(
      pageLength = 50,
      dom = 'Brt',
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

