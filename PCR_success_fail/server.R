# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
source("../utils/io.R")
source("../utils/format.R")


#-- functions --#
#' reading table pasted in
read_table_pasted = function(x, plate=NULL, header=TRUE){
  # read table
  if(is.null(x) | nchar(x) == 0){
    return(NULL)
  }
  df = read.delim(text=x, sep='\t', header=header)
  if(!is.null(plate)){
    df$Plate = plate
  }
  return(df)
}

#' selecting negative controls based on regex
get_neg_controls = function(df, regex){
  if(is.null(df) || nrow(df) == 0){
    return(NULL)
  }
  df = df[grepl(regex, df$Sample),]
  return(df)
}

#' determining PCR success/fail
pcr_success_fail = function(df, min_endRFU, min_num_success, labware_type){
  if(is.null(df) || nrow(df) == 0){
    return(NULL)
  }
  df = df %>%
    group_by(Sample) %>%
    mutate(End.RFU = End.RFU %>% as.character %>% as.numeric,
           n_passed_pcr_reps = sum(End.RFU >= min_endRFU),
           mean_End.RFU = mean(End.RFU)) %>%
    ungroup() %>%
    mutate(labware_type = labware_type,
           labware_name = Plate,
           Call = ifelse(n_passed_pcr_reps >= min_num_success, 'Success', 'Fail'),
           Call = ifelse(End.RFU < min_endRFU, 'Fail', Call)) %>%
    rename("End RFU" = End.RFU) %>%
    dplyr::select(Well, Fluor, Content, Sample, `End RFU`, Call, 
                  labware_name, labware_type, n_passed_pcr_reps)
  
  return(df)
}

#' wrapper for sprintf to handle NULL values
val_to_txt = function(val, txt){
  if(is.null(val)){
    return('')
  }
  return(sprintf(txt, val))
}


#-- server --#
shinyServer(function(input, output, session) {
  # loading pasted tables
  data_tbl = reactive({
    df1 = read_table_pasted(input$pcr_plate_1, plate='Plate1')
    df2 = read_table_pasted(input$pcr_plate_2, plate='Plate2')
    df3 = read_table_pasted(input$pcr_plate_3, plate='Plate3')
    df = do.call(rbind, list(df1, df2, df3))
    return(df)
  }) 
  
  # combined raw table
  output$raw_endRFU = DT::renderDataTable(
    data_tbl(),
    rownames= FALSE,
    extensions = c('Buttons'),
    options = list(
      pageLength = 96,
      lengthMenu = c(96, 384, 1536),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  # filtering 
  ## negative controls (blanks)
  data_neg_ctrl = reactive({
    df_neg = get_neg_controls(data_tbl(), 
                              regex = input$neg_ctrl_regex)
    return(df_neg)
  }) 
  
  output$raw_neg_cntl = DT::renderDataTable(
    data_neg_ctrl(),
    rownames= FALSE,
    extensions = c('Buttons'),
    options = list(
      pageLength = 96,
      lengthMenu = c(96, 384, 1536),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  ### min endRFU
  min_endRFU =  reactive({
    df_neg = data_neg_ctrl()
    cutoff_RFU = input$endRFU_cutoff
    if(!is.null(df_neg) && nrow(df_neg) > 0){
      mean_endRFU = mean(df_neg$End.RFU, na.rm=TRUE)
      sd_endRFU = sd(df_neg$End.RFU, na.rm=TRUE)
      sd_endRFU = ifelse(is.na(sd_endRFU), 1, sd_endRFU)
      cutoff_RFU = mean_endRFU + sd_endRFU * input$neg_ctrl_std
    }
    return(cutoff_RFU)
  }) 
  #### status
  output$min_RFU_txt = renderText(val_to_txt(min_endRFU(),
                                            'Min. End-RFU: %s'))
  
  # success/fail table
  data_tbl_filt = reactive({
    df_f = pcr_success_fail(data_tbl(), 
                            min_endRFU = min_endRFU(),
                            min_num_success = input$min_num_success,
                            labware_type = input$labware_type)
    return(df_f)
  }) 
  
  num_success = reactive({
    df = data_tbl_filt()
    if(is.null(df)){
      return(NULL)
    }
    n_succ = df %>% filter(Call == 'Success') %>% nrow
    return(n_succ)
  })  
  
  output$num_suc_txt = renderText(val_to_txt(num_success(),
                                             'No. of success PCRs: %s'))
  
  num_success_by_sample = reactive({
    df = data_tbl_filt()
    if(is.null(df)){
      return(NULL)
    }
    n_succ = df %>% 
      filter(n_passed_pcr_reps >= input$min_num_success) %>%
      .$Sample %>% unique %>% length
    return(n_succ)
  })  
  
  output$num_suc_samp_txt = renderText(val_to_txt(num_success_by_sample(),
                                             'No. of success samples: %s'))
  
  output$filt_endRFU = DT::renderDataTable(
    data_tbl_filt(),
    rownames= FALSE,
    extensions = c('Buttons'),
    options = list(
      pageLength = 96,
      lengthMenu = c(96, 384, 1536),
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

