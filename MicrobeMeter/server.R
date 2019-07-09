# Shiny server
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
source("../utils/io.R")
source("../utils/format.R")

# plate2long = function(x){
#   y = read.delim(text=x, sep='\t', header=FALSE)
#   y = as.data.frame(as.vector(as.matrix(y)))
#   colnames(y) = c('long_format')
#   return(y)
# }
# 
# long2plate = function(x, plate_type = '96-well'){
#   y = read.delim(text=x, sep='\t', header=FALSE)
#   n_rows_per_column = ifelse(plate_type == '96-well', 8, 16)
#   y = y %>%
#     unite_('plate_format', colnames(.), sep=';')
#   
#   n_col = ceiling(nrow(y) / n_rows_per_column)
#   y = as.vector(as.matrix(y))
#   y = matrix(y, nrow=n_rows_per_column, ncol=n_col)
#   y = as.data.frame(y)
#   colnames(y) = as.character(1:n_col)
#   rownames(y) = LETTERS[1:n_rows_per_column]
#   
#   return(y)
# }

load_ex_data = function(){
  F = '/Users/nyoungblut/dev/R/RTecanFluentShiny/MicrobeMeter/data/MicrobeMeter_output1.tsv'
  df = read.delim(F, sep='\t', skip=1, fill=TRUE, stringsAsFactors=TRUE)
  return(df)
}

turbidityCalculator = function(turbidityCTMD, timeUnit='minutes', windowSize=0) {
  # Setting the time unit for x-axis
  timeUnit <- switch (timeUnit, seconds = 1, minutes = 60, hours = 3600, s = 1, m = 60, h = 3600)
  xLabel <- switch (as.character(timeUnit), "1" = "Time (s)", "60" = "Time (m)", "3600" = "Time (h)")
  
  # Getting rid of unwanted information
  colnames(turbidityCTMD) <- turbidityCTMD[1,]
  turbidityCTMD <- turbidityCTMD[,c(-2,-7)]
  turbidityCTMD <- turbidityCTMD[-1,]
  
  # Converting the time stamp to Unix timestamp (seconds)
  turbidityCTMD[,1] <- as.numeric(as.POSIXct(strptime(turbidityCTMD[,1], "%c")))
  turbidityCTMD[,1] <- turbidityCTMD[,1] - turbidityCTMD[2,1]
  turbidityCTMD <- data.matrix(turbidityCTMD)
  
  print(turbidityCTMD)
  
  ############################################
  # Normalising the measurements of Port 1-3 using Port 4 for removing temperature bias
  turbidityCTMDNorm <- NULL
  for (i in 2:4) {
    turbidityCTMDNorm <- cbind(turbidityCTMDNorm, turbidityCTMD[,i]*(turbidityCTMD[1,i]/turbidityCTMD[,5]))
  }
  
  # Calculating the turbidity: divide each measurement using the corresponding Blank and calculate the -log, then multiply with 1/1.6 for path-length correction
  pathLength <- 1.6
  turbidityCTMDFull <- cbind((turbidityCTMD[,1]/timeUnit), -log10(t(t(turbidityCTMDNorm)/turbidityCTMDNorm[1,]))*(1/pathLength))[-1,]
  colnames(turbidityCTMDFull) <- colnames(turbidityCTMD)[-5]
  
  # Conducting moving average
  if (windowSize > 0) turbidityCTMDFull <- filter(turbidityCTMDFull, rep(1/windowSize, windowSize), sides = 1)[windowSize:nrow(turbidityCTMDFull),]
  
  # Calculating average and SD
  turbidityCTMDMean <- rowMeans(turbidityCTMDFull[,2:4])
  turbidityCTMDSD <- apply((turbidityCTMDFull[,2:4]), 1, sd)
  # formatting table
  turbidityCTMD = rbind(c(colnames(turbidityCTMD)[-5], "Mean", "SD"), cbind(turbidityCTMDFull, turbidityCTMDMean, turbidityCTMDSD))
  
  print(turbidityCTMD)
  
  return(turbidityCTMD)
}
  
#   # Saving the turbidity results
#   #write.table("TurbidityResults.tsv", sep = "\t", row.names = F, col.names = F)
#   ############################################
#   
#   ############################################
#   # Generating the plot
#   # Turbidity Plot
#   pdf("Turbidity.pdf")
#   matplot(turbidityCTMDFull[,1], turbidityCTMDFull[,2:4], type = "p", ylab = "Turbidity", xlab = xLabel, pch = 1:3, cex = 0.5, col = 1:3)
#   legend('topleft', legend = c("Port 1", "Port 2", "Port 3"), pch = 1:3, col = 1:3)
#   dev.off()
#   
#   # Turbidity Mean-SD Plot
#   pdf("Turbidity_Mean-SD.pdf")
#   matplot(turbidityCTMDFull[,1], turbidityCTMDMean, type = "n", ylab = "Turbidity", ylim = c(min(turbidityCTMDMean-turbidityCTMDSD), max(turbidityCTMDMean+turbidityCTMDSD)), xlab = xLabel, pch = 20, cex = 0.5, col = 'black')
#   arrows(turbidityCTMDFull[,1], turbidityCTMDMean-turbidityCTMDSD, turbidityCTMDFull[,1], turbidityCTMDMean+turbidityCTMDSD, length = 0.02, angle = 90, code = 3, col = 'grey')
#   points(turbidityCTMDFull[,1], turbidityCTMDMean, pch = 20, cex = 0.5, col = 'black')
#   dev.off()
#   ############################################
# }

#-- server --#
shinyServer(function(input, output, session) {
  df_turbidity = reactive({
    if(input$input_text == ''){
      return(NULL)
    }
    df = read.delim(text=input$input_text, sep='\t', skip=1, fill=TRUE, stringsAsFactors=TRUE)
    #df = as.data.frame(as.vector(as.matrix(df)))
    #print(class(df))
    #print(df)
    turbidityCalculator(df, timeUnit=input$time_unit, windowSize=input$window_size)
  })
  
  output$turbidity_tbl = DT::renderDataTable(
    df_turbidity(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 30,
      lengthMenu = c(30, 100, 500),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
  
  output$ex_data = DT::renderDataTable(
    load_ex_data(),
    filter = 'bottom',
    extensions = c('Buttons'),
    options = list(
      pageLength = 30,
      lengthMenu = c(30, 100, 500),
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )
})

