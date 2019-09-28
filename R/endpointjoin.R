#' endpointjoin  
#' For fʌn.nɔlɛdʒi (KS Dissertation)  
#' Combine Looking-While-Listening Data with PsychoPy Endpoint Touchscreen Data  
#'   
#' This takes LWL data from DataWiz that has been compiled and processed in excel and combines it with PsychoPy Data.    
#' The two .CSV file inputs should have the same number of rows. 
#'   
#' LWLcsv Data should have:   
#' A WIDE format (each timepoint is a column and each row is a trial where there are 38 trials per subject)   
#' Backfilled rows  
#' Columns up to F2400  
#' GoodLooks column is added to the dataset  
#'   
#' PsychoPycsv Data should:  
#' Be compiled across ALL subjects (using a postprocess_LWL function)  
#'   
#' N.B. Example in this documentation will only work in the LWL.Rproj
#' 
#'
#' @param LWLcsv .CSV filename (string) with cleaned LWL data output from DataWiz (wide format)
#' @param PsychoPycsv .CSV filename (string) with all PsychoPy trials - this should have the same number of rows as the LWLcsv input
#' @param experiment either experiment 1 (NO prime) or experiment 2 (with semantic prime). Must be a numeric value.
#' @param group String value for Hearing Group (either "NH" for normal hearing or "HL" for hearing loss). This will be inputted into filename
#'
#' @return Saves a file with LWL and endpoint accuracy data combined in a single CSV. Saves the dataframe in environment as "FUNK_combined"
#' @export
#'
#' @examples 
#' endpointjoin("01data/Merging/2019-09-23-E1_LWL_HL_Compiled_forMerge.csv",
#' "01data/Merging/2019-09-09-Exp1_HL_Endpoint_forMerge.csv", 1,"HL") 
#' # Excuse the poor filenaming ... eek!
#' 
#' 
endpointjoin <- function(LWLcsv,PsychoPycsv,experiment,group) {
  data_fr_file1 <- read.csv(LWLcsv) # LOAD LWL DATA
  LWL <- data.frame(data_fr_file1)
  
  colnames(LWL)[1] <- "Subject"
  colnames(LWL)[5] <- "Trial"

  LWL$merge_ID <- paste0(LWL$Subject,"_",LWL$Trial)
  
  # LOAD PSYCHOPY DATA
  data_fr_file2 <- read.csv(PsychoPycsv)
  endpoint <- data.frame(data_fr_file2)
  
  endpoint$merge_ID <- paste0(endpoint$Subject,"_",endpoint$Trial)
  
  FUNK_ALL <- LWL %>% 
    left_join(endpoint, by = "merge_ID")
  
  if (experiment==1) {
    FUNK_clean <- cbind(FUNK_ALL[,1:97],FUNK_ALL[,101],FUNK_ALL[,106],FUNK_ALL[,110:112]) # EXP 1
    colnames(FUNK_clean)[98] <- "audio"
    colnames(FUNK_clean)[99] <- "RT_psychopy"
  } else if (experiment==2) {
    FUNK_clean <- cbind(FUNK_ALL[,1:97],FUNK_ALL[,101:102],FUNK_ALL[,106:107],FUNK_ALL[,111:113]) # EXP 2
    colnames(FUNK_clean)[101] <- "RT_psychopy"
  } else {
    print("Error: experiment value must be numeric 1 or 2")
  }
  
  # Remove the '.x' part of the Subject and Trial Column
  colnames(FUNK_clean)[1] <- "Subject"
  colnames(FUNK_clean)[5] <- "Trial"
  
  write.csv(FUNK_clean,file=paste0(Sys.Date(),"-","E",experiment,"_","LWL_ALL_",group,".csv"))
  
  FUNK_combined <<- FUNK_clean
}
