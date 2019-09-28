#' Post-process PILOT fʌn.nɔlɛdʒi (KS Dissertation)
#'
#' Post-process endpoint LWL accuracy from PsychoPy for fʌn.nɔlɛdʒi Exp. 1.2 (KS Dissertation)
#' This function was used for Fun-knowledgi PILOT DATA
#'
#' @param filename The Psychopy Output File. Should be a string (with '') with a .csv extension.
#'
#' @return A cleaned dataframe with individual trials for a given participant.
#' @export
#'
#' @examples
#' postprocess_FUNK('FK_Pilot2_L1_testrun_2018_Nov_18_1009.csv')
#' This function was originally designed for pilot data of Exp. 1.2 of KS Dissertation
postprocess_FUNK <- function(filename) {
  data_fr_file <- read.csv(filename)
  FK <- data.frame(data_fr_file)

  FK <- FK[2:39,]
  FK1 <- FK[,1:5]
  FK2 <- cbind(FK$mouse.RT,as.character(FK$Location),as.character(FK$participant))

  FUNK <- cbind(FK2[,3],FK1,FK2[,1:2])

  colnames(FUNK) <- c("Subject","Target","Competitor_Condition","Left_Pic","Right_Pic","Answer_Location","RT_Seconds","Selection_Location")

  Subject_Col <- data.frame(do.call('rbind', strsplit(as.character(FUNK$Subject),'_',fixed=TRUE)))

  FUNK <- cbind(Subject_Col[,2:3],FUNK[,2:8])

  colnames(FUNK) <- c("Subject","List","Target","Competitor_Condition","Left_Pic","Right_Pic","Answer_Location","RT_Seconds","Selection_Location")

  FUNK

  # Get rid if extensions and other unnecessary text
  FUNK$Right_Pic <- gsub(FUNK$Right_Pic, pattern=".jpg$", replacement="")
  FUNK$Left_Pic <- gsub(FUNK$Left_Pic, pattern=".jpg$", replacement="")
  FUNK$Target <- gsub(FUNK$Target, pattern=".wav$", replacement="")


  ##### Add Accuracy Column
  num = c(1:nrow(FUNK))
  for (i in num) {
    FUNK$Accuracy[i] <- ifelse(identical(as.character(FUNK$Answer_Location[i]),as.character(FUNK$Selection_Location[i])),1,0)
  }

  FUN_knowledgi <<- FUNK
  print(FUNK)
}
