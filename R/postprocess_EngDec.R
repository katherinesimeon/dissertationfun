#' postprocess_EngDec
#'
#' For English Decision/Novel Label Sorting in fʌn.nɔlɛdʒi Exp. 1.1 (KS Dissertation) and DegPhon (CS Capstone)
#'
#' @param filename The Psychopy Output File. Should be a string (with '') with a .csv extension. Must be written with correct participant label conventions (e.g. DegPhon_Subject #_ED_L#)
#' @param order The order that was assigned to the participant. This must be numeric.
#' @param group A string that indicates a group distinction (for example, group can be age or whether the participant is a child or adult)
#'
#' @return cleaned data for an individual participant to be added to larger data frame and accuracy based on the individual list.
#' @export
#'
#' @examples
#' postprocess_EngDec('DegPhon_001_ED_L1_english_decision_2019_Feb_07_1038.csv')
#' postprocess_EngDec(file.choose()) # Use this for ease to pick out the file you want to use
postprocess_EngDec <- function(filename,order,group) {
  data_fr_file <- read.csv(file.choose())
  ed <- data.frame(data_fr_file)

  # Get participant number
  subject <- as.character(ed$participant[1])

  # Get stuff that we need
  ed <- ed[2:nrow(ed),]
  ed <- ed[,1:(ncol(ed)-1)]
  ed1 <- ed[,1:2]
  ed2 <- ed[,4:7]
  ed3 <- ed[,14:15]
  full <- cbind(ed1,ed2,ed3)

  # Re-label columns
  colnames(full) <- c("audio1","audio2","answer","high","low","diff","response","RT-seconds")

  full$audio1 <- gsub(full$audio1, pattern=".wav$", replacement="")
  full$audio1 <- gsub(full$audio1, pattern="^NW_", replacement="")
  full$audio2 <- gsub(full$audio2, pattern=".wav$", replacement="")
  full$audio2 <- gsub(full$audio2, pattern="^NW_", replacement="")

  # Accuracy column
  num = c(1:nrow(full))
  for (i in num) {
    full$Accuracy[i] <- ifelse(identical(full$answer[i],full$response[i]),1,0)
  }

  Subject_Col <- data.frame(do.call('rbind', strsplit(as.character(subject),'_',fixed=TRUE)))

  full$Subject <- rep(Subject_Col$X2,nrow(full))

  full <- cbind(full$Subject,full[,1:9])

  full$List <- rep(Subject_Col$X4,nrow(full))

  audio_sep <- data.frame(do.call('rbind', strsplit(as.character(full$audio2),'_',fixed=TRUE)))

  colnames(full) <- c("Subject","audio1","audio2","answer","high","low","diff","response","RT-seconds","Accuracy","List")

  ED <- full

  ED <- add_manual_var(ED,"Order",as.numeric(order))
  ED <- add_manual_var(ED,"Group",as.character(group))

  total <- (sum(ED$Accuracy))

  ED <<- ED

  ### Output
  print(ED)
  print(paste0("Overall Accuracy: ",total,"/",nrow(ED)))
}
