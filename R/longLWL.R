#' longLWL
#' For fʌn.nɔlɛdʒi (KS Dissertation)
#' Make Looking-While-Listening Data from DataWiz into long form
#'
#' This function will take LWL data in its CLEANED form and gathers data so Timebin and Fixations are two columns instead of each timebin being a column with fixation values for each participant.
#' LWL data is CLEANED if:
#' A GoodLooks column is added to the dataset
#' The LWL trials are paired with Endpoint accuracy from PsychoPy
#'
#' This function will turn all center looks (0.5 fixation values) into "C" non-numeric indications for analysis.
#' This function will also get rid of file extensions for image and audio filenames.
#'
#'
#' @param filename a string that indicates the filename for the LWL data (including path)
#' @param experiment either experiment 1 (NO prime) or experiment 2 (with semantic prime). Must be a numeric value.
#'
#' @return a data frame labelled LWL_long in your environment.
#' @export
#'
#' @examples
longLWL <- function(filename,experiment) {
  data_fr_file <- read.csv(filename)
  FUNK <- data.frame(data_fr_file)

  head(FUNK)
  dim(FUNK)

  FUNK_clean <- FUNK %>%
    filter(GoodLooks == 1) %>% # Removes trials that had more than 12 consecutive frames that were away
    filter(Accuracy == 1) # Removes trials where child selected the incorrect image

  if (experiment==1){
    FUNK_long <- gather(FUNK_clean,key="Timebin",value="Fixation",
                        -Subject,-Months,-Sex,-Order,
                        -Trial,-Prescreen.Notes,-L.image,-C.Image,-R.image,-Target.Side,-Target.Image,
                        -Condition,-Response,-First.Shift.Gap,-RT,-CritOnSet,-CritOffSet,-GoodLooks,-audio,
                        -RT_psychopy,-Accuracy,-Experiment,-Group)
  } else if (experiment==2) {
    FUNK_long <- gather(FUNK_clean,key="Timebin",value="Fixation",
                        -Subject,-Months,-Sex,-Order,
                        -Trial,-Prescreen.Notes,-L.image,-C.Image,-R.image,-Target.Side,-Target.Image,
                        -Condition,-Response,-First.Shift.Gap,-RT,-CritOnSet,-CritOffSet,-GoodLooks,-audio,
                        -competitor,-RT_psychopy,-Accuracy,-Experiment,-Group)
  } else {
    print("Error: experiment value must be numeric 1 or 2")
  }

  # Remove letters from timebin values
  FUNK_long$Timebin <- sub("P", "-", FUNK_long$Timebin)
  FUNK_long$Timebin <- sub("F", "", FUNK_long$Timebin)

  FUNK_long$Fixation <- sub("^0.5$", "C", FUNK_long$Fixation) # Change center looks to non-numeric value
  table(FUNK_long$Fixation)

  # Remove .jpg extension from pic names
  FUNK_long$Target.Image <- sub(".jpg", "", FUNK_long$Target.Image)
  FUNK_long$L.image <- sub(".jpg", "", FUNK_long$L.image)
  FUNK_long$R.image <- sub(".jpg", "", FUNK_long$R.image)

  LWL_long <<- FUNK_long
  head(FUNK_long)
}

