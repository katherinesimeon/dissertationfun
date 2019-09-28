#' Add a (Categorical) Variable Manually
#'
#'
#' Takes a given dataframe and adds a column for a categorical variable
#' The dataframe must only have entries that would all have the same categorical value (i.e. only one level of the variable can be assigned)
#' This (basically useless and arbitrary) function is for adding a label to individual trials (i.e. marking if a list of trials was in quiet or noise)
#'
#' @param data Dataframe. What data you want to use. Data should be in a by-trial format and all trials should have the same condition/level that you want to assign within this dataframe
#' @param col Column Name. What you want the name of your variable to be. MUST BE A STRING/CHARACTER. Will also be the new column name in your dataframe
#' @param value Value you want to assign ALL rows in your dataframe. MUST BE A STRING/CHARACTER.
#'
#' @return Returns printed dataframe to console window. Assign to variable to add in workspace
#' @export
#'
#' @examples
#' CogLAW_with_attribute <- add_manual_var(CogLAW,"Attribute_Type","Familiar")
#' Will return dataframe with new column labelled "Attribute_Type"
add_manual_var <- function(data,col,value) {
  data[, col] <- rep(value,nrow(data))
  return(data)
}
