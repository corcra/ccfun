#' @title Generates the APACHE Glasgow Coma Scale score
#'
#' @description
#' Generates the APACHE gcs score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param output Column Name for the result of computation
#' @param input a vector of numeric data
#'

 
#'  @export

gen_apache_gcs <- function(dt, input, output = NULL) {
  #  ===============================
  #  = APACHE - Glasgow Coma Scale =
  #  ===============================
  # appending _ to var gcsmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  pars$input <- input
  
  # Set to gcs by default (numeric)
  if(is.null(output)) {
    output <- "apache_gcs"
  }
  
  dt[, (output) := suppressWarnings(as.numeric(NA))]
  

  # Set mf variable as numeric
  if (is.factor(dt[,get(input)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(input)))))]
    dt[, (input) :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }

  # Define conditions via dummy vars
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 15- GCS
  dt[,   (output) := round(15 - get(input),0)]
  dt[get(output) > 12, (output) := 12]
}
  
  