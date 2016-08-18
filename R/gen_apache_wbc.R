#' @title Generates the APACHE White Blood Cell Count score
#'
#' @description
#' Generates the APACHE wbc score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param output Column Name for the result of computation
#' @param input a vector of numeric data
#'

 
#'  @export

gen_apache_wbc <- function(dt, input, output = NULL) {
  #  ===================================
  #  = APACHE - White Blood Cell Count =
  #  ===================================
  # appending _ to var wbcmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  pars$input <- 
  
  # Set to wbc by default (numeric)
  if(is.null(output)) {
    output <- "apache_wbc"
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
  
  # APACHE = 0
  dt[(get(input) > c(2.999)),   (output) := 0]

  # APACHE = 1
  dt[(get(input) > c(14.999)), (output) := 1]

  # APACHE = 2
  dt[(get(input) < c(3.000)) | (get(input) > c(19.999)), (output) := 2]

  # APACHE = 4
  dt[(get(input) < c(1.000)) | (get(input) > c(39.999)), (output) := 4]

}
  
  