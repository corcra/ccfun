#' @title Generates the APACHE score from the APACHE sub-categories
#'
#' @description
#' Generates the APACHE score using the preexisting APACHE sub-categories
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param output Column Name for the result of computation
#' @param input a list of numeric data
#'

 
#'  @export

gen_apache_main <- function(mylist, output = NULL) {
  #  =============================
  #  =          APACHE           =
  #  =============================
  # appending _ to var mainmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  input <- pars$input
  
  # Set to main by default (numeric)
  if(is.null(output)) {
    output <- "apache_main"
  }
  dt[, (output) := suppressWarnings(as.numeric(NA))]
  

  
  # Define conditions via dummy vars
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = sum APACHE_sc
  apache_m = apply(sum,input)
  
  dt[,(output) := apache_m]
}
  
  