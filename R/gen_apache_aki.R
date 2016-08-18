#' @title Generates the APACHE Acute Kidney Injury score
#'
#' @description
#' Generates the APACHE aki score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param output Column Name for the result of computation
#' @param input a vector of numeric data
#' @param with_ckd a logical vector, preexisting chronic renal function impairement
#'

 
#'  @export




gen_apache_aki <- function(dt, input, crrt, output = NULL) {
  #  ================================
  #  = APACHE - Acute Kidney Injury =
  #  ================================
  # appending _ to var akimes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  pars$input <- input  
  pars$crrt <- crrt
  
  # Set to aki by default (numeric)
  if(is.null(output)) {
    output <- "apache_aki"
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
  dt[(get(input) > c(54)), (output) := 0]

  # APACHE = 1
  dt[(get(input) < c(55)) | (get(input) > c(129)), (output) := 1]

  # APACHE = 2
  dt[(get(input) > c(169)), (output) := 2]

  # APACHE = 3
  dt[(get(input) > c(5.9)), (output) := 3]
  
  # APACHE = 4
  dt[(get(input) > c(304)), (output) := 4]
  
  # crrt
  dt[get(crrt) < 1, (output) := get(output)*2]
  
}
  
  