#' @title Generates the APACHE Sodium score
#'
#' @description
#' Generates the APACHE Sodium score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param output Column name for the result of computation
#' @param input a vector of numeric data
#'

 
#'  @export

gen_apache_Na <- function(dt, input, output = NULL) {
  #  ===================
  #  = APACHE - Sodium =
  #  ===================
  # appending _ to var names for readability and to ensure uses scoped version
  # requires Sodium concentration [Na] in mmol/l
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  input <- pars$input
  
  # Set to NA by default (numeric)
  if(is.null(output)) {
    output <- "apache_Na"
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
  dt[(get(input) > c(129)), (output) := 0]

  # APACHE = 1
  dt[(get(input) > c(149)), (output) := 1]

  # APACHE = 2
  dt[(get(input) < c(130))  | (get(input) > c(154)), (output) := 2]

  # APACHE = 3
  dt[(get(input) < c(120))  | (get(input) > c(159)), (output) := 3]
  
  # APACHE = 4
  dt[(get(input) < c(111))  | (get(input) > c(179)), (output) := 4]

}

  