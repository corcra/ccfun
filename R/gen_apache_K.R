#' @title Generates the APACHE Potassium score
#'
#' @description
#' Generates the APACHE Potassium score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param output Column Kme for the result of computation
#' @param input a vector of numeric data
#'

 
#'  @export

gen_apache_K <- function(dt, input, output = NULL) {
  #  ======================
  #  = APACHE - Potassium =
  #  ======================
  # appending _ to var Kmes for readability and to ensure uses scoped version
  # requires Potassium concentration [K] in mmol/l
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  pars$input <- input  
  
  # Set to K by default (numeric)
  if(is.null(output)) {
    output <- "apache_K"
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
  dt[(get(input) > c(3.4)), (output) := 0]

  # APACHE = 1
  dt[(get(input) > c(2.9)), (output) := 1]

  # APACHE = 2
  dt[(get(input) < c(3))  | (get(input) > c(5.4)), (output) := 2]

  # APACHE = 3
  dt[(get(input) > c(5.9)), (output) := 3]
  
  # APACHE = 4
  dt[(get(input) < c(2.5))  | (get(input) > c(6.9)), (output) := 4]

}

  