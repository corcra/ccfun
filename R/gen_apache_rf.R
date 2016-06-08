#' @title Generates the APACHE Respiratory Failure score
#'
#' @description
#' Generates the APACHE Respiratory Failure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param input Respiratory Failure Component
#' @param output Column name for the result of computation
#' @param pao2 a vector of numeric data
#' @param paco2 a vector of numeric data
#'

 
#'  @export

gen_apache_rf <- function(dt, input, output = NULL, pao2, paco2) {
  #  =============================
  #  = APACHE - Respiratory Failure =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version
  # requires respiratory rate (rr)
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  input <- pars$input
  
  # Set to NA by default (numeric)
  if(is.null(output)) {
    output <- "apache_rf"
  }
  dt[, (output) := suppressWarnings(as.numeric(NA))]
  

  # Set rf variable as numeric
  if (is.factor(dt[,get(input)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(input)))))]
    dt[, input :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }
  
  if (is.factor(dt[,get(pao2)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(pao2)))))]
    dt[, pao2 :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }
  
  if (is.factor(dt[,get(paco2)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(paco2)))))]
    dt[, paco2 :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }
  
    
  # Define the algorithme. FiO2, PaCO2, PaO2 are required 
  calc_rf <- function(dt,input, pao2, paco2){
    dt[, rf := ((1 - 0.062) * get(input) - get(paco2) - get(pao2))]
    dt[get(input) >49, rf := get(pao2)]
  }
  
  calc_rf(dt, input, pao2, paco2)
  
  # Define conditions via dummy vars
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[(get(input) < 50)  & (rf > c(9.3))  , (output) := 0]
  dt[(get(input) > 49) & (rf < c(26.7)), (output) := 0]
  
  # APACHE = 1
  dt[(get(input) < 50) & (rf < c(9.3)), (output) := 1]
  
  # APACHE = 2
  dt[(get(input) > 49) & (rf > c(26.6)), (output) := 2]
  
  # APACHE = 3
  dt[(get(input) < 50)  & (rf < c(8.1))    , (output) := 3]
  dt[(get(input) > 49) & (rf > c(46.4)), (output) := 3]
  
  # APACHE = 4
  dt[(get(input) < 50)  & (rf < c(7.3)) , (output) := 4]
  dt[(get(input) > 49) & (rf > c(66.3)), (output) := 4]

  dt[,rf := NULL]
}

  