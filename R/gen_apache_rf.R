#' @title Generates the APACHE Respiratory Failure score
#'
#' @description
#' Generates the APACHE Respiratory Failure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param fio2 inspired fraction of oxygen. Can be the raw value of fio2, or it can be extrapolated from pao2 on fio2 ratio
#' @param pao2 on fio2 ratio. Logical. 
#' @param output Column name for the result of computation
#' @param pao2 a vector of numeric data
#' @param paco2 a vector of numeric data
#'

 
#'  @export

gen_apache_rf <- function(dt, fio2, Gradient, output = NULL) {
  #  =============================
  #  = APACHE - Respiratory Failure =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version
  # requires either fiO2 component and alveolo arterial gradient of oxygen. 
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  pars$fio2 <- fio2 
  pars$Gradient <- Gradient
  
  # Set to NA by default (numeric)
  if(is.null(output)) {
    output <- "apache_rf"
  }
  dt[, (output) := suppressWarnings(as.numeric(NA))]
  

  # Set fio2 variable as numeric
  if (is.factor(dt[,get(fio2)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(fio2)))))]
    dt[, fio2 :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }
  
  
  # Define conditions via dummy vars
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[(get(fio2) < 0.5)  & (get(Gradient) > c(9.3))  , (output) := 0]
  dt[(get(fio2) > 0.49) & (get(Gradient) < c(26.7)), (output) := 0]
  
  # APACHE = 1
  dt[(get(fio2) < 0.5) & (get(Gradient) < c(9.3)), (output) := 1]
  
  # APACHE = 2
  dt[(get(fio2) > 0.49) & (get(Gradient) > c(26.6)), (output) := 2]
  
  # APACHE = 3
  dt[(get(fio2) < 0.5)  & (get(Gradient) < c(8.1))    , (output) := 3]
  dt[(get(fio2) > 0.49) & (get(Gradient) > c(46.4)), (output) := 3]
  
  # APACHE = 4
  dt[(get(fio2) < 0.5)  & (get(Gradient) < c(7.3)) , (output) := 4]
  dt[(get(fio2) > 0.49) & (get(Gradient) > c(66.3)), (output) := 4]

}

  