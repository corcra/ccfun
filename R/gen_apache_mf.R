#' @title Generates the APACHE Metabolic Failure score
#'
#' @description
#' Generates the APACHE Metabolic Failure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param output Column name for the result of computation
#' @param hco3 a vector of numeric data
#' @param ph a vector of numeric data
#'

 
#'  @export

gen_apache_mf <- function(dt, hco3, ph, output = NULL) {
  #  =============================
  #  = APACHE - Metabolic Failure =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  input <- pars$input
  
  # Set to NA by default (numeric)
  if(is.null(output)) {
    output <- "apache_mf"
  }
  dt[, (output) := suppressWarnings(as.numeric(NA))]
  

  # Set mf variable as numeric
  if (is.factor(dt[,get(hco3)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(hco3)))))]
    dt[, (hco3) :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }

  if (is.factor(dt[,get(ph)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(ph)))))]
    dt[, (ph) :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }


  # Define conditions via dummy vars
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[is.na(get(ph))  & (get(hco3) > c(21))  , (output) := 0]
  dt[!is.na(get(ph)) & (get(ph)   > c(7.32)), (output) := 0]
  
  # APACHE = 1
  dt[is.na(get(ph))  & (get(hco3) > c(31))  , (output) := 1]
  dt[!is.na(get(ph)) & (get(ph)   > c(7.49)), (output) := 1]
  
  # APACHE = 2
  dt[is.na(get(ph))  & (get(hco3) < c(22))  , (output) := 2]
  dt[!is.na(get(ph)) & (get(ph)   < c(7.33)), (output) := 2]
  
  # APACHE = 3
  dt[(is.na(get(ph)) & (get(hco3) < c(18)))  | (is.na(get(ph))  & (get(hco3) > c(40))) , (output) := 3]
  dt[!is.na(get(ph)) & (get(ph)   > c(7.25)) | !is.na(get(ph))  & (get(ph)   > c(7.59)), (output) := 3]
  
  # APACHE = 4
  dt[(is.na(get(ph)) & (get(hco3) < c(16)))  | (is.na(get(ph))  & (get(hco3) > c(51))) , (output) := 4]
  dt[!is.na(get(ph)) & (get(ph)   > c(7.16)) | !is.na(get(ph))  & (get(ph)   > c(7.69)), (output) := 4]
  
}

  