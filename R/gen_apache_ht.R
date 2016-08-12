#' @title Generates the APACHE Hematocrit score
#'
#' @description
#' Generates the APACHE ht score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param output Column Name for the result of computation
#' @param input a vector of numeric data
#'

 
#'  @export

gen_apache_ht <- function(dt, input, haemoglobin = TRUE, vgm = 3,  output = NULL) {
  #  =======================
  #  = APACHE - Hematocrit =
  #  =======================
  # appending _ to var htmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  input <- pars$input
  
  # Set to ht by default (numeric)
  if(is.null(output)) {
    output <- "apache_ht"
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
  ifelse(is.true(heamoglobin), 
         dt[(get(input) > c(29/vgm)), (output) := 0],
         dt[(get(input) > c(29)), (output) := 0]
  )

  # APACHE = 1
  ifelse(is.true(heamoglobin),
         dt[(get(input) > c(45.9/vgm)), (output) := 1],
         dt[(get(input) > c(45.9)), (output) := 1],
  )

  # APACHE = 2
  ifelse(is.true(heamoglobin),
         dt[(get(input) < c(30/vgm)) | (get(input) > c(49.9/vgm)), (output) := 2],
         dt[(get(input) < c(30)) | (get(input) > c(49.9)), (output) := 2]
  )

  # APACHE = 4
  ifelse(is.true(heamoglobin),
         dt[(get(input) < c(20/vgm)) | (get(input) > c(59.9/vgm)), (output) := 4],
         dt[(get(input) < c(20)) | (get(input) > c(59.9)), (output) := 4]
  )

}
  
  