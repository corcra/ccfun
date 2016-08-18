#' @title Generates the APACHE Age score
#'
#' @description
#' Generates the APACHE Age score; requires at least one respiratory rate input
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param input Respiratory Rate (RR), integer or numeric
#' @param output Column name for the result of computation
#'
#' @examples
#' # system.time(gen_apache_rr(ddata, input = RR))
#' # table(ddata$apache_rr, useNA="always")
#' # ddata[RR < 45][sample(nrow(ddata[RR < 45]),20), .(RR)]


#'  @export


gen_apache_age <- function(dt, input, output = NULL) {
  #  =============================
  #  = APACHE - Respiratory Rate =
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
    output <- "apache_age"
  }
  
  dt[, (output) := suppressWarnings(as.numeric(NA))]
  
  
  # Set input variable as numeric
  if (is.factor(dt[,get(input)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(input)))))]
    dt[, input :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }
  
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[get(input) < c(45), (output) := 0]
  
  # APACHE = 1
  dt[get(input) %between% c(45,54), (output) := 2]
  
  # APACHE = 2
  dt[get(input) %between% c(55,64), (output) := 3]
  
  # APACHE = 3
  dt[get(input) %between% c(65,74), (output) := 5]
  
  # APACHE = 4
  dt[get(input) > c(74), (output) := 6]
  
}

