#' @title Generates the APACHE Temperature score
#'
#' @description
#' Generates the APACHE Temperature score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param input Temperature
#' @param output Column name for the result of computation
#'
#' @examples
#' # system.time(gen_apache_temp(ddata, temp_ = Temperature))
#' # table(ddata$apache_temp, useNA="always")
#' # ddata[Temperature > 40][sample(nrow(ddata[Temperature > 40]),20), .(Temperature)]
 
 
#'  @export
gen_apache_temp <- function(dt, input, output = NULL) {
  #  =============================
  #  = APACHE - Temperature =
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
    output <- "apache_temp"
  }

  dt[, (output) := suppressWarnings(as.numeric(NA))]
  

  # Set rr_ variable as numeric
  if (is.factor(dt[,get(input)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(input)))))]
    dt[, input :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }
    
  
  
  # Define conditions via dummy vars
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[get(input) %between% c(36,38.4), (output) := 0]
  
  # APACHE = 1
  dt[(get(input) %between% c(34,35.9)) | (get(input) %between% c(38.5,38.9)), (output) := 1]
  
  # APACHE = 2
  dt[get(input) %between% c(32,33.9), (output) := 2]
  
  # APACHE = 3
  dt[(get(input) %between% c(30,31.9)) | (get(input) %between% c(39,40)), (output) := 3]
  
  # APACHE = 4
  dt[(get(input) < c(30)) | (get(input) > c(40)), (output) := 4]
  
}

  