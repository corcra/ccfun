#' @title Generates the APACHE Arterial Pressure score
#'
#' @description
#' Generates the APACHE Arterial Pressure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param input Arterial Pressure
#' @param output Column name for the result of computation
#'
#' @examples
#' # system.time(gen_apache_map(ddata, temp_ = map))
#' # table(ddata$apache_map, useNA="always")
#' # ddata[map > 40][sample(nrow(ddata[map > 40]),20), .(map)]
 
 
#'  @export
gen_apache_map <- function(dt, input, output = NULL) {
  #  ==============================
  #  = APACHE - Arterial Pressure =
  #  ==============================
  # appending _ to var names for readability and to ensure uses scoped version
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  pars$input <- input
  
  # Set to NA by default (numeric)
  if(is.null(output)) {
    output <- "apache_map"
  }

  #dt[, (output) := suppressWarnings(as.numeric(NA))]
  

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
  dt[get(input) %between% c(70,109), (output) := 0]
  
  # APACHE = 2
  dt[(get(input) %between% c(50,69)) | (get(input) %between% c(110,129)), (output) := 2]
  
  # APACHE = 3
  dt[get(input) %between% c(130,159), (output) := 3]
  
  # APACHE = 4
  dt[(get(input) < c(50)) | (get(input) > c(159)), (output) := 4]
  
}

  