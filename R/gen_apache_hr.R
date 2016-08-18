#' @title Generates the APACHE Heart Rate score
#'
#' @description
#' Generates the APACHE Heart Rate score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param input Heart Rate
#' @param output Column name for the result of computation
#'
#' @examples
#' # system.time(gen_apache_hr(ddata, hr = HR))
#' # table(ddata$apache_hr, useNA="always")
#' # ddata[hr > 40][sample(nrow(ddata[hr > 40]),20), .(hr)]
 
 
#'  @export
gen_apache_hr <- function(dt, input, output = NULL) {
  #  =============================
  #  = APACHE - Heart Rate =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  pars$input <- input 
  
  # Set to NA by default (numeric)
  if(is.null(output)) {
    output <- "apache_hr"
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
  dt[get(input) %between% c(70,109), (output) := 0]
  
  # APACHE = 2
  dt[(get(input) %between% c(55,69)) | (get(input) %between% c(110,139)), (output) := 2]
  
  # APACHE = 3
  dt[(get(input) %between% c(40,54)) | (get(input) %between% c(140,179)), (output) := 3]
  
  # APACHE = 4
  dt[(get(input) < c(40)) | (get(input) > c(179)), (output) := 4]
  
}

  