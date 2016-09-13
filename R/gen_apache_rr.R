#' @title Generates the APACHE Respiratory Rate score
#'
#' @description
#' Generates the APACHE Respiratory Rate score; requires at least one respiratory rate input
#'
#' @import data.table
#' @param dt data.table containing physiology data
#'
#' @examples
#' # system.time(gen_apache_rr(ddata, input = RR))
#' # table(ddata$apache_rr, useNA="always")
#' # ddata[RR < 45][sample(nrow(ddata[RR < 45]),20), .(RR)]
 
 
#'  @export
  

gen_apache_rr <- function(dt) {
  #  =============================
  #  = APACHE - Respiratory Rate =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version
   # 
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  apache_rr <- "apache_rr"
  
  if (!"q_rr" %in% names(data)){
    stop("q_rr hasn't been found. Run gen_q_rr function!")}
  
  # Define conditions via dummy vars
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # Set the Default Value
  dt[, (apache_rr) := 100000]
  
  # APACHE = 1
  dt[q_rr %between% c(25,34), (apache_rr) := 1]
  
  # APACHE = 0
  dt[q_rr %between% c(12,24), (apache_rr) := 0]
  
  # APACHE = 2
  dt[q_rr %between% c(10,11), (apache_rr) := 2]
  
  # APACHE = 3
  dt[q_rr %between% c(6,9) | (q_rr %between% c(35,49)), (apache_rr) := 3]
  
  # APACHE = 4
  dt[(q_rr < c(6)) | (q_rr > c(49)), (apache_rr) := 4]
  
  dt[apache_rr == 100000, (apache_rr) := NA]
}

  