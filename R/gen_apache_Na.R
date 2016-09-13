#' @title Generates the APACHE Sodium score
#'
#' @description
#' Generates the APACHE Sodium score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param apache_Na Column name for the result of computation
#' @param Na a vector of numeric data
#'

 
#'  @export

gen_apache_Na <- function(dt, format = "dataItem") {
  #  ===================
  #  = APACHE - Sodium =
  #  ===================
  # appending _ to var names for readability and to ensure uses scoped version
  # requires Sodium concentration [Na] in mmol/l
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the apache_Na
  apache_Na <- "apache_Na"
  
  # Prioritize the value to take into account for the temperature
  switch(format, dataItem =  {Na <- "Sodium"}, 
                 NIHCC =     {Na <- "NIHR_HIC_ICU_0168"},
                 shortName = {Na <- "sodium"}
  )
  
 
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[(get(Na) > c(129)), (apache_Na) := 0]

  # APACHE = 1
  dt[(get(Na) > c(149)), (apache_Na) := 1]

  # APACHE = 2
  dt[(get(Na) < c(130))  | (get(Na) > c(154)), (apache_Na) := 2]

  # APACHE = 3
  dt[(get(Na) < c(120))  | (get(Na) > c(159)), (apache_Na) := 3]
  
  # APACHE = 4
  dt[(get(Na) < c(111))  | (get(Na) > c(179)), (apache_Na) := 4]

}

  