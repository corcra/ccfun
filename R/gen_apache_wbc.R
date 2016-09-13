#' @title Generates the APACHE White Blood Cell Count score
#'
#' @description
#' Generates the APACHE wbc score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.
#' @examples
#' # system.time(gen_apache_wbc(ddata, format = "dataItem"))
#' # table(ddata$apache_wbc, useNA="always")


 
#'  @export

gen_apache_wbc <- function(dt, format = "dataItem") {
  #  ===================================
  #  = APACHE - White Blood Cell Count =
  #  ===================================
  # appending _ to var wbcmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the apache_wbc
  apache_wbc <- "apache_wbc"
  
  # Prioritize the value to take into account for leucocytes count
  switch(format, dataItem =  {wbc <- "White cell count"}, 
         NIHCC =     {wbc <- "NIHR_HIC_ICU_0182"},
         shortName = {wbc <- "White cell"}
  )
  
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[(get(wbc) > c(2.999)),   (apache_wbc) := 0]

  # APACHE = 1
  dt[(get(wbc) > c(14.999)), (apache_wbc) := 1]

  # APACHE = 2
  dt[(get(wbc) < c(3.000)) | (get(wbc) > c(19.999)), (apache_wbc) := 2]

  # APACHE = 4
  dt[(get(wbc) < c(1.000)) | (get(wbc) > c(39.999)), (apache_wbc) := 4]

}
  
  