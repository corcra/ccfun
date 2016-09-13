#' @title Generates the APACHE Glasgow Coma Scale score
#'
#' @description
#' Generates the APACHE gcs score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.
#' 
#' @examples
#' # system.time(gen_apache_wbc(ddata, format = "dataItem"))
#' # table(ddata$apache_wbc, useNA="always")
#'

 
#'  @export

gen_apache_gcs <- function(dt, format = "dataItem") {
  #  ===============================
  #  = APACHE - Glasgow Coma Scale =
  #  ===============================
  # appending _ to var gcsmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the apache_gcs
  apache_gcs <- "apache_gcs"
  
  # Prioritize the value to take into account for Glasgow Coma Scale Score
  switch(format, dataItem =  {gcs <- "GCS - total"}, 
         NIHCC =     {gcs <- "NIHR_HIC_ICU_0156"},
         shortName = {gcs <- "gcs"}
  )

  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 15- GCS
  dt[,   (apache_gcs) := round(15 - get(gcs),0)]
}
  
  