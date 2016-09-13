#' @title Generates the APACHE Acute Kidney Injury score
#'
#' @description
#' Generates the APACHE aki score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param crrt Strings. The name of the comorbid variable relating to chronic renal failure.
#' @param format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.


 
#'  @export




gen_apache_aki <- function(dt, crrt, format = "dataItem") {
  #  ================================
  #  = APACHE - Acute Kidney Injury =
  #  ================================
  # appending _ to var akimes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the apache_aki
  apache_aki <- "apache_aki"
  
  # Prioritize the value to take into account for the acute kidney injury
  
  switch(format, dataItem =  {aki <- "Creatinine"}, 
         NIHCC =     {aki <- "NIHR_HIC_ICU_0166"},
         shortName = {aki <- "creatinine"}
  )
  
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[(get(aki) > c(54)), (apache_aki) := 0]

  # APACHE = 1
  dt[(get(aki) %between% c(55, 129)), (apache_aki) := 1]

  # APACHE = 2
  dt[(get(aki) > c(129)), (apache_aki) := 2]

  # APACHE = 3
  dt[(get(aki) > c(169)), (apache_aki) := 3]
  
  # APACHE = 4
  dt[(get(aki) > c(304)), (apache_aki) := 4]
  
  # crrt
  dt[get(crrt) < 1, (apache_aki) := (apache_aki)*2]
  
}
  
  