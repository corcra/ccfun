#' @title Generates the APACHE Temperature score
#'
#' @description
#' Generates the APACHE Temperature score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.

#'
#' @examples
#' # system.time(gen_apache_temp(ddata, temp_ = Temperature))
#' # table(ddata$apache_temp, useNA="always")
#' # ddata[Temperature > 40][sample(nrow(ddata[Temperature > 40]),20), .(Temperature)]
 
 
#'  @export
gen_apache_temp <- function(dt, format = "dataItem") {
  #  =============================
  #  = APACHE - Temperature =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the output
  apache_temp <- "apache_temp"
  
  # Prioritize the value to take into account for the temperature
  switch(format, dataItem =  {temp_p <- "Temperature - Central"
                              temp_pp <- "Temperature - Non-central"}, 
                 NIHCC =     {temp_p <- "NIHR_HIC_ICU_0141"
                              temp_pp <- "NIHR_HIC_ICU_0142"},
                 shortName = {temp_p <- "temperature_central"
                              temp_pp <- "temperature_non_central"}
         )

  
  if (temp_p %in% names(data) & !temp_pp %in% names(data)){
    dt[, d_temp := get(temp_p)]
  }
  
  if (!temp_p %in% names(data) & temp_pp %in% names(data)){
    dt[, d_temp := get(temp_pp)]
  }
  
  if (temp_p %in% names(data) & temp_pp %in% names(data)){
    dt[, d_temp := get(temp_pp)]
    dt[!is.na(get(temp_p)), d_temp := get(temp_p)]
  }
    
      
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  #Set the Default Value
  dt[, (apache_temp) := 100000]
  
  # APACHE = 0
  dt[d_temp %between% c(36,38.4), (apache_temp) := 0]

  # APACHE = 1
  dt[(d_temp %between% c(34,35.9)) | (d_temp %between% c(38.5,38.9)), (apache_temp) := 1]

  # APACHE = 2
  dt[d_temp %between% c(32,33.9), (apache_temp) := 2]
 
  
  # APACHE = 3
  dt[(d_temp %between% c(30,31.9)) | (d_temp %between% c(39,40)), (apache_temp) := 3]

  # APACHE = 4
  dt[(d_temp < c(30)) | (d_temp > c(40)), (apache_temp) := 4]

  # Reset the NA'S
  dt[apache_temp == 100000, (apache_temp) := NA]
  
}

  