#' @title Generates the APACHE Heart Rate score
#'
#' @description
#' Generates the APACHE Heart Rate score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.
#'
#' @examples
#' # system.time(gen_apache_hr(ddata, hr = HR))
#' # table(ddata$apache_hr, useNA="always")
#' # ddata[hr > 40][sample(nrow(ddata[hr > 40]),20), .(hr)]
 
 
#'  @export
gen_apache_hr <- function(dt, format = "dataItem") {
  #  =============================
  #  = APACHE - Heart Rate =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the apache_hr
  apache_hr <- "apache_hr"
  
  # Prioritize the value to take into account for the heart rate
  switch(format, dataItem =  {hr_p <- "Heart rate"}, 
                 NIHCC =     {hr_p <- "NIHR_HIC_ICU_0108"},
                 shortName = {hr_p <- "hrate"}
  )

  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[get(hr_p) %between% c(70,109), (apache_hr) := 0]
  
  # APACHE = 2
  dt[(get(hr_p) %between% c(55,69)) | (get(hr_p) %between% c(110,139)), (apache_hr) := 2]
  
  # APACHE = 3
  dt[(get(hr_p) %between% c(40,54)) | (get(hr_p) %between% c(140,179)), (apache_hr) := 3]
  
  # APACHE = 4
  dt[(get(hr_p) < c(40)) | (get(hr_p) > c(179)), (apache_hr) := 4]
  
}

  