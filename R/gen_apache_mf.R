#' @title Generates the APACHE Metabolic Failure score
#'
#' @description
#' Generates the APACHE Metabolic Failure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.

#'
#' @examples
#' # system.time(gen_apache_mf(ddata, format = "dataItem"))
#' # table(ddata$apache_mf, useNA="always")

 
#'  @export

gen_apache_mf <- function(dt, format = "dataItem") {
  #  =============================
  #  = APACHE - Metabolic Failure =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the output
  apache_mf <- "apache_mf"
  
  # Prioritize the value to take into account for the Metabolic Failure
  switch(format, dataItem =  {ph <- "pH - ABG / VBG"
                              hco3 <- "HCO3 - ABG / VBG"}, 
                 NIHCC =     {ph <- "NIHR_HIC_ICU_0136"
                              hco3 <- "NIHR_HIC_ICU_0138"},
                 shortName = {ph <- "ph_abg_vbg"
                              if (!ph %in% names(data)){
                                stop("Unable to derive metabolic component. Verify the availability of the item, or try to convert the shortnames to dataItem or NIHH code")
                              }
                 }
  )
 
  if (!ph %in% names(data)) {ph <- hco3}
 
  

  
  # Update based on conditions
  # Order of conditions is IMPORTANT

 
  
  # APACHE = 0
  if (format != "shortName"){
    dt[(get(ph)   > c(7.32)), (apache_mf) := 0]
  }
  dt[is.na(apache_mf)  & (get(hco3) > c(21))  , (apache_mf) := 0]
  
  
  # APACHE = 1
  dt[(get(ph)   > c(7.49)), (apache_mf) := 1]
  dt[is.na(apache_mf)  & (get(hco3) > c(31))  , (apache_mf) := 1]
 
  
  # APACHE = 2
  if (format != "shortName"){
    dt[ (get(ph)   < c(7.33)), (apache_mf) := 2]
  }
  dt[is.na(apache_mf)  & (get(hco3) < c(22))  , (apache_mf) := 2]
  
  # APACHE = 3
  if (format != "shortName"){
    dt[(get(ph)   > c(7.25)) | (get(ph)   > c(7.59)), (apache_mf) := 3]
  }
  dt[(is.na(apache_mf)) & (((get(hco3) < c(18)))  | (get(hco3) > c(40))) , (apache_mf) := 3]
  
  # APACHE = 4
  if (format != "shortName"){
    dt[ (get(ph)   > c(7.16)) | (get(ph)   > c(7.69)), (apache_mf) := 4]
  }
  dt[(is.na(apache_mf)) & ((get(hco3) < c(16))  | (get(hco3) > c(51))) , (apache_mf) := 4]
  
}

  