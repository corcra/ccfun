#' @title Generates the APACHE Hematocrit score
#'
#' @description
#' Generates the APACHE ht score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.
#'
#' @examples
#' # system.time(gen_apache_ht(ddata, format = "dataItem"))
#' # table(ddata$apache_ht, useNA="always")

 
#'  @export

gen_apache_ht <- function(dt, format = "dataItem") {
  #  =======================
  #  = APACHE - Hematocrit =
  #  =======================
  # appending _ to var htmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the apache_ht
  apache_ht <- "apache_ht"
  
  
  if (!"d_ht" %in% names(data)){
   stop("Haematocrit variable unavailable, see gen_haemo for mor informations")
  }
  
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[d_ht > c(0.29), (apache_ht) := 0]
  
  # APACHE = 1
  dt[(d_ht > c(0.459)), (apache_ht) := 1]

  # APACHE = 2
  dt[(d_ht < c(0.30)) | (d_ht > c(0.499)), (apache_ht) := 2]

  # APACHE = 4
  dt[(d_ht < c(0.20)) | (d_ht > c(0.599)), (apache_ht) := 4]

}
  
  