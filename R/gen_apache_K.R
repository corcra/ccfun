#' @title Generates the APACHE Potassium score
#'
#' @description
#' Generates the APACHE Potassium score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.

#'
#' @examples
#' # system.time(gen_apache_K(ddata, format = "dataItem"))
#' # table(ddata$apache_K, useNA="always")

 
#'  @export

gen_apache_K <- function(dt, format = "dataItem") {
  #  ======================
  #  = APACHE - Potassium =
  #  ======================
  # appending _ to var Kmes for readability and to ensure uses scoped version
  # requires Potassium concentration [K] in mmol/l
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the apache_K
  apache_K <- "apache_K"
  
  # Prioritize the value to take into account for the temperature
  
  switch(format, dataItem =  {K <- "Potassium"}, 
         NIHCC =     {K <- "NIHR_HIC_ICU_0171"},
         shortName = {stop("shortName is'nt defined for potassium variable")}
  )
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[(get(K) > c(3.4)), (apache_K) := 0]

  # APACHE = 1
  dt[(get(K) > c(2.9)), (apache_K) := 1]

  # APACHE = 2
  dt[(get(K) < c(3))  | (get(K) > c(5.4)), (apache_K) := 2]

  # APACHE = 3
  dt[(get(K) > c(5.9)), (apache_K) := 3]
  
  # APACHE = 4
  dt[(get(K) < c(2.5))  | (get(K) > c(6.9)), (apache_K) := 4]

}

  