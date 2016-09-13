#' @title Generates the APACHE Respiratory Failure score
#'
#' @description
#' Generates the APACHE Respiratory Failure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#'

 
#'  @export

gen_apache_rf <- function(dt, format = "dataItem") {
  #  =============================
  #  = APACHE - Respiratory Failure =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version
  # requires either fiO2 component and alveolo arterial gradient of oxygen. 
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  apache_rf <- "apache_rf"
  
  # Prioritize the value to take into account for the fiO2
  switch(format, dataItem =  {fio2_p <- "Inspired fraction of oxygen"
                              fio2_pp <- "PaO2/FiO2 ratio"
                              PaO2  <- "PaO2 - ABG"}, 
                 NIHCC =     {fio2_p <- "NIHR_HIC_ICU_0150"
                              fio2_pp <- "NIHR_HIC_ICU_0913"
                              PaO2  <- "NIHR_HIC_ICU_0132"},
                 shortName = {fio2_p <- "fiO2"
                              fio2_pp <- "pf_ratio"
                              PaO2 <- "~"}
  )

  # Set Default value
  dt[, (apache_rf) := 100000]
  
  # APACHE = 0
  dt[(get(fio2_p) < 0.5)  & (get(PaO2) > c(9.3))  , (apache_rf) := 0]
  dt[(get(fio2_p) > 0.49) & (grad_rf < c(26.7)), (apache_rf) := 0]
  dt[(get(fio2_pp)/get(PaO2) < 0.5)  & (get(PaO2) > c(9.3))  , (apache_rf) := 0]
  dt[ (get(fio2_pp)/get(PaO2) > 0.49) & (grad_rf < c(26.7)), (apache_rf) := 0]
  
  
  # APACHE = 1
  dt[(get(fio2_p) < 0.5) & (get(PaO2) < c(9.3)), (apache_rf) := 1]
  dt[(get(PaO2) / get(fio2_pp) < 0.5) & (get(PaO2) < c(9.3)), (apache_rf) := 1]
  
  # APACHE = 2
  dt[(get(fio2_p) > 0.49) & (grad_rf > c(26.6)), (apache_rf) := 2]
  dt[(get(PaO2) / get(fio2_pp) > 0.49) & (grad_rf > c(26.6)), (apache_rf) := 2]
  
  # APACHE = 3
  dt[(get(fio2_p) < 0.5)  & (get(PaO2) < c(8.1))    , (apache_rf) := 3]
  dt[(get(fio2_p) > 0.49) & (grad_rf > c(46.4)), (apache_rf) := 3]
  dt[(get(PaO2) / get(fio2_pp) < 0.5)  & (get(PaO2) < c(8.1))    , (apache_rf) := 3]
  dt[(get(PaO2) / get(fio2_pp) > 0.49) & (grad_rf > c(46.4)), (apache_rf) := 3]
  
  # APACHE = 4
  dt[(get(fio2_p) < 0.5)  & (get(PaO2) < c(7.3)) , (apache_rf) := 4]
  dt[(get(fio2_p) > 0.49) & (grad_rf > c(66.3)), (apache_rf) := 4]
  dt[(get(PaO2) / get(fio2_pp) < 0.5)  & (get(PaO2) < c(7.3)) , (apache_rf) := 4]
  dt[(get(PaO2) / get(fio2_pp) > 0.49) & (grad_rf > c(66.3)), (apache_rf) := 4]

  dt[apache_rf == 100000, (apache_rf) := NA]
}

  