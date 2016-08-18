#' @title Generates the Alveolar to Arterial Gradient of Oxygen
#'
#' @description
#' Generates the APACHE Age score; requires at least fio2, paO2 and paCO2 input
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param input list of column names to extract alveolar to arterial gradient information.
#' @param output Column name for the result of computation.
#'
#' @examples


#'  @export


gen_alveolar_gradient <- function(dt, input, pao2, paco2, pfratio = TRUE, percentage = T, units = "kpa", output = NULL) {
  #  =========================================================
  #  =   APACHE - Generator of alveolar gradient Variable    =
  #  =========================================================
  # appending _ to var names for readability and to ensure uses scoped version
  # requires - fio2, paO2 and paCO2 variables
  
  
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  pars$pao2 <- pao2
  pars$paco2 <- paco2
  pars$input <- input
  
  # Set to NA by default (numeric)
  if(is.null(output)) {
    output <- "Grad"
  }
  
  if (is.factor(dt[,get(paco2)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(paco2)))))]
    dt[, paco2 :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }
  
  if (is.factor(dt[,get(pao2)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(pao2)))))]
    dt[, pao2 :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }
  
  if (is.factor(dt[,get(input)])) {
    dt[, `:=`(dummy_variable = suppressWarnings(as.numeric(as.character(get(input)))))]
    dt[, input :=  dummy_variable, with = F]
    dt[, dummy_variable := NULL]
  }
  
  dt[, (output) := suppressWarnings(as.numeric(NA))]

  
  # Build Comorbidity variable:
  
  # The fio2 is expressed as a ratio, and extracted from the pao2 on fio2 ratio. PaO2 is expressed in kpa.
  if (pfratio == T && units == "kpa" && percentage == F){
    dt[, (output) := ((100 - 6.2) * get(pao2) * 7.500615)]
    dt[, (output) := get(output) / get(input)]
    dt[, (output) := get(output) - get(pao2)- get(paco2)]
  }
  
  # The fio2 is expressed as a ratio, and extracted from the pao2 on fio2 ratio. PaO2 is expressed in mmHg.
  if (pfratio == T && units == "mmHg" && percentage == F){
    dt[, (output) := ((760 - 47) * get(pao2))]
    dt[, (output) := get(output) / get(input)]
    dt[, (output) := get(output) - get(pao2)- get(paco2)]
  }
  
  # The fio2 is expressed as a ratio PaO2 is expressed in kpa.
  if (pfratio == F && units == "kpa" && percentage == F){
    dt[, (output) := ((100 - 6.2) * get(input) - (get(pao2)- get(paco2)))]
  }
  
  # The fio2 is expressed as a ratio PaO2 is expressed in mmHg.
  if (pfratio == F && units == "mmHg" && percentage == F){
    dt[, (output) := ((760 - 47) * get(input)- get(pao2)- get(paco2))]
  }
  
  # The fio2 is expressed as a percentage, and extracted from the pao2 on fio2 ratio. PaO2 is expressed in kpa.
  if (pfratio == T && units == "kpa" && percentage == T){
    dt[, (output) := ((100 - 6.2) * get(pao2) * 7.500615)]
    dt[, (output) := get(output) / get(input)]
    dt[, (output) := get(output) - get(pao2)- get(paco2)]
  }
  
  # The fio2 is expressed as a percentage, and extracted from the pao2 on fio2 ratio. PaO2 is expressed in mmHg.
  if (pfratio == T && units == "mmHg" && percentage == T){
    dt[, (output) := ((760 - 47) * get(pao2))]
    dt[, (output) := get(output) / get(input)]
    dt[, (output) := get(output) - get(pao2)- get(paco2)]
  }
  
  # The fio2 is expressed as a percentage. PaO2 is expressed in mmHg.
  if (pfratio == F && units == "kpa" && percentage == T){
    dt[, (output) := ((100 - 6.2) * get(input)*0.01 - (get(pao2)- get(paco2)))]
  }
  
  # The fio2 is expressed as a percentage. PaO2 is expressed in kpa.
  if (pfratio == F && units == "mmHg" && percentage == T){
    dt[, (output) := ((760 - 47) * get(input)*0.01 - get(pao2) - get(paco2))]
  }
  

}

