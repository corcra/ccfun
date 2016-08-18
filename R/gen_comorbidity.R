#' @title Generates the APACHE Admission Type score
#'
#' @description
#' Generates the APACHE Age score; requires at least one respiratory rate input
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param input list of column names to extract comorbidity information.
#' @param output Column name for the result of computation.
#'
#' @examples


#'  @export


gen_comorbidity <- function(dt, input, output = NULL) {
  #  =========================================================
  #  =      APACHE - Generator of Comorbidity Variable       =
  #  =========================================================
  # appending _ to var names for readability and to ensure uses scoped version
  # requires - At least one condition among:
  #                 - Chronic Heart Failure : NYHA 4
  #                 - Hepatic Disease : Biopsy Proven Cirrhosis ; 
  #                                     Documented Portal Hypertension ;
  #                                     Upper Gastro-Intestinal Bleedings due to Portal Hypertension ;
  #                                     Hepatic encephalopathy or comatose episode.
  #                 - Chroncic respiratory insufficiency: Restrictive or Obstructive or Vascular Disease
  #                                                       with severe impairement of physical activities ;
  #                                                       Secondary Polycythemia ;
  #                                                       Documented Chronic Hypoxia or Hypercarbia ; 
  #                                                       Severe Pulmonary Hypertension or
  #                                                       Long-Term Oxygenotherapy.
  #                 - Chronic Renal Replacement Therapy
  #                 - Imunosuppression :  Immunosuppressive Therapy ;
  #                                       Chemotherapy ;
  #                                       Radiotherapy ;
  #                                       Long Lasting Corticotherapy ;
  #                                       Oncohaematologic Disease ;
  #                                       AIDS.

  
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  pars$input <- input
  
  # Set to NA by default (numeric)
  if(is.null(output)) {
    output <- "Comorbidity"
  }
  
  dt[, (output) := suppressWarnings(as.numeric(NA))]

  
  # Buy Comorbidity variable:
    dt[, (output) := 0]
    
    for (i in 1:length(input)){
      dt[get(input[[i]])    %in% "1",(output) := Comorbidity + 1]
    }
    
    for (i in 1:length(input)){
      dt[get(input[[i]])    %in% "NA", (output) := NA]
    }
  
    dt[get(output) > 0, (output) := 1]
 
}

