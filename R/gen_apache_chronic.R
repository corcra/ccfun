#' @title Generates the APACHE Admission Type score
#'
#' @description
#' Generates the APACHE Age score; requires at least one respiratory rate input
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param admission Type of Admission. L: unplanned Local admission ; U: Unplanned transfer in ;
#' P: Planned transfer in ; S: planned local Surgical admission ; M: planned local Medical admission ;
#' R: Repatriation. character
#' @param surgery Type of surgery.M: Emergency ; U: Urgent ; S: Scheduled ; L: eLective
#' @param Comorbidity Chronic Health Status. Binary. 
#' @param output Column name for the result of computation.
#'
#' @examples


#'  @export


gen_apache_chronic <- function(dt, admission, surgery , Comorbidity, output = NULL) {
  #  =========================================================
  #  = APACHE - Chronic Health Assessment and Admission Type =
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
  #            - AND one of the following criteria for admission type:
  #                 - 2: + Elective Surgery
  #                 - 5: + Medical Reason for Admission || + Emergent Surgery
  
  
  # Non-standard evaluation
  pars <- as.list(match.call()[-1])
  # input <- pars$input
  
  # Set to NA by default (numeric)
  if(is.null(output)) {
    output <- "apache_chronic"
  }
  
  dt[, (output) := suppressWarnings(as.numeric(NA))]
  
    
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  
  # APACHE = 0
  dt[Comorbidity %in% NA, (output) := NA]
  
  # APACHE = 0
  dt[Comorbidity == 0, (output) := 0]
  
  # APACHE = 2
  dt[Comorbidity > 0 & (get(admission) %in% c("S") | get(surgery) %in% c("S") | get(surgery) %in% c("L")) , (output) := 2]
  
  # APACHE = 5
  dt[Comorbidity > 0 & (get(admission) %in% c("M") | get(admission) %in% c("L") 
                         | get(surgery) %in% c("U") | get(surgery) %in% c("M")), (output) := 5]
  
  dt[, Comorbidity := NULL]
  

}

