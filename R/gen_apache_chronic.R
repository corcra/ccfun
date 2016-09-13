#' @title Generates the APACHE Chronic Health score
#'
#' @description
#' Generates the APACHE Chronic Health score; requires 15 comorbidity variables as inputs. A warning is
#' displayed in case of missing fields
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param admission Type of Admission. L: unplanned Local admission ; U: Unplanned transfer in ;
#' P: Planned transfer in ; S: planned local Surgical admission ; M: planned local Medical admission ;
#' R: Repatriation. character
#' @param surgery Type of surgery.M: Emergency ; U: Urgent ; S: Scheduled ; L: eLective
#' @param Comorbidity Chronic Health Status. Numerical. Needs a derived comorbidity score. See gen_comorbidity.
#'
#' @examples
#' dt <- NULL
#' dt$"Admission type" <- c(sample(c("L", "U", "P", "S", "M", "R"), 200, replace = T))
#' dt$"classification of surgery" <- c(sample(c("M", "U", "S", "L"), 200, replace = T))
#' dt$"d_comorbidity" <- c(sample(c(0, 1, 2, 3, 4, NA), 200, replace = T))
#' dt <- as.data.table(dt)
#' gen_apache_chronic(dt, admission = "Admission type", surgery = "classification of surgery")
#' dt[, .N, by = b_comorbidity]
#' dt[, .N, by = apache_chronic]


#'  @export



# TO DO:
# To build link between any function already built


gen_apache_chronic <- function(dt, admission, surgery) {
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
  pars$admission <- admission
  pars$surgery <- surgery
  b_comorbidity <- "b_comorbidity"

  
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  if ("d_comorbidity" %in% names(dt)){
    dt[, (b_comorbidity) := 0]
    dt[!is.na(`d_comorbidity`) & `d_comorbidity` > 0, (b_comorbidity) := 1]
    dt[!is.na(`d_comorbidity`) & `d_comorbidity` == 0, (b_comorbidity) := 0]
    dt[is.na(`d_comorbidity`), (b_comorbidity) := NA]
  }else{
    stop("derived comorbidity field is requested")
  }
  
  apache_chronic <- "apache_chronic"


  # APACHE = 0
  dt[, (apache_chronic) := 0]

  # APACHE = 2
  dt[b_comorbidity > 0 & (get(admission) %in% c("S") | get(surgery) %in% c("S") | get(surgery) %in% c("L")) ,
     (apache_chronic) := 2]

  # APACHE = 5
  dt[b_comorbidity > 0 & (get(admission) %in% c("M") | get(admission) %in% c("L") | get(surgery) %in% c("U") | get(surgery) %in% c("M")),
     (apache_chronic) := 5]

  # APACHE = NA
  dt[is.na(`b_comorbidity`), (apache_chronic) := NA]
  
}

