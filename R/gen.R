#' Various helper functions that GENerate derived variables
#'


#  =================================
#  = Positive pressure ventilation =
#  =================================
#' @examples
#' # gen_ppv(ddata, time, id, Total_Resp_Rate_Ventil)
#' # gen_ppv makes a logical vector if positive pressure ventilated

#' @export
gen_ppv <- function(dt, t_=time, id_= id, rrate_ppv_) {
    # - [ ] TODO(2016-05-20): add in airway and ventilated fields

    # Non-standard evaluation
    pars <- as.list(match.call()[-1])

    id_ <- as.character(pars$id_)
    t_ <- as.character(pars$t_)
    rrate_ppv_ <- as.character(pars$rrate_ppv_)

    # need ccdata wide
    # remember that data.table will copy as reference
    # need a unique id (site+episode_id)
    # plus time
    # plus a series of columns or logical expression that can be used to define PPV

    setkeyv(dt, c(id_, t_))
    if ("ppv" %chin% names(dt)) dt[, `:=`(ppv =NULL)]
    dt[, `:=`(
        ppv = get(rrate_ppv_) > 0
        ),
        roll=+Inf, # roll forwards without limit
        rollends=c(FALSE, TRUE) # roll forwards from the last value, but not back from first
                 ]
}

#  ==========================
#  = Mean arterial pressure =
#  ==========================
# - [ ] FIXME(2016-05-20): wrap these two functions into one (gen_map and choose_first_nonmissing)

#' Generate MAP from blood pressure
#' gen_map derives MAP
#' @export
gen_map <- function(bps, bpd) {
    return(bpd + (bps-bpd)/3)
}

#' Choose first non-missing
#' @export
choose_first_nonmissing <- function(colvalues) {
    # library(purrr)
    reduce(colvalues, function(x,y) ifelse(!is.na(x), x, y))
}

#  ==========================
#  =      Comorbidity       =
#  ==========================
# - [ ] FIXME(2016-05-20): wrap these two functions into one (gen_map and choose_first_nonmissing)

#' Generate Comorbidity Score according to the APACHE default
#' gen_comorbidity counts number of comorbidity available in dataset
#' @description A function that counts the number of comorbidity expressed, according to APACHE definitions: 
#'                - Chronic Heart Failure : NYHA 4
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
#' @param fields. vector of strings naming the fields requested for the comorbidity score calculation. 
#' Fields have to be formatted according to ITEM_REF yaml file, as "NIHCC code", or "Data Item", or "Short name".
#' @param d_comorbidity. Strings. Name for the derived comorbidity fields. default : "d_comorbidity".
#' @export
#' 
gen_comorbidity <- function(dt, fields, d_comorbidity = NULL) {
  
  # Set the output field name
  if(is.null(d_comorbidity)) {
    d_comorbidity <- "d_comorbidity"
  }
  
  # set NA as dummy number. Very convenient to discriminate NA's, or the number of comorbidity for each patients.
  # See following comments for more explanations.
  dt[, (fields) := lapply(dt[,fields, with = F], function(x){x <- ifelse(is.na(x), 100000, x)})]
  
  
  # Generate Comorbidity variable:
      # Set default value as 0
      dt[, (d_comorbidity) := 0]
      
      # Add each column to d_comorbidity:
      for (i in 1:length(fields)){
        dt[, (d_comorbidity) := (d_comorbidity + get(fields[i]))]
      }
      
      # Example:
      # > dt[,.N, by = d_comorbidity]
            #   d_comorbidity       N
            # 1:             0  307165
            # 2:             1  111559
            # 3:             2   67237
            # 4:             3    5187
            # 5:       1500000 1635704
            # 6:       1400001  151582
            # 7:       1300002   39655
            # 8:       1200003   10656
            # 9:       1100004     791
            # 10:        100000    7367
            # 11:             4     325
            # 12:        100001    4453
            # 13:        100002     438
          # The last digit of each number represents exactly the number of comorbidity per patient.
          # The first two digits of each number represent exactly the number of comorbidity field that are filled with NA
          # If last digits + first two digits != 15, the difference represents exactly the number of comorbidity that doesn't express the patient.
          
      
      # Decode the results of the last computation according to the aforementionned rule.
      # Here is to consider only subgroup for whom d_comorbidity contains NA's and at least one comorbidity
      dt[`d_comorbidity` > 15 & `d_comorbidity`/10000 != as.integer(`d_comorbidity`/10000), 
         (d_comorbidity) := unlist(lapply(`d_comorbidity`, function(x){ x <- as.character(x/10000)
         x <- strsplit(x,"[.]")
         x <- unlist(x)
         x <- x[seq(2,length(x),2)]
         x <- as.numeric(x)
         }))]
      
      # Here is to consider only subgroup for whom d_comorbidity contains NA's and no comorbidities
      dt[`d_comorbidity` %between% c(16,1490000), (d_comorbidity) := 0]
      
      # Reset The NA's
      dt[`d_comorbidity` == 1500000, (d_comorbidity) := NA]
      dt[, (fields) := lapply(dt[,fields, with = F], function(x){x <- ifelse(x == 100000, NA , x)})]
}


#  ===========================
#  = Generate quality for rr =
#  ===========================
# - [ ] FIXME(2016-05-20): wrap these two functions into one (gen_map and choose_first_nonmissing)

#' Generate q_rr from respiratory fields
#' gen_q_rr derives a quality rr field, according to the parameters set up by the user
#' @param dt data for computation.
#' @param format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.
#' @param qual vector of inner and outer bounds of quality assessment for respiratory rate. 
#' @export


# requires at least one surrogate of respiratory rate (rr). Fields used for the calculation have to be 
# formatted as DataItem, or NIHCC code, or short name, among:
#           - Total respiratory rate (monitor) / NIHR_HIC_ICU_0145 / Total_Resp_Rate_Monit
#           - Total respiratory rate (ventilator) / NIHR_HIC_ICU_0146 / Total_Resp_Rate_Ventil
#           - Spontaneous Respiratory Rate / NIHR_HIC_ICU_0549 / rrate_spont
#           - Mandatory Respiratory Rate / NIHR_HIC_ICU_0147 / Mandat_Resp_Rate

gen_q_rr <- function(dt, format = "dataItem", qual = c(3,60)) {
  
  
  # Set q_rr = 0 as a default
  dt[, q_rr := 100000]
  
  # Prioritize fields according to the filled value according to this rule:
  #   - Spontaneous Respiratory Rate has to be preferred if patient is ventilator free
  #   - If patient is on ventilator:
  #     - Spontaneous Respiratory Rate if available and between 3 and 60 c/min
  #     - or Total respiratory rate (monitor) if available and between 3 and 60 c/min
  #     - or Total respiratory rate (ventilator) if available and between 3 and 60 c/min
  
  
  # Set the label
  switch(format, dataItem =  {field_p <- "Spontaneous Respiratory Rate"
                              field_pp <- "Total respiratory rate (monitor)"
                              field_ppp <- "Total respiratory rate (ventilator)"}, 
                 NIHCC =     {field_p <- "NIHR_HIC_ICU_0549"
                              field_pp <- "NIHR_HIC_ICU_0145"
                              field_ppp <- "NIHR_HIC_ICU_0146"},
                 shortName = {field_p <- "rrate_spont"
                              field_pp <- "Total_Resp_Rate_Monit"
                              field_ppp <- "Total_Resp_Rate_Ventil"}
         )
  
  
  # Define the value for q_rr
  if (field_p %in% names(dt)){
    dt[!is.na(get(field_p)) 
       & get(field_p) 
       %between% qual, q_rr := get(field_p)]
  }else{
    warning(paste(filed_p, "not available"))
  }
  
  if (field_pp %in% names(dt)){
    dt[q_rr == 10000 
       & !is.na(get(field_pp)) 
       & get(field_pp) 
       %between% qual, q_rr := get(field_pp)]
  }else{
    warning(paste(filed_pp, "not available"))
  }
  
  if (field_ppp %in% names(dt)){
    dt[q_rr == 10000 
       & !is.na(get(field_ppp)) 
       & get(field_ppp) 
       %between% qual, q_rr := get(field_ppp)]
  }else{
    warning(paste(filed_ppp, "not available"))
  }
  
  dt[q_rr == 100000, q_rr := NA]
  
}


#  ======================================================
#  = Generate alveolar gradient for respiratory failure =
#  ======================================================
# - [ ] FIXME(2016-05-20): wrap these two functions into one (gen_map and choose_first_nonmissing)

#' Generate grad from fio2 and/or PaO2 and PaCO2 fields
#' gen_grad derives a calculated alveolar gradient
#' @param dt data for computation.
#' @export

# requires FiO2 and/or(P/F ratio and PaO2)
# formatted as DataItem, or NIHCC code, or short name, among:
#           - Inspired fraction of oxygen / NIHR_HIC_ICU_0150 / fiO2
#           - PaO2/FiO2 ratio / NIHR_HIC_ICU_0913 / pf_ratio

# Calculations are performed according to the following Formula:
#     - Formula (simplified): (A-a)O2 Gradient = (( PAtm - PH2O) * FIO2 - ( PaCO2 / RQ )) - PaO2 
#                                     with RQ = 1 , PAtm = 100, PH2O = 6.2 
#                             (A-a)O2 Gradient = (100 - 6.2) * FIO2 - PaCO2 - Pa02


gen_grad <- function(dt, format = "dataItem"){

  # Prioritize the value to take into account for the fiO2
  switch(format, dataItem =  {fio2_p <- "Inspired fraction of oxygen"
                              fio2_pp <- "PaO2/FiO2 ratio"
                              PaO2  <- "PaO2 - ABG"
                              PaCO2 <-  "PaCO2 - ABG"}, 
                 NIHCC =     {fio2_p <- "NIHR_HIC_ICU_0150"
                              fio2_pp <- "NIHR_HIC_ICU_0913"
                              PaO2  <- "NIHR_HIC_ICU_0132"
                              PaCO2 <- "NIHR_HIC_ICU_0134"},
                 shortName = {fio2_p <- "fiO2"
                              fio2_pp <- "pf_ratio"
                              PaO2 <- "~"
                              PaCO2 <- "~"}
  )
  
  # Verify the availability of items
  if  (!(fio2_p %in% names(dt) | fio2_pp %in% names(dt)) & PaO2 %in% names(dt) & PaCO2 %in% names(dt)){
    stop(paste("Missing item for Oxygen Gradient calculation.", 
               fio2_p, "or", fio2_pp, "and", PaO2, "and", PaCO2, "are requested"))
  }
  
  # Set the default value to 0
  dt[, grad_rf := 100000]
  
  # Compute the gradient
  if(fio2_p %in% names(dt)){
    dt[!is.na(get(PaO2)) & !is.na(get(fio2_p)) & !is.na(get(PaCO2)), 
       grad_rf := ((100-6.2)*get(fio2_p) - get(PaO2) - get(PaCO2))]
  }else{
    dt[!is.na(get(PaO2)) & !is.na(get(fio2_pp)) & !is.na(get(PaCO2)), 
       grad_rf := (100-6.2)*get(PaO2)/get(fio2_pp)-get(PaO2)-get(PaCO2)]
  }
  
  dt[grad_rf == 100000 & !is.na(get(PaO2)) & !is.na(get(fio2_pp)) & !is.na(get(PaCO2)), grad_rf := (100-6.2)*get(PaO2)/get(fio2_pp)-get(PaO2)-get(PaCO2)]
  
  
  # Reset the Na's
  dt[grad_rf == 100000, grad_rf := NA]
  
}


#  ==================================================
#  = Generate haematocrit from haemoglobin variable =
#  ==================================================
# 

#' Generate Haematocrit variable from the haemoglobin
#' gen_grad derives a Haematocrit from haemoglobin and mean globular volume
#' @param dt data for computation.
#' @export

  gen_heamo <- function(dt, ccmh = NULL, format = "dataItem"){
    
    # Create the variable d_ccmh if ccmh unavailable
    ifelse (is.null(ccmh), dt[, d_ccmh := 330], dt[, d_ccmh := ccmh])
    
    # Calculate the derived haematocrit from haemoglobin
    switch(format, dataItem =  {haemo <- "Haemoglobin"}, 
                   NIHCC =     {haemo <- "NIHR_HIC_ICU_0179"},
                   shortName = {haemo <- stop("No shortNames available for this variable")}
    )
    
   dt[, d_ht := get(haemo) / d_ccmh]
  
  }
  
  #  =========================================
  #  = Generate weights from diagnosis codes =
  #  =========================================
  # 
  
  #' Generate weights variable from the primary diagnosis for asmission
  #' gen_weights derives a Haematocrit from haemoglobin and mean globular volume
  #' @param dt data for computation.
  #' @export
  
  gen_weights <- function (dt){
    # Diagnostic Category Weight:
      # Nonoperative patients: 
        # Respiratory failure or insufficiency from : 
          # Asthma/allergy : -2.108 
          # COPD : -0.367 
          # Pulmonary edema (noncardiogenic) : -0.251 
          # Postrespiratory arrest : -0.168 
          # Aspiration/poisoning/toxic : -0.142 
          # Pulmonary embolus : -0.128 
          # Infection : 0 
          # Neoplasm : 0.891 
        # Cardiovascular failure or insufficiency from : 
          # Hypertension : -1.798 
          # Rhythm disturbance : -1.368 
          # Congestive heart failure : -0.424 
          # Hemorrhagic shock/hypovolemia : 0.493 
          # Coronary artery disease : -0.191 
          # Sepsis : 0.113 
          # Postcardiac arrest : 0.393 
          # Cardiogenic shock : -0.259 
          # Dissecting thoracic/abdominal aneurysm : 0.731 
        # Trauma : 
          # Multiple trauma : -1.228 
          # Head trauma : -0.517 
          # Neurologic : 
          # Seizure disorder : -0.584 
          # ICH/SDH/SAH : 0.723 
        # Other : 
          # Drug overdose : -3.353 
          # Diabetic ketoacidosis : -1.507 
          # GI bleeding : 0.334 
        # If not in one of the specific groups above, then which major vital organ system was the principal 
        # reason for admission? 
          # Metabolic/renal : -0.885 
          # Respiratory : -0.890 
          # Neurologic : -0.759 
          # Cardiovascular : 0.470 
          # Gastrointestinal : 0.501 
      # Postoperative patients: 
          # Multiple trauma : -1.684 
          # Admission due to chronic cardiovascular disease : -1.376 
          # Peripheral vascular surgery : -1.315 
          # Heart valve surgery : -1.261 
          # Craniotomy for neoplasm : -1.245 
          # Renal surgery for neoplasm : -1.204 
          # Renal transplant : -1.042 
          # Head trauma : -0.955 
          # Thoracic surgery for neoplasm : -0.802 
          # Craniotomy for ICH/SDH/SAH : -0.788
          # Laminectomy and other spinal cord surgery : -0.699
          # Hemorrhagic shock : -0.682 
          # GI bleeding : -0.617 
          # GI surgery for neoplasm : -0.248 
          # Respiratory insufficiency after surgery : -0.140 
          # GI perforation/obstruction : 0.060 
        # For postoperative patients admitted to the ICU for sepsis or postarrest, use the corresponding weights
        # for non operative patients
        # If not in one of the above, which major vital organ system led to ICU admission postsurgery?
          # Neurologic : -1.150 
          # Cardiovascular : -0.797 
          # Respiratory : -0.610 
          # Gastrointestinal : -0.613 
          # Metabolic/renal : -0.196 
    
  }
  