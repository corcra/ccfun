#' @title Generates the APACHE White Blood Cell Count score
#'
#' @description
#' Generates the APACHE wbc score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.
#' 
#' @examples
#' # system.time(gen_apache_wbc(ddata, format = "dataItem"))
#' # table(ddata$apache_wbc, useNA="always")


 
#'  @export


gen_smr <- function(dt, pred, observed, group, window, diagnosis = F, format = "dataItem"){
  
  
  
  
  # Naming  the apache_aki
  apache_smr <- "apache_smr"
  
  # Prioritize the value to take into account for the acute kidney injury
  
  switch(format, dataItem =  {patient <- "NHS number"}, 
                 NIHCC =     {patient <- "NIHR_HIC_ICU_0073"},
                 shortName = {patient <- "nhs_id"}
  )
  
  
  dt[get(observed) != "D" & get(observed) != "NA", status:= 0]
  dt[get(observed) == "D" & get(observed) != "NA", status:= 1]
  
  if (diagnosis == F){
    dt[is.na(get(group)), ("d_group") := 0]
    dt[!is.na(get(group)), ("d_group") := get(group)]
  }else{
    dt[, ("d_group") := get(group)]
    }
      
  
  return(dt[time %between% window , .(pred_mort = round(max(get(pred), na.rm = T), 2)), by = c(patient, "d_group", "status", group) ][
    , .(observed_mortality = sum(status, na.rm = T)/length(which(!is.na(status))), 
        predicted_mortality = mean(pred_mort),
        smr = (sum(status, na.rm = T)/length(which(!is.na(status))))/mean(pred_mort), 
        event = sum(status, na.rm = T),
        .N) , group])
  
  dt[, `:=` ("d_group" = NULL, "status" = NULL)]
 
}


