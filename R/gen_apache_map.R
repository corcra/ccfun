#' @title Generates the APACHE Arterial Pressure score
#'
#' @description
#' Generates the APACHE Arterial Pressure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.
#'
#' @examples
#' # system.time(gen_apache_map(ddata, temp_ = map))
#' # table(ddata$apache_map, useNA="always")
#' # ddata[map > 40][sample(nrow(ddata[map > 40]),20), .(map)]
 
 
#'  @export
gen_apache_map <- function(dt, format = "dataItem") {
  #  ==============================
  #  = APACHE - Arterial Pressure =
  #  ==============================
  # appending _ to var names for readability and to ensure uses scoped version
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the output
  apache_map <- "apache_map"
  
  # Prioritize the value to take into account for map or calculate it from gen_map
  switch(format, dataItem =  {map_mean_art <- "Mean arterial blood pressure - Art BPMean arterial blood pressure"
                              map_mean_cuff <- "Mean arterial blood pressure - NBPMean arterial blood pressure"
                              map_syst_art <- "Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure"
                              map_sys_cuff <- "Systolic Arterial blood pressure - NBPSystolic Arterial blood pressure"
                              map_dia_art <- "Diastolic arterial blood pressure - Art BPDiastolic arterial blood pressure"
                              map_dia_cuff <- "Diastolic arterial blood pressure - NBPDiastolic arterial blood pressure"}, 
                 NIHCC =     {map_mean_art <- "NIHR_HIC_ICU_0110"
                              map_mean_cuff <- "NIHR_HIC_ICU_0111"
                              map_syst_art <- "NIHR_HIC_ICU_0112"
                              map_syst_cuff <- "NIHR_HIC_ICU_0113"
                              map_dia_art <- "NIHR_HIC_ICU_0114"
                              map_dia_cuff <- "NIHR_HIC_ICU_0115"},
                 shortName = {map_mean_art <- "bp_m_a"
                              map_mean_cuff <- "bp_m_ni"
                              map_syst_art <- "bp_sys_a"
                              map_sys_cuff <- "bp_sys_ni"
                              map_dia_art <- "bp_dia_a"
                              map_dia_cuff <- "bp_dia_ni"}
  )
  
  
  if (map_mean_art %in% names(data) & map_mean_cuff %in% names(data)){
    dt[!is.na(get(map_mean_art)), d_map := get(map_mean_art)]
    dt[is.na(get(map_mean_art)), d_map := get(map_mean_cuff)]
  }
  
  if (map_mean_art %in% names(data) & !map_mean_cuff %in% names(data)){
    dt[, d_map := map_mean_art]
  }
  
  if (!map_mean_art %in% names(data) & map_mean_cuff %in% names(data)){
    dt[, d_map := map_mean_cuff]
  }
  
  if (map_syst_art %in% names(data) | map_dia_art %in% names(data)){
    dt[is.na(d_map), d_map := gen_map(get(map_syst_art), get(map_dia_art)) ]
  }
  
  if (map_sys_cuff %in% names(data) | map_dia_cuff %in% names(data)){
    dt[is.na(d_map), d_map := gen_map(get(map_sys_cuff), get(map_dia_cuff)) ]
  }
  
  
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = 0
  dt[d_map %between% c(70,109), (apache_map) := 0]
  
  # APACHE = 2
  dt[(d_map %between% c(50,69)) | (d_map %between% c(110,129)), (apache_map) := 2]
  
  # APACHE = 3
  dt[d_map %between% c(130,159), (apache_map) := 3]
  
  # APACHE = 4
  dt[(d_map < c(50)) | (d_map > c(159)), (apache_map) := 4]
  
}

  