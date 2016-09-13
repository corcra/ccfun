#' @title Generates the APACHE Score
#'
#' @description
#' Generates the APACHE score from the calculated variables;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.
#' 
#' @examples
#' # system.time(gen_apache_score(ddata, format = "dataItem"))
#' # table(ddata$apache_score, useNA="always")
#'

 
#'  @export

gen_apache_score <- function(dt) {
  #  ===============================
  #  =        APACHE - main        =
  #  ===============================
  # appending _ to var gcsmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status
  
  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference
  
  # Naming  the apache_score
  apache_score <- "apache_score"
  apache_na <- "apache_na"
  
  # Define the fields requested for full computation
  apache <- c("apache_age", "apache_chronic", "apache_rr", "apache_temp", "apache_map", "apache_hr", "apache_mf", 
     "apache_gcs", "apache_wbc", "apache_ht", "apache_aki", "apache_K", "apache_Na", "apache_rf")
  
  # Display a warning if fields are missing
  if (length(which(is.na(match(apache, names(dt))))) > 0 ){
    warning( paste("Fields are missing for complete apache computation:", apache[which(is.na(!match(apache, names(dt))))]))
    apache <- apache[which(!is.na(match(apache, names(dt))))]
  }
  
  
  # Update based on conditions
  # Order of conditions is IMPORTANT
  
  # APACHE = sum of 11 physiologic variables + comorbidity variable + age variable
    # Set a reference value for APACHE score
    dt[, apache_score := 0]
    dt[, apache_na := 0]
    
    # Replace NA by 1000000 in each apache sub fields
    dt[, (apache) := lapply(dt[, apache, with = F], function(x){x <- ifelse(is.na(x), 100000, x)})]
    
    
    # Add each column to apache score:
    for (i in 1:length(apache)){
      dt[, (apache_score) := (apache_score + get(apache[i]))]
    }
    
    # Decode the value
    # Here is to consider only subgroup for whom apache subgroupes contains NA's and at least one positive apache assessment
    apache_nas <- length(apache)*100000
    dt[, (apache_na) := round(apache_score/100000, 0)]
    
    
    dt[`apache_score` > 100 & `apache_score` / 1000!= as.integer(`apache_score`/1000), (apache_score):= unlist(lapply(`apache_score`, function(x){x <- as.character(x/1000)
                                     x <- strsplit(x,"[.]")
                                     x <- unlist(x)
                                     x <- x[seq(2,length(x), 2)]
                                     x <- as.numeric(x)
                                     }))]
    
    # Here is to consider only subgroup for whom apache_score contains NA's and no positive apache assessment (subgroups = 0)
    dt[`apache_score` %between% c(100,length(apache)*100000-1), (apache_score) := 0]
    
    # Reset The NA's
    dt[`apache_score` == length(apache)*100000, `:=` (`apache_score` = 0)]
    dt[, (apache) := lapply(dt[,apache, with = F], function(x){x <- ifelse(x == 100000, NA , x)})]
 
}
  
  