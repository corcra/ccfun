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

gen_apache_pred <- function(dt){
  
  # Naming  the apache_score
  apache_pred <- "apache_pred"
  
  # Display a warning if fields are missing
  if (!match("apache_score", names(dt)) != F ){
    stop( paste("¨Please, compute APACHE score First"))
  }
  
  if (!match("weight", names(dt)) != F ){
    stop( paste("¨Please, report diagnoses wheight on the dataset"))
  }
  
  if (!match("emergent", names(dt)) != F ){
    stop( paste("¨Please, label the admission data by post-operative or medical"))
  }


  # The predicted mortality is calculated according to the following equation:
  # ln(R/(1-R)) = -3.157 + (apache_score * 0.146) + (0.603 for emergent surgery) + Diagnostic category weight
  data[`emergent` == "Medical" , "R/1-R" := exp(-3.157 + `apache_score` * 0.146 + `weight`)]
  data[`emergent` == "Post-operative" , "R/1-R" := exp(-3.157 + `apache_score` * 0.146 + 0.603 + `weight`)]
  
  data[ , "risk" := (`R/1-R`/(1+`R/1-R`))]
 
}


