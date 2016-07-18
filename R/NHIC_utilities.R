#' Utilities that might be of use for working with NHIC data

#' Extract NHIC codes based on the value of a key:value pair in list
# Works at just top level?
#' @import purrr
#' @param dict_ NHIC dictionary from YAML file
#' @param k key
#' @param v value
#' @export
which_NHIC <- function(k, v, dict_=dict) {
  x <- names(Filter(function(y) (!is.null(y) && y == v) , purrr::map(dict_, k)))
  return(x)
}

#' Look for possible matches in NHIC dict
# Works at just top level?
#' @import purrr
#' @param dict_ NHIC dictionary from YAML file
#' @param k key
#' @param v value
#' @export
find_NHIC <- function(k, v, dict_=dict) {
  x <- names(Filter(function(y) (!is.null(y) && grepl(v, y)) , map(dict_, k)))
  return(sapply(x, function(y) dict[[y]]))
}

#' Convert those columns to numeric
#' @import data.table
#' @param dict_ NHIC dictionary from YAML file
#' @param dt data.table to convert
#' @export
convert_numeric <- function(dt, dict_=dict) {
    # Function to convert to numeric based on type in ITEM_REF
    # Type conversion based on ANALYSIS_REF.yaml
    # Expects cols to be labelled with NHICcode
    # - [ ] TODO(2016-05-21): check this
    Datatype <- map_chr(dict_, "Datatype")

    for (i in 1:length(Datatype)) {
        x <- names(Datatype)[i]
        if (!(x %in% names(dt))) {
            print("Not numeric - skipping")
            next
        }
        print(Datatype[i])
        if (Datatype[i] == "numeric") {
            print(x)
            setnames(dt, x, "x")
            dt[,  x := as.numeric(as.character(x))]
            setnames(dt, "x", x)
            # print(summary(tdt[[x]]))
        }
    }
}
