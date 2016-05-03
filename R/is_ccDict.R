#' Confirms that the data dictionary is correctly formed
#' 
#' Checks that the object is a list with items named by their HIC code
#' in the "NHIC_HIC_ICU_nnnn" format where n is [0,9]
#' 
#' @param d the object to be checked
#' @return \code{TRUE} 
#' @examples
#' is.ccDict(d)

is.ccDict <- function(d) {
    # Check the object is a list of lists
    stopifnot(class(d)[1]=="list" & class(d)[[1]]=="list")
    # Check the object is a named list
    stopifnot(!is.null(names(d)))
    # Check that list items match naming format
    stopifnot(sum(grepl("NIHR_HIC_ICU_\\d{4}", names(d))) == length(d))
    # Check that list items contain NHIC code 
    stopifnot("NHICcode" %in% names(d[[1]]))
    return(TRUE)
}

# Development and testing
# d <- load_ccDict("/Users/steve/usr/github/ccdata/data/ITEM_REF.yaml")
# is.ccDict(d)
