#' Confirms that a data object matches the HIC specification
#'
#' Checks that the object is a \code{data.table}, and then confirms that
#' it contains the 8 columns in the correct order
#'
#' @param d the object (data.table) to be checked
#' @return \code{TRUE}
#' @examples
#' # is.hic(d)

#' @export
is.hic <- function(d) {
    stopifnot(class(d)[1]=="data.table")
    # Col names for readItems
    readItems.names <- c("nhs_number", "pas_number", "episode_id",
        "site_id", "item", "short_name", "time", "val")
    # - [ ] NOTE(2016-06-17): just checks 1st row and 1st 8 cols
    assert_that(are_equal(names(d[1,1:8,with=FALSE]), readItems.names))
    return(TRUE)
}
