#' Confirms that a data object matches the HIC specification
#'
#' Checks that the object is a \code{data.table}, and then confirms that
#' it contains the 7 columns in the correct order
#'
#' @param d the object (data.table) to be checked
#' @return \code{TRUE}
#' @examples
#' # is.hic(d)

is.hic <- function(d) {
    stopifnot(class(d)[1]=="data.table")
    # Col names for readItems
    readItems.names <- c("nhs_number", "pas_number", "episode_id",
        "site_id", "item", "short_name", "time", "val")
    assert_that(are_equal(names(d), readItems.names))
    return(TRUE)
}
