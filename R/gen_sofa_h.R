#' @title Generates the SOFA haematology score
#'
#' @description
#' Generates the SOFA haematology score; requires platelets
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param platelets_ platelets
#'
#' @examples
#' # gen_sofa_h(ddata, platelets_ = platelets)
#' # ddata[platelets<400][sample(nrow(ddata[platelets<400]),20), .(platelets, sofa_h)]
#' # table(ddata$sofa_h, useNA="always")

#' @export
gen_sofa_h <- function(dt, platelets_) {
    #  ===============
    #  = SOFA - haem =
    #  ===============

    # appending _ to var names for readability and to ensure uses scoped version

    # library(data.table)
    # data.table changes the object in place unless you use dt1 <- copy(dt)
    # so passing data.tables via function is actually just passing a reference

    # Non-standard evaluation
    pars <- as.list(match.call()[-1])

    platelets_ <- as.character(pars$platelets_)

    # Set to NA by default (numeric)
    dt[, `:=`(sofa_h = as.numeric(NA))]

    # Update based on conditions
    # Order of conditions is IMPORTANT

    # SOFA = 0
    dt[get(platelets_) >= 150, "sofa_h" := 0]

    # SOFA = 1
    dt[get(platelets_) < 150, "sofa_h" := 1]

    # SOFA = 2
    dt[get(platelets_) < 100, "sofa_h" := 2]

    # SOFA = 3
    dt[get(platelets_) < 50, "sofa_h" := 3]

    # SOFA = 4
    dt[get(platelets_) < 20, "sofa_h" := 4]

}
