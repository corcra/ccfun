context("Testing NHIC utilities")

# First arg = description of function
# Subsequent args = function for testing e.g. expect_equal

test_that("Check relabel_cols", {
    dd <- data.table(NIHR_HIC_ICU_0108 = rep(100,10))
    dict <- list("NIHR_HIC_ICU_0108" = 
                 list(
                      "dataItem" = "Heart rate",
                      "shortName" = "hrate",
                      "NHICcode" = "NIHR_HIC_ICU_0108"
                      ))
    capture.output(relabel_cols(dd, "NHICcode", "shortName", dict))
    expect_equivalent(names(dd)[1], "hrate")
    capture.output(relabel_cols(dd, "shortName", "dataItem", dict))
    expect_equivalent(names(dd)[1], "Heart rate")
    capture.output(relabel_cols(dd, "dataItem", "NHICcode", dict))
    expect_equivalent(names(dd)[1], "NIHR_HIC_ICU_0108")
})


test_that("Check relabel NHICcode to shortName with NHICcode missing from dict", {
    dd <- data.table(NIHR_HIC_ICU_0108 = rep(100,10))
    dict <- list("NIHR_HIC_ICU_0108" = 
                 list(
                      "dataItem" = "Heart rate",
                      "shortName" = "hrate"
                      ))
    capture.output(relabel_cols(dd, "NHICcode", "shortName", dict))
    expect_equivalent(names(dd)[1], "hrate")
    # expect_error(gen_sofa_h(dd,y))
})