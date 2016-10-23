context("Testing ICNARC parsing functions")

# First arg = description of function
# Subsequent args = function for testing e.g. expect_equal

test_that("Check DKA", {
    expect_equivalent(1,1)
    d <- data.table(code="2.8.4.10.1", diag="Diabetic ketoacidosis")
    d <- chop.icnarc.code(d, "code")
    expect_equivalent(
        c(2,8,4,10,1),
        as.numeric(d[,.(dc.1,dc.2,dc.3,dc.4,dc.5)]))
    d <- parse.icnarc.code(d, "code")
    expect_equivalent(d$condition, d$diag)
    # d <- label.icnarc.dc1(d)
    expect_equivalent(as.character(d$dc.1), "non-surgical condition")
    # d <- label.icnarc.dc2(d)
    expect_equivalent(as.character(d$dc.2), "Endocrine, Metabolic, Thermoregulation and Poisoning")

})

