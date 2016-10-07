context("Testing SOFA score generating functions")

# First arg = description of function
# Subsequent args = function for testing e.g. expect_equal

test_that("Check SOFA haematology calculator", {
    dd <- data.table(platelets = c(0,15,25,75,125,200))
    gen_sofa_h(dd,platelets)
    expect_equivalent(dd$sofa_h, c(4,4,3,2,1,0))
    expect_error(gen_sofa_h(dd,y))
})

test_that("Check SOFA neurology calculator", {
    dd <- data.table(gcs = c(3,5,9,12,14,15))
    gen_sofa_n(dd,gcs)
    expect_equivalent(dd$sofa_n, c(4,4,3,2,1,0))
    expect_error(gen_sofa_n(dd,y))
})

test_that("Check SOFA liver calculator", {
    dd <- data.table(bili = c(10,20,33,102,204))
    gen_sofa_l(dd,bili)
    expect_equivalent(dd$sofa_l, c(0,1,2,3,4))
    expect_error(gen_sofa_l(dd,y))
})

test_that("Check SOFA renal calculator", {
    dd <- data.table(creat = c(10,100,110,171,300,441), uvol24h=c(0,100,200,500,1000,1000))
    gen_sofa_k(dd,creat)
    expect_equivalent(dd$sofa_k, c(0,0,1,2,3,4))

    gen_sofa_k(dd,creat,uvol24h_=uvol24h)
    expect_equivalent(dd$sofa_k, c(4,4,3,2,3,4))
    expect_error(gen_sofa_k(dd,y))
})


test_that("Check SOFA respiratory calculator", {
    dd <- data.table(pf = c(500,400,300,200,100,0) / 7.6, ppv=c(F,F,F,T,T,T))
    suppressWarnings(gen_sofa_r(dd,pf))
    expect_equivalent(dd$sofa_r, c(0,0,1,2,2,2))

    expect_warning(gen_sofa_r(dd[1,],pf))

    gen_sofa_r(dd,pf,ppv)
    expect_equivalent(dd$sofa_r, c(0,0,1,2,3,4))
    expect_error(suppressWarnings(gen_sofa_r(dd,y)))
})


test_that("Check SOFA cardiovascular calculator", {
    dd <- data.table(
        map   = c(100,70,50 ,50,50,50,50, 50,50),
        norad = c(0,0,0, 0,0,0,0, 0.01,0.1),
        dopa  = c(0,0,0, 0, 5, 15, 25, 0,0),
        dobu  = c(0,0,0, 10,0,0,0, 0,0)
        )
    # print(dd)
    gen_sofa_c(dd,map,norad,dopa_=dopa, dobu_=dobu)
    expect_equivalent(dd$sofa_c, c(0,0,1, 2, 2,3,4, 3,4))
    expect_error(suppressWarnings(gen_sofa_c(dd,y)))
})