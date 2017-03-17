#' Various helper functions that GENerate derived variables
#'

#' @param new.col name for mortality variable
#' @param old.col name for existing mortality variable

#  =================================
#  = Positive pressure ventilation =
#  =================================
#' @examples
#' # gen_ppv(ddata, time, id, Total_Resp_Rate_Ventil)
#' # gen_ppv makes a logical vector if positive pressure ventilated

#' @export
gen_ppv <- function(dt, t_=time, id_= id, rrate_ppv_=NULL, ppv_=NULL, roll_limit_ = +Inf) {
    # - [ ] TODO(2016-05-20): add in airway and ventilated fields
    # rrate_ppv_ = Total_Resp_Rate_Ventil (0146)
    # ppv_ = ventilated (0144)
    # Non-standard evaluation
    pars <- as.list(match.call()[-1])
    # print(pars)
    id_ <- as.character(pars$id_)
    t_ <- as.character(pars$t_)
    rrate_ppv_ <- as.character(pars$rrate_ppv_)
    ppv_ <- as.character(pars$ppv_)

    if (is.null(rrate_ppv_) & is.null(ppv_)) {
      stop("Need at least one of ppv_ OR rrate_ppv_")
    }


    # Indicator var for ventiltion
    if ("ppv" %chin% names(dt)) dt[, `:=`(ppv =NULL)]
    dt[, ppv := as.logical(NA)]

    # Update indicator if rrate_ppv_
    if (!is.null(pars$rrate_ppv_)) {
      dt[get(rrate_ppv_) > 0, ppv := TRUE]
    }
    # Update indicator if ventilator mode reported
    if (!is.null(pars$ppv_)) {
      dt[get(ppv_) %chin% c('1','2'), ppv := TRUE]
    }
}

#  ==========================
#  = Mean arterial pressure =
#  ==========================
# - [ ] FIXME(2016-05-20): wrap these two functions into one (gen_map and choose_first_nonmissing)

#' Generate MAP from blood pressure
#' gen_map derives MAP
#' @export
gen_map <- function(bps, bpd) {
    return(bpd + (bps-bpd)/3)
}

#' Choose first non-missing
#' @param colvalues columns to combine
#' @export
choose_first_nonmissing <- function(colvalues) {
    # library(purrr)
    reduce(colvalues, function(x,y) ifelse(!is.na(x), x, y))
}


#' @export
gen_admx.cat <- function(dt, var.name="adm.cat",
    adm.col="NIHR_HIC_ICU_0398",
    loca.col="NIHR_HIC_ICU_0068") {
    dt[get(adm.col) == "S", adm.cat := "Elective surgery"]
    dt[get(adm.col) != "S" & get(loca.col) == "T", adm.cat := "Emergency surgery"]
    dt[get(adm.col) != "S" & get(loca.col) != "T", adm.cat := "Emergency medical"]
}
# gen_admx.cat(wdt)

#  ========================
#  = Past medical history =
#  ========================

#' @export
gen_pmhx.sum <- function(dt, pmhx.cols=pmhx.cols) {
    if (is.null(pmhx.cols)) {
        pmhx <- c(
            "pmhx_chemo",
            "pmhx_dxt",
            "pmhx_immuncomp",
            "pmhx_leuk_acute",
            "pmhx_leuk_chronic",
            "pmhx_lymphoma",
            "pmhx_metastatic",
            "pmhx_crrt",
            "pmhx_cirrhosis",
            "pmhx_alf",
            "pmhx_portal_htn",
            "pmhx_hiv",
            "pmhx_home_vent",
            "pmhx_severe_rs",
            "pmhx_steroids",
            "pmhx_severe_cvs",
            "pmhx_other")
    }  else {
        pmhx <- pmhx.cols
    }
    dt[,(pmhx):=lapply(.SD,as.numeric),.SDcols=pmhx]
    dt[, pmhx.sum := rowSums(.SD, na.rm=TRUE), .SDcols=pmhx]
    # data.table works 'by reference' so no return
}

#  =============
#  = Mortality =
#  =============


#' @export
gen_mortality <- function(dt, new.col, old.col) {
    if (new.col %in% names(dt)) dt[, (new.col) := NULL]
    dt[!is.na(old.col), (new.col) := ifelse(get(old.col)=="D", 1, 0), with=FALSE]
    dt[, (new.col) := factor(get(new.col),
        levels=c(0L,1L),
        labels=c("survived", "died"))]
    table(dt[,get(new.col)])
}


#' @export
gen_age <- function(dt, var.name="age",
        dob.dt="NIHR_HIC_ICU_0033",
        icu.in.ts="NIHR_HIC_ICU_0411",
        sim=TRUE) {

    if (sim) {
        dt[, (var.name) := rnorm(1, 65, 10), by=id]
    } else {
        dt[, (var.name) := as.double(round((
               as.Date(get(icu.in.ts)) - as.Date(get(dob.dt))
               )/365, 2)),with=FALSE]
    }
}
# gen_age(wdt,sim=TRUE)

#' @export
gen_male <- function(dt, var.name="male",
            sex="NIHR_HIC_ICU_0093",
            sim=TRUE) {
    if (sim) {
        dt[, (var.name) := as.logical(rbinom(1,1,0.5)), by=id]
    }
    else {
        dt[tolower(get(sex)) == "m", (var.name) := TRUE]
        dt[tolower(get(sex)) == "f", (var.name) := FALSE]
    }
}
# gen_male(wdt,sim=TRUE)
# head(wdt[,.(id,time,male,age)])
# wdt[id==sample(wdt$id,1),.(id,time,male,age)]
# table(wdt$male)

#  ==================
#  = Length of stay =
#  ==================

#' @export
gen_los.icu <- function(dt, var.name="los.icu",
        icu.in.ts="NIHR_HIC_ICU_0411",
        icu.dc.ts="NIHR_HIC_ICU_0412",
        sim=TRUE) {
    '
    Generate ICU length of stay (hours)
    '
    if (sim) {
        # check id and time available
        assert_that(all(sapply(c("id", "time"), function(x) x %in% names(wdt))))
        dt[, (var.name) := max(time, na.rm=TRUE), by=id]
    } else {
        dt[, (var.name) := get(icu.dc.ts) - get(icu.in.ts), with=FALSE]
    }
}

#' @export
gen_los.hosp <- function(dt, var.name="los.hosp",
        hosp.in.dt="NIHR_HIC_ICU_0032",
        hosp.dc.dt="NIHR_HIC_ICU_0406",
        sim=TRUE) {
    '
    Generate hospital length of stay (days)
    '
    if (sim) {
        # check id and time available
        assert_that(all(sapply(c("id", "time"), function(x) x %in% names(wdt))))
        wdt[, ("los.hosp") := (max(time, na.rm=TRUE) + runif(1,0,2400)) %/% 24
            , by=id][,.(id,time,mort.icu,los.icu,los.hosp)]
    } else {
        dt[, (var.name) := get(hosp.dc.dt) - get(hosp.in.dt), with=FALSE]
    }

}
