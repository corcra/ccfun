#' @title Parses ICNARC diagnostic codes
#'
#' @description
#' Split text codes by level and then label as per ICNARC codes
#'
#' @param dt data.table containing diagnostic codes to be parsed
#' @param dc.col column name within data table with diagnostic codes
#' @param prefix new diagnostic component column prefix after parsing

#' Split ICNARC text code, and convert to numeric
#' @export
chop.icnarc.code <- function(dt, dc.col, prefix=NULL) {
    # Split each code into components and convert to numeric to handle padding
    if (is.null(prefix)) {
        prefix <- "dc"
    }
    dc.labels <- paste(prefix, seq(5), sep=".")
    dt[, (dc.labels) := tstrsplit(get(dc.col), "\\.")]
    dt[, (dc.labels) := lapply(.SD, as.numeric), .SDcols=dc.labels]
    return(dt)
}

#' Split and label ICNARC diagnostic code
#' @export
parse.icnarc.code <- function(dt, dc.col, prefix=NULL) {
  icnarc.dc <- icnarc.dc.load()
  icnarc.dc <- chop.icnarc.code(icnarc.dc, "icnarc.dc")
  setkeyv(icnarc.dc, paste0("dc.", seq(5)) )
  chop.icnarc.code(dt, dc.col)
  setkeyv(dt, paste0("dc.", seq(5)) )
  dt <- icnarc.dc[dt]

  if (!is.null(prefix)) {
    print(paste0(prefix, seq(5)))
    setnames(dt, paste0("dc.", seq(5)), paste0(prefix, seq(5)) )
  }

  return(dt)
}

#' Load ICNARC diagnostic code info
#' helper function
#' @export
icnarc.dc.load <- function() {
    path <- system.file("config", "icnarc_codes.csv", package="ccfun")
    # path <-  file.path(path.package("ccfun"), "icnarc_codes.csv")
    icnarc.dc <- read.csv(path, stringsAsFactors=FALSE)
    setDT(icnarc.dc)
    icnarc.dc <- subset(icnarc.dc, Condition!="")
    icnarc.dc <- data.table::melt(icnarc.dc, id.vars=c("Condition") )
    setnames(icnarc.dc, "value", "icnarc.dc")
    setnames(icnarc.dc, "variable", "adm.type")
    setnames(icnarc.dc, "Condition", "condition")
    setkey(icnarc.dc, icnarc.dc)
    return(icnarc.dc)
}

#' Label up ICNARC surgical level code
#' @export
label.icnarc.dc1 <- function(dt, dc.1.col="dc.1") {
    # Add organ coding labels # as factor labels
    # 1 - A condition where surgery has been performed (Surgical code)
    # 2 - A condition where no surgery has been performed (non-surgical code)
    i <- data.table(
        llabel = c(
            "surgical condition",
            "non-surgical condition"
            ),
        dc.1 = c(1,2))
    dt[, (dc.1.col) := factor(get(dc.1.col), levels=i$dc.1, labels=i$llabel)]
}

#' Label up ICNARC organ level code
#' @export
label.icnarc.dc2 <- function(dt, dc.2.col="dc.2") {
    # Add organ coding labels # as factor labels
    i <- data.table(
        llabel = c(
            "Respiratory",
            "Cardiovascular",
            "Gastrointestinal",
            "Neurological (including eyes)",
            "Poisoning",
            "Genito-urinary",
            "Endocrine, Metabolic, Thermoregulation and Poisoning",
            "Haematological/Immunological",
            "Musculoskeletal",
            "Dermatological",
            "Psychiatric",
            "Trauma" ),
        # Order is as per coding (there is no 5)
        dc.2 = c(1,2,3,4,6,7,8,9,10,11,12,13))
    dt[, (dc.2.col) := factor(get(dc.2.col), levels=i$dc.2, labels=i$llabel)]
}
