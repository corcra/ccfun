#' @title Inspects NHIC data items
#'
#' @description
#' Produces basic summary data for NHIC data items when passed NHIC data
#' in the form produced by the readItems() function. Calls seperate helper
#' functions for the different data types available.
#'
#' @import data.table
#' @param d data generated from the ccdata::readItems() function
#' @param f.dict field dictionary generated from ITEM_REF.yml
#' via ccfun::load_ccDict.R
#' @param ... args to helper functions (e.g. palette="Greens" to ggMMplot)
#' @return r (a list) containing details of the item and a plot
#' including details broken down by site_id (ICU)
#'
#' @examples
#' # Example call to inspect item function
#' # system.time(r <- inspect_item(rdt, ccdata.dict[["NIHR_HIC_ICU_0108"]]))
#' # Example plotting function
#' # ggplot(r$inspect$plot_data) +
#' #     geom_freqpoly(aes(x=val, y=..density.., colour=site_id),
#' #    binwidth=1)


inspect_factor <- function(d) {
    # Use if factor (or) logical
    r <- list()
    r$miss <- sum(d$val=="") + sum(d$val=="NULL") + sum(d$val=="NA")
    r$levels <- as.character(unique(d$val))
    r$tab <- with(d, table(as.character(val)))
    r$tab.bysite <- with(d, table(site_id,as.character(val)))
    d <- droplevels(d) # droplevels else forces all into plot
    # Plot data only
    r$plot_data <- d[,.(site_id,val)]
    # Call ggMMplot via explicit package reference
    # ggplot object to return
    r$plot <- ccfun::ggMMplot(d$site_id, d$val, palette="Blues")
    return(r)
}

inspect_date <- function(d) {
    # Use if date, will need separate for times
    # Place holder function
    return(NULL)
}

inspect_numeric <- function(d) {
    r <- list()
    # Convert from factor if necessary
    if (is.factor(d$val)) {
        d[, val := as.numeric(as.character(val))]
    }
    # is.na not possible b/c of way data is structured
    r$miss <- sum(is.na(d$val)) + sum(is.null(d$val))
    r$miss.bysite <- d[, as.list(sum(is.na(val))),by=site_id]
    r$summ <- summary(d$val)
    r$summ.bysite <- d[, as.list(summary(val)),by=site_id]
    # Just return the ggplot object (you can format more later)
    r$plot_data <- d[,.(site_id,val)]
    return(r)
}

inspect_item <- function(d, f.dict, ...) {
    # Generate list to return
    r <- list()
    r$item <- f.dict[["dataItem"]]
    r$Datatype <- tolower(f.dict[["Datatype"]])
    r$data.dim <- dim(d)
    # Now subset the data
    df <- d[item==f.dict[["NHICcode"]]]
    r$length <- nrow(df)

    # "numeric",
    if (r$Datatype=="numeric") {
        r$inspect <- inspect_numeric(df)

    # "list",
    # "list / logical",
    # "Logical"
    } else if (r$Datatype %in% c("list", "list / logical", "logical")) {
        r$inspect <- inspect_factor(df)

    # "date",
    # "date/time",
    # "time",
    # "text",
    } else {
        stop(paste0("!!! Datatype:", as.character(r$Datatype), "NOT recognised"))
    }
    return(r)
}
