#' Loads the YAML field dictionary for ccdata
#'
#' Tries to load the file from the ccdata package (assuming this is
#' installed) and if that fails then falls back to a local copy
#' Work with YAML file **NOT** with data.checklist
#' Extract file from ccdata package if possible
#'
#' @param path path to the fall back copy of the dictionary
#' @return ccdata.dict
#' @examples
#' # ccdata.dict <- load_ccDict() # loads using default values
#' # ccdata.dict <- load_ccDict("my/local/path") # alternative fall back path


load_ccDict <- function(path=NULL) {
    if (is.null(path)) {
        path <- paste0(system.file("data", package="ccdata"), "/ITEM_REF.yaml")
    }
    assert_that(file.exists(path))
    ccdata.dict <- yaml::yaml.load_file(path)
    print(paste0("*** Returning ccData dictionary from ", path))
    return(ccdata.dict)
}
