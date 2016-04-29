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
#' ccdata.dict <- load_ccDict() # loads using default values
#' ccdata.dict <- load_ccDict("my/local/path") # alternative fall back path
load_ccDict <- function(path=NULL) {
    ccdata.dict <- paste0(system.file("data", package="ccdata"), "/ITEM_REF.yaml")
    if (file.exists(ccdata.dict)) {
        ccdata.dict <- yaml::yaml.load_file(ccdata.dict)
    } else {
        if (is.null(path)) {
            ccdata.dict.bu <- "/Users/steve/aor/academic/hic/critical-care/dataset/N_DataItems.yml"
        } else {
            ccdata.dict.bu <- path
        }
        assert_that(file.exists(ccdata.dict.bu))
        ccdata.dict <- yaml::yaml.load_file(ccdata.dict.bu)
    }
}
