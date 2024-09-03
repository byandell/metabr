#' Title
#'
#' @param dirname name of directory
#'
#' @return data frame
#' @export
#' @importFrom dplyr bind_rows mutate select
#' @importFrom purrr map set_names
#' @importFrom readr read_csv
#' @importFrom stringr str_remove str_replace
#'
read_metab_minute <- function(dirname) {
  elmaven1 <- 
    dplyr::select(
      dplyr::mutate(
        # Bind data frames by `label`.
        dplyr::bind_rows(
          # Process each file in `dirname` into data frame.
          purrr::map(
            purrr::set_names(dir(dirname)),
            function(x) {
              out <- readr::read_csv(file.path(dirname, x))
              # Identify files that are `combined` runs.
              # Change `label` to be run column name.
              # Create `AUC` as value.
              runs <- grep("run", names(out))
              if(length(runs) == 1) {
                out$label <- last(names(out))
                out$AUC <- out[[runs]]
                out$combined <- FALSE
              } else {
                out$label <- last(names(out))
                out$AUC <- NA
                out$combined <- TRUE
              }
              out[-runs]  
            })),
        # Extract `minute`, `posneg` and `mouse` from `label`.
        # This is specific to runs of `minute`.
        minute = 
          as.numeric(stringr::str_replace(.data$label, "run.*_([0-9]+)min.*", "\\1")),
        posneg = ifelse(grepl("_HC_", .data$label), "HC", "RP"),
        mouse = stringr::str_remove(stringr::str_remove(.data$label, "_20.*$"), "^.*_"),
        # Change `label` for combined dataset.
        label = ifelse(.data$combined, "combined", .data$label)),
      # De-select `combinded` column.
      -combined)
}