#' 1. Transpose Data and Extract Info from Sample Names
#'
#' @param dirpath path to directory
#' @param filename name of file
#' @param sheet sheet number if excel type file
#' @param skip number of lines to skip
#' @param Batch_Plate add `Batch` and `Plate` if not `NULL`.
#' 
#' @rdname qc_steps
#' @export
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#' 
read_raw_metab <- function(dirpath, filename, sheet = 1, skip = 0,
                           Batch_Plate = NULL) {
  # Data are in sheet 1 starting on line 1.
  filename <- file.path(dirpath, filename)
  if(tools::file_ext(filename) == "csv") {
    out <- readr::read_csv(filename, show_col_types = FALSE)
  } else {
    out <- readxl::read_excel(filename, sheet, skip = skip)
  } 
  
  compounds <- unique(out$compound)
  
  out <- out |>
    # Pivot traits, which begin after `parent` column.
    tidyr::pivot_longer(-(label:parent),
                        names_to = "sample", values_to = "value") |>
    dplyr::mutate(
      compound = factor(.data$compound, compounds),
      mouse_id = stringr::str_remove(
        stringr::str_remove(.data$sample, "^run[0-9]+-[0-9A-Za-z]+_"), "_.*$"),
      run = stringr::str_replace(.data$sample, "^run([0-9]+)-.*", "\\1"),
      rundate = as.Date(
        stringr::str_replace(.data$sample, "^run[0-9]+-([0-9A-Za-z]+)_.*", "\\1"),
        "%d%b%y"))
  
  # Add Batch and Plate if not `NULL`.
  # Raw data does not have Batch and Plate in it.
  if(!is.null(Batch_Plate)) {
    out <- dplyr::left_join(
      out, 
      dplyr::select(Batch_Plate, rundate, Batch, Plate),
      by = "rundate")
  }
  
  out
}
