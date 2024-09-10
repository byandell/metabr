#' Read Batch Corrected Values
#'
#' Batch corrected values are transposed so steps are different.
#' @param dirpath path to directory
#' @param filename name of file
#' @param sheet sheet number if excel type file
#' @param skip number of lines to skip
#' 
#' @rdname qc_steps
#' @export
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything filter mutate select
#' @importFrom stringr str_remove str_replace
#' @importFrom rlang .data
#' 
read_batch_metab <- function(dirpath, filename, sheet = 4) {
  # Data are in sheet 1 starting on line 1.
  filename <- file.path(dirpath, filename)
  
  # Guess column type early.
  out <- readxl::read_excel(filename, sheet, guess_max = 80)
  # Find `n_max` as just before second try
  out <- readxl::read_excel(filename, sheet, guess_max = 80,
                            skip = grep("^(HC|RP)", out[[1]]) + 2)
  # Drop Batch summaries.
  out <- out[!grepl("^Batch", out[[1]]),] |>
    dplyr::rename(sample = "Original Sample Name",
                  run = "Run Order",
                  rundate = "Run Date") |>
    dplyr::filter(.data$Strains != "QC") |>
    dplyr::mutate(
      run = as.character(.data$run),
      rundate = as.Date(.data$rundate))
  
  compounds <- names(dplyr::select(out, -(sample:Replicates)))

  out |>
    # Pivot traits, which begin after `parent` column.
    tidyr::pivot_longer(-(sample:Replicates),
                        names_to = "compound", values_to = "value") |>
    dplyr::mutate(
      compound = factor(.data$compound, compounds),
      mouse_id = stringr::str_remove(
        stringr::str_remove(.data$sample, "^run[0-9]+-[0-9A-Za-z]+_"), "_.*$"),
      Batch = as.character(.data$Batch),
      Plate = as.character(.data$Plate)) |>
    # Parse Time and Rep
    dplyr::mutate(
      out,
      time = stringr::str_replace(sample, "^.*_([0-9]+min)_.*$", "\\1"),
      rep = stringr::str_replace(sample, "^.*_[0-9]+min_([0-9]+)_.*$", "\\1"))
}
