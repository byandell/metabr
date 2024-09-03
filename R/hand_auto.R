#' Compare hand to auto batch correction
#'
#' @param hand 
#' @param auto 
#'
#' @return ggplot object
#' @export
#' @importFrom dplyr full_join select
#' @importFrom ggplot2 autoplot facet_wrap geom_hline geom_point ggplot
#'             scale_x_log10 scale_y_log10 theme ylab
#' @importFrom rlang .data
#'
hand_auto <- function(hand, auto) {
  out <- dplyr::full_join(
    dplyr::select(hand, sample, mouse_id, Batch, Plate, compound, value),
    dplyr::select(auto, sample, mouse_id, compound, value),
    by = c("sample", "mouse_id", "compound"),
    suffix = c("_hand", "_auto")) |>
    dplyr::mutate(ratio = value_hand / value_auto) 
  class(out) <- c("hand_auto", class(out))
  out
}
#' @param object object of class `hand_auto`
#' @param ... parameters ignored
#' 
#' @export
#' @rdname hand_auto
ggplot_hand_auto <- function(object, ...) {  
  ggplot2::ggplot(object) +
    ggplot2::aes(x = .data$value_auto, y = .data$ratio,
                 col = as.character(.data$Plate)) +
    ggplot2::geom_point(size = 0.25) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::facet_wrap(~ .data$Batch) +
    ggplot2::geom_hline(yintercept = 1, color = "black")
}
#' @export
#' @method autoplot hand_auto
#' @rdname hand_auto
autoplot.hand_auto <- function(object, ...) {
  ggplot_hand_auto(object, ...)
}
#' @export
#' @method plot hand_auto
#' @rdname hand_auto
plot.hand_auto <- function(object, ...) {
  ggplot_hand_auto(object, ...)
}
