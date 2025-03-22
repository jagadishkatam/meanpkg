
#' to generate mean summary data frame
#'
#' @param data input dataframe
#' @param by input by group or groups
#' @param var input column which we need to summarize
#'
#' @returns a dataframe
#' @export mean.summary
#'
#' @importFrom rlang enquo
#'
#' @examples
#'mean.summary(mtcars, rlang::exprs(cyl, vs, am), mpg)
#'
mean.summary <- function(data, by, var) {

  var <- enquo(var)  # Capture variable name

  df <- data %>%
    dplyr::group_by(!!!by) %>%  # Unquote list of quosures
    dplyr::summarise(avg = mean({{ var }}, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(avg=round(avg,1))

  return(df)
}


