

#' @title from_msconvert_parameters_to_code
#' @description From msconvert_parameters class to running code
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param msconvert_parameters msconvert_parameters class object.
#' @return Running code for docker.
#' @export
#' @examples
#' msconvert_parameters =
#' create_msconvert_parameters(
#'   output_format = "mzXML",
#'   binary_encoding_precision = "64",
#'   filter = c("peakPicking true 1-",
#'              "msLevel 1"),
#'   zlib = TRUE,
#'   write_index = TRUE
#' )
#' msconvert_parameters
#' from_msconvert_parameters_to_code(msconvert_parameters)

from_msconvert_parameters_to_code =
  function(msconvert_parameters) {
    paste(
      paste0("--", msconvert_parameters@output_format),
      paste0("--", msconvert_parameters@binary_encoding_precision),
      ifelse(msconvert_parameters@zlib, "--zlib", ""),
      ifelse(msconvert_parameters@write_index, "", "--noindex"),
      lapply(msconvert_parameters@filter, function(x) {
        paste0('--filter ', '"', x, '"')
      }) %>%
        unlist() %>%
        paste(collapse = " ")
    )
  }