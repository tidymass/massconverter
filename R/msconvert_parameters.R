#' @title create_msconvert_parameters
#' @description Create msconvert parameters for convert_raw_data
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param output_format output_format
#' @param binary_encoding_precision binary_encoding_precision
#' @param filter filter
#' @param zlib zlib
#' @param write_index write_index
#' @return A msconvert_parameters class object.
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


create_msconvert_parameters =
  function(output_format = c("mzXML",
                             "mzML",
                             "mz5",
                             "mgf",
                             "text",
                             "ms1",
                             "cms1",
                             "ms2",
                             "cms2"),
           binary_encoding_precision = c("64", "32"),
           filter = c(
             "peakPicking true 1-",
             "polarity positive",
             "polarity negative",
             "msLevel 1",
             "msLevel 2",
             "msLevel 3"
           ),
           zlib = TRUE,
           write_index = TRUE) {
    output_format = match.arg(output_format)
    binary_encoding_precision = match.arg(binary_encoding_precision)
    
    ###check filter
    purrr::walk(filter, function(x) {
      if (!x %in% c(
        "peakPicking true 1-",
        "polarity positive",
        "polarity negative",
        "polarity negative",
        "msLevel 1",
        "msLevel 2",
        "msLevel 3"
      )) {
        stop(x, "is not a valid filter.\n")
      }
    })
    
    parameters = new(
      Class = "msconvert_parameters",
      output_format = output_format,
      binary_encoding_precision = binary_encoding_precision,
      filter = filter,
      zlib = zlib,
      write_index = write_index
    )
    parameters
    
  }



####msconvert_parameters S4 class object
##S4 class for msconvert_parameters
#' An S4 class that stores the parameters
#' @docType class
#' @slot output_format output format, mzML, mzXML, mz5, mgf, text, ms1, cms1,
#' ms2, or cms2.
#' @slot binary_encoding_precision binary encoding, 64 or 32
#' @slot filter a spectrum list filter
#' @slot zlib use zlib compression for binary data or not.
#' @slot write_index write index or not.
#' @exportClass msconvert_parameters

setClass(
  Class = "msconvert_parameters",
  slots = c(
    output_format = "character",
    binary_encoding_precision = "character",
    filter = "character",
    zlib = "logical",
    write_index = "logical"
  ),
  prototype = list(
    output_format = "mzXML",
    binary_encoding_precision = "64",
    filter = c("peakPicking true 1-"),
    zlib = TRUE,
    write_index = TRUE
  )
)

setMethod(
  f = "show",
  signature(object = "msconvert_parameters"),
  definition = function(object) {
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("Output format:", object@output_format), "\n")
    cat(
      crayon::green(
        "binary encoding precision:",
        object@binary_encoding_precision
      ),
      "\n"
    )
    cat(crayon::green("Use zlib compression:", object@zlib), "\n")
    cat(crayon::green("Write index:", object@write_index), "\n")
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::yellow("Filter:\n"))
    for (idx in seq_along(object@filter)) {
      cat(crayon::green(object@filter[idx]), "\n")
    }
  }
)
