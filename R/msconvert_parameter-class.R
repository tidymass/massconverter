#' @title create_msconvert_parameter
#' @description Create msconvert parameters for convert_raw_data
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param output_format "mzXML","mzML", "mz5", "mgf", "text", "ms1",
#' "cms1", "ms2", or "cms2".
#' @param binary_encoding_precision "64" or "32"
#' @param zlib TRUE or not.
#' @param write_index TRUE or not.
#' @param peak_picking_algorithm "vendor" or "cwt"
#' @param vendor_mslevels A two numeric vector. Second can be set as NA.
#' @param cwt_mslevels A two numeric vector. Second can be set as NA.
#' @param cwt_min_snr minimum signal-to-noise ratio
#' @param cwt_min_peak_spacing minimum peak spacing
#' @param subset_polarity "any", "positive" or "negative"
#' @param subset_scan_number A two numeric vector. 
#' Can be c(NA, NA) if don't use this.
#' @param subset_scan_time A two numeric vector. 
#' Can be c(NA, NA) if don't use this.
#' @param subset_mslevels A two numeric vector. Second can be set as NA. 
#' @param zero_samples_mode "no", "removeExtra", or "addMissing".
#' @param zero_samples_mslevels A two numeric vector. Second can be set as NA.
#' @param zero_samples_add_missing_flanking_zero_count = 5
#' @return error or code list
#' @export
#' @examples
#' parameter =
#' create_msconvert_parameter(
#'   output_format = "mzXML",
#'   binary_encoding_precision = "64",
#'   zlib = TRUE,
#'   write_index = TRUE,
#'   peak_picking_algorithm = "vendor",
#'   vendor_mslevels = c(1, 2),
#'   cwt_mslevels = c(1, NA),
#'   cwt_min_snr = 0.1,
#'   cwt_min_peak_spacing = 0.1,
#'   subset_polarity = "any",
#'   subset_scan_number = c(NA, NA),
#'   subset_scan_time = c(NA, NA),
#'   subset_mslevels = c(1, NA),
#'   zero_samples_mode = "no",
#'   zero_samples_mslevels = c(1, NA),
#'   zero_samples_add_missing_flanking_zero_count = 5
#' )
#' parameter

create_msconvert_parameter <-
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
           zlib = TRUE,
           write_index = TRUE,
           peak_picking_algorithm = c("vendor", "cwt", "no"),
           vendor_mslevels = c(1, NA),
           cwt_mslevels = c(1, NA),
           cwt_min_snr = 0.1,
           cwt_min_peak_spacing = 0.1,
           subset_polarity = c("any", "positive", "negative"),
           subset_scan_number = c(NA, NA),
           subset_scan_time = c(NA, NA),
           subset_mslevels = c(1, NA),
           zero_samples_mode = c("no", "removeExtra", "addMissing"),
           zero_samples_mslevels = c(1, NA),
           zero_samples_add_missing_flanking_zero_count = 5) {
    
    ####check parameters
    result <-
    check_msconvert_parameter(
      output_format = output_format,
      binary_encoding_precision = binary_encoding_precision,
      zlib = zlib,
      write_index = write_index,
      peak_picking_algorithm = peak_picking_algorithm,
      vendor_mslevels = vendor_mslevels,
      cwt_mslevels = cwt_mslevels,
      cwt_min_snr = cwt_min_snr,
      cwt_min_peak_spacing = cwt_min_peak_spacing,
      subset_polarity = subset_polarity,
      subset_scan_number = subset_scan_number,
      subset_scan_time = subset_scan_time,
      subset_mslevels = subset_mslevels,
      zero_samples_mode = zero_samples_mode,
      zero_samples_mslevels = zero_samples_mslevels,
      zero_samples_add_missing_flanking_zero_count = 
        zero_samples_add_missing_flanking_zero_count
    )
    
    parameter <- new(
        Class = "msconvert_parameter",
        pacakge_name = "massconverter",
        function_name = "create_msconvert_parameter",
        parameter = list(
          output_format = output_format,
          binary_encoding_precision = binary_encoding_precision,
          zlib = zlib,
          write_index = write_index,
          peak_picking_algorithm = peak_picking_algorithm,
          vendor_mslevels = vendor_mslevels,
          cwt_mslevels = cwt_mslevels,
          cwt_min_snr = cwt_min_snr,
          cwt_min_peak_spacing = cwt_min_peak_spacing,
          subset_polarity = subset_polarity,
          subset_scan_number = subset_scan_number,
          subset_scan_time = subset_scan_time,
          subset_mslevels = subset_mslevels,
          zero_samples_mode = zero_samples_mode,
          zero_samples_mslevels = zero_samples_mslevels,
          zero_samples_add_missing_flanking_zero_count = 
            zero_samples_add_missing_flanking_zero_count
        ),
        time = Sys.time()
      )

    parameter
  }


##S4 class for parameter
#' An S4 class that stores the parameters
#' @docType class
#' @slot pacakge_name pacakge_name
#' @slot function_name function_name
#' @slot parameter parameter
#' @slot time time
#' @exportClass msconvert_parameter


setClass(
  Class = "msconvert_parameter",
  representation(
    pacakge_name = "character",
    function_name = "character",
    parameter = "list",
    time = "POSIXct"
  )
)

setMethod(
  f = "show",
  signature(object = "msconvert_parameter"),
  definition = function(object) {
    cat(crayon::yellow(paste(rep("-", 20), collapse = ""), "\n"))
    cat(crayon::green("pacakge_name:", object@pacakge_name), "\n")
    cat(crayon::green("function_name:", object@function_name),
        "\n")
    cat(crayon::green("time:", object@time), "\n")
    cat(crayon::green("parameters:\n"))
    for (idx in seq_along(object@parameter)) {
      cat(crayon::green(names(object@parameter)[idx], ":" , object@parameter[idx]), "\n")
    }
  }
)