#' @title check_msconvert_parameter
#' @description check_msconvert_parameter
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
#' output_format = "mzXML"
#' binary_encoding_precision = "64"
#' zlib = TRUE
#' write_index = TRUE
#' peak_picking_algorithm = "vendor"
#' vendor_mslevels = c(1, NA)
#' cwt_mslevels = c(1, NA)
#' cwt_min_snr = 0.1
#' cwt_min_peak_spacing = 0.1
#' subset_polarity = "any"
#' subset_scan_number = c(NA, NA)
#' subset_scan_time = c(NA, NA)
#' subset_mslevels = c(1, NA)
#' zero_samples_mode = "no"
#' zero_samples_mslevels = c(1, NA)
#' zero_samples_add_missing_flanking_zero_count = 5
#'
#' check_msconvert_parameter(
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

check_msconvert_parameter <-
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
    ###check range of all the vector arguments
    check_items <-
      list(
        vendor_mslevels = vendor_mslevels,
        cwt_mslevels = cwt_mslevels,
        subset_scan_number = subset_scan_number,
        subset_scan_time = subset_scan_time,
        subset_mslevels = subset_mslevels,
        zero_samples_mslevels = zero_samples_mslevels
      )
    
    purrr::walk2(names(check_items), check_items, function(x, y) {
      if (length(y) != 2) {
        stop(x, " should be a numeric vector with length of 2.\n")
      }
      if (!is.na(y[1]) & !is.na(y[2])) {
        if (y[1] > y[2]) {
          stop(x, "[2] should be bigger than ", x, "[1].\n or set it as NA.\n")
        }
      }
    })
    
    #####Output format
    output_format <- match.arg(output_format)
    
    ####binary_encoding_precision
    binary_encoding_precision <- match.arg(binary_encoding_precision)
    
    ######peak picking
    peak_picking_algorithm <- match.arg(peak_picking_algorithm)
    
    ##vendor
    if (length(vendor_mslevels) != 2) {
      stop("vendor_mslevels should be a numeric vector with length of 2.\n")
    }
    
    if (peak_picking_algorithm == "vendor") {
      if (!vendor_mslevels[1] %in% c(seq_len(3))) {
        stop("vendor_mslevels[1] must be 1, 2 or 3.\n")
      }
    }
    
    if (!is.na(vendor_mslevels[2])) {
      if (!vendor_mslevels[2] %in% c(seq_len(5), NA)) {
        stop("vendor_mslevels[2] must be 1, 2, 3, 4, 5 or NA.\n")
      }
    }
    
    ##cwt
    if (peak_picking_algorithm == "cwt") {
      if (!cwt_mslevels[1] %in% c(seq_len(3))) {
        stop("cwt_mslevels[1] must be 1, 2 or 3.\n")
      }
    }
    
    if (!is.na(cwt_mslevels[2])) {
      if (!cwt_mslevels[2] %in% c(seq_len(5), NA)) {
        stop("cwt_mslevels[2] must be 1, 2, 3, 4, 5 or NA.\n")
      }
    }
    
    if (is.na(vendor_mslevels[2])) {
      vendor_mslevels[2] <- '-'
    } else{
      vendor_mslevels[2] <- paste0('-', vendor_mslevels[2])
    }
    
    if (is.na(cwt_mslevels[2])) {
      cwt_mslevels[2] <- '-'
    } else{
      cwt_mslevels[2] <- paste0('-', cwt_mslevels[2])
    }
    
    if (peak_picking_algorithm == "no") {
      peak_picking <- ""
    }
    
    if (peak_picking_algorithm == "vendor") {
      peak_picking <-
        paste0("peakPicking vendor ",
               "msLevel=",
               vendor_mslevels[1],
               vendor_mslevels[2])
    }
    
    if (peak_picking_algorithm == "cwt") {
      peak_picking <-
        paste0(
          "peakPicking cwt ",
          "snr=",
          cwt_min_snr,
          " peakSpace=",
          cwt_min_peak_spacing,
          " msLevel=",
          vendor_mslevels[1],
          vendor_mslevels[2]
        )
    }
    
    #####subset
    ###polarity
    subset_polarity <- match.arg(subset_polarity)
    if (subset_polarity == "positive") {
      subset_polarity <- paste0("polarity ", "positive")
    }
    
    if (subset_polarity == "negative") {
      subset_polarity <- paste0("polarity ", "negative")
    }
    
    if (subset_polarity == "any") {
      subset_polarity <- ""
    }
    
    ###scan number
    if (is.na(subset_scan_number[1]) |
        is.na(subset_scan_number[2])) {
      scan_number <- ""
    } else{
      scan_number <- paste0("scanNumber [",
                           subset_scan_number[1],
                           ",",
                           subset_scan_number[2],
                           "]")
    }
    
    ###scan time (second)
    if (is.na(subset_scan_time[1]) |
        is.na(subset_scan_time[2])) {
      scan_time <- ""
    } else{
      scan_time <- paste0("scanTime [",
                         subset_scan_time[1],
                         ",",
                         subset_scan_time[2],
                         "]")
    }
    
    ###MS levels
    if (!subset_mslevels[1] %in% c(seq_len(3))) {
      stop("subset_mslevels[1] must be 1, 2 or 3.\n")
    }
    
    if (!is.na(subset_mslevels[2])) {
      if (!subset_mslevels[2] %in% c(seq_len(5), NA)) {
        stop("subset_mslevels[2] must be 1, 2, 3, 4 , 5or NA.\n")
      }
    }
    
    if (is.na(subset_mslevels[2])) {
      subset_mslevels[2] <- '-'
    } else{
      subset_mslevels[2] <- paste0('-', subset_mslevels[2])
    }
    
    subset_mslevels <- paste0("msLevel ", subset_mslevels[1], subset_mslevels[2])
    
    ####zero samples
    zero_samples_mode <- match.arg(zero_samples_mode)
    
    if (!zero_samples_mslevels[1] %in% c(seq_len(3))) {
      stop("zero_samples_mslevels[1] must be 1, 2 or 3.\n")
    }
    
    if (!is.na(zero_samples_mslevels[2])) {
      if (!zero_samples_mslevels[2] %in% c(seq_len(5))) {
        stop("zero_samples_mslevels[2] must be 1, 2, 3, 4 ,5 or NA.\n")
      }
    }
    
    if (is.na(zero_samples_mslevels[2])) {
      zero_samples_mslevels[2] <- '-'
    } else{
      zero_samples_mslevels[2] <- paste0(' ', zero_samples_mslevels[2])
    }
    
    zero_samples_mslevels <- paste0(zero_samples_mslevels[1],
                                   zero_samples_mslevels[2])
    
    if (zero_samples_mode == "no") {
      zero_samples <- ""
    }
    
    if (zero_samples_mode == "removeExtra") {
      zero_samples <- paste0("zeroSamples removeExtra ",
                            zero_samples_mslevels)
    }
    
    if (zero_samples_mode == "addMissing") {
      if (missing(zero_samples_add_missing_flanking_zero_count)) {
        stop("provide zero_samples_add_missing_flanking_zero_count.\n")
      }
      
      zero_samples <- paste(
        paste0(
          "zeroSamples addMissing=",
          zero_samples_add_missing_flanking_zero_count
        ),
        zero_samples_mslevels
      )
    }
    
    result <- list(
      output_format = output_format,
      binary_encoding_precision = binary_encoding_precision,
      zlib = zlib,
      write_index = write_index,
      peak_picking = peak_picking,
      subset_polarity = subset_polarity,
      scan_number = scan_number,
      scan_time = scan_time,
      subset_mslevels = subset_mslevels,
      zero_samples = zero_samples
    )
    return(result)
  }


#' @title from_msconvert_parameter_to_code
#' @description From msconvert_parameter class to running code
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param msconvert_parameter msconvert_parameter class object.
#' @return Running code for docker.
#' @export
#' @examples
#' parameter1 =
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
#' from_msconvert_parameter_to_code(parameter1)
#' 
#' parameter2 =
#'   create_msconvert_parameter(
#'     output_format = "mzXML",
#'     binary_encoding_precision = "32",
#'     zlib = TRUE,
#'     write_index = TRUE,
#'     peak_picking_algorithm = "cwt",
#'     vendor_mslevels = c(1, NA),
#'     cwt_mslevels = c(1, NA),
#'     cwt_min_snr = 0.1,
#'     cwt_min_peak_spacing = 0.1,
#'     subset_polarity = "positive",
#'     subset_scan_number = c(NA, NA),
#'     subset_scan_time = c(60, 300),
#'     subset_mslevels = c(1, 2),
#'     zero_samples_mode = "removeExtra",
#'     zero_samples_mslevels = c(1, NA),
#'     zero_samples_add_missing_flanking_zero_count = 5
#'   )
#' parameter2
#' from_msconvert_parameter_to_code(parameter2)

from_msconvert_parameter_to_code <-
  function(msconvert_parameter) {
    code_list <-
      check_msconvert_parameter(
        output_format = msconvert_parameter@parameter$output_format,
        binary_encoding_precision = 
          msconvert_parameter@parameter$binary_encoding_precision,
        zlib = msconvert_parameter@parameter$zlib,
        write_index = msconvert_parameter@parameter$write_index,
        peak_picking_algorithm = 
          msconvert_parameter@parameter$peak_picking_algorithm,
        vendor_mslevels = msconvert_parameter@parameter$vendor_mslevels,
        cwt_mslevels = msconvert_parameter@parameter$cwt_mslevels,
        cwt_min_snr = msconvert_parameter@parameter$cwt_min_snr,
        cwt_min_peak_spacing = 
          msconvert_parameter@parameter$cwt_min_peak_spacing,
        subset_polarity = msconvert_parameter@parameter$subset_polarity,
        subset_scan_number = msconvert_parameter@parameter$subset_scan_number,
        subset_scan_time = msconvert_parameter@parameter$subset_scan_time,
        subset_mslevels = msconvert_parameter@parameter$subset_mslevels,
        zero_samples_mode = msconvert_parameter@parameter$zero_samples_mode,
        zero_samples_mslevels = 
          msconvert_parameter@parameter$zero_samples_mslevels,
        zero_samples_add_missing_flanking_zero_count = 
          msconvert_parameter@parameter$zero_samples_add_missing_flanking_zero_count
      )
    
    paste0(
      paste0("--", code_list$output_format, " "),
      paste0("--", code_list$binary_encoding_precision, " "),
      ifelse(code_list$zlib, "--zlib ", ""),
      ifelse(code_list$write_index, "", "--noindex "),
      ifelse(code_list$peak_picking == "", "",
             paste0('--filter ', '"', code_list$peak_picking, '" ')),
    
      ifelse(code_list$subset_polarity == "", "", 
             paste0('--filter ', '"', code_list$subset_polarity, '" ')),
      
      ifelse(code_list$scan_number == "", "", 
             paste0('--filter ', '"', code_list$scan_number, '" ')),
      
      ifelse(code_list$scan_time == "", "", 
             paste0('--filter ', '"', code_list$scan_time, '" ')),
      
      ifelse(code_list$subset_mslevels == "", "", 
             paste0('--filter ', '"', code_list$subset_mslevels, '" ')),
      
      ifelse(code_list$zero_samples == "", "", 
             paste0('--filter ', '"', code_list$zero_samples, '" '))
    ) %>% 
      stringr::str_trim()
  }