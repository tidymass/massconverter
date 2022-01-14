#' @title convert_raw_data
#' @description Convert raw data to different format data.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param input_path input path (raw data)
#' @param output_path output path. If missing, same with input_path.
#' @param msconvert_parameter msconvert parameters. See 
#' @param docker_parameters docker_parameters
#' @param process_all process all raw data together or one by one.
#' @return mzXML or something like this format.
#' @export
#' @examples
#' parameter =
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
#' parameter
#' 
#' convert_raw_data(input_path = "demo_data/raw_data",
#'                  output_path = "demo_data/mzxml",
#'                  msconvert_parameter = parameter,
#'                  process_all = FALSE
#'                  )

convert_raw_data <-
  function(input_path = ".",
           output_path,
           msconvert_parameter,
           docker_parameters = c(),
           process_all = FALSE) {
    dir.create(input_path, showWarnings = FALSE, recursive = TRUE)
    dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
    ###check docker is available or not
    if (!stevedore::docker_available()) {
      stop("Please install and run docker first 
           (https://www.docker.com/get-started).\n")
    }
    
    ##pull pwiz image
    docker_pull_pwiz(force = FALSE)
    
    input_path <- normalizePath(input_path)
    if (missing(output_path)) {
      output_path <- input_path
    } else{
      output_path <- normalizePath(output_path)
    }
    dir.create(output_path, showWarnings = FALSE)
    file_name <- dir(input_path, full.names = TRUE) %>%
      normalizePath()
    if (length(file_name) == 0) {
      warning("No raw data in ", input_path)
      return(NULL)
    }
    ###docker parameters
    docker_parameters <- paste(docker_parameters, collapse = ' ')
    docker_run_code <-
      paste(
        "docker run --rm -e WINEDEBUG=-all ",
        docker_parameters,
        "-v ",
        input_path,
        ':/data',
        ' -v ',
        output_path,
        ':/outpath ',
        'chambm/pwiz-skyline-i-agree-to-the-vendor-licenses wine msconvert ',
        sep = ""
      )
code <-
  from_msconvert_parameter_to_code(msconvert_parameter = msconvert_parameter)
    code <-
      paste0(' --ignoreUnknownInstrumentError ',
             code)
    if (process_all) {
      ####for each type of raw data
      file_extension <- paste0("*.", 
                              unique(tools::file_ext(basename(file_name))))
      for (data_type in file_extension) {
        run_code <-
          paste0(docker_run_code, '/data/', data_type, code, '  -o /outpath/')
        system(run_code, intern = FALSE)
      }
    } else{
      for (i in file_name) {
        run_code <-
          paste0(docker_run_code, '/data/', basename(i), code, 
                 '  -o /outpath/')
        system(run_code, intern = FALSE)
      }
    }
  }