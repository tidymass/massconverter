#' @title convert_raw_data
#' @description Convert raw data to different format data.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param input_path input path (raw data)
#' @param output_path output path. If missing, same with input_path.
#' @param msconvert_parameters msconvert parameters. See 
#' @param docker_parameters docker_parameters
#' @param process_all process all raw data together or one by one.
#' @return mzXML or something like this format.
#' @export
#' @examples
#' \dontrun{
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
#' convert_raw_data(msconvert_parameters =
#' msconvert_parameters)
#' }

convert_raw_data <-
  function(input_path = ".",
           output_path,
           msconvert_parameters,
           docker_parameters = c(),
           process_all = FALSE) {
    ###check docker is available or not
    if (!stevedore::docker_available()) {
      stop("Please install docker first (https://www.docker.com/get-started).\n")
    }
    
    input_path <- normalizePath(input_path)
    
    if (missing(output_path)) {
      output_path <- input_path
    } else{
      output_path <- normalizePath(output_path)
    }
    
    dir.create(output_path, showWarnings = FALSE)
    
    file_name = dir(input_path,
                    full.names = TRUE) %>%
      normalizePath()
    
    if (length(file_name) == 0) {
      stop("No raw data in ", input_path)
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
    
    code =
      from_msconvert_parameters_to_code(msconvert_parameters = msconvert_parameters)
    
    code <-
      paste0(' --ignoreUnknownInstrumentError ',
             code)
    
    if (process_all) {
      ####for each type of raw data
      file_extension = paste0("*.", unique(tools::file_ext(basename(file_name))))
      
      for (data_type in file_extension) {
        run_code <-
          paste0(docker_run_code,
                 '/data/',
                 data_type,
                 code,
                 '  -o /outpath/')
        system(run_code, intern = FALSE)
      }
    } else{
      for (i in file_name) {
        run_code <-
          paste0(docker_run_code,
                 '/data/',
                 basename(i),
                 code,
                 '  -o /outpath/')
        system(run_code, intern = FALSE)
      }
    }
  }