#' @title massconverter_logo
#' @description Get the detailed information of massconverter package.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @import leaflet
#' @importFrom crayon green blue col_align red black
#' white style make_style num_colors
#' @importFrom purrr walk
#' @importFrom tools file_ext
#' @importFrom magrittr %>%
#' @importFrom stevedore docker_available docker_client
#' @importFrom methods new
#' @importFrom stringr str_trim
#' @export
#' @return logo
#' @examples
#' massconverter_logo()

massconverter_logo <- function() {
  message(crayon::green("Thank you for using massconverter!\n"))
  message(crayon::green("Version",
                        massconverter_version, "(", update_date, ')\n'))
  message(crayon::green("More information:
                        search 'tidymass massconverter'.\n"))
  cat(crayon::green(
    c(
      "                           _____                          _            ",
      "                          / ____|                        | |           ",
      "  _ __ ___   __ _ ___ ___| |     ___  _ ____   _____ _ __| |_ ___ _ __ ",
      " | '_ ` _ \\ / _` / __/ __| |    / _ \\| '_ \\ \\ / / _ \\ '__| __/ _ \\ '__|",
      " | | | | | | (_| \\__ \\__ \\ |___| (_) | | | \\ V /  __/ |  | ||  __/ |   ",
      " |_| |_| |_|\\__,_|___/___/\\_____\\___/|_| |_|\\_/ \\___|_|   \\__\\___|_|   ",
      "                                                                       ",
      "                                                                       "
    )
  ), sep = "\n")
}

massconverter_version <- "0.99.1"
update_date <- "2022-01-08"

#' @title get_massconverter_version
#' @description Get massconverter package version
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @export
#' @return version
#' @examples
#' get_massconverter_version()
get_massconverter_version <- function() {
  return(massconverter_version)
}


# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), 
# con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# massconverter_logo <-
#   c("                          _____        _                 _   ",
#     "                         |  __ \\      | |               | |  ",
#     "  _ __ ___   __ _ ___ ___| |  | | __ _| |_ __ _ ___  ___| |_ ",
#     " | '_ ` _ \\ / _` / __/ __| |  | |/ _` | __/ _` / __|/ _ \\ __|",
#     " | | | | | | (_| \\__ \\__ \\ |__| | (_| | || (_| \\__ \\  __/ |_ ",
#     " |_| |_| |_|\\__,_|___/___/_____/ \\__,_|\\__\\__,_|___/\\___|\\__|",
#     "                                                             ",
#     "                                                             "
#   )
# cat(massconverter_logo, sep = "\n")
