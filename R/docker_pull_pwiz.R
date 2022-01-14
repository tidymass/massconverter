#' @title docker_pull_pwiz
#' @description Pull docker pwiz image
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param force Force to install pwiz
#' @return docker pwiz image
#' @export
#' @examples
#' docker_pull_pwiz(force = FALSE)


docker_pull_pwiz <- function(force = FALSE) {
  ###check docker is available or not
  if (!stevedore::docker_available(verbose = FALSE)) {
    stop("Please install docker first (https://www.docker.com/get-started).\n")
  }
  
  ##construct an object with which we can talk with the docker server
  docker <- stevedore::docker_client()
  ###what images we have had in docker
  images <- docker$image$list()
  
  ###check if we have pwiz image or not
  pwiz_available =
    any(
      unlist(images$repo_tags) ==
        "chambm/pwiz-skyline-i-agree-to-the-vendor-licenses:latest"
    )
  
  ###if force is true
  if (force) {
    docker$image$pull('chambm/pwiz-skyline-i-agree-to-the-vendor-licenses')
    message("Done.\n")
  } else{
    if (pwiz_available) {
      message("You have installed pwiz.\n")
    } else{
      docker$image$pull('chambm/pwiz-skyline-i-agree-to-the-vendor-licenses')
      message("Done.\n")
    }
    
  }
}