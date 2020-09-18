#' Define the root endpoint of api service
#'
#' @param platformName A character of "github", "gitter"
#'
#' @return endpoint url
#' @export
#'
#' @examples none
rootEndpoints <- function(platformName){
  switch(
    platformName,
    "github"="https://api.github.com",
    "gitter"="https://api.gitter.im"
  )

}
