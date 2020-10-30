#' Create a room under group
#'
#' @param groupId A character of groupId
#' @param name A character determing the prefix of room name attached by its sha1 10-digit code
#' @param topic A character, default=""
#' @param securityType A character, default="PRIVATE". The other option is "PUBLIC"
#'
#' @return
#' @export
#'
#' @examples none.
create_aPrivateRoomUnderGroup <- function(groupId, name,topic="", securityType="PRIVATE")
{
  postingMessage <- glue::glue(
    'POST /v1/groups/{groupId}/rooms'
  )
  create_aRoomUnderGroupFun <- gitter_apiFunctional(postingMessage)
  name <- paste0(name,"_",digest::sha1(name,10))
  security <-
    list(
      security=securityType #'PRIVATE'
    )
  bodyContent <-
    list(
      name=name,
      topic=topic,
      security=security
    )
  create_aRoomUnderGroupFun(
    body=bodyContent,
    encode="json"
  ) -> response
  invisible(response)
}
