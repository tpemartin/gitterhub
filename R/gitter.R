#' Gitter OAuth2.0 process
#'
#' @return
#' @export
#'
#' @examples gitter_oauth()
gitter_oauth <- function(){
  httr::oauth2.0_token(
    endpoint = gitterEndpoint,
    app = httr::oauth_app(
      appname="Gitterhub",
      key="3855c927a5befc2a7783809e90fda33b8d44f5a3",
      secret="a1f104c95c63717255eda4e588d4f2eb11eb44d8",
      redirect_uri = "http://localhost:1410/"
    )
  ) -> .token
  .token
}


#' Create gitter service
#'
#' @description The service contains list_groups/list_groupsAsDataframe, list_roomsUnderAGroup/list_roomsUnderAGroupAsDataFrame(groupId), list_usersInARoom/list_usersInARoomAsDataFrame(roomId)
#' @return
#' @export
#'
#' @examples gs <- gitterService()
gitterService <- function(){
  list(
    list_groups=gitter_apiFunctional("GET /v1/groups"),
    list_groupsAsDataframe=list_groupsAsDataframe,
    list_roomsUnderAGroup=list_roomsUnderAGroup,
    list_roomsUnderAGroupAsDataFrame=
      list_roomsUnderAGroupAsDataFrame,
    list_usersInARoom=list_usersInARoom,
    list_usersInARoomAsDataFrame=list_usersInARoomAsDataFrame
  )
}

# helpers -----------------------------------------------------------------
# gitterEndpoint = httr::oauth_endpoint(
#   request=NULL,
#   authorize = "https://gitter.im/login/oauth/authorize",
#   access="https://gitter.im/login/oauth/token"
# )
# gitterApiUrl="https://api.gitter.im"
#
# usethis::use_data(gitterEndpoint, gitterApiUrl, internal=T, overwrite=T)

list_usersInARoom <- function(roomId){
  stringr::str_replace(
    "GET /v1/rooms/:roomId/users",
    ":roomId",
    roomId
  ) -> postingMessage
  (gitter_apiFunctional(postingMessage))()
}

list_usersInARoomAsDataFrame <- function(roomId){
  roomUsers=list_usersInARoom(roomId)
  purrr::map_dfr(
    roomUsers,
    ~{.x}
  )
}
list_groupsAsDataframe <- function(){
  list_groups=gitter_apiFunctional("GET /v1/groups")
  list_groups() -> myGroups
  purrr::map_dfr(myGroups,~{.x[c("id","name")]})
}

list_roomsUnderAGroup <- function(groupId){
  stringr::str_replace(
    "GET /v1/groups/:groupId/rooms",
    ":groupId",
    groupId
  ) -> postingMessage
  (gitter_apiFunctional(postingMessage))()
}
list_roomsUnderAGroupAsDataFrame <- function(groupId){
  rooms=list_roomsUnderAGroup(groupId)
  purrr::map_dfr(rooms,
             ~{.x[c("id","name")]})
}


gitter_apiFunctional <- function(postingMessage){
  split_postingMessage=stringr::str_split(postingMessage,"\\s")
  VERB=split_postingMessage[[1]][[1]]
  path=split_postingMessage[[1]][[2]]
  VERB=as.name(VERB)
  library("httr")
  function(...){
    requestExpr=rlang::quo({
      loadNamespace("httr")
      (!!VERB)(
        url=gitterApiUrl,
        path=path,
        config=httr::config(token=gitter_oauth()),...
      )
    })

    response <- rlang::eval_tidy(
      requestExpr
    )

    content(response)
  }
}
