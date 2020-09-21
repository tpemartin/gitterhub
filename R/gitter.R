#' Gitter OAuth2.0 process
#'
#' @return
#' @export
#'
#' @examples gitter_oauth()
gitter_oauth <- function(){
  httr::oauth2.0_token(
    endpoint = gitterhub:::gitterEndpoint(),
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
    list_usersInARoomAsDataFrame=list_usersInARoomAsDataFrame,
    list_messagesInRoom100limit=list_messagesInRoom100limit,
    get_threadMessagesInRoomFromAMessage=get_threadMessagesInRoomFromAMessage,
    search_messagesInRoom100limit=search_messagesInRoom100limit,
    list_compactMessagesInRoom100limit=list_compactMessagesInRoom100limit
  )
}

# helpers -----------------------------------------------------------------
gitterEndpoint = function(){
  httr::oauth_endpoint(
    request=NULL,
    authorize = "https://gitter.im/login/oauth/authorize",
    access="https://gitter.im/login/oauth/token"
  )
}
# gitterApiUrl="https://api.gitter.im"
#
# usethis::use_data(gitterEndpoint, gitterApiUrl, internal=T, overwrite=T)

list_usersInARoom_apiFun <- function(roomId){
  stringr::str_replace(
    "GET /v1/rooms/:roomId/users",
    ":roomId",
    roomId
  ) -> postingMessage
  gitter_apiFunctional(postingMessage)
}

list_usersInARoom <- function(roomId){
  list_30usersInARoom <- list_usersInARoom_apiFun(roomId)

  allusers <- newusers <- list_30usersInARoom(
    query=list(
      skip=0
    )
  )
  count=0; max_count=10
  while(length(allusers)==30 && count <= max_count){
    count=count+1
    skip=count*30
    newusers <- list_30usersInARoom(
      query=list(
        skip=skip
      )
    )
    allusers <- append(allusers, newusers)
  }
  allusers
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

list_messagesInRoom100limit <- function(roomId){
  # roomId="5f60610fd73408ce4feee869"
  postingMessage=glue::glue("GET /v1/rooms/{roomId}/chatMessages")
  list_messagesInRoom_apiFun <- gitter_apiFunctional(postingMessage)
  list_messsages <- list_messagesInRoom_apiFun(
    query=list(
      limit=100
    )
  )

}
search_messagesInRoom100limit <- function(roomId, searchKeyword){
  # roomId="5f60610fd73408ce4feee869"
  postingMessage=glue::glue("GET /v1/rooms/{roomId}/chatMessages")
  list_messagesInRoom_apiFun <- gitter_apiFunctional(postingMessage)
  list_messsages <- list_messagesInRoom_apiFun(
    query=list(
      limit=100,
      q=searchKeyword
    )
  )

}

get_threadMessagesInRoomFromAMessage <- function(roomId, messageId){
  postingMessage=glue::glue("GET /v1/rooms/{roomId}/chatMessages/{messageId}/thread")
  list_messagesInRoom_apiFun <- gitter_apiFunctional(postingMessage)
  list_messsages <- list_messagesInRoom_apiFun(
    query=list(
      limit=100
    )
  )

}

browseMessage <- function(messageId){
  browseURL(
  glue::glue("https://gitter.im/gitter/api?at={messageId}")
  )
}
get_1message <- function(roomId, messageId){

  glue::glue("GET https://api.gitter.im/v1/rooms/{roomId}/chatMessages/{messageId}") -> postingMessage
  apifun <- gitter_apiFunctional(postingMessage)
  apifun()
}

gitter_apiFunctional <- function(postingMessage){
  split_postingMessage=stringr::str_split(postingMessage,"\\s")
  VERB=split_postingMessage[[1]][[1]]
  path=split_postingMessage[[1]][[2]]
  VERB=as.name(VERB)
  library("httr")
  function(...){
    group_vars=rlang::enquos(...)
    requestExpr=rlang::quo({
      loadNamespace("httr")
      (!!VERB)(
        url=gitterhub:::rootEndpoints("gitter"),
        path=path,
        config=httr::config(token=gitter_oauth()),!!!group_vars
      )
    })

    response <- rlang::eval_tidy(
      requestExpr
    )

    content(response)
  }
}

gen_groupIndex <- function(userIds){
  count=0;
  previousId="0"
  group_index <- c()
  for(.x in seq_along(userIds)){
    count <- ifelse(userIds[[.x]]!=previousId,
                    count+1, count)
    previousId=userIds[[.x]]
    group_index <- c(
      group_index,
      count
    )
  }
  group_index
}

compact_messages <- function(originalMSGs){
  # 同一人連發的訊息會有不同messageId, 但實際上是同一段對話的一小部份
  library(dplyr)
  originalMSGs %>%
    purrr::map_chr(
      ~{.x$fromUser$id}
    ) %>%
    unlist() -> userIds

  group_index <- gen_groupIndex(userIds)
  group_index_unique <- unique(group_index)
  validPosts <- vector("list", length(group_index_unique))
  idnames <- vector("list", length(group_index_unique))
  messageIds <- vector("list", length(group_index_unique))
  textContent <- vector("character", length(group_index_unique))
  for(.x in group_index_unique){
    whichBelong2Index <- which(group_index==group_index_unique[[.x]])
    validPosts[[.x]] <- originalMSGs[whichBelong2Index]
    validPosts[[.x]] %>%
      purrr::map(~.x$text) %>%
      purrr::reduce(paste, collpase="\n") -> textContent[[.x]]
    idnames[[.x]] <- originalMSGs[[whichBelong2Index[[1]]]]$fromUser[c("id", "username","displayName","avatarUrlSmall")]
    messageIds[[.x]] <- purrr::map_chr(originalMSGs[whichBelong2Index], ~.x$id)
  }
  tibble(
    userId=purrr::map_chr(idnames, ~.x[[1]]),
    username=purrr::map_chr(idnames, ~.x[[2]]),
    displayName=purrr::map_chr(idnames, ~.x[[3]]),
    avatar=purrr::map_chr(idnames, ~.x[[4]]),
    messageIds=messageIds,
    text=textContent,
    messages=validPosts
  ) -> compactMessage
  compactMessage
}

# forEach_getMessageId <-function(detail){
#   purrr::map(
#     detail,
#     ~purrr::pluck(.x,"id")
#   )
# }
# forEach_getMessageId <-function(detail){
#   purrr::map(
#     detail,
#     ~purrr::pluck(.x,"id")
#   )
# }
list_compactMessagesInRoom100limit <- function(roomId){
  msgs <- list_messagesInRoom100limit(roomId)

  msgsCompact <- compact_messages(msgs)

  msgsCompact
}
