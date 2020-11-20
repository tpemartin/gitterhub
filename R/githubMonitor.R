#' Get user events whose created at time is after certain time
#'
#' @param username A character
#' @param startTime A Date/Time
#'
#' @return A list of possible events that fit the criterion
#' @export
#'
#' @examples none
getUserEvents_afterTime <- function(username, startTime){
  if(!is.POSIXct(startTime) || !is.POSIXt(startTime)){
    stop("startTime must be a Date/Time class")
  }
  it=0; flag_lastPage=F; max_it=20
  eventStack <- list()
  while(!flag_lastPage && it< max_it){
    it=it+1
    newBatchOfEvents <- getUserEvents30(username,it)
    eventStack <- append(
      eventStack,
      newBatchOfEvents
    )
    if(lubridate::ymd_hms(newBatchOfEvents[[30]]$created_at) < startTime) { # 若最舊event在時間前，最後頁了
      flag_lastPage=T
    }
  }
  return(eventStack)
}


# helpers -------------------------------------------------------------------


getUserEvents100 <- function(username, pageNumber=1){
  postingMessage = glue::glue("GET /users/{username}/events")
  requestFun <- github_apiFunctionalOnePage(postingMessage)
  requestFun(
    query=list(
      page=pageNumber,
      per_page=100
    )
  )
}
getUserEvents30 <- function(username, pageNumber=1){
  postingMessage = glue::glue("GET /users/{username}/events")
  requestFun <- github_apiFunctionalOnePage(postingMessage)
  requestFun(
    query=list(
      page=pageNumber,
      per_page=30
    )
  )
}
utc2twTime <- function(x){
  lubridate::with_tz(
    lubridate::ymd_hms(x),
    tzone="Asia/Taipei"
  )
}


getUserReposAll <- function(){
  count=1; maxcount=10
  pageLength=100
  allrepos <- list()
  while(pageLength==100 && count < maxcount){
    myRepos <- getUserRepos(
      query=list(
        page=count,
        per_page=100
      )
    )
    allrepos <- append(allrepos, myRepos)
    pageLength <- length(myRepos)
    count=count+1
  }
  allrepos
}

# getUserReposAll <- function(){
#   count=1; maxcount=10
#   pageLength=100
#   allrepos <- list()
#   while(pageLength==100 && count < maxcount){
#     myRepos <- getUserRepos(
#       query=list(
#         page=count,
#         per_page=100
#       )
#     )
#     allrepos <- append(allrepos, myRepos)
#     pageLength <- length(myRepos)
#     count=count+1
#   }
#   allrepos
# }




getReposUpdatedAfterADate <- function(afterDateTime){
  # afterDate="2020-11-03 12:55:00"
  require(dplyr)
  require(lubridate)
  require(purrr)
  getUserReposAll() %>%
    purrr::keep(
      ~{ymd_hms(.x$updated_at) >
          afterDateTime}
    ) -> latestUpdatedRepos

  map_dfr(
    latestUpdatedRepos,
    ~{
      data.frame(
        id=.x$id,
        full_name=.x$full_name,
        updated_at=.x$updated_at
      )
    }
  ) -> reposeLastUpdated


}

