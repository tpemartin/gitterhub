
#' Create a Github service
#'
#' @return a list of service
#' @export
#'
#' @examples
#' gs <- githubService()
#' gs$get_userInfo
#' gs$get_allRepos(username = "tpemartin")
#' gs$get_oneRepoInfo(owner = "tpemartin", repo="109-1-inclass-practice")
#' gs$get_repoForks(owner = "tpemartin", repo="109-1-inclass-practice")
githubService <- function(){
  require("httr")
  list(
    get_userInfo=get_user(),
    get_allRepos=list_reposOfAUser,
    get_oneRepoInfo=get_repoInfo,
    get_repoForks=get_repoForks,
    choose_courseRepoFromLatest30=choose_courseRepoFromLatest30

  )

}
# gs <- githubService()
# gs$get_userInfo
# gs$get_allRepos(username = "tpemartin")
# gs$get_oneRepoInfo(owner = "tpemartin", repo="109-1-inclass-practice")
# gs$get_repoForks(owner = "tpemartin", repo="109-1-inclass-practice")


# helpers -----------------------------------------------------------------

github_oauth <- function(){
  httr::oauth2.0_token(
    endpoint=httr::oauth_endpoints("github"),
    app=httr::oauth_app(
      appname="Class management",
      key="02df56bc561d2c62aa5f",
      secret="b94678c95157f22e381e1b13fe7e6ab8c208cdc7",
      redirect_uri = httr::oauth_callback()
    ),
    scope = set_githubScopes()
  )
}
set_githubScopes <- function(){
  #
  c(
    "repo"
  )
}
rootEndpoints <- function(platformName){
  switch(
    platformName,
    "github"="https://api.github.com",
    "gitter"="https://api.gitter.im",
    "hypothesis"="https://hypothes.is/api/"
  )

}

"GET /repos/:owner/:repo/issues"


github_apiFunctionalOnePage <- function(postingMessage){
  split_postingMessage=stringr::str_split(postingMessage,"\\s")
  VERB=split_postingMessage[[1]][[1]]
  path=split_postingMessage[[1]][[2]]
  VERB=as.name(VERB)
  require("httr")
  endpoint=gitterhub:::rootEndpoints("github")
  function(...){
    requestExpr=rlang::quo({
      loadNamespace("httr")
      (!!VERB)(
        url=endpoint,
        path=path,
        config=httr::config(token=github_oauth()),...
      )
    })

    response <- rlang::eval_tidy(
      requestExpr
    )
    # response
    content(response)
  }
}
choose_courseRepoFromLatest30 <- function(username){
  list_latest30ReposOfAUser <- list_reposOfAUser_apiFun(username)
  list_latest30ReposOfAUser(
    query=list(
      type="owner",
      sort="created",
      direction="desc"
    )
  ) -> myRepos
  purrr::map_chr(
    myRepos,
    ~.x$full_name
  ) -> repoNames
  paste0(seq_along(repoNames),": ",repoNames,"\n") -> items
  message(items)
  choice <- as.integer(readline("請選數字："))
  courseRepo <- myRepos[[choice]]
}
get_multiplePages <- function(apiFun){
  allpages <- newpage <- apiFun(query=list(page=1, per_page=100))
  count=0; max_count=30
  while(length(newpage)==100 && count < max_count){
    count=count+1
    newpage=apiFun(
      query=list(
        page=count+1,
        per_page=100
      )
    )
    allpages <- append(allpages, newpage)
  }
  allpages
}
filename="README.md"
owner="tpemartin"
repo="109-1-inclass-practice"
path="hello.txt"
commitMessage="test"

upload2fork <- function(owner,repo,path,commitMessage, filename){
  postingMessage = glue::glue("PUT /repos/{owner}/{repo}/contents/{path}")
  content <- readLines(con=filename)
  filecontent=jsonlite::base64_enc(content)
  body=list(
    message=commitMessage,
    content=filecontent
  )
  requestFun <- github_apiFunctionalOnePage(postingMessage)
  requestFun(body=jsonlite::toJSON(body, auto_unbox = T))
}

get_user <- github_apiFunctionalOnePage("GET /user")
# GET /repos/:owner/:repo/forks
get_repoForks_apiFun <- function(owner, repo){
  requestFun <- github_apiFunctionalOnePage(
    glue::glue("GET /repos/{owner}/{repo}/forks")
  )
  requestFun
}
# owner="tpemartin"
# repo="109-1-inclass-practice"
get_repoForks <- function(owner, repo){
  apiFun <- get_repoForks_apiFun(owner, repo)
  get_multiplePages(apiFun)
}
# forks <- get_repoForks(owner, repo)
get_repoInfo <- function(owner, repo){
  requestFun <- github_apiFunctionalOnePage(
    glue::glue("GET /repos/{owner}/{repo}")
  )
  requestFun()
}
list_reposOfAUser_apiFun <- function(username){
  requestFun <- github_apiFunctionalOnePage(
    glue::glue("GET /users/{username}/repos"))
  requestFun
}
list_reposOfAUser <- function(username){
  #username <- "tpemartin"
  apiFun <- list_reposOfAUser_apiFun(username)
  get_multiplePages(apiFun)
}
# list_reposOfAUser('tpemartin') -> myrepos


