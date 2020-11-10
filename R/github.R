
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
    choose_courseRepoFromLatest30=choose_courseRepoFromLatest30,
    list_issues=list_issues,
    create_issue=create_issue,
    get_userProfile=get_userProfile_github

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
github_apiFunctionalOnePageRaw <- function(postingMessage){
  split_postingMessage=stringr::str_split(postingMessage,"\\s")
  VERB=split_postingMessage[[1]][[1]]
  path=split_postingMessage[[1]][[2]]
  VERB=as.name(VERB)
  require("httr")
  endpoint=rootEndpoints("github")
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

    response
  }
}
github_apiFunctionalOnePage <- function(postingMessage){
  split_postingMessage=stringr::str_split(postingMessage,"\\s")
  VERB=split_postingMessage[[1]][[1]]
  path=split_postingMessage[[1]][[2]]
  VERB=as.name(VERB)
  require("httr")
  endpoint=rootEndpoints("github")
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
# filename="README.md"
# owner="tpemartin"
# repo="109-1-inclass-practice"
# path="hello.txt"
# commitMessage="test"

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

get_me <- github_apiFunctionalOnePageRaw("GET /users/me")
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

# owner="tpemartin"
# repo="course-programming-for-data-science"
# myIssues <- list_issues(owner, repo)
list_issues <- function(owner, repo){
  postingMessage=glue::glue("GET /repos/{owner}/{repo}/issues")
  github_apiFunctionalOnePage(postingMessage) -> list_issuesFun
  list_issuesFun()
}

create_issue <- function(owner, repo, .title, .body, ...){
  postingMessage=glue::glue("POST /repos/{owner}/{repo}/issues")
  create_issueFun <- github_apiFunctionalOnePage(postingMessage)
  create_issueFun(
    body=jsonlite::toJSON(
      list(
        title=.title,
        body=.body,
        ...
      ), auto_unbox = T
    ))
}

getUserRepos <- function(username, ...){
  postingMessage = glue::glue("GET /user/repos")
  requestFun <- github_apiFunctionalOnePage(postingMessage)
  requestFun(...)
}

get_userProfile_github <- function(){
  requestFun <- github_apiFunctionalOnePage("GET /user")
  requestFun()
}

# .title="test gitter-repost"
# .body="[![image.png](https://files.gitter.im/5f60610fd73408ce4feee869/vSEg/thumb/image.png)](https://files.gitter.im/5f60610fd73408ce4feee869/vSEg/image.png) Your name is: library(econDS); setup() \n 老師 請問我名字打錯的話 有方法可以再重打一次嗎 \n"
# create_issue(owner,repo, .title, .body, labels=list("gitter"))
