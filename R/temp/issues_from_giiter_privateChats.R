# studentGitterUsername = "kkqq123123123"

#' Extract private gitter message and output its html content to the clipboard
#'
#' @description Copy the first two line of a chat (first line is like "kkqq123123123 @kkqq123123123 12:23", the second line sentence will be used to define the starting location of message extraction.)
#' @return
#' @export
#'
#' @examples none.
extractPrivateGitterChatMessage <- function(){
  require(dplyr); require(purrr)
  inputText <- clipr::read_clip()
  inputText[[1]] %>%
    stringr::str_extract(
      "[:graph:]+(?=\\s@)"
    ) -> studentGitterUsername
  gt <- gitterService()
  gt$list_allMyRooms() -> myRooms
  myRooms %>% keep(
    ~.x$name==studentGitterUsername
  ) -> targetRooms
  roomId <- targetRooms %>%
    map_chr(~.x$id)
  msg1 <- gt$list_messagesInRoom100limit(roomId=roomId)
  compactMsg1 <- compact_messages(msg1)
  # View(compactMsg1$html)

  compactMsg1$text %>% stringr::str_which(inputText[[1]]) ->
    beginRow
  endingText <- rstudioapi::showPrompt(title="Ending Text",message="Please copy and paste the ending sentence:")
  compactMsg1$text %>% stringr::str_which(endingText) ->
    endRow

  compactMsg1Selected <- compactMsg1[beginRow:endRow,]
  # compactMsg1Selected %>% View()

  compactMsg1Selected$html %>%
    map(
      ~{c(
        .x,
        "***\n"
      )}
    ) %>%
    unlist() -> htmlConcatenated

  clipr::write_clip(htmlConcatenated)
}


