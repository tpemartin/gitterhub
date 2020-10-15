gt <- gitterService()
gt$list_allMyRooms() -> myRooms
gitterUsername = "kkqq123123123"
myRooms %>% keep(
  ~.x$name=="kkqq123123123"
) -> targetRooms
roomId <- targetRooms %>%
  map_chr(~.x$id)
msg1 <- gt$list_messagesInRoom100limit(roomId=roomId)
compactMsg1 <- compact_messages(msg1)


compactMsg1$text %>% stringr::str_which("我想要練習這個收集資料的方法") ->
  beginRow
compactMsg1$text %>% stringr::str_which("The essence of computer programmi") ->
  endRow

compactMsg1Selected <- compactMsg1[beginRow:endRow,]
compactMsg1Selected %>% View()

compactMsg1Selected$text %>%
  map(
    ~{c(
      .x,
      "***"
    )}
  ) -> textConcatenated
# View(textConcatenated
#      )
unlist(textConcatenated) -> textConcatenated
textConcatenated %>%
  stringr::str_remove_all(
    "(?<=```)\\{r[\\s[:graph:]]*\\}"
  ) %>%
  stringr::str_replace_all(
    "```(?!\\{)","```\n"
  )  %>%
  stringr::str_replace_all(
    "(?<!^)```","\n```"
  ) -> textConcatenated
textConcatenated %>%
  stringr::str_split("\n") %>%
  unlist() -> textLong
textLong %>%
  stringr::str_trim(side="both") -> textLongNoWhite

which(textLongNoWhite == ">") -> loc_onlyQuote
textLong[loc_onlyQuote] <- ""

textLong %>%
  xfun::write_utf8(con="text.md")
