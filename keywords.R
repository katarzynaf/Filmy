# keywords
keywords <- function(link){
      # przejscie do unikalnej strony z keywords
      key_link <- paste0(link, "/keywords?ref_=tttg_ql_4")
      pages <- html(key_link)
      # keywords
      key_movie <- getNodeSet(pages, "//div[@class='sodatext']") %>%
            xml_text %>%
            stri_trim_both
      if(length(key_movie) == 0) return(NA)
      # zwracamy wektor
      vec <- scal(key_movie)
      names(vec) <- "keywords"
      return(vec)
}