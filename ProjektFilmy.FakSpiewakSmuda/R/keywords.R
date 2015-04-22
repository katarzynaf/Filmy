#' Pobieranie slow kluczowych o filmie.
#'
#' Funkcja \code{keywords} pobiera i zapisuje do wektora kluczowe slowa dotyczace filmu.
#'
#' @aliases keywords
#' @param link link do strony glownej filmu na IMDb'ie, \code{typu character}
#' @param sep znak rozdzielania wielu informacji w jednej komorce danych
#' @return jednowierszowa ramka danych o nazwach kolumn: "title"|"year"|"duration"|"genres"|"rating"|"votes"
#' @import stringi
#' @import dplyr
#' @import rvest
#' @export

keywords <- function(link, sep=",") {
    # przejscie do unikalnej strony z keywords
    key_link <- paste0(link, "/keywords?ref_=tttg_ql_4")
    pages <- html(key_link)
    # keywords
    key_movie <- getNodeSet(pages, "//div[@class='sodatext']") %>% xml_text %>% stri_trim_both
    if (length(key_movie) == 0)
        return(NA)
    # zwracamy wektor
    vec <- paste0(key_movie, collapse=sep)
    names(vec) <- "keywords"
    return(vec)
    rm(vec,pages)
}
