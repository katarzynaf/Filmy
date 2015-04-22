#' Pobieranie informacji z kodu zrodlowego strony glownej filmu w bazie IMDb.
#'
#' Funkcja \code{from_page_source} zwraca jednowierszowa ramke danych z informacja o krajach produkcji,
#' jezyku danego filmu oraz wersji kolorystycznej. Dane wyszukiwane sa bezposrednio z kodu
#' zrodlowego danej strony. Ewentualnych kilka nazw (jak np. przy gatunku) rozdzielanych jest
#' separatorem \code{sep}.
#'
#' @aliases from_page_source
#' @param link link do strony glownej filmu na IMDb'ie, \code{typu character}
#' @param sep znak rozdzielania wielu informacji w jednej komorce danych
#' @return jednowierszowa ramka danych o nazwach kolumn: "production_countries"|"language"|"color"
#' @import stringi
#' @export

from_page_source <- function(link, sep=",") {
    page <- readLines(link)
    page <- paste(page, collapse = "")
    znaczniki <- c(production_countries = "(?<=Country:).+?(?=</div)", language = "(?<=Language:).+?(?=</div)", color = "(?<=Color:).+?(?=</a)")
    details <- function(znacznik) {
        item <- unlist(stri_extract_all_regex(page, znacznik))
        if (!is.na(item)) {
            item <- unlist(stri_extract_all_regex(item, "(?<=itemprop='url'>)([a-zA-Z]| )+"))
            paste0(item, collapse=sep)
        } else item <- NA
    }
    a <- sapply(znaczniki, details)
    names(a) <- names(znaczniki)
    return(a)
    rm(page,a,znaczniki)
}
