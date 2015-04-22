#' Rozklad glosow
#'
#' Funkcja \code{rating_stats} zwraca trzykolumnowa tabelke z rozkladem poszczegolnych glosow.
#'
#' @aliases rating_stats
#' @param link link do strony glownej filmu na IMDb'ie, \code{typu character}
#' @return trzykolumnowa ramka danych o nazwach kolumn: "Votes"|"Percentage"|"Rating"
#' @import rvest
#' @import stringi
#' @export

rating_stats <- function(link) {
      link <- paste0(link, ifelse(stri_sub(link, -1) == "/","","/"),"ratings")
      rating_stats <- readHTMLTable(link)[[1]]
      if (ncol(rating_stats) == 1) {
            # gdy nie ma statystyk z glosami
            rating_stats <- NA
            return(rating_stats)
      }
      names(rating_stats) <- stri_extract_all_regex(names(rating_stats), "[a-zA-Z]+", simplify = TRUE)
      rating_stats[, 2] <- stri_extract_all_regex(rating_stats[, 2], "[0-9]{1,2}.[0-9]{1,2}%", simplify = TRUE)
      return(rating_stats)
}
