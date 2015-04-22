#' Rozklad glosow w grupach plciowo-wiekowych
#'
#' Funkcja \code{user_rating_stats} zwraca trzykolumnowa tabelke z rozkladem glosow w poszczegolnych grupach
#' plciowo-wiekowych.
#'
#' @aliases user_rating_stats
#' @param link link do strony glownej filmu na IMDb'ie, \code{typu character}
#' @return trzykolumnowa ramka danych o nazwach kolumn: "Who"|"Votes"|"Average"
#' @import rvest
#' @import stringi
#' @export

user_rating_stats <- function(link) {
      link <- paste0(link, ifelse(stri_sub(link, -1) == "/","","/"),"ratings")
      user_rating_stats <- readHTMLTable(link)[[2]]
      if (names(user_rating_stats)[3] != "Average") {
            user_rating_stats <- NA
            return(user_rating_stats)
      }
      names(user_rating_stats)[1] <- "Who"
      user_rating_stats[, 2] <- stri_extract_all_regex(user_rating_stats[, 2], "[0-9]+", simplify = TRUE)
      user_rating_stats[, 3] <- stri_extract_all_regex(user_rating_stats[, 3], "[0-9]+", simplify = TRUE)
      user_rating_stats[, 3] <- stri_paste(user_rating_stats[, 3][, 1], user_rating_stats[, 3][, 2], sep = ".")
      user_rating_stats <- user_rating_stats[-which(is.na(user_rating_stats[, 2])), ]
      rownames(user_rating_stats) <- seq_len(nrow(user_rating_stats))
      return(user_rating_stats)
}
