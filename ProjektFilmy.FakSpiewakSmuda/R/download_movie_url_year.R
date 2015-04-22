#' Pobieranie linkow do filmow na IMDb
#'
#' Funkcja \code{download_movie_url_year} pobiera wszystkie linki do filmow z bazy IMDb. Wynikiem
#' jest dwukolumnowa ramka danych postaci | year | link |. Ramka zapisywana jest do roznych plikow
#' z rozdzieleniem na lata.
#'
#' @aliases download_movie_url_year
#' @param from filmy od roku...
#' @param to filmy do roku... wlacznie.
#' @return zapis do pliku(ow) w folderze ~/movies_link ramki danych postaci |rok|link|.
#' @import stringi
#' @import dplyr
#' @import rvest
#' @import RCurl
#' @export

download_movie_url_year <- function(from, to = from) {
    stopifnot(from <= to)
    stopifnot(from >= 1874)  # wczesniejsze lata w linku ponizej powoduja wyswietlenie sie roznych filmow z obecnych lat.
    IMBD = "http://www.imdb.com/"

    # petla po latach, z ktorych maja być sciagniete filmy
    for (year in from:to) {
        # year <- 1909 unikalny adres mail, dzieki ktoremu w latwy sposob bedziemy przechodzic miedzy interesujacymi nas latami
        imbd_year <- paste0("http://www.imdb.com/search/title?year=", year, "%2C", year, "&title_type=feature&sort=moviemeter%2Casc")
        # liczba filmow z danego roku
        count_of_movies <- imbd_year %>% rvest::html %>% rvest::html_nodes("#left") %>%
              rvest::html_text %>% stringi::stri_extract_first_regex("[0-9,]*(?=\\ntitles)") %>% 1[] %>%
            stringi::stri_replace_all_regex(",", "") %>% as.numeric

        if (!is.na(count_of_movies)) {
            # movie_urls <- character() zmienna pomocnicza, w ktorej przechowujemy kolejne strony z listami filmow
            tmp <- tryCatch(seq(1, count_of_movies, by = 50), finally = function(x) NULL)
            # petla po kolejnych stronach z filmami z danego roku
            for (i in tmp) {
                # i <- 1 unikalny link - zmiana i = zmiana strony
                imbd_page <- paste0("http://www.imdb.com/search/title?sort=moviemeter,asc&start=", i, "&title_type=feature&year=", year, ",", year)
                pages <- html(imbd_page)
                # unikalna nazwa kazdego filmu
                info_movie <- getNodeSet(pages, "//span[@class='wlb_wrapper']")
                movie_title <- xml_attr(info_movie, "data-tconst")
                # unikalna www filmu
                movie_urls <- paste0(IMBD, "title/", movie_title)

                if (!any(is.na(movie_title))) {
                  if (!file.exists(file.path("movies_link"))) {
                    # jesli nie, tworzymy takowy
                    dir.create(file.path("movies_link"))
                  }
                  fname <- paste0("movies_link/", year, ".csv")
                  suppressWarnings(write.table(data.frame(year = year, link = movie_urls), fname, append = TRUE, row.names = FALSE, col.names = !file.exists(fname)))
                }
            }

        }
    }
    return(invisible(NA))
}
