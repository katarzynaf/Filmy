library(RCurl)
library(XML)
library(stringi)
library(rvest)
library(dplyr)

# pobieranie www kazdego filmu
download__movie_url_year <- function(from, to=from){
  stopifnot(from <= to)
  stopifnot( from>=1874)  # wczesniejsze lata w linku ponizej powoduja wyswietlenie sie roznych filmow z obecnych lat.
  IMBD = "http://www.imdb.com/"

  # petla po latach, z ktorych maja być sciagniete filmy
  for(year in from:to){
    # year <- 1909
    # unikalny adres mail, dzieki ktoremu w latwy sposob bedziemy przechodzic
    # miedzy interesujacymi nas latami
    imbd_year <- paste0("http://www.imdb.com/search/title?year=",
                        year, "%2C", year, "&title_type=feature&sort=moviemeter%2Casc")
    # liczba filmow z danego roku
    count_of_movies <- imbd_year %>%
      html %>% html_nodes("#left") %>% html_text %>%
      stri_extract_first_regex("[0-9,]*(?=\\ntitles)") %>% "["(1) %>%
      stri_replace_all_regex(",", "") %>% as.numeric

    if(!is.na(count_of_movies)){
      #movie_urls <- character()
      # zmienna pomocnicza, w ktorej przechowujemy kolejne strony z listami filmow
      tmp <- tryCatch(seq(1, count_of_movies, by=50),
                      finally = function(x) NULL)
      # petla po kolejnych stronach z filmami z danego roku
      for(i in tmp){
        #i <- 1
        # unikalny link - zmiana i = zmiana strony
        imbd_page <- paste0("http://www.imdb.com/search/title?sort=moviemeter,asc&start=",
                            i, "&title_type=feature&year=", year, ",", year)
        pages <- html(imbd_page)
        # unikalna nazwa kazdego filmu
        info_movie <- getNodeSet(pages, "//span[@class='wlb_wrapper']")
        movie_title <- xml_attr(info_movie, "data-tconst")
        # unikalna www filmu
        movie_urls <- paste0(IMBD, "title/", movie_title)

        if(!any(is.na(movie_title))){
          if(!file.exists(file.path("movies_link"))){
            # jesli nie, tworzymy takowy
            dir.create(file.path("movies_link"))
          }
          fname <- paste0("movies_link/", year, ".csv")
          suppressWarnings(write.table(
                data.frame(year = year, link = movie_urls),
                fname,
                append = TRUE,
                row.names=FALSE,
                col.names=!file.exists(fname)
                      ))
        }
      }

    }
  }
  return(invisible(NA))
}
download__movie_url_year(1874, 2020)
