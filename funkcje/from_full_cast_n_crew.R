library(XML)
library(rvest)
library(dplyr)
library(stringi)


# info z readHTMLTable
from_full_cast_n_crew <- function( link,sep="," ){
      tytuly_kolumn <- c("DirectedBy","Cast","Writing","ProducedBy","MusicBy","CinematographyBy")

      link <- paste0(link, ifelse(stri_sub(link,-1)=="/", "", "/"), "fullcredits")
      tables <- link %>% readHTMLTable          # wczytanie wszystkich tabel z podstrony Cast&Crew
      n <- length(tables)
      headers <- link %>%                       # wczytanie naglowkow tabelek
            html %>% html_nodes("h4") %>% html_text %>% "["(1:n)
      # Zamieniam najistotniejsze nazwy, aby byly uniwersalne dla kazdej
      # pobranej tabelki (czasami dopisuja jakies pierdoly w nawiasach)
      headers[ stri_detect_regex(headers, "Directed") ] <- tytuly_kolumn[1]
      headers[ stri_detect_regex(headers, "Cast[^\\p{L}]") ] <- tytuly_kolumn[2]   # uwaga, zeby Casting By nie zamienilo na Cast!
      headers[ stri_detect_regex(headers, "Writing") ] <- tytuly_kolumn[3]
      headers[ stri_detect_regex(headers, "Produced") ] <- tytuly_kolumn[4]
      headers[ stri_detect_regex(headers, "Music [B|b]y") ] <- tytuly_kolumn[5]
      headers[ stri_detect_regex(headers, "Cinematography") ] <- tytuly_kolumn[6]
      # Nadanie nazw tabelom:
      names(tables) <- headers
      tables$Cast <- tables$Cast[,-1]      # pierwsza kolumna Cast jest pusta, bo jest na zdjecia.
      # Wydobycie *pierwszych* kolumn tabel: interesuja nas tylko nazwiska aktorow, a nie np. ze Eddie Murphy byl glosem Osla.
      info_z_cast_crew <- lapply(tytuly_kolumn, function(h){
            zawartosc_tabelki <- as.character(tables[[h]][,1])
            paste0(zawartosc_tabelki[nchar(zawartosc_tabelki)>1], collapse=sep)
            # nchar>1 dlatego, ze czasem \n bierze jako char dlugosci=1.
      })
      names(info_z_cast_crew) <- tytuly_kolumn
      return(as.data.frame(info_z_cast_crew))
}
