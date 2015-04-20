################################################################################
##       Funkcja zwraca liste nazwana zawierajaca pelne informacje nt:        ##
##                                                                            ##
##    1. rezyseria:                                   $DirectedBy             ##
##    2. wystepuja:                                   $Cast                   ##
##    3. scenariusz:                                  $WritingBy              ##
##    4. produkcja:                                   $ProducedBy             ##
##    5. zdjecia:                                     $CinematographyBy       ##
##    6. muzyka:                                      $MusicBy                ##
##                                                                            ##
## i wiele wiele innych. W zaleznosci od filmu mamy rozne informacje,         ##
## niektorych czasem nie mamy (np. autora muzyki do filmu z 1894),            ##
## tym niemniej mamy tyle, ile tylko jest. Moze wszystkiego nie potrzebujemy, ##
## na razie lece ze wszystkimi tabelami, zeby bylo z czego usuwac.            ##
################################################################################

library(XML)
library(rvest)
library(dplyr)
library(RCurl)
library(stringi)

link <- "http://www.imdb.com/title/tt0023694"
napisy_koncowe(link)
napisy_koncowe <- function(link){
      
      # wydlubanie tytulu filmu:
      title <- link %>% html %>% html_nodes(".header .itemprop") %>% html_text
      # przejscie do strony z tabela informacji:
      link <- paste0(link, ifelse(stri_sub(link,-1)=="/", "", "/"), "fullcredits?ref_=tt_ov_st_sm")
      
      tables <- link %>% readHTMLTable          # wczytanie wszystkich tabel z podstrony Cast&Crew    
      n <- length(tables)
      tables <- tables[-n]                      # usuwanie ostatniej tabelki z informacjami amazona. Od teraz mamy n-1 tabelek.
      headers <- link %>%                       # wczytanie naglowkow tabelek
                  html %>% html_nodes("h4") %>% html_text %>% "["(1:(n-1))
      
      # Zamieniam najistotniejsze nazwy, aby byly uniwersalne dla kazdej
      # pobranej tabelki (czasami dopisuja jakies pierdoly w nawiasach)
      headers[ stri_detect_regex(headers, "Directed") ] <- "Directedby"
      headers[ stri_detect_regex(headers, "Cast[^\\p{L}]") ] <- "Cast"      # uwaga, zeby Casting By nie zamienilo na Cast!
      headers[ stri_detect_regex(headers, "Writing") ] <- "Writing"
      headers[ stri_detect_regex(headers, "Produced") ] <- "Producedby"
      headers[ stri_detect_regex(headers, "Music [B|b]y") ] <- "Musicby"
      headers[ stri_detect_regex(headers, "Cinematography") ] <- "Cinematographyby"
      # A w reszcie usuwam spacje:
      headers <- stri_replace_all_regex(headers, " ", "")
      # Nadawanie nazw
      tables <- append(title,tables)
      names(tables) <- c("Title",headers[1:(n-1)])
      return(tables)  
}