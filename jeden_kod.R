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

#link <- "http://www.imdb.com/title/tt0023694"

napisy_koncowe <- function(link){
      
      main_page <- html(link)
      
      # 1. wydlubanie informacji ze strony glownej filmu:
      all_nodes <- c(title=".header .itemprop",
                     year=".header a",                      #zwraca character => mozna zmienic na numeric
                     duration="#overview-top time",         #zwraca character => mozna zmienic na numeric
                     genres=".infobar .itemprop",
                     rating="div.titlePageSprite")
      
      wydlubanie <- function(node_name){
            item <- main_page %>% html_nodes(all_nodes[node_name]) %>% html_text %>% stri_trim
            return(item)
      }
      
      info_z_glownej <- lapply(names(all_nodes),wydlubanie)
      names(info_z_glownej) <- names(all_nodes)
      
#### Kasiouwaga:   
#       if( length(duration)>0 ){
#             duration <- unlist(stri_extract_all_regex(duration,"[0-9]+"))}else{
#             duration <- NA
#             }                 #zwraca character/NA 
#  Moze teraz tego nie robmy, tylko jak bedziemy opracowywac dane?

      # 2. przejscie do strony z tabela informacji:
      link <- paste0(link, ifelse(stri_sub(link,-1)=="/", "", "/"), "fullcredits?ref_=tt_ov_st_sm")
      
      # 3. Pobranie tabelki
      tables <- link %>% readHTMLTable          # wczytanie wszystkich tabel z podstrony Cast&Crew    
      n <- length(tables)
      tables <- tables[-n]                      # usuwanie ostatniej tabelki z informacjami amazona. Od teraz mamy n-1 tabelek.
      headers <- link %>%                       # wczytanie naglowkow tabelek
            html %>% html_nodes("h4") %>% html_text %>% "["(1:(n-1))
      
      # 4. Zamieniam najistotniejsze nazwy, aby byly uniwersalne dla kazdej pobranej tabelki
      # (czasami dopisuja jakies pierdoly w nawiasach)
      headers[ stri_detect_regex(headers, "Directed") ] <- "Directedby"
      headers[ stri_detect_regex(headers, "Cast[^\\p{L}]") ] <- "Cast"      # uwaga, zeby Casting By nie zamienilo na Cast!
      headers[ stri_detect_regex(headers, "Writing") ] <- "Writing"
      headers[ stri_detect_regex(headers, "Produced") ] <- "Producedby"
      headers[ stri_detect_regex(headers, "Music [B|b]y") ] <- "Musicby"
      headers[ stri_detect_regex(headers, "Cinematography") ] <- "Cinematographyby"
      # A w reszcie usuwam spacje:
      headers <- stri_trim(stri_replace_all_regex(headers, " ", ""))
      # Nadawanie nazw tabelom:
      names(tables) <- c(headers[1:(n-1)])
      # 5.Laczenie inforamcji ze strony glownej z tabelka:
      tables <- append(info_z_glownej, tables)
      
      return(tables)  
}