link <- "http://www.imdb.com/title/tt0022628"
    
# info z nodesow
from_main_page <- function( link ){
      # 1. wydlubanie informacji ze strony glownej filmu nodesami:
      all_nodes <- c(title=".header .itemprop",
                     year=".header a",                      #zwraca character => mozna zmienic na numeric
                     duration="#overview-top time",         #zwraca character => mozna zmienic na numeric
                     genres=".infobar .itemprop",
                     rating="div.titlePageSprite",
                     votes=".star-box-details a:nth-child(3) span")
      wydlub <- function(node_name){
            item <- link %>% html %>% html_nodes( all_nodes[node_name] ) %>% html_text %>% stri_trim
            return(item)
      }
      info_z_glownej <- lapply(names(all_nodes),wydlub)
      names(info_z_glownej) <- names(all_nodes)
      # zmiana formatowania czasu trwania filmu.
      if( length(info_z_glownej$duration)>0 ){
            info_z_glownej$duration <- unlist(stri_extract_all_regex(duration,"[0-9]+"))  #zwraca character/NA 
      }else{
            info_z_glownej$duration <- NA
      }
      return(info_z_glownej)
}
from_main_page(link)

# info z readHTMLTable
from_full_cast_n_crew <- function( link ){
      tytuly_kolumn <- c("DirectedBy","Cast","Writing","ProducedBy","MusicBy","CinematographyBy")
      
      link <- paste0(link, ifelse(stri_sub(link,-1)=="/", "", "/"), "fullcredits") 
      tables <- link %>% readHTMLTable          # wczytanie wszystkich tabel z podstrony Cast&Crew    
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
      names(tables) <- c(headers[1:(n-1)])
      tables$Cast <- tables$Cast[,-1]      # pierwsza kolumna Cast jest pusta, bo jest na zdjecia.
      # Wydobycie *pierwszych* kolumn tabel: interesuja nas tylko nazwiska aktorow, a nie np. ze Eddie Murphy byl glosem Osla.
      info_z_cast_crew <- lapply(tytuly_kolumn, function(h){
            zawartosc_tabelki <- as.character(tables[[h]][,1])
            paste0(zawartosc_tabelki[nchar(zawartosc_tabelki)>1], collapse = ";")
            # nchar>1 dlatego, ze czasem \n bierze jako char dlugosci=1.
      })
      names(info_z_cast_crew) <- tytuly_kolumn
      return(info_z_cast_crew)
}
from_full_cast_n_crew( link )


# info z readLines
from_page_source <- function(link){
      page <- readLines(link)
      page <- paste(page, collapse="")
      znaczniki <- c(production_countries="(?<=Country:).+?(?=</div)",
                     language="(?<=Language:).+?(?=</div)",
                     color="(?<=Color:).+?(?=</a)")
      details <- function( znacznik ){
            item <- unlist(stri_extract_all_regex(page,znacznik))
            if(!is.na(item)){
                  item <- unlist(stri_extract_all_regex(item,"(?<=itemprop=\'url\'>)([a-zA-Z]| )+"))
                  paste0(item,collapse = ";")
            }else
                  item <- NA
      }
      a <- sapply(znaczniki,details)
      names(a) <- names(znaczniki)
      return(a)
}
from_page_source( link )
      
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
      return(paste0(key_movie,collapse = ";"))
}

keywords( link )

