require(XML)
require(rvest)
require(dplyr)
require(RCurl)
require(stringi)

args<-commandArgs(TRUE)
from <- as.numeric(args[1])
to <- as.numeric(args[2])
min <- as.numeric(args[3])
max <- as.numeric(args[4])

#setwd("D://R//Filmy")

scal <- function(co) {
      paste0(co, collapse = ",")
}

from_full_cast_n_crew <- function( link ){
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
            scal(zawartosc_tabelki[nchar(zawartosc_tabelki)>1])
            # nchar>1 dlatego, ze czasem \n bierze jako char dlugosci=1.
      })
      names(info_z_cast_crew) <- tytuly_kolumn
      return(as.data.frame(info_z_cast_crew))
}

# info z nodesow
from_main_page <- function( link ){
      # 1. wydlubanie informacji ze strony glownej filmu nodesami:
      all_nodes <- c(title=".header .itemprop",
                     year=".header a",                      #zwraca character => mozna zmienic na numeric
                     duration="#overview-top time",         #zwraca character => mozna zmienic na numeric
                     genres=".infobar .itemprop",
                     rating="div.titlePageSprite",
                     votes=".star-box-details a:nth-child(3) span")
      page <- html(link)
      wydlub <- function(node_name){
            item <- page %>% html_nodes( all_nodes[node_name] ) %>% html_text %>% stri_trim
            if( length(item)>0 ) return(item)
            else return(NA)
      }
      info_z_glownej <- lapply(names(all_nodes),wydlub)
      names(info_z_glownej) <- names(all_nodes)
      
      # zmiana formatowania czasu trwania filmu.
      if( length(info_z_glownej$duration)>0 )
            info_z_glownej$duration <- unlist(stri_extract_all_regex(info_z_glownej$duration,"[0-9]+"))  #zwraca character/NA
      
      # zmiana gatunkow na wiele kolumn
      info_z_glownej$genres <- scal(info_z_glownej$genres)
      return(as.data.frame(info_z_glownej))
}

# info z readLines
from_page_source <- function(link) {
      page <- readLines(link)
      page <- paste(page, collapse = "")
      znaczniki <- c(production_countries = "(?<=Country:).+?(?=</div)", language = "(?<=Language:).+?(?=</div)", color = "(?<=Color:).+?(?=</a)")
      details <- function(znacznik) {
            item <- unlist(stri_extract_all_regex(page, znacznik))
            if (!is.na(item)) {
                  item <- unlist(stri_extract_all_regex(item, "(?<=itemprop='url'>)([a-zA-Z]| )+"))
                  scal(item)
            } else item <- NA
      }
      a <- sapply(znaczniki, details)
      names(a) <- names(znaczniki)
      return(a)
}

# keywords
keywords <- function(link) {
      # przejscie do unikalnej strony z keywords
      key_link <- paste0(link, "/keywords?ref_=tttg_ql_4")
      pages <- html(key_link)
      # keywords
      key_movie <- getNodeSet(pages, "//div[@class='sodatext']") %>% xml_text %>% stri_trim_both
      if (length(key_movie) == 0)
            return(NA)
      # zwracamy wektor
      vec <- scal(key_movie)
      names(vec) <- "keywords"
      return(vec)
}

save_lines <- function(plik_linkow, liczba_linkow, names_of_table, ile = 100){
      i <- liczba_linkow
      con <- file(names_of_table)
      open(con,"a+")
      while( i <= (liczba_linkow + ile - 1)){
            link <- as.character(plik_linkow[i])
            a <- from_main_page(link)
            b <- from_full_cast_n_crew(link)
            c <- from_page_source(link)
            d <- keywords(link)
            
            suppressWarnings(
                  write.table(
                        as.data.frame(c(a,b,c,d)),
                        file=names_of_table, append=TRUE, sep=";", row.names=FALSE,
                        col.names=FALSE #!file.exists(names_of_table[j])
                  )
            )
            i<-i+1
      }
      close(con)
      return(invisible(NULL))
}

#from <- to <- 1894
pobieraczek <- function(from, to=from, min = 1, max = 1000){
      
      if(!file.exists(file.path("dane"))){
            # jesli nie, tworzymy takowy
            dir.create(file.path("dane"))
      }
      if(!file.exists(file.path("dane/glowne"))){
            # jesli nie, tworzymy takowy
            dir.create(file.path("dane/glowne"))
      }
      #lista plikow w folderze
      which_to_download <- unlist(sapply(as.character(from:to),
                                         function(x) list.files(paste0(getwd(),"/movies_link"),
                                                                x, full.names=TRUE)),
                                  use.names = FALSE)
      if(length(which_to_download)==0){
            return(invisible(NULL))
      }
      #interesujace nas pliki
      #files<-files[which_to_download]
      names_of_files<-stri_sub(which_to_download,-8,-5)
      names_of_table<-paste0("dane/glowne/",names_of_files,".csv")
      
      m <- length(names_of_table)
      j <- 1
      while(j <= m){
            
            #con <- file(names_of_table[j])
            plik_linkow <- read.table(which_to_download[j],header=TRUE)$link
            liczba_linkow <- length(plik_linkow)
            i=1
            if(!file.exists(names_of_table[j]))
                  write.table(t(c("title","year","duration","genres","rating","votes","DirectedBy","Cast","Writing","ProducedBy",
                                  "MusicBy","CinematographyBy","production_countries","language","color","keywords")),
                              file=names_of_table[j], row.names=FALSE, col.names=FALSE, sep=";")
            # co ile konczymy zapisywanie - alokacja pamieci
            # seq(1, liczba_linkow, by = 100)
            if(liczba_linkow < max) max <- liczba_linkow
            tmp <- seq(min, max, by = 100)
            tmp_l <- length(tmp)
            k <- 1
            while(k <= tmp_l){
                  if(k == tmp_l) save_lines(plik_linkow, tmp[k], names_of_table[j], ile = liczba_linkow)
                  else save_lines(plik_linkow, tmp[k], names_of_table[j])
                  k <- k+1
            }
            j <- j+1
      }
      cat("\nDone\n")
}


pobieraczek(from, to, min, max)
