############# FUNKCJE UZYTE W from_main_page() ############# 
# 
title<-function(link){
   page<-html(link)
   title<-html_nodes(page,".header .itemprop")
   title<-html_text(title)
}

(tytul<-title("http://www.imdb.com/title/tt0099674"))

# patrz -> from_main_page()
year<-function(link){
   #mozna tez zrobic by na poczatku szukania po latach przypisywal rok
   page<-html(link)
   year<-html_nodes(page,".header a")
   year<-html_text(year) #zwraca character => mozna zmienic na numeric
}

(rok<-year("http://www.imdb.com/title/tt0099674"))

#
length_of_movie<-function(link){
   page<-html(link)
   length_of_movie<-html_nodes(page,"#overview-top time")
   length_of_movie<-html_text(length_of_movie)
   if(length(length_of_movie)>0){
      length_of_movie<-unlist(stri_extract_all_regex(length_of_movie,"[0-9]+")) #zwraca character/NA => mozna zmienic na numeric
   }
   else{
      length_of_movie<-NA
   }
}

(dlugosc_filmu<-length_of_movie("http://www.imdb.com/title/tt0099674"))

director<-function(link){
   page<-html(link)
   director<-html_nodes(page,"#overview-top :nth-child(8) .itemprop")
   director<-html_text(director)
}

(rezyser<-director("http://www.imdb.com/title/tt0099674"))

#
genres<-function(link){
   page<-html(link)
   genres<-html_nodes(page,".infobar .itemprop")
   genres<-html_text(genres)
}

(gatunek<-genres("http://www.imdb.com/title/tt0099674"))
# 
rating<-function(link){
   page<-html(link)
   rating<-html_nodes(page,"div.titlePageSprite")
   rating<-stri_trim(html_text(rating)) #zwraca character => mozna na numeric zmienic
}


# Votes idzie latwo nodesem zrobic z poziomu strony glownej :P
# uzywajac html_nodes(page, ".star-box-details a:nth-child(3) span")
votes<-function(link){
   if(stri_sub(link,-1)=="/"){
      link<-paste0(link,"ratings")
   }
   else{
      link<-paste0(link,"/ratings")
   }
   page<-html(link)
   votes<-votes<-html_nodes(page,"p:nth-child(4)")
   votes<-stri_trim(html_text(votes))
   votes<-unlist(stri_extract_all_regex(votes,"^[0-9]+")) #zwraca character => mozna na numeric zmienic
}


# TA NIEUZYTA
rating2<-function(link){ #ta raczej zawsze zadziala, nie wiem jak poprzednia :P
      if(stri_sub(link,-1)=="/"){
            link<-paste0(link,"ratings")
      }
      else{
            link<-paste0(link,"/ratings")
      }
      page<-html(link)
      rating<-html_nodes(page,"p:nth-child(4)")
      rating<-stri_trim(html_text(rating))
      rating<-unlist(stri_extract_all_regex(rating,"(?<=of ).+(?= /)")) #zwraca character => mozna na numeric zmienic
}

(ocena<-rating2("http://www.imdb.com/title/tt0099674"))
(ocena<-rating2("http://www.imdb.com/title/tt2718492"))

############# FUNKCJE UZYTE W from_page_source() ############# 

language<-function(link){
   page<-readLines(link)
   page<-paste(page,collapse="")
   language<-unlist(stri_extract_all_regex(page,"(?<=Language:).+?(?=</div)"))
   if(!is.na(language)){
      language<-unlist(stri_extract_all_regex(language,"(?<=itemprop=\'url\'>).+?(?=</a)"))
      #zwraca character/NA => mozna zmienic na numeric lub cos innego
   }
   else{
      language<-NA
   }
}
#test dla Idy
link<-"http://www.imdb.com/title/tt2718492"

color2<-function(link){
   page<-readLines(link)
   page<-paste(page, collapse="")
   color<-unlist(stri_extract_all_regex(page,"(?<=Color:).+?(?=</a)"))
   if(!is.na(color)){
      color<-unlist(stri_extract_all_regex(color,"([a-zA-Z]| )+$"))
      color<-stri_trim(color) #zwraca character/NA => mozna zmienic na numeric lub cos innego
   }
   else{
      color<-NA
   }
}

#
production_countries<-function(link){
   page<-readLines(link)
   page<-paste(page, collapse="")
   production_countries<-unlist(stri_extract_all_regex(page,"(?<=Country:).+?(?=</div)"))
   if(!is.na(production_countries)){
      production_countries<-unlist(stri_extract_all_regex(production_countries,"(?<=itemprop=\'url\'>).+?(?=</a)"))
      #zwraca character/NA => mozna zmienic na numeric lub cos innego
   }
   else{
      production_countries<-NA
   }
}

(kraje_produkcji<-production_countries("http://www.imdb.com/title/tt0099674"))
(kraje_produkcji<-production_countries("http://www.imdb.com/title/tt2718492"))

############# FUNKCJE UZYTE W keywords() ############# 

# keywords <- function(link){
#   # przejscie do unikalnej strony z keywords
#   key_link <- paste0(link, "/keywords?ref_=tttg_ql_4")
# 
#   pages <- html(key_link)
#   # keywords
#   key_movie <- getNodeSet(pages, "//div[@class='sodatext']") %>%
#     xml_text %>%
#     stri_trim_both
#   if(length(key_movie) == 0) return(NA)
#   # zwracamy wektor
#   return(key_movie)
# }
# 
# keywords("http://www.imdb.com/title/tt0008634")
##

############# FUNKCJA DO POBIERANIA NAPISOW KONCOWYCH ############# 

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
      headers <- stri_trim(stri_replace_all_regex(headers, " ", ""))
      names(tables) <- headers[1:(n-1)]
      # tabela Cast ma pusta pierwsza kolumne (fotografie aktorow) i trzecia (bezsensowne kropki)
      # pozostale maja pusta druga kolumne (bezsensowne kropki)
      tables$Cast <- tables$Cast[,-1]
      tables <- lapply(tables, function(tabelka) tabelka[,-2])
      
      tables <- append(title, tables)
      
      return(tables)  
}


