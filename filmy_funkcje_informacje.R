library("rvest")
library("XML")
library("stringi")

# prosba: linki na koncu bez '/' - tak zapisywane w csv
#test
link<-"http://www.imdb.com/title/tt0463985"
#test 2
link<-"http://www.imdb.com/title/tt3155794"

merge_csv<-function(){ ##funkcja sklejaca wszystkie csv do jednej ramki danych
  files<-list.files(paste0(getwd(),"/movies_link"),full.names=TRUE)
  data<-do.call(rbind,lapply(files,read.table,header=TRUE))
  data
}

##
merge_csv<-function(){ ##funkcja sklejaca wszystkie csv do jednej ramki danych
   files<-list.files(paste0(getwd(),"/movies_link"),full.names=TRUE)
   data<-do.call(rbind,lapply(files,read.table,header=TRUE))
   data
}

#zapis do jednego pliku
#write.table(merge_csv(),"movies_links.csv")

##
title<-function(link){
   page<-html(link)
   title<-html_nodes(page,".header .itemprop")
   title<-html_text(title)
}

(tytul<-title("http://www.imdb.com/title/tt0099674/"))

##
year<-function(link){
   #mozna tez zrobic by na poczatku szukania po latach przypisywal rok
   page<-html(link)
   year<-html_nodes(page,".header a")
   year<-html_text(year) #zwraca character => mozna zmienic na numeric
}

(rok<-year("http://www.imdb.com/title/tt0099674/"))

##
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

(rezyser<-director("http://www.imdb.com/title/tt0099674/"))

#test dla Idy
#link<-"http://www.imdb.com/title/tt2718492/"

##
budget<-function(link){ #nie kazdy film ma budget - Ida np. nie ma http://www.imdb.com/title/tt2718492/
   page<-html(link)
   budget<-html_nodes(page,"#titleDetails :nth-child(11)")
   budget<-html_text(budget)
   budget<-paste(unlist(stri_extract_all_regex(budget,"[0-9]+")),collapse="") #zwraca character => mozna zmienic na numeric
}

budget2<-function(link){ #nie kazdy film ma budget - Ida np. nie ma http://www.imdb.com/title/tt2718492/ - ta funkcja omija ten problem
   page<-readLines(link)
   page<-paste(page, collapse="")
   budget<-unlist(stri_extract_all_regex(page,"(?<=Budget:).+?(?=<span)"))
   if(!is.na(budget)){
      budget<-unlist(stri_extract_all_regex(budget,"(?<=>).+"))
      budget<-paste0(unlist(stri_extract_all_regex(budget,"[0-9]+")),collapse="") #zwraca character/NA => mozna zmienic na numeric lub cos innego
   }
   else{
      budget<-NA
   }
}

(budzet<-budget("http://www.imdb.com/title/tt0099674/"))
(budzet2<-budget2("http://www.imdb.com/title/tt0099674/"))

(budzet<-budget("http://www.imdb.com/title/tt2718492/")) #to nie dziala prawidlowo - zczytuje zarobek w dniu premiery
(budzet2<-budget2("http://www.imdb.com/title/tt2718492/"))

##
color<-function(link){ #nie dziala np dla testu 1
   page<-html(link)
   color<-html_nodes(page,"#titleDetails :nth-child(22) a")
   color<-stri_trim(html_text(color))
}

color2<-function(link){ #nie dziala np dla testu 1
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

(kolor<-color("http://www.imdb.com/title/tt0099674/"))
(kolor<-color("http://www.imdb.com/title/tt2718492/"))
(kolor<-color("http://www.imdb.com/title/tt0463985/")) #nie dziala prawidlowo

(kolor<-color2("http://www.imdb.com/title/tt0099674/"))
(kolor<-color2("http://www.imdb.com/title/tt2718492/"))
(kolor<-color2("http://www.imdb.com/title/tt0463985/")) #to juz dziala prawidlowo

##
genres<-function(link){
   page<-html(link)
   genres<-html_nodes(page,".infobar .itemprop")
   genres<-html_text(genres)
}

(gatunek<-genres("http://www.imdb.com/title/tt0099674/"))
(gatunek<-genres("http://www.imdb.com/title/tt2718492/"))

## nastepne 4 funkcje mozna jakos sprobowac skleic
rating<-function(link){
   page<-html(link)
   rating<-html_nodes(page,"div.titlePageSprite")
   rating<-stri_trim(html_text(rating)) #zwraca character => mozna na numeric zmienic
}

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

(ocena<-rating("http://www.imdb.com/title/tt0099674/"))
(ocena<-rating("http://www.imdb.com/title/tt2718492/"))

(ocena<-rating2("http://www.imdb.com/title/tt0099674/"))
(ocena<-rating2("http://www.imdb.com/title/tt2718492/"))

##
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

(glosy<-votes("http://www.imdb.com/title/tt0099674/"))
(glosy<-votes("http://www.imdb.com/title/tt2718492/"))

##
rating_stats<-function(link){
   if(stri_sub(link,-1)=="/"){
      link<-paste0(link,"ratings")
   }
   else{
      link<-paste0(link,"/ratings")
   }
   rating_stats<-readHTMLTable(link)[[1]]
   names(rating_stats)<-stri_extract_all_regex(names(rating_stats),"[a-zA-Z]+",simplify=TRUE)
   rating_stats[,2]<-stri_extract_all_regex(rating_stats[,2],"[0-9]{1,2}.[0-9]{1,2}%",simplify=TRUE)
   rating_stats
}

(statystyki<-rating_stats("http://www.imdb.com/title/tt0099674/"))
(statystyki<-rating_stats("http://www.imdb.com/title/tt2718492/"))

##
user_rating_stats<-function(link){
   if(stri_sub(link,-1)=="/"){
      link<-paste0(link,"ratings")
   }
   else{
      link<-paste0(link,"/ratings")
   }
   user_rating_stats<-readHTMLTable(link)[[2]]
   names(user_rating_stats)[1]<-"Who"
   user_rating_stats[,2]<-stri_extract_all_regex(user_rating_stats[,2],"[0-9]+",simplify=TRUE)
   user_rating_stats[,3]<-stri_extract_all_regex(user_rating_stats[,3],"[0-9]+",simplify=TRUE)
   user_rating_stats[,3]<-stri_paste(user_rating_stats[,3][,1],user_rating_stats[,3][,2],sep=".")
   user_rating_stats<-user_rating_stats[-which(is.na(user_rating_stats[,2])),]
   rownames(user_rating_stats)<-seq_len(nrow(user_rating_stats))
   user_rating_stats
}

(statystyki<-user_rating_stats("http://www.imdb.com/title/tt0099674"))
(statystyki<-user_rating_stats("http://www.imdb.com/title/tt2718492"))

##
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

(kraje_produkcji<-production_countries("http://www.imdb.com/title/tt0099674/"))
(kraje_produkcji<-production_countries("http://www.imdb.com/title/tt2718492/"))

### obsada 

cast <- function(link){
  
  # przejscie do unikalnej strony z obsada
  link_cast <- paste0(link, "/fullcredits?ref_=tt_cl_sm#cast")
  pages <- html(link_cast)
  # aktorzy
  cast_movie <- getNodeSet(pages, "//span[@class='itemprop']")
  
  # jesli brak obsady, zwracamy ramke dane z wartosciami NA
  # (poki co takie rozwiazanie przy zapisie do bazy zawsze mozna zmienic) 
  if(length(cast_movie)==0) return(data.frame(actor = NA, character = NA))
  
  cast_movie <- cast_movie %>% xml_text
  
  # odgrywane postacie
  character <- getNodeSet(pages, "//td[@class='character']//div") %>% 
    xml_text %>% stri_trim_both %>% 
    stri_extract_first_regex("[a-zA-Z. ]+")
  # ramka wynikowa
  cast <- data.frame(actor = cast_movie, character = character, 
                     stringsAsFactors = FALSE)
  return(cast)
}

cast("http://www.imdb.com/title/tt2718492")
