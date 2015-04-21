library("rvest")
library("XML")
library("stringi")

# prosba: linki na koncu bez '/' - tak zapisywane w csv
#test
link<-"http://www.imdb.com/title/tt0463985"
#test 2
link<-"http://www.imdb.com/title/tt3155794"

##
merge_csv<-function(folder){ ##funkcja sklejaca wszystkie csv do jednej ramki danych,
   #folder musi sie zaczynac od "/" np "/movies_link"
      # EDIT: juz nie musi :P:P [kf]
   files<-list.files(paste0(getwd(),"/", folder),full.names=TRUE)
   data<-do.call(rbind,lapply(files,read.table,header=TRUE))
   data
}

#zapis do jednego pliku
#write.table(merge_csv(),"movies_links.csv")

############# FUNKCJE UZYTE W from_main_page() ############# 
## 
# title<-function(link){
#    page<-html(link)
#    title<-html_nodes(page,".header .itemprop")
#    title<-html_text(title)
# }
#
# (tytul<-title("http://www.imdb.com/title/tt0099674"))

## patrz -> from_main_page()
# year<-function(link){
#    #mozna tez zrobic by na poczatku szukania po latach przypisywal rok
#    page<-html(link)
#    year<-html_nodes(page,".header a")
#    year<-html_text(year) #zwraca character => mozna zmienic na numeric
# }
#
# (rok<-year("http://www.imdb.com/title/tt0099674"))

##
# length_of_movie<-function(link){
#    page<-html(link)
#    length_of_movie<-html_nodes(page,"#overview-top time")
#    length_of_movie<-html_text(length_of_movie)
#    if(length(length_of_movie)>0){
#       length_of_movie<-unlist(stri_extract_all_regex(length_of_movie,"[0-9]+")) #zwraca character/NA => mozna zmienic na numeric
#    }
#    else{
#       length_of_movie<-NA
#    }
# }
#
# (dlugosc_filmu<-length_of_movie("http://www.imdb.com/title/tt0099674"))
#
# director<-function(link){
#    page<-html(link)
#    director<-html_nodes(page,"#overview-top :nth-child(8) .itemprop")
#    director<-html_text(director)
# }
#
# (rezyser<-director("http://www.imdb.com/title/tt0099674"))
# 
##
# genres<-function(link){
#    page<-html(link)
#    genres<-html_nodes(page,".infobar .itemprop")
#    genres<-html_text(genres)
# }
#
# (gatunek<-genres("http://www.imdb.com/title/tt0099674"))
## 
# rating<-function(link){
#    page<-html(link)
#    rating<-html_nodes(page,"div.titlePageSprite")
#    rating<-stri_trim(html_text(rating)) #zwraca character => mozna na numeric zmienic
# }
## Votes idzie latwo nodesem zrobic z poziomu strony glownej :)
# uzywajac html_nodes(page, ".star-box-details a:nth-child(3) span")
# votes<-function(link){
#    if(stri_sub(link,-1)=="/"){
#       link<-paste0(link,"ratings")
#    }
#    else{
#       link<-paste0(link,"/ratings")
#    }
#    page<-html(link)
#    votes<-votes<-html_nodes(page,"p:nth-child(4)")
#    votes<-stri_trim(html_text(votes))
#    votes<-unlist(stri_extract_all_regex(votes,"^[0-9]+")) #zwraca character => mozna na numeric zmienic
# }

############# FUNKCJE UZYTE W from_page_source() ############# 

# language<-function(link){
#    page<-readLines(link)
#    page<-paste(page,collapse="")
#    language<-unlist(stri_extract_all_regex(page,"(?<=Language:).+?(?=</div)"))
#    if(!is.na(language)){
#       language<-unlist(stri_extract_all_regex(language,"(?<=itemprop=\'url\'>).+?(?=</a)"))
#       #zwraca character/NA => mozna zmienic na numeric lub cos innego
#    }
#    else{
#       language<-NA
#    }
# }
#test dla Idy
#link<-"http://www.imdb.com/title/tt2718492"

# color2<-function(link){
#    page<-readLines(link)
#    page<-paste(page, collapse="")
#    color<-unlist(stri_extract_all_regex(page,"(?<=Color:).+?(?=</a)"))
#    if(!is.na(color)){
#       color<-unlist(stri_extract_all_regex(color,"([a-zA-Z]| )+$"))
#       color<-stri_trim(color) #zwraca character/NA => mozna zmienic na numeric lub cos innego
#    }
#    else{
#       color<-NA
#    }
# }

##
# production_countries<-function(link){
#    page<-readLines(link)
#    page<-paste(page, collapse="")
#    production_countries<-unlist(stri_extract_all_regex(page,"(?<=Country:).+?(?=</div)"))
#    if(!is.na(production_countries)){
#       production_countries<-unlist(stri_extract_all_regex(production_countries,"(?<=itemprop=\'url\'>).+?(?=</a)"))
#       #zwraca character/NA => mozna zmienic na numeric lub cos innego
#    }
#    else{
#       production_countries<-NA
#    }
# }
# 
# (kraje_produkcji<-production_countries("http://www.imdb.com/title/tt0099674"))
# (kraje_produkcji<-production_countries("http://www.imdb.com/title/tt2718492"))

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

############# FUNKCJE STATYSTYCZNE #############
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

(ocena<-rating("http://www.imdb.com/title/tt0099674"))
(ocena<-rating("http://www.imdb.com/title/tt2718492"))

(ocena<-rating2("http://www.imdb.com/title/tt0099674"))
(ocena<-rating2("http://www.imdb.com/title/tt2718492"))



(glosy<-votes("http://www.imdb.com/title/tt0099674"))
(glosy<-votes("http://www.imdb.com/title/tt2718492"))

##
rating_stats<-function(link){
   if(stri_sub(link,-1)=="/"){
      link<-paste0(link,"ratings")
   }
   else{
      link<-paste0(link,"/ratings")
   }
   rating_stats<-readHTMLTable(link)[[1]]
   if(ncol(rating_stats)==1){ #gdy nie ma statystyk z glosami
      rating_stats<-NA
      return(rating_stats)
   }
   names(rating_stats)<-stri_extract_all_regex(names(rating_stats),"[a-zA-Z]+",simplify=TRUE)
   rating_stats[,2]<-stri_extract_all_regex(rating_stats[,2],"[0-9]{1,2}.[0-9]{1,2}%",simplify=TRUE)
   return(rating_stats)
}

(statystyki_r<-rating_stats("http://www.imdb.com/title/tt0099674"))
(statystyki_r<-rating_stats("http://www.imdb.com/title/tt2718492"))
(statystyki_r<-rating_stats("http://www.imdb.com/title/tt0000009"))

##
user_rating_stats<-function(link){
   if(stri_sub(link,-1)=="/"){
      link<-paste0(link,"ratings")
   }
   else{
      link<-paste0(link,"/ratings")
   }
   user_rating_stats<-readHTMLTable(link)[[2]]
   if(names(user_rating_stats)[3]!="Average"){
      user_rating_stats<-NA
      return(user_rating_stats)
   }
   names(user_rating_stats)[1]<-"Who"
   user_rating_stats[,2]<-stri_extract_all_regex(user_rating_stats[,2],"[0-9]+",simplify=TRUE)
   user_rating_stats[,3]<-stri_extract_all_regex(user_rating_stats[,3],"[0-9]+",simplify=TRUE)
   user_rating_stats[,3]<-stri_paste(user_rating_stats[,3][,1],user_rating_stats[,3][,2],sep=".")
   user_rating_stats<-user_rating_stats[-which(is.na(user_rating_stats[,2])),]
   rownames(user_rating_stats)<-seq_len(nrow(user_rating_stats))
   return(user_rating_stats)
}

(statystyki_u<-user_rating_stats("http://www.imdb.com/title/tt0099674"))
(statystyki_u<-user_rating_stats("http://www.imdb.com/title/tt2718492"))
(statystyki_u<-user_rating_stats("http://www.imdb.com/title/tt0000009"))


#user_stats<-statystyki_u
#votes_stats<-statystyki_r

stats_to_one_row<-function(link,user_stats=user_rating_stats(link),votes_stats=rating_stats(link)){
   #kolumny jakie ma tabelka / nazwy elementow rzedu
   table_columns<-c("Title","Overall_Rating","Votes","Males","Females","Aged_under_18","Males_under_18","Females_under_18",
   "Aged_18-29","Males_Aged_18-29","Females_Aged_18-29","Aged_30-44","Males_Aged_30-44","Females_Aged_30-44",
   "Aged_45+","Males_Aged_45+","Females_Aged_45+","IMDb_staff","Top_1000_voters","US_users","Non-US_users",
   "Vote_10","Vote_9","Vote_8","Vote_7","Vote_6","Vote_5","Vote_4","Vote_3","Vote_2","Vote_1")
   #finalny wektor, brak danych na -1 ustawiam, bo 0 moze jednak cos znaczyc
   table_row<-c(title(link),rep("-1",length(table_columns)-1))
   names(table_row)<-table_columns
   if(length(user_stats)==1){ #gdy nie ma statystyk (dlugosc NA to 1, a ramki danych ze statystykami to 3)
      return(table_row)
   }
   #zapamietujemy dlugosc tabelki, bo ostatni wiersz nam sie przyda
   m<-nrow(user_stats)
   #nazwy kolumn dla tabelki z ocenami, ostatni wiersz jest 2 kolumnami
   cols_user_stats<-as.character(user_stats$Who[-m])
   cols_user_stats<-stri_replace_all_regex(cols_user_stats," ","_")
   #nazwy kolumn dla tabelki z glosami
   cols_votes_stats<-paste0("Vote_",as.character(votes_stats$Rating))
   #nazwy kolumn
   columns<-c("Overall_Rating","Votes",cols_user_stats,cols_votes_stats)
   #wartosci w wierszu
   row<-c(user_stats[m,3],user_stats[m,2],user_stats$Average[-m],as.character(votes_stats$Votes))
   #ktore mamy kolumny w ramkach, moze sie zdarzyc ze jakiegos przedzialu wiekowego nie ma
   which_columns<-unlist(lapply(columns,function(col_name){
        which(col_name==table_columns)
      }))
   table_row[which_columns]<-row
   return(table_row) #zwraca character => mozna na numeric zmienic
}




############# FUNKCJE NIEUZYTE ############# 
# budget<-function(link){ #nie kazdy film ma budget - Ida np. nie ma http://www.imdb.com/title/tt2718492
#    page<-html(link)
#    budget<-html_nodes(page,"#titleDetails :nth-child(11)")
#    budget<-html_text(budget)
#    budget<-paste(unlist(stri_extract_all_regex(budget,"[0-9]+")),collapse="") #zwraca character => mozna zmienic na numeric
# }
# 
# budget2<-function(link){ #nie kazdy film ma budget - Ida np. nie ma http://www.imdb.com/title/tt2718492 - ta funkcja omija ten problem
#    page<-readLines(link)
#    page<-paste(page, collapse="")
#    budget<-unlist(stri_extract_all_regex(page,"(?<=Budget:).+?(?=<span)"))
#    if(!is.na(budget)){
#       budget<-unlist(stri_extract_all_regex(budget,"(?<=>).+"))
#       budget<-paste0(unlist(stri_extract_all_regex(budget,"[0-9]+")),collapse="") #zwraca character/NA => mozna zmienic na numeric lub cos innego
#    }
#    else{
#       budget<-NA
#    }
# }
# 
# (budzet<-budget("http://www.imdb.com/title/tt0099674"))
# (budzet2<-budget2("http://www.imdb.com/title/tt0099674"))
# 
# (budzet<-budget("http://www.imdb.com/title/tt2718492")) #to nie dziala prawidlowo - zczytuje zarobek w dniu premiery
# (budzet2<-budget2("http://www.imdb.com/title/tt2718492"))
# 
# ##
# color<-function(link){ #nie dziala np dla testu 1
#    page<-html(link)
#    color<-html_nodes(page,"#titleDetails :nth-child(22) a")
#    color<-stri_trim(html_text(color))
# }
# 
# ### obsada
# 
# cast <- function(link){
#       
#       # przejscie do unikalnej strony z obsada
#       link_cast <- paste0(link, "/fullcredits?ref_=tt_cl_sm#cast")
#       pages <- html(link_cast)
#       # aktorzy
#       cast_movie <- getNodeSet(pages, "//span[@class='itemprop']")
#       
#       # jesli brak obsady, zwracamy ramke dane z wartosciami NA
#       # (poki co takie rozwiazanie przy zapisie do bazy zawsze mozna zmienic)
#       if(length(cast_movie)==0) return(data.frame(actor = NA, character = NA))
#       
#       cast_movie <- cast_movie %>% xml_text
#       
#       # odgrywane postacie
#       character <- getNodeSet(pages, "//td[@class='character']//div") %>%
#             xml_text %>% stri_trim_both %>%
#             stri_extract_first_regex("[a-zA-Z. ]+")
#       # ramka wynikowa
#       cast <- data.frame(actor = cast_movie, character = character,
#                          stringsAsFactors = FALSE)
#       return(cast)
# }
# 
# cast("http://www.imdb.com/title/tt2718492")

# (kolor<-color("http://www.imdb.com/title/tt0099674"))
# (kolor<-color("http://www.imdb.com/title/tt2718492"))
# (kolor<-color("http://www.imdb.com/title/tt0463985")) #nie dziala prawidlowo
# 
# (kolor<-color2("http://www.imdb.com/title/tt0099674"))
# (kolor<-color2("http://www.imdb.com/title/tt2718492"))
# (kolor<-color2("http://www.imdb.com/title/tt0463985")) #to juz dziala prawidlowo
# 
# 
