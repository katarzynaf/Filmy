library(rvest)
library(dplyr)
library(stringi)

# info z nodesow
from_main_page <- function( link, sep="," ){
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
      info_z_glownej$genres <- paste0(info_z_glownej$genres, collapse = sep)
      return(as.data.frame(info_z_glownej))
}
