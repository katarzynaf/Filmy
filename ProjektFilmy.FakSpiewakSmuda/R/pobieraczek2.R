#' Pobieranie informacji o filmach z bazy IMDb wersja 2.
#'
#' Funkcja \code{pobieraczek2} pobiera wszystkie dane o filmach z podanych lat i zapisuje do
#' ramki danych w folderze ~/dane/glowne/rok.csv.
#'
#' @aliases pobieraczek
#' @param from filmy od roku...
#' @param to filmy do roku... wlacznie.
#' @param min minimalna liczba linijek do pobrania
#' @param maksymalna liczba linijek do pobrania
#' @return zapis do pliku(ow) w folderze ~/dane/glowne/rok.csv dane filmow z podanych lat.
#' @import stringi
#' @export

args<-commandArgs(TRUE)
from <- as.numeric(args[1])
to <- as.numeric(args[2])
min <- as.numeric(args[3])
max <- as.numeric(args[4])

pobieraczek2 <- function(from, to=from, min = 1, max = 1000){

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
                  if(k == tmp_l) save_lines(plik_linkow, tmp[k], names_of_table[j], ile = (max - tmp[k]+1))
                  else save_lines(plik_linkow, tmp[k], names_of_table[j])
                  k <- k+1
            }
            j <- j+1
      }
      cat("\nDone\n")
}

save_lines <- function(plik_linkow, liczba_linkow, names_of_table, ile = 100){
      i <- liczba_linkow
      con <- file(names_of_table)
      open(con,"a+")
      while( i <= (liczba_linkow + ile)){
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
