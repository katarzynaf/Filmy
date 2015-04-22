library("stringi")
library("XML")

table_rating <- function(links,name_of_table) {
      data_frame <- do.call(rbind, lapply(links, stats_to_one_row))
      if(file.exists(name_of_table)){
            suppressWarnings(write.table(data_frame,name_of_table,append=TRUE,col.names=FALSE,row.names=FALSE))
      }
      else{
            write.table(data_frame,name_of_table,row.names=FALSE)
      }
}

save_table_rating <- function(from, to) {
      if (!file.exists(file.path("dane"))) {
            # jesli nie, tworzymy takowy
            dir.create(file.path("dane"))
      }
      if (!file.exists(file.path("dane/statystyki"))) {
            # jesli nie, tworzymy takowy
            dir.create(file.path("dane/statystyki"))
      }
      # lista plikow w folderze
      files <- list.files(paste0(getwd(), "/movies_link"), full.names = TRUE)
      # wyciagamy nazwy plikow, ktore sa latami produkcji filmu
      names_of_files <- stri_sub(files, -8, -5)
      # ktore mieszcza sie w przedziale czasowym
      which_to_download <- which(names_of_files >= from & names_of_files <= to)
      if (length(which_to_download) == 0) {
            return(invisible(NULL))
      }
      # interesujace nas pliki
      files <- files[which_to_download]
      names_of_table <- paste0("dane/statystyki/", names_of_files[which_to_download], ".csv")
      for (i in 1:length(files)) {
            links <- as.character(read.table(files[i], header = TRUE)$link)
            n<-length(links)
            ifelse(n>1000,n<-1000,n<-n)
            how_many_times<-ceiling(n/50)
            counter<-1
            while(counter<=how_many_times){
                  if(counter!=(how_many_times-1)) {
                        part_of_links<-links[(1+(counter-1)*50):(counter*50)]
                  }
                  else {
                        part_of_links<-links[(1+(counter-1)*50):n]
                  }
                  table_rating(part_of_links,names_of_table[i])
                  gc()
                  counter<-counter+1
            }
      }
      return(invisible(NULL))
}


save_table_rating(from, to)