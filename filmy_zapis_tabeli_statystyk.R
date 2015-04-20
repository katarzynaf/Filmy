library("stringi")
library("XML")

table_rating<-function(file){
   links<-as.character(read.table(file,header=TRUE)$link)
   data_frame<-as.data.frame(do.call(rbind,lapply(links,stats_to_one_row)))
}

from<-1800
to<-1905

save_table_rating<-function(from,to){
   if(!file.exists(file.path("dane"))){
      # jesli nie, tworzymy takowy
      dir.create(file.path("dane"))
   }
   if(!file.exists(file.path("dane/statystyki"))){
      # jesli nie, tworzymy takowy
      dir.create(file.path("dane/statystyki"))
   }
   #lista plikow w folderze
   files<-list.files(paste0(getwd(),"/movies_link"),full.names=TRUE)
   #wyciagamy nazwy plikow, ktore sa latami produkcji filmu
   names_of_files<-stri_sub(files,-8,-5)
   #ktore mieszcza sie w przedziale czasowym
   which_to_download<-which(names_of_files>=from & names_of_files<=to)
   if(length(which_to_download)==0){
      return(invisible(NULL))
   }
   #interesujace nas pliki
   files<-files[which_to_download]
   names_of_table<-paste0("dane/statystyki/",names_of_files[which_to_download],".csv")
   for(i in 1:length(files)){
      table<-table_rating(files[i])
      write.table(table,names_of_table[i])
   }
}

