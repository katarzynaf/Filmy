# widze to jakos tak, ze jak sie dzielimy rokami, to wrzucamy sobie csv-ki z naszymi latami do 
# jednego folderu i on z niego wszystko automatycznie laczy i pobiera :)
# poprawilam na te potrzeby fnkcje merge.csv() (tzn. zdazyl to zrobic Piotrek lub Martyna
# ale jeszcze dopisalam Wam tam "/" :P)

pobieraczek <- function(from, to=from){
  
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
    
    for(j in 1:length(names_of_table)){
      
      con <- file(names_of_table[j])
      plik_linkow <- read.table(which_to_download[j],header=TRUE)$link
      liczba_linkow <- length(plik_linkow)
      i=1
      open(con,"a")
      while( i <= liczba_linkow ){
            link <- as.character(plik_linkow[i])
            a <- from_main_page(link)
            b <- from_full_cast_n_crew(link)
            c <- from_page_source(link)
            d <- keywords(link)
            suppressWarnings(
                  write.table(
                        as.data.frame(c(a,b,c,d)),
                        file=names_of_table[j], append=TRUE, sep=";", row.names=FALSE,
                        col.names=!( file.exists(names_of_table[j]) )
                  )
            )
            i=i+1
      }
      close(con)
    }
      cat("\nDone\n")
}

pobieraczek(1894)

