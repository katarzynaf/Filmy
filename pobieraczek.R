# widze to jakos tak, ze jak sie dzielimy rokami, to wrzucamy sobie csv-ki z naszymi latami do 
# jednego folderu i on z niego wszystko automatycznie laczy i pobiera :)
# poprawilam na te potrzeby fnkcje merge.csv() (tzn. zdazyl to zrobic Piotrek lub Martyna
# ale jeszcze dopisalam Wam tam "/" :P)

folder_z_moja_czescia <- "test_linki"
file_destination <- "rekordy"

pobieraczek <- funkcja( file_destination, folder_z_moja_czescia ){
      
      plik_linkow <- merge_csv( folder_z_moja_czescia )
      dest_file <- paste( destination,"csv",sep="." )
      if( dest_file%in%list.files(getwd()) )
            message("Dopisuje dane do istniejacego pliku!")
      
      con <- file(dest_file)
      liczba_linkow <- nrow(plik_linkow)
      i=1
      open(con,"a")
      while( i <= liczba_linkow ){
            link <- as.character(plik_linkow[i,2])
            a <- from_main_page(link)
            b <- from_full_cast_n_crew(link)
            c <- from_page_source(link)
            d <- keywords(link)
            suppressWarnings(
                  write.table(
                        as.data.frame(c(a,b,c,d)),
                        file=dest_file, append=TRUE, sep=";", row.names=FALSE,
                        col.names=!( file.exists(dest_file) )
                  )
            )
            i=i+1
      }
      close(con)
      cat("\nDone\n")
}

pobieraczek( destination, plik_linkow )

