merge_csv<-function(folder){ ##funkcja sklejaca wszystkie csv do jednej ramki danych,
      #folder musi sie zaczynac od "/" np "/movies_link"
      # EDIT: juz nie musi :P:P [kf]
      files<-list.files(paste0(getwd(),"/", folder),full.names=TRUE)
      data<-do.call(rbind,lapply(files,read.table,header=TRUE))
      data
      
}