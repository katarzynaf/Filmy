# n - po to, zeby domyslnie nie liczylo wszystkich filmow tylko pierwszych n, pomoc do testow.
# po czym - wartosc z nazw kolumn obiektu movies10IMDB
load("movies10IMDB.rda", verbose=FALSE)
library(stringi)
library(parallel)

wskaznik <- function(po_czym, n){
      start.time <- Sys.time()
      slowaRozdzielone <- sapply(movies10IMDB[,po_czym], function(x) stri_extract_all_words(x)[[1]])
      slowaRozdzielone <- slowaRozdzielone[1:n]
      ile <- detectCores()
      cl <- makeCluster(ile)
      clusterExport(cl, c("slowaRozdzielone"), envir=environment())
      X <- parSapply(cl, slowaRozdzielone, function(x){
            sapply(slowaRozdzielone, function(y){
                  if(all(is.na(x),is.na(y))==FALSE)
                        length(intersect(x,y))/length(union(x,y)) else
                              NA
            })
      })
      stopCluster(cl)
      stop.time <- Sys.time()
      rownames(X) <- 1:n
      colnames(X) <- 1:n
      assign(po_czym,X)
      save(list=po_czym, file=paste0(po_czym,".rda"))
      return(stop.time-start.time)
}

