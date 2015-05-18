args<-commandArgs(TRUE)
po_czym <- as.character(args[1])
n <- as.numeric(args[2])

load("movies10IMDB.rda", verbose=FALSE)
library(stringi)
library(parallel)

wskaznik <- function(po_czym, n){
      slowaRozdzielone <- sapply(movies10IMDB[,po_czym], function(x) tolower(stri_extract_all_words(x)[[1]]))
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
      rownames(X) <- 1:n
      colnames(X) <- 1:n
      assign(po_czym,X)
      save(list=po_czym, file=paste0(po_czym,".rda"))
}

start.time <- Sys.time()
wskaznik(po_czym,10)
stop.time <- Sys.time()
cat(po_czym,":\t\t")
stop.time-start.time

