#' Sklejanie plikow
#'
#' Funkcja \code{merge_csv} skleja do jednej ramki danych wszystkie pliki .csv ze wskazanego folderu.
#' Sluzy do laczenia w jedna tabele pobranych linkow do filmow.
#'
#' @aliases merge_csv
#' @param folder nazwa folderu z plikami .csv
#' @return ramka danych
#' @export

merge_csv<-function(folder){
      files<-list.files(paste0(getwd(),"/", folder),full.names=TRUE)
      data<-do.call(rbind,lapply(files,read.table,header=TRUE))
      data

}
