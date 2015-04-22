#' Statystyki wielu filmow
#'
#' Funkcja ulatwia pobranie i zapis (w getwd()) wszystkich statystyk o wielu filmach do jednego pliku.
#'
#' @aliases table_rating
#' @param links wektor linkow do stron glownych filmow na IMDb.
#' @param name_of_table nazwa pliku wyjsciowego
#' @return ramka danych za statystykami filmow z linkow podanych parametrem links.
#' @export

table_rating <- function(links,name_of_table) {
      data_frame <- do.call(rbind, lapply(links, stats_to_one_row))
      suppressWarnings(
            write.table(
                  data_frame,
                  name_of_table,
                  append=TRUE,
                  col.names=!file.exists(name_of_table),
                  row.names=FALSE)
            )
}

