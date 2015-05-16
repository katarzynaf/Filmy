rating_stats <- function(link) {
      if (stri_sub(link, -1) == "/") {
            link <- paste0(link, "ratings?ref_=tt_ov_rt")
      } else {
            link <- paste0(link, "/ratings?ref_=tt_ov_rt")
      }
      rating_stats <- readHTMLTable(link)[[1]]
      if (ncol(rating_stats) == 1) {
            # gdy nie ma statystyk z glosami
            rating_stats <- NA
            return(rating_stats)
      }
      names(rating_stats) <- stri_extract_all_regex(names(rating_stats), "[a-zA-Z]+", simplify = TRUE)
      rating_stats[, 2] <- stri_extract_all_regex(rating_stats[, 2], "[0-9]{1,2}.[0-9]{1,2}%", simplify = TRUE)
      return(rating_stats)
}
