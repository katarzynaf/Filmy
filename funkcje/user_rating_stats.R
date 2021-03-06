user_rating_stats <- function(link) {
      if (stri_sub(link, -1) == "/") {
            link <- paste0(link, "ratings?ref_=tt_ov_rt")
      } else {
            link <- paste0(link, "/ratings?ref_=tt_ov_rt")
      }
      user_rating_stats <- readHTMLTable(link)[[2]]
      if (names(user_rating_stats)[3] != "Average") {
            user_rating_stats <- NA
            return(user_rating_stats)
      }
      names(user_rating_stats)[1] <- "Who"
      user_rating_stats[, 2] <- stri_extract_all_regex(user_rating_stats[, 2], "[0-9]+", simplify = TRUE)
      user_rating_stats[, 3] <- stri_extract_all_regex(user_rating_stats[, 3], "[0-9]+", simplify = TRUE)
      user_rating_stats[, 3] <- stri_paste(user_rating_stats[, 3][, 1], user_rating_stats[, 3][, 2], sep = ".")
      user_rating_stats <- user_rating_stats[-which(is.na(user_rating_stats[, 2])), ]
      rownames(user_rating_stats) <- seq_len(nrow(user_rating_stats))
      return(user_rating_stats)
}
