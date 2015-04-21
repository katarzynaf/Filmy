# info z readLines
from_page_source <- function(link) {
    page <- readLines(link)
    page <- paste(page, collapse = "")
    znaczniki <- c(production_countries = "(?<=Country:).+?(?=</div)", language = "(?<=Language:).+?(?=</div)", color = "(?<=Color:).+?(?=</a)")
    details <- function(znacznik) {
        item <- unlist(stri_extract_all_regex(page, znacznik))
        if (!is.na(item)) {
            item <- unlist(stri_extract_all_regex(item, "(?<=itemprop='url'>)([a-zA-Z]| )+"))
            scal(item)
        } else item <- NA
    }
    a <- sapply(znaczniki, details)
    names(a) <- names(znaczniki)
    return(a)
} 
