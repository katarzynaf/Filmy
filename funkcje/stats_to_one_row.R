title<-function(link){
   page<-html(link)
   title<-html_nodes(page,".header .itemprop")
   title<-html_text(title)
}

stats_to_one_row <- function(link, user_stats = user_rating_stats(link), votes_stats = rating_stats(link)) {
      # kolumny jakie ma tabelka / nazwy elementow rzedu
      table_columns <- c("Title", "Overall_Rating", "Votes", "Males", "Females", "Aged_under_18", "Males_under_18", "Females_under_18", "Aged_18-29", 
                         "Males_Aged_18-29", "Females_Aged_18-29", "Aged_30-44", "Males_Aged_30-44", "Females_Aged_30-44", "Aged_45+", "Males_Aged_45+", "Females_Aged_45+", 
                         "IMDb_staff", "Top_1000_voters", "US_users", "Non-US_users", "Vote_10", "Vote_9", "Vote_8", "Vote_7", "Vote_6", "Vote_5", "Vote_4", "Vote_3", 
                         "Vote_2", "Vote_1")
      # finalny wektor, brak danych na -1 ustawiam, bo 0 moze jednak cos znaczyc
      table_row <- c(title(link), rep("-1", length(table_columns) - 1))
      names(table_row) <- table_columns
      if (length(user_stats) == 1) {
            # gdy nie ma statystyk (dlugosc NA to 1, a ramki danych ze statystykami to 3)
            return(table_row)
      }
      # zapamietujemy dlugosc tabelki, bo ostatni wiersz nam sie przyda
      m <- nrow(user_stats)
      # nazwy kolumn dla tabelki z ocenami, ostatni wiersz jest 2 kolumnami
      cols_user_stats <- as.character(user_stats$Who[-m])
      cols_user_stats <- stri_replace_all_regex(cols_user_stats, " ", "_")
      # nazwy kolumn dla tabelki z glosami
      cols_votes_stats <- paste0("Vote_", as.character(votes_stats$Rating))
      # nazwy kolumn
      columns <- c("Overall_Rating", "Votes", cols_user_stats, cols_votes_stats)
      # wartosci w wierszu
      row <- c(user_stats[m, 3], user_stats[m, 2], user_stats$Average[-m], as.character(votes_stats$Votes))
      # ktore mamy kolumny w ramkach, moze sie zdarzyc ze jakiegos przedzialu wiekowego nie ma
      which_columns <- unlist(lapply(columns, function(col_name) {
            which(col_name == table_columns)
      }))
      table_row[which_columns] <- row
      return(table_row)  #zwraca character => mozna na numeric zmienic
}