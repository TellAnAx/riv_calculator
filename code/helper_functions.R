read_journal_data <- function(journal_data_path) {
  journal_data <- readxl::read_excel(journal_data_path) %>%
    as_tibble()
  
  return(journal_data)
}



find_unique_journals <- function(journal_data) {
  unique_journals <- journal_data %>%
    distinct(journal_name, .keep_all = TRUE) %>%
    select(-year,
           -subject_area_name,
           -Faculty_Quartiles,
           -Ranking,
           -edition)
  
  return(unique_journals)
}



add_journal_ranking <- function(journal_data) {
  journal_ranking <- journal_data %>%
    tidyr::separate_wider_delim(Ranking,
                                names = c("rank", "out_of"),
                                delim = " / ") %>%
    select(-Faculty_Quartiles, -edition) %>%
    mutate(rank = as.numeric(rank), out_of = as.numeric(out_of))
  
  return(journal_ranking)
}



select_journal <- function(journal_ranking, selected_name) {
  selected_journal <- journal_ranking %>%
    filter(journal_name == selected_name)
  
  return(selected_journal)
}



calculate_mean_factor <- function(selected_journal) {

  count <- nrow(selected_journal)
  
  if (count == 0)
    return(NA)
  
  total_N <- sum((selected_journal$rank - 1) / (selected_journal$out_of - 1))
  
  N <- total_N / count
  
  mean_factor <- (1 - N) / (1 + (N / 0.057))
  
  return(mean_factor)
}



calculate_riv_points <- function(mean_factor, selected_journal) {
  
  selected_journal <- selected_journal %>%
    distinct(Impact.Factor, Article.Influence)
  
  if (!is.na(mean_factor)) {
    if (selected_journal$Article.Influence == 0 & selected_journal$Impact.Factor == 0) {
      riv_points <- 10 + 140 * mean_factor # neither AIS nor IF
      
    } else if (selected_journal$Article.Influence == 0 & selected_journal$Impact.Factor != 0) {
      riv_points <- 10 + 190 * mean_factor # no AIS but with IF
      
    } else {
      riv_points <- 10 + 290 * mean_factor # both AIS and IF
    }
  }
  
}
