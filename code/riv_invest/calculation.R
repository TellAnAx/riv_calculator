library(tidyverse)
library(readxl)


journals <- read_journal_data("JCI_2024.xlsx")


unique_journals <- find_unique_journals(journals)


ranked_journals <- add_journal_ranking(journals)


riv_points <- vector(mode = "numeric")
for(n in 1:nrow(unique_journals)) {
  chosen_journal <- unique_journals[n, "journal_name"] %>% as.vector()
  
  journal_rank <- filter(ranked_journals, journal_name == chosen_journal)
  
  riv_points[n] <- calculate_riv_points(calculate_mean_factor(journal_rank), journal_rank)
}


journals_with_riv <- unique_journals %>% 
  bind_cols(riv_points = riv_points) %>% 
  mutate(
    review = if_else(grepl("REVIEW", journal_name), "yes", "no")
  ) %>% 
  arrange(desc(riv_points))


journals_with_riv %>% 
  group_by(review) %>% 
  summarise(
    n = n(),
    min = min(riv_points),
    mean = mean(riv_points),
    median = median(riv_points),
    `70%` = quantile(riv_points, p = 0.75),
    `90%` = quantile(riv_points, p = 0.9),
    max = max(riv_points)
  )


journals_with_riv %>% 
  ggplot(aes(x = review, y = riv_points, fill = review)) +
  geom_violin(alpha = 0.5) + 
  theme_minimal() +
  labs(y = "RIV points")





# Proportions----
#
# proportion of review articles relative to all articles published
# by some research institutions in the Czech Republic and elsewhere

records <- tibble(
  data_path = list.files("code/riv_invest")
  ) %>% 
  filter(!grepl(".R", data_path)) %>% 
  mutate(
    data = map(data_path, ~read_csv(paste0("code/riv_invest/", .))),
    
    )

  