# GENERAL
#
# best would be to modularize the calculation process because
# that way the only thing that needs to be changed between
# faculty tabs is the calculation function for mean factors, 
# RIV points, and perhaps author weights. 

# create dummy variables----
input <- list()
input$n_categories


# read journal data----
journal_data <- readxl::read_excel("JCI_2023.xlsx")



# create dataset with unique journal names----

# this dataset will be displayed and searchable by journal name
# and subject area for the user
journal_data %>% 
  distinct(journal_name, .keep_all = T) %>% 
  select(-subject_area_name, -Faculty_Quartiles, -Ranking, -edition)



# create dataset with journal names, IF, AIS, and ranking split into two columns----
journal_data %>% 
  tidyr::separate_wider_delim(Ranking, 
                              names = c("rank", "out_of"),  
                              delim = " / ") %>% 
  select(-Faculty_Quartiles, -edition)



# calculate the mean factor----

# this calculation approach is described in Deans Measure XXX
calcMeanFactor <- function() {
  
  total_N <- 0
  count <- 0
  
  
  # Iterate over each subject_area_name, ensuring inputs exist
  for (i in 1:input$n_categories) {
    
    ind_rank <- input[[paste0("ranking", i)]]
    ind_Pmax <- input[[paste0("Pmax", i)]]
    
    if (!is.null(ind_rank) & !is.null(ind_Pmax)) {
      
      ind_N <- (ind_rank - 1) / (ind_Pmax - 1)
      
      total_N <- total_N + ind_N
      count <- count + 1
    }
  }
  
  # Calculate N
  N <- total_N/count
  
  Factor <- (1 - N) / (1 + (N / 0.057))
  
  return(Factor)
}



# create function to calculate RIV points----
calculate_RIV_points <- function() {
  if (input$resultType == "book") {
    return(200)  # Directly return 200 for book
  } else if (input$resultType == "patent") {
    return(40)  # Directly return 40 for patent
  } else if (input$resultType == "chapter") {
    return(200 * input$pageShare)  # Calculate based on page share for chapter
  } else {
    
    meanFactor <- calcMeanFactor()  # Use the mean factor from all categories
    
    if (!is.na(meanFactor)) {
      switch(input$resultType,
             "jimp_ais" = 10 + 290 * meanFactor,
             "jimp_no_ais" = 10 + 190 * meanFactor,
             "jsc" = 10 + 140 * meanFactor,
             "proceedings" = 10 + 90 * meanFactor,
             NA  # Default case if result type is not matched
      )
    } else {
      NA  # Return NA if mean factor is NA
    }
  }
}



# create function to calculate author weights----



