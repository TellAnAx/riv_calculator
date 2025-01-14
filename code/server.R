server <- function(input, output) {
  
  # TABLE----
  # Load data from a predefined Excel file in the app directory
  journal_data <- reactive({
    read_excel("JCI_2023.xlsx") %>% 
      as_tibble()
  })
  
  # Create dataset with unique journal names for display
  unique_journals <- reactive({
    req(journal_data())
    
    journal_data() %>% 
      distinct(journal_name, .keep_all = TRUE) %>% 
      select(-subject_area_name, -Faculty_Quartiles, -Ranking, -edition)
  })
  
  
  # Render the unique journals table
  #
  # This is the table from where the user can select a journal to get further
  # information about the possible number of RIV points.
  output$unique_journals <- renderDT({
    datatable(unique_journals(), 
              selection = "single", 
              options = list(pageLength = 3, 
                             autoWidth = TRUE))
  })
  
  
  # Create ranked journals dataset for internal processing
  ranked_journals <- reactive({
    req(journal_data())
    
    journal_data() %>% 
      tidyr::separate_wider_delim(Ranking, 
                                  names = c("rank", "out_of"),  
                                  delim = " / ") %>% 
      select(-Faculty_Quartiles, -edition) %>% 
      mutate(
        rank = as.numeric(rank),
        out_of = as.numeric(out_of))
  })

  
  # Filter ranked_journals based on selected journal_name
  selected_journal <- reactive({
    req(input$unique_journals_rows_selected)
    
    selected_index <- input$unique_journals_rows_selected
    selected_name <- unique_journals()[selected_index, ][["journal_name"]]
    ranked_journals() %>% 
      filter(journal_name == selected_name)
  })
  
  
  # Calculate the Factor based on selected_journal data
  calcMeanFactor <- reactive({
    req(selected_journal())
    
    data <- selected_journal()
    
    count <- nrow(data)
    
    if (count == 0) return(NA)
    
    total_N <- sum((data$rank - 1) / (data$out_of - 1))
    
    N <- total_N / count
    
    Factor <- (1 - N) / (1 + (N / 0.057))
    
    #round(Factor, 4)
    
    return(Factor)
  })
  
  
  # Display the calculated Factor
  output$calculated_factor <- renderText({
    req(calcMeanFactor())
    
    paste("Calculated Factor:", calcMeanFactor())
  })
  

  
  
  # Calculate RIV points based on the type of result and computed factors
  calc_points <- reactive({
    req(calcMeanFactor(), input$resultType, input$pageShare)
    
    if (input$resultType == "book") {
      return(200)  # return 200 for book
      
    } else if (input$resultType == "patent") {
      return(40)  # return 40 for patent
      
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
  })
  
  
  # Render the calculated points
  output$riv_points <- renderText({
    req(calc_points())
    
    points <- calc_points()
    
    if (is.na(points)) {
      return("-")  # Show "-" if points are not calculated
    } else {
      return(round(points, 2))  # Show rounded points
    }
  })
  
  
  author_weights <- reactive({
    req(input$n_coauthors, 
        input$n_coauthors_foreign,
        input$firstauthor_ffpw,
        input$firstauthor_other,
        input$lastauthor_foreign)
    
    # Create author vector
    if (input$n_coauthors > 0) {
      authors <- c("First author",
                   paste("Co-author", seq(1, input$n_coauthors)))

      authors[length(authors)] <- "Last author"

    } else {
      authors <- "First author"
    }


    # Create weights vector
    weights <- c(1, rep(1, input$n_coauthors))

    # First author weight modification (if affiliated with FFPW USB)
    if (input$firstauthor_ffpw == TRUE) {
      weights[1] <- weights[1] * 2
    }

    if (input$firstauthor_other == FALSE) {
      weights[1] <- weights[1] * 0.5
    }

    # Last author gets a 1.5 weight (if affiliated with FFPW USB)
    if (input$n_coauthors > 0) {
      weights[input$n_coauthors + 1] <- weights[input$n_coauthors + 1] * 1.5
    }

    # Co-authors with foreign affiliation get 0.5 weight
    if (input$n_coauthors_foreign == 1 & input$lastauthor_foreign == FALSE) {
      weights[input$n_coauthors + 1] <- weights[input$n_coauthors + 1] * 0.5
    } else if (input$n_coauthors_foreign > 1 & input$lastauthor_foreign == FALSE) {
      foreign_indexes <- c(seq(2, input$n_coauthors_foreign), input$n_coauthors + 1)
      weights[foreign_indexes] <- weights[foreign_indexes] * 0.5
    } else if (input$n_coauthors_foreign > 0) {
      foreign_indexes <- seq(2, input$n_coauthors_foreign + 1)
      weights[foreign_indexes] <- weights[foreign_indexes] * 0.5
    }

    df <- tibble(authors, 
                 weights)

    return(df)
  })
  
  
  output$riv_points_per_author <- renderDT({
    req(author_weights(), calc_points())
    
    weight_table <- author_weights() %>%
      mutate(sum = sum(weights),
             prop = weights / sum,
             points = calc_points() * prop) %>%
      rename(Author = "authors",
             `Individual weight` = "weights",
             `Sum of all weights` = "sum",
             `Resulting weighing factor` = "prop",
             `Resulting individual RIV points` = "points")
    
    datatable(weight_table, options = list(pageLength = 5, autoWidth = TRUE))
  })
  

}
