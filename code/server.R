server <- function(input, output) {
  
  # Render dynamic UI for category inputs based on the number of categories
  output$categoryInputs <- renderUI({
    input_list <- lapply(1:input$n_categories, function(i) {
      fluidRow(
        column(6, numericInput(paste0("ranking", i), sprintf("Ranking of journal in category %d:", i), value = 1, min = 1)),
        column(6, numericInput(paste0("Pmax", i), sprintf("Total number of journals in category %d:", i), value = 1, min = 1))
      )
    })
    do.call(tagList, input_list)
  })
  
  
  # Reaactive to calculate factor based on ranking and Pmax
  calcMeanFactor <- reactive({
    
    total_N <- 0
    count <- 0
    
    # Iterate over each category, ensuring inputs exist
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
  })
  
  
  
  # Calculate RIV points based on the type of result and computed factors
  calcPoints <- reactive({
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
  })
  
  
  # Render the calculated points
  output$rivPoints <- renderText({
    
    points <- calcPoints()
    
    if (is.na(points)) {
      return("-")  # Show "-" if points are not calculated
    } else {
      return(round(points, 2))  # Show rounded points
    }
  })
  
  
  author_weights <- reactive({
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

    df <- data.frame(authors, weights)
    
    return(df)
  })
  
  output$weights <- renderTable({
    weight_table <- author_weights() %>% 
      mutate(sum = sum(weights),
             prop = weights/sum,
             points = calcPoints() * prop) %>% 
      
      rename(Author = "authors",
             `Individual weight` = "weights",
             `Sum of all weights` = "sum",
             `Resulting weighing factor` = "prop",
             `Resulting individual RIV points` = "points")
  })
}
