server <- function(input, output) {
  # Render dynamic UI for category inputs based on the number of categories
  output$categoryInputs <- renderUI({
    num <- max(as.numeric(input$n_categories), 1)  # Ensure at least 1 category
    input_list <- lapply(1:num, function(i) {
      fluidRow(
        column(6, numericInput(paste0("ranking", i), sprintf("Ranking of journal in category %d:", i), value = 1, min = 1)),
        column(6, numericInput(paste0("Pmax", i), sprintf("Total number of journals in category %d:", i), value = 1, min = 1))
      )
    })
    do.call(tagList, input_list)
  })
  
  # Helper function to calculate factor based on ranking and Pmax
  calcFactor <- function(ranking, Pmax) {
    # Ensure ranking and Pmax are not NULL or zero to prevent errors
    if (!is.null(ranking) && !is.null(Pmax) && Pmax > 0) {
      N <- (ranking - 1) / (Pmax - 1)
      return((1 - N) / (1 + (N / 0.057)))
    } else {
      return(0)  # Return 0 if conditions are not met to avoid errors
    }
  }
  
  # Reactive function to calculate the mean factor from multiple category inputs
  calcMeanFactor <- reactive({
    num <- max(as.numeric(input$n_categories), 1)
    totalFactor <- 0
    count <- 0
    
    # Iterate over each category, ensuring inputs exist
    for (i in 1:num) {
      ranking <- input[[paste0("ranking", i)]]
      Pmax <- input[[paste0("Pmax", i)]]
      if (!is.null(ranking) && !is.null(Pmax)) {
        totalFactor <- totalFactor + calcFactor(ranking, Pmax)
        count <- count + 1
      }
    }
    
    # Only calculate the average if we have valid counts
    if (count > 0) {
      return(totalFactor / count)
    } else {
      return(NA)  # Return NA if no valid counts to prevent errors
    }
  })
  
  # Calculate RIV points based on the type of result and computed factors
  calcPoints <- reactive({
    meanFactor <- calcMeanFactor()  # Use the mean factor from all categories
    if (!is.na(meanFactor)) {
      switch(input$resultType,
             "book" = 200,
             "patent" = 40,
             "chapter" = 200 * input$pageShare,
             "jimp_ais" = 10 + 290 * meanFactor,
             "jimp_no_ais" = 10 + 190 * meanFactor,
             "jsc" = 10 + 140 * meanFactor,
             "proceedings" = 10 + 90 * meanFactor,
             NA  # Default case if result type is not matched
      )
    } else {
      NA  # Return NA if the mean factor is NA
    }
  })
  
  # Render the calculated points
  output$rivPoints <- renderText({
    points <- calcPoints()
    if (is.na(points)) "-" else round(points, 2)
  })
}
