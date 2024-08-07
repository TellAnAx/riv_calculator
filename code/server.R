server <- function(input, output) {
  calcFactor <- reactive({
    ranking <- input$ranking
    Pmax <- input$Pmax
    N <- (ranking - 1) / (Pmax - 1)
    factor <- (1 - N) / (1 + (N / 0.057))
    return(factor)
  })
  
  calcPoints <- reactive({
    if (input$resultType == "book") {
      points <- 200
    } else if (input$resultType == "patent") {
      points <- 40
    } else if (input$resultType == "chapter") {
      points <- 200 * input$pageShare
    } else {
      factor <- calcFactor()
      if (input$resultType == "jimp_ais") {
        points <- 10 + 290 * factor
      } else if (input$resultType == "jimp_no_ais") {
        points <- 10 + 190 * factor
      } else if (input$resultType == "jsc") {
        points <- 10 + 140 * factor
      } else if (input$resultType == "proceedings") {
        points <- 10 + 90 * factor
      } else {
        points <- NA
      }
    }
    return(round(points, 2))
  })
  
  
  output$rifPoints <- renderText({
    calcPoints()
  })
  
}