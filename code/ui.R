ui <- fluidPage(
  
  titlePanel("RIV Point Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("resultType", "Type of Result:",
                  choices = list("Article (WoS-listed, with AIS)" = "jimp_ais",
                                 "Article (WoS-listed, without AIS)" = "jimp_no_ais",
                                 "Article (not WoS-listed, Scopus-listed)" = "jsc",
                                 "Conference proceedings" = "proceedings",
                                 "Book" = "book",
                                 "Book chapter" = "chapter",
                                 "Patent" = "patent")),
      # Only show category inputs for journal-related result types
      conditionalPanel(
        condition = "input.resultType != 'book' && input.resultType != 'patent' && input.resultType != 'chapter' && input.resultType != 'proceedings'",
        numericInput("n_categories", "Number of categories the journal is listed in", value = 1),
        uiOutput("categoryInputs")  # Dynamic UI for ranking and Pmax inputs
      ),
      conditionalPanel(
        condition = "input.resultType == 'chapter'",
        numericInput("pageShare", "Page share in the book:", value = 1, min = 0, max = 1, step = 0.01)
      )
    ),
    mainPanel(
      h3("Calculated RIV Points:"),
      textOutput("rivPoints")
    )
  )
)
