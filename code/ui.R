ui <- fluidPage(
  tags$head(tags$link(rel = "icon", type = "image/png", 
                      sizes = "32x32", href = "logo_frov_small.png")),
  
  titlePanel(title = div(img(src="logo_frov_long.png",
                             height = 80,
                             width = 400)), 
             windowTitle = "RIV Point Calculator"),
  
  tags$h1("RIV Point Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("resultType", "Type of Output:",
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
      tags$h3("Calculated RIV Points:"),
      textOutput("rivPoints"),
      tags$h3("Explanation"),
      tags$text("RIV points are the main Key Performance Indicator for research 
                institutions in the Czech Republic.")
    ),
  ),
  tags$br(),
  tags$br(),
  tags$b("written by:"),
  tags$a(href = "https://anil.tellbuescher.online", 
         "Anıl Axel Tellbüscher"),
  tags$text(", University of South Bohemia, Czech Republic"),
  tags$br(),
  tags$br()
)
