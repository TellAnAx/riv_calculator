library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Pick a theme like 'flatly', 'cerulean', 'darkly', etc.
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo_frov_small.png")),
  
  titlePanel(title = div(img(src="logo_frov_long.png", height = 80, width = 400)), windowTitle = "RIV Point Calculator"),
  
  tags$h1("RIV Point Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h2("Authorship"),
      checkboxInput("firstauthor_ffpw", "Is the first author affiliated with FFPW USB?", value = TRUE),
      checkboxInput("firstauthor_other", "Is FFPW USB the first author's only affiliation?", value = TRUE),
      numericInput("n_coauthors", "Number of co-authors", value = 0, min = 0, step = 1),
      numericInput("n_coauthors_foreign", "Number of co-authors with foreign affiliation", value = 0, min = 0, step = 1),
      conditionalPanel(
        condition = "input.n_coauthors_foreign > 0 & input.n_coauthors > input.n_coauthors_foreign",
        checkboxInput("lastauthor_foreign", "Is the last author affiliated with FFPW USB?", value = TRUE)
      ),
      tags$h2("Publication"),
      selectInput("resultType", "Type of Output:",
                  choices = list("Article (WoS-listed, with AIS)" = "jimp_ais",
                                 "Article (WoS-listed, without AIS)" = "jimp_no_ais",
                                 "Article (not WoS-listed, Scopus-listed)" = "jsc",
                                 "Conference proceedings" = "proceedings",
                                 "Book" = "book",
                                 "Book chapter" = "chapter",
                                 "Patent" = "patent")),
      conditionalPanel(
        condition = "input.resultType != 'book' && input.resultType != 'patent' && input.resultType != 'chapter' && input.resultType != 'proceedings'",
        numericInput("n_categories", "Number of categories the journal is listed in", value = 1),
        uiOutput("categoryInputs")
      ),
      conditionalPanel(
        condition = "input.resultType == 'chapter'",
        numericInput("pageShare", "Rel. page share in the book:", value = 0.5, min = 0, max = 1, step = 0.01)
      )
    ),
    
    mainPanel(
      
      tags$h3("Calculated RIV Points"),
      tags$div(
        style = "font-weight: bold; background-color: #f0f0f0; padding: 10px; border-radius: 5px; font-size: 20px;",
        textOutput("rivPoints")
      ),
      
      tags$h3("RIV points for each author"),
      tableOutput("weights"),
      
      tags$h3("Explanation"),
      tags$text("RIV points are the main Key Performance Indicator for 
                research institutions in the Czech Republic. The maximum 
                number of RIV points for a particular publication type
                is pre-defined. The distribution of the points is then done 
                according to a weighing system that works as follows:"),
      tags$br(),
      tags$br(),
      tags$ul(
        tags$li("Assign each author an initial weight of 1."),
        tags$li("Multiply the weight of the first author by 2."),
        tags$li("Multiply the weight of the last author by 1.5."),
        tags$li("Multiply the weight of those authors (including first 
                and last) with an exclusively foreign affiliation by 0.5.")
      )
    )
  ),
  tags$br(),
  tags$br(),
  tags$b("written by:"),
  tags$a(href = "https://anil.tellbuescher.online", "Anıl Axel Tellbüscher"),
  tags$text(", University of South Bohemia, Czech Republic"),
  tags$br(),
  tags$br()
)
