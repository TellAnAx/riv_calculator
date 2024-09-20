library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Pick a theme like 'flatly', 'cerulean', 'darkly', etc.
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo_frov_small.png")),
  
  titlePanel(title = div(img(src="logo_frov_long.png", height = 80, width = 400)), windowTitle = "RIV Point Calculator"),
  
  tags$h1("RIV Point Calculator"),
  
  tabsetPanel(
    tabPanel(
      title = "Calculator",
      
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
          
          tags$h2("Where to find the necessary information?"),
          tags$b("Journals with AIS:"),
          tags$text("Sort the journals based on their Article Influence Score (AIS)
                    on Web of Science. Determine the rank of the journal (= position in 
                    the list) for each category in which it is listed."),
          tags$br(),
          
          tags$b("Journals without AIS but with IF:"),
          tags$text("Same as with AIS, but use the Impact Factor (IF) for sorting instead."),
          tags$br(),
          
          tags$b("Journals without AIS or IF:"),
          tags$text("Same as with AIS, but use the CiteScore for sorting instead."),
        
          tags$h3("Calculated RIV Points"),
          tags$div(
            style = "font-weight: bold; background-color: #f0f0f0; padding: 10px; border-radius: 5px; font-size: 20px;",
            textOutput("rivPoints")
          ),
          
          tags$h3("RIV points per author"),
          tableOutput("weights")
        )
      )
    ),
    
    tabPanel(
      title = "FAQ",
      tags$h3("What are RIV points?"),
      tags$text("RIV points are the main Key Performance Indicator for 
                research institutions in the Czech Republic."),
      tags$h3("What is the maximum number of RIV points that can be obtained?"),
      tags$text("The maximum number of RIV points for a particular publication type
                is defined as follows:"),
      tags$ul(
        tags$li("Journal article with AIS: 10-300"),
        tags$li("Journal article with IF (without AIS): 10-200"),
        tags$li("Journal article with CiteScore (without AIS and IF): 10-150"),
        tags$li("Book: 200"),
        tags$li("Book chapter: According to the page share in the book."),
        tags$li("Patent: 40"),
        tags$li("Conference proceeding: 10-100"),
      ),
      
      # tags$h3("How are the RIV points calculated?"),
      # withMathJax(
      #   helpText("$$N = \\frac{(P - 1)}{(P_{\\text{max}} - 1)}$$")
      # ),
      # tags$text("")

      tags$h3("How are the RIV points per author calculated?"),
      tags$text("The distribution of the points between the authors 
      is done according to a weighing system that works as follows:"),
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
  tags$b("Written by:"),
  tags$a(href = "https://anil.tellbuescher.online", "Anıl Axel Tellbüscher"),
  tags$text(", University of South Bohemia, Czech Republic."),
  tags$b("Reporting issues:"),
  tags$text("Please report issues via"),
  tags$a(href = "https://github.com/TellAnAx/riv_calculator/issues", "GitHub"),
  tags$text(" or contact the admin via email:"),
  tags$a(href = "mailto:admin@tellbuescher.online", "admin@tellbuescher.online"),
  tags$br(),
  tags$br()
)
