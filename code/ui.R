library(shinythemes)

ui <- fluidPage(
  
  theme = shinytheme("cerulean"),  # Theme selection
  
  # HEADER----
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo_ju_small.png")),
  
  # TITLE----
  titlePanel(title = div(img(src = "logo_ju_long.png", height = 80, width = 400)), 
             windowTitle = "RIV Point Calculator"),
  
  tags$h1("RIV Point Calculator"),
  
  # TABS----
  tabsetPanel(
    
    ## Tab 1: FAQ----
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
        tags$li("Conference proceeding: 10-100")
      ),
      
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
    ),
  
  
  
  
  
  ## Tab 2: Calculator----
  tabPanel(
    title = "FFPW",
    
    ### Sidebar Layout----
    sidebarLayout(
      
      # Sidebar Panel for Inputs----
      sidebarPanel(
        selectInput("resultType", "Type of Output:",
                    choices = list("Article (WoS-listed, with AIS)" = "jimp_ais",
                                   "Article (WoS-listed, without AIS)" = "jimp_no_ais",
                                   "Article (not WoS-listed, Scopus-listed)" = "jsc",
                                   "Conference proceedings" = "proceedings",
                                   "Book" = "book",
                                   "Book chapter" = "chapter",
                                   "Patent" = "patent"),
                    selected = "jimp_ais"),
        
        conditionalPanel(
          condition = "input.resultType == 'chapter'",
          numericInput("pageShare", "Rel. page share in the book:", 
                       value = 0.5, min = 0, max = 1, step = 0.01)
        ),
        
        tags$h2("Authorship"),
        checkboxInput("firstauthor_ffpw", "Is the first author affiliated with FFPW USB?", value = TRUE),
        checkboxInput("firstauthor_other", "Is FFPW USB the first author's only affiliation?", value = TRUE),
        numericInput("n_coauthors", "Number of co-authors", value = 0, min = 0, step = 1),
        numericInput("n_coauthors_foreign", "Number of co-authors with foreign affiliation", value = 0, min = 0, step = 1),
        
        conditionalPanel(
          condition = "input.n_coauthors_foreign > 0 & input.n_coauthors > input.n_coauthors_foreign",
          checkboxInput("lastauthor_foreign", "Is the last author affiliated with FFPW USB?", value = TRUE)
        )
      ),
      
      # Main Panel for Outputs----
      mainPanel(
        DTOutput("unique_journals"),
        
        tags$br(),
        tags$h4("Calculated Factor"),
        textOutput("calculated_factor"),
        
        tags$br(),
        tags$h4("Calculated RIV points"),
        textOutput("riv_points"),
        
        tags$br(),
        tags$h4("RIV points per author"),
        DTOutput("riv_points_per_author")
      )
    )
  ),
  ),
  
  
  
  
  
  # FOOTER----
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
