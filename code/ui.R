library(shinythemes)

ui <- fluidPage(
  
  theme = shinytheme("cerulean"),  # Theme selection
  
  # HEADER----
  tags$head(tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo_frov_small.png")),
  
  # TITLE----
  titlePanel(title = div(img(src = "logo_frov_long.png", height = 80, width = 400)), 
             windowTitle = "RIV Point Calculator"),
  
  tags$h1("RIV Point Calculator"),
  
  # TABS----
  tabsetPanel(
  
  ## Tab 1: Calculator----
  tabPanel(
    title = "Calculator",
    
    ### Sidebar Layout----
    sidebarLayout(
      
      # Sidebar Panel for Inputs----
      sidebarPanel(
        tags$h2("Authorship"),
        numericInput("n_authors", 
                     "How many authors contributed?", 
                     value = 1, min = 1, step = 1),
        numericInput("n_authors_foreign", 
                     "How many authors have a foreign affiliation?", 
                     value = 0, min = 0, step = 1),
        
        # Conditional checkbox for the first author
        conditionalPanel(
          condition = "input.n_authors_foreign > 0 && input.n_authors >= 1", 
          checkboxInput("firstauthor_foreign", "First author foreign", 
                        value = FALSE)
        ),
        
        # Conditional checkbox for the last author
        conditionalPanel(
          condition = "input.n_authors_foreign > 0 && input.n_authors > 2", 
          checkboxInput("lastauthor_foreign", "Last author foreign", 
                        value = FALSE)
        ),
        
        
        # RIV POINTS OVERVIEW
        tags$br(),
        tags$h2("RIV points"),
        
        tags$h4("Points per institution"),
        tableOutput("riv_overview"),
        
        tags$br(),
        tags$h4("Points per author"),
        tableOutput("riv_points_per_author")
      ),
      
      # Main Panel for Outputs----
      mainPanel(
        DTOutput("unique_journals")
      )
    )
  ),
  
  
  
  ## Tab 2: FAQ----
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
                and last) with a foreign affiliation by 0.5.")
    )
  ),
  ),
  
  
  
  
  
  # FOOTER----
  tags$br(),
  tags$br(),
  tags$b("Written by:"),
  tags$a(href = "https://anil.tellbuescher.online", "Anıl Axel Tellbüscher"),
  tags$text(", University of South Bohemia, Czech Republic."),
  tags$br(),
  tags$b("Reporting issues:"),
  tags$text("Please report issues via"),
  tags$a(href = "https://github.com/TellAnAx/riv_calculator/issues", "GitHub"),
  tags$text(" or contact the admin via email:"),
  tags$a(href = "mailto:admin@tellbuescher.online", "admin@tellbuescher.online"),
  tags$br(),
  tags$br()
)
