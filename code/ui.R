app_version <- read.dcf("DESCRIPTION")[, "Version"]

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # Theme selection
  
  # HEADER----
  tags$head(
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "32x32",
      href = "logo_frov_small.png"
    )
  ),
  
  # TITLE----
  titlePanel(title = div(
    img(
      src = "logo_frov_long.png",
      height = 80,
      width = 400
    )
  ), windowTitle = paste0("RIVcalc v", app_version)),
  
  
  tags$h1(
    "RIV Point Calculator ",
    tags$small(
      paste0("v", app_version),
      style = "color: #6c757d;"
    )),
  
  # TABS----
  tabsetPanel(
    ## Tab 1: Calculator----
    tabPanel(title = "Calculator", sidebarLayout(
      sidebarPanel(

          # Input for the number of authors
          tags$h2("Authorship"),
          numericInput(
            inputId = "n_authors",
            label = "How many authors contributed?",
            value = 1,
            min = 1,
            step = 1
          ),
          helpText("Enter the total number of authors."),
          
          hr(),
          
          # Dynamic matrix of checkboxes
          uiOutput("matrix_ui"),
          
          # Button to confirm selections
          actionButton("submit", "Confirm Selections", class = "btn btn-primary"),

          hr(),
          
        wellPanel(
          # Dataset selection
          tags$h2("Dataset"),
          selectInput(
            inputId = "dataset",
            label = "Which dataset to use?",
            choices = c(
              "JCI 2023" = "JCI_2023",
              "JCI 2024" = "JCI_2024",
              "JCI 2025" = "JCI_2025"
            ),
            selected = "JCI_2025"
          ),
          helpText("Default: latest dataset. Older datasets can be used for 
        comparison with past results.")
        )
        ),
      
      mainPanel(
        wellPanel(tags$h3("Journal Overview"), DTOutput("unique_journals")),
        
        wellPanel(
          tags$h4("RIV points per Institution"),
          tableOutput("riv_overview")
        ),
        
        wellPanel(
          tags$h4("RIV points per Author"),
          tableOutput("riv_points_per_author")
        )
      )
    )),
    
    
    
    ## Tab 2: FAQ----
    tabPanel(
      title = "FAQ",
      tags$h3("What are RIV points?"),
      tags$p(
        "RIV points are the main Key Performance Indicator for
              research institutions in the Czech Republic."
      ),
      
      tags$h3("What is the maximum number of RIV points that can be obtained?"),
      tags$p(
        "The maximum number of RIV points for a particular publication type
              is defined as follows:"
      ),
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
      tags$p(
        "The distribution of the points between the authors
        is done according to a weighing system that works as follows:"
      ),
      tags$ul(
        tags$li("Assign each author an initial weight of 1."),
        tags$li("Multiply the weight of the first author by 2."),
        tags$li("Multiply the weight of the last author by 1.5."),
        tags$li(
          "Multiply the weight of those authors (including first
                  and last) with an affiliation outside of the Czech 
          Republic by 0.5. More information can be found in Dean's 
          Measure No. 18/2024."
        )
      )
    )
  ),
  
  # FOOTER----
  tags$br(),
  tags$br(),
  tags$text(paste0("You are using RIVcalc v", app_version)),
  tags$br(),
  tags$b("Written by:"),
  tags$a(href = "https://anil.tellbuescher.online", "Anıl Axel Tellbüscher"),
  tags$text(", University of South Bohemia, Czech Republic."),
  tags$br(),
  tags$b("Bug reporting:"),
  tags$text("Please report any issues you face during the useage of this app via"),
  tags$a(href = "https://github.com/TellAnAx/riv_calculator/issues", "GitHub"),
  tags$text(" or contact the admin via email:"),
  tags$a(href = "mailto:admin@tellbuescher.online", "admin@tellbuescher.online"),
  tags$br(),
  tags$br()
)
