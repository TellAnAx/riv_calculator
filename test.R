library(shiny)
library(tibble)

ui <- fluidPage(
  titlePanel("Matrix-Style Author Affiliation Selection"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h2("Authorship"),
      
      # Input for the number of authors
      numericInput(
        inputId = "n_authors", 
        label = "How many authors contributed?", 
        value = 1, min = 1, step = 1
      ),
      
      # Dynamic matrix of checkboxes
      uiOutput("matrix_ui"),
      
      # Button to confirm selections
      actionButton("submit", "Confirm Selections")
    ),
    
    mainPanel(
      h3("Finalized Author Table"),
      
      # Render the final table after submitting
      tableOutput("final_author_table")
    )
  )
)




server <- function(input, output, session) {
  
  # Reactive function to generate the author names
  author_names <- reactive({
    req(input$n_authors)
    n <- input$n_authors
    
    # Generate author names dynamically
    c("First author", paste("Co-author", seq(1, n - 2)), "Last author")[1:n]
  })
  
  # Dynamic UI for the matrix of checkboxes in the sidebar
  output$matrix_ui <- renderUI({
    req(input$n_authors)
    authors <- author_names()
    n <- length(authors)
    
    # Create the checkbox matrix: Authors x {FFPW, Czech, Non-Czech}
    matrix_ui <- tagList(
      tags$table(
        style = "width: 100%; border-collapse: collapse;",  # Full width styling
        tags$thead(
          tags$tr(
            tags$th("Author", style = "text-align:left; width = 40%;"),
            tags$th("FFPW", style = "text-align:center; width = 20%;"),
            tags$th("Czech", style = "text-align:center; width = 20%;"),
            tags$th("Non-Czech", style = "text-align:center; width = 20%;")
          )
        ),
        tags$tbody(
          lapply(1:n, function(i) {
            tags$tr(
              tags$td(authors[i], style = "text-align:left;"),
              tags$td(checkboxInput(paste0("ffpw_", i), 
                                    label = NULL, value = TRUE),
                      style = "text-align:center;"),
              tags$td(checkboxInput(paste0("czech_", i), 
                                    label = NULL, value = FALSE),
                      style = "text-align:center;"),
              tags$td(checkboxInput(paste0("non_czech_", i), 
                                    label = NULL, value = FALSE),
                      style = "text-align:center;")
            )
          })
        )
      )
    )
    
    return(matrix_ui)
  })
  
  
  
  
  # Observe the "Confirm Selections" button and generate the final table
  final_data <- eventReactive(input$submit, {
    req(input$n_authors)
    authors <- author_names()
    n <- length(authors)
    
    # Collect checkbox inputs dynamically
    ffpw <- sapply(1:n, function(i) input[[paste0("ffpw_", i)]])
    czech <- sapply(1:n, function(i) input[[paste0("czech_", i)]])
    non_czech <- sapply(1:n, function(i) input[[paste0("non_czech_", i)]])
    
    # Create a tibble with the data
    tibble(
      Author = authors,
      FFPW = ffpw,
      Czech = czech,
      `Non-Czech` = non_czech
    )
  })
  
  # Render the final table in the main panel
  output$final_author_table <- renderTable({
    final_data()
  })
}

shinyApp(ui = ui, server = server)
