server <- function(input, output) {
  
  # IMPORT DATA ----
  
  ## Load data from app directory
  journal_data <- reactive({
    tryCatch({
      req(input$dataset)
      dataset <- input$dataset
      path_to_dataset <- paste0("data/", dataset, ".xlsx")
      
      journal_data <- read_journal_data(path_to_dataset)
      
      print("Journal data loaded successfully!")
      print(head(journal_data))
      
      return(journal_data)
      
    }, error = function(e) {
      print(paste("Error loading data:", e$message))
      return(NULL)
    })
  })
  
  
  
  
  # UI----
  
  ## Create checkbox matrix----
  
  author_names <- reactive({
    req(input$n_authors)
    n <- input$n_authors
    
    c(paste("Author", seq(1, n)))[1:n]
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
            tags$th("", style = "text-align:left; width = 40%;"),
            tags$th("First author", style = "text-align:center; width = 15%;"),
            tags$th("FFPW affil.", style = "text-align:center; width = 15%;"),
            tags$th("Czech affil", style = "text-align:center; width = 15%;"),
            tags$th("Foreign affil.", style = "text-align:center; width = 15%;")
          )
        ),
        tags$tbody(
          lapply(1:n, function(i) {
            tags$tr(
              tags$td(authors[i], style = "text-align:left;"),
              tags$td(checkboxInput(paste0("first_", i), 
                                    label = NULL, 
                                    value = i == TRUE),
                      style = "text-align:center;"),
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
  
  
  
  ## Confirm selection----
  # Observe the "Confirm Selections" button and generate the final table
  author_data <- reactive({
    req(input$n_authors)
    authors <- author_names()
    n <- length(authors)
    
    # Collect checkbox inputs dynamically
    ffpw <- sapply(1:n, function(i) input[[paste0("ffpw_", i)]])
    czech <- sapply(1:n, function(i) input[[paste0("czech_", i)]])
    non_czech <- sapply(1:n, function(i) input[[paste0("non_czech_", i)]])
    first_author <- sapply(1:n, function(i) input[[paste0("first_", i)]])

    # Create a tibble with the data
    tibble(
      authors,
      ffpw,
      czech,
      non_czech,
      first_author
    )
  })
  
  # Render the final table in the main panel
  output$final_author_table <- renderTable({
    author_data() %>% 
      rename(
        Author = authors,
        FFPW = ffpw,
        Czech = czech,
        `Non-Czech` = non_czech,
        `First author`= first_author
      )
  })
  
  
  
  
  # UNIQUE JOURNALS----
  #
  # This is the table from where the user can select a journal to get further
  # information about the possible number of RIV points.
  
  ## Create dataset----
  unique_journals <- reactive({
    
    req(journal_data())
    
    tryCatch({
      
      journal_data <- journal_data()
      unique_journals <- find_unique_journals(journal_data)
      
      print("Dataset with unique journal names created successfully!")
      print(head(unique_journals))
      
      return(unique_journals)
      
    }, error = function(e) {
      print(paste("Error creating unique journals table:", e$message))
      return(NULL)
    })
  })
  
  
  
  ## Render table----
  output$unique_journals <- renderDT({
    tryCatch({
      unique_journals <- unique_journals() %>%
        rename(`Journal name` = "journal_name",
               `IF` = "Impact.Factor",
               `AIS` = "Article.Influence")
      
      unique_journals_table <- datatable(
        unique_journals,
        selection = list(
          mode = "single",
          target = "row",
          selected = 1
        ),
        filter = "none",
        options = list(pageLength = 5, 
                       autoWidth = TRUE)
      )
      
      print("Table with unique journal names rendered successfully!")
      
      return(unique_journals_table)
      
    }, error = function(e) {
      print(paste("Error rendering unique journals table:", e$message))
      NULL
    })
  })
  
  
  
  
  
  # JOURNAL RANKING----
  
  ## Create dataset----
  journal_ranking <- reactive({
    
    req(journal_data())
    
    tryCatch({
      
      journal_data <- journal_data()
      journal_ranking <- add_journal_ranking(journal_data)
      
      print("Journal ranking dataset created successfully!")
      print(head(journal_ranking))
      
      return(journal_ranking)
      
    }, error = function(e) {
      print(paste("Error creating journal ranking dataset:", e$message))
      return(NULL)
    })
  })
  
  
  
  ## Create filtered dataset----
  selected_journal <- reactive({
    
    req(input$unique_journals_rows_selected,
        unique_journals(),
        journal_ranking())
    
    tryCatch({
      selected_index <- input$unique_journals_rows_selected
      selected_name <- unique_journals()[selected_index, ][["journal_name"]]
      selected_journal <- journal_ranking() %>%
        filter(journal_name == selected_name)
      
      print("Journal ranking subset based on user selection created successfully!")
      print(head(selected_journal))
      
      return(selected_journal)
      
    }, error = function(e) {
      print(paste("Error creating journal ranking subset based on user selection:", e$message))
      return(NULL)
    })
  })
  
  
  
  # MEAN FACTOR----
  
  ## Calculate mean_factor
  mean_factor <- reactive({
    
    req(selected_journal())
    
    tryCatch({
      
      selected_journal <- selected_journal()
      mean_factor <- calculate_mean_factor(selected_journal)
      
      print("Mean factor calculated successfully!")
      print(mean_factor)
      
      return(mean_factor)
      
    }, error = function(e) {
      print(paste("Error calculating the mean factor:", e$message))
      return(NULL)
    })
  })
  
  
  
  
  
  
  
  # TOTAL RIV POINTS----
  
  ## Calculate RIV points based on result type and computed factors
  riv_points <- reactive({
    
    req(mean_factor(), 
        selected_journal())
    
    tryCatch({
      mean_factor <- mean_factor()
      selected_journal <- selected_journal() %>%
        distinct(Impact.Factor, Article.Influence)
      
      print("Selected journal subset created:")
      print(selected_journal)
      
      
      riv_points <- calculate_riv_points(mean_factor, selected_journal)
      
      print("RIV points calculated successfully!")
      print(riv_points)
      
      return(riv_points)
      
    }, error = function(e) {
      print(paste("Error in calculating the RIV points:", e$message))
      return(NULL)
    })
  })
  
  
  
  
  
  # AUTHOR WEIGHTS----
  
  author_weights <- eventReactive(input$submit, {
    req(author_data())
    

    tryCatch({
      author_weights <- author_data() %>% 
        
        mutate(
          foreign_only    = !ffpw & !czech & non_czech,
          czech_only      = !ffpw & czech & !non_czech,
          ffpw_czech      = ffpw & czech & !non_czech,
          ffpw_foreign    = ffpw & !czech & non_czech,
          czech_foreign   = !ffpw & czech & non_czech
        ) %>%
        mutate(
          weight =
            1 +
            if_else(first_author, 1 / sum(first_author), 0) +
            if_else(row_number() == n(), 0.5, 0),
          
          weight_corr = if_else(foreign_only, weight * 0.5, weight),
          
          weight_ffpw = case_when(
            ffpw_czech   ~ weight_corr * 0.5,
            ffpw_foreign ~ weight_corr * 0.5,
            czech_only   ~ 0,
            czech_foreign ~ 0,
            foreign_only ~ 0,
            TRUE ~ weight_corr
          )
        ) %>%
        select(-foreign_only, -czech_only, -ffpw_czech,
               -ffpw_foreign, -czech_foreign)
      
          

      
      
      print("Author weights determined successfully!")
      print(author_weights)
      
      return(author_weights)  
    }, error = function(e) {
      print(paste("Error determining author weights:", e$message))
      return(NULL)
    })
  })
  
  
  
  
  
  
  # RIV POINTS PER AUTHOR----
  
  ## Calculate RIV points per author----
  riv_points_per_author <- reactive({
    req(author_weights(), riv_points())
    
    tryCatch({
      riv_points_per_author <- author_weights() %>%
        
        # calculate RIV points per author
        mutate(
          sum_weights = sum(weight_corr),
          riv_per_weight = riv_points() / sum_weights,
          points_per_author_max = weight_corr * riv_per_weight,
          points_per_author_ffpw = weight_ffpw * riv_per_weight
        )
      
      
      print("RIV points per author calculated successfully!")
      
      print("Output: riv_points_per_author")
      print(riv_points_per_author)
      
      return(riv_points_per_author)
      
    }, error = function(e) {
      print(paste("Error in calculating RIV points per author:", e$message))
      return(NULL)
    })
  })
  
  
  
  ## Render riv_overview table----
  output$riv_overview <- renderTable({
    req(riv_points_per_author())

    tryCatch({
      riv_overview <- riv_points_per_author() %>%
        summarise(
          TOTAL = sum(points_per_author_max),
          FFPW = sum(points_per_author_ffpw)
          )

      print("riv_overview table rendered successfully!")
      print(riv_overview)

      return(riv_overview)

    }, error = function(e) {
      print(paste("Error in rendering riv_overview table:", e$message))
      return(NULL)
    })
  },
  width = "100%",
  digits = 2)
  
  
  
  ## Render riv_per_author table----
  output$riv_points_per_author <- renderTable({
    req(riv_points_per_author())
    
    tryCatch({
      riv_points_per_author_table <- riv_points_per_author() %>%
        #mutate(authors = ifelse(!is.na(id), paste(authors, id), authors)) %>% 
        select(authors, 
               weight, weight_corr, weight_ffpw, 
               points_per_author_max, points_per_author_ffpw) %>%
        rename(
          `Author` = "authors",
          `Base weight` = "weight",
          `Corr. weight` = "weight_corr",
          `FFPW weight` = "weight_ffpw",
          `Author RIV points` = "points_per_author_max",
          `FFPW RIV points` = "points_per_author_ffpw"
        )
      
      
      print("riv_points_per_author table rendered successfully!")
      print(riv_points_per_author_table)
      
      return(riv_points_per_author_table)
      
    }, error = function(e){
      print(paste("Error in rendering riv_points_per_author_table table:", e$message))
      return(NULL)
    })
  },
  width = "100%",
  digits = 2)
}
