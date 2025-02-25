server <- function(input, output) {
  
  # DATA IMPORT----
  
  ## Load data from app directory----
  journal_data <- reactive({
    tryCatch({
      journal_data <- read_excel("JCI_2023.xlsx") %>%
        as_tibble()
      
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
  
  # Reactive function to generate the author names
  author_names <- reactive({
    req(input$n_authors)
    n <- input$n_authors
    
    # Generate author names dynamically
    c("First author", "Last author", paste("Other author", seq(1, n - 2)))[1:n]
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
    
    # Create a tibble with the data
    tibble(
      authors,
      ffpw,
      czech,
      non_czech
    )
  })
  
  # Render the final table in the main panel
  output$final_author_table <- renderTable({
    author_data() %>% 
      rename(
        Author = authors,
        FFPW = ffpw,
        Czech = czech,
        `Non-Czech` = non_czech
      )
  })
  
  
  
  
  # UNIQUE JOURNALS TABLE----
  #
  # This is the table from where the user can select a journal to get further
  # information about the possible number of RIV points.
  
  ## Create unique journal names table----
  unique_journals <- reactive({
    
    req(journal_data())
    
    tryCatch({
      unique_journals <- journal_data() %>%
        distinct(journal_name, .keep_all = TRUE) %>%
        select(-year,
               -subject_area_name,
               -Faculty_Quartiles,
               -Ranking,
               -edition)
      
      print("Dataset with unique journal names created successfully!")
      print(head(unique_journals))
      
      return(unique_journals)
      
    }, error = function(e) {
      print(paste("Error creating unique journals table:", e$message))
      return(NULL)
    })
  })
  
  
  
  ## Render unique journals table----
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
  
  
  
  
  
  # JOURNAL RANKING TABLE----
  
  ## Create journal ranking dataset----
  journal_ranking <- reactive({
    
    req(journal_data())
    
    tryCatch({
      journal_ranking <- journal_data() %>%
        tidyr::separate_wider_delim(Ranking,
                                    names = c("rank", "out_of"),
                                    delim = " / ") %>%
        select(-Faculty_Quartiles, -edition) %>%
        mutate(rank = as.numeric(rank), out_of = as.numeric(out_of))
      
      print("Journal ranking dataset created successfully!")
      print(head(journal_ranking))
      
      return(journal_ranking)
      
    }, error = function(e) {
      print(paste("Error creating journal ranking dataset:", e$message))
      return(NULL)
    })
  })
  
  
  
  ## Create filtered dataset based on user selection----
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
  
  
  
  # MEAN FACTOR CALCULATION----
  
  ## Calculate mean_factor based on selected_journal----
  mean_factor <- reactive({
    
    req(selected_journal())
    
    tryCatch({
      selected_journal <- selected_journal()
      
      count <- nrow(selected_journal)
      
      if (count == 0)
        return(NA)
      
      total_N <- sum((selected_journal$rank - 1) / (selected_journal$out_of - 1))
      
      N <- total_N / count
      
      mean_factor <- (1 - N) / (1 + (N / 0.057))
      
      
      print("Mean factor calculated successfully!")
      print(mean_factor)
      
      return(mean_factor)
      
    }, error = function(e) {
      print(paste("Error calculating the mean factor:", e$message))
      return(NULL)
    })
  })
  
  
  
  
  
  
  
  # RIV POINT CALCULATION----
  
  ## Calculate RIV points based on result type and computed factors----
  riv_points <- reactive({
    
    req(mean_factor(), 
        selected_journal())
    
    tryCatch({
      factor <- mean_factor()
      selected_journal <- selected_journal() %>%
        distinct(Impact.Factor, Article.Influence)
      
      print("Selected journal subset created:")
      print(selected_journal)
      
      
      if (!is.na(factor)) {
        if (selected_journal$Article.Influence == 0 & selected_journal$Impact.Factor == 0) {
          riv_points <- 10 + 140 * factor
          
        } else if (selected_journal$Article.Influence == 0 & selected_journal$Impact.Factor != 0) {
          riv_points <- 10 + 190 * factor
          
        } else {
          riv_points <- 10 + 290 * factor
        }
      }
      
      print("RIV points calculated successfully!")
      print(riv_points)
      
      return(riv_points)
      
    }, error = function(e) {
      print(paste("Error in calculating the RIV points:", e$message))
      return(NULL)
    })
  })
  
  
  
  
  
  # AUTHOR WEIGHTS----
  
  ## Calculate author weights----
  author_weights <- eventReactive(input$submit, {
    req(author_data())
    
    tryCatch({
      weights <- tibble(
        authors = c("First author", "Last author", "Other author"),
        weight = c(2, 1.5, 1)
      )
      
      print("Weights tibble created successfully:")
      print(weights)
    }, error = function(e) {
      print(paste("Error creating weights tibble:", e$message))
      return(NULL)
    })

    tryCatch({
      author_weights <- author_data() %>% 
        mutate(authors = str_remove(authors, " [0-9]+")) %>% 
        left_join(weights, join_by(authors))
      
      print("author_weights created successfully:")
      print(author_weights)
    }, error = function(e) {
      print(paste("Error creating author weights dataset:", e$message))
      return(NULL)
    })
    
    
    tryCatch({
      author_weights <- author_weights %>% 
        mutate(
          total_weight = case_when(
            ffpw == TRUE | czech == TRUE ~ weight * 1,
            non_czech == TRUE ~ weight * 0.5
          ),
          ffpw_weight = case_when(
            ffpw == FALSE ~ 0,
            ffpw == TRUE & czech == FALSE & non_czech == FALSE ~ weight,
            ffpw == TRUE & czech == TRUE  & non_czech == FALSE ~ (weight + 1) / 2,
            ffpw == TRUE & czech == FALSE & non_czech == TRUE ~  (weight + 0.5) / 2,
            ffpw == TRUE & czech == TRUE  & non_czech == TRUE ~  (weight + 1 + 0.5) / 3
          )
        )
      
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
      author_weights <- author_weights()
      riv_points <- riv_points()
      
      riv_points_per_author <- author_weights %>%
        mutate(
          sum_total = sum(total_weight),
          prop_total = total_weight / sum_total,
          points_per_author = riv_points * prop_total,
          sum_ffpw = sum(ffpw_weight),
          prop_ffpw = ffpw_weight / sum_ffpw,
          points_ffpw = riv_points * prop_ffpw
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
  
  
  
  # Render riv_overview table----
  output$riv_overview <- renderTable({
    req(riv_points_per_author())

    tryCatch({

      riv_overview <- riv_points_per_author() %>%
        summarise(
          Total = sum(points_per_author),
          FFPW = sum(points_ffpw)
          )

      print("riv_overview table rendered successfully!")
      print(riv_overview)

      return(riv_overview)

    }, error = function(e) {
      print(paste("Error in rendering RIV overview table:", e$message))
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
        select(-c(ffpw, czech, non_czech, weight, sum_total, sum_ffpw, 
                  prop_total, prop_ffpw)) %>%
        rename(
          `Author` = "authors",
          `Resulting author weight` = "total_weight",
          `Resulting RIV points` = "points_per_author"
        )
      
      
      print("riv_points_per_author table rendered successfully!")
      print(riv_points_per_author_table)
      
      return(riv_points_per_author_table)
      
    }, error = function(e){
      print(paste("Error in rendering RIV overview table:", e$message))
      return(NULL)
    })
  },
  width = "100%",
  digits = 2)
}
