server <- function(input, output) {
  
  # DATA IMPORT----
  
  ## Load data from app directory----
  journal_data <- reactive({
    tryCatch({
      journal_data <- read_excel("JCI_2024.xlsx") %>%
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
          riv_points <- 10 + 140 * factor # neither AIS nor IF
          
        } else if (selected_journal$Article.Influence == 0 & selected_journal$Impact.Factor != 0) {
          riv_points <- 10 + 190 * factor # no AIS but with IF
          
        } else {
          riv_points <- 10 + 290 * factor # both AIS and IF
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
  
  author_weights <- eventReactive(input$submit, {
    req(author_data())
    
    ## Step 1----
    # Assigning weights according to authorship
    tryCatch({
      author_weights <- author_data() %>% 
        mutate(
          id = str_extract(authors, "[0-9]+"),
          authors = str_remove(authors, " [0-9]+"),
          
          ### 1.1----
          # assign all authors a weight of 1
          weight = 1,
          
          ### 1.2----
          # add a weight of 1/n (n = number of first authors) for all first authors
          weight = if_else(first_author == FALSE, 
                           weight, 
                           weight + 1/sum(first_author)),
          
          ### 1.3----
          # add a weight of 0.5 to the last author
          weight = if_else(row_number() != n(), 
                           weight, 
                           weight + 0.5),
          
          ### 2.1----
          weight_corr = weight,
          
          # if the author is affiliated with FFPW and another Czech 
          # institution, then the hypothetical weights are being divided by 2
          weight_corr = if_else(ffpw == TRUE & czech == TRUE & non_czech == FALSE, 
                                weight * (1/2), 
                                weight_corr),
          
          
          weight_corr = if_else(ffpw == FALSE & czech == TRUE & non_czech == FALSE, 
                                weight * 0, 
                                weight_corr),
          
          weight_corr = if_else(ffpw == TRUE & czech == FALSE & non_czech == TRUE, 
                                weight * (1/2), 
                                weight_corr),
          
          weight_corr = if_else(ffpw == FALSE & czech == TRUE & non_czech == TRUE, 
                                weight * 0, 
                                weight_corr),
          
          # the weights determined by following steps 1-3 is multiplied by 0.5 
          # if the author is exclusively affiliated with an institution 
          # outside of the Czech Republic
          weight_corr = if_else(ffpw == FALSE & czech == FALSE & non_czech == TRUE,
                           weight * (1/2), 
                           weight_corr)
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
      riv_points_per_author <- author_weights() %>%
        
        # calculate RIV points per author
        mutate(
          sum_weights = sum(weight),
          riv_per_weight = riv_points() / sum_weights,
          points_per_author_max = weight * riv_per_weight,
          points_per_author = weight_corr * riv_per_weight
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
          Total = sum(points_per_author_max),
          FFPW = sum(points_per_author)
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
        mutate(authors = ifelse(!is.na(id), paste(authors, id), authors)) %>% 
        select(authors, weight, weight_corr, points_per_author_max, points_per_author) %>%
        rename(
          `Author` = "authors",
          `Author weight: Total` = "weight",
          `Author weight: FFPW` = "weight_corr",
          `RIV points: Author` = "points_per_author_max",
          `RIV points: FFPW` = "points_per_author"
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
