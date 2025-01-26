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
  
  
  
  
  
  # UNIQUE JOURNALS TABLE----
  #
  # This is the table from where the user can select a journal to get further
  # information about the possible number of RIV points.
  
  ## Create unique journal names table----
  unique_journals <- reactive({
    tryCatch({
      req(journal_data())
      
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
        options = list(pageLength = 15, 
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
    tryCatch({
      req(journal_data())
      
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
    tryCatch({
      req(input$unique_journals_rows_selected,
          unique_journals(),
          journal_ranking())
      
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
    tryCatch({
      req(selected_journal())
      
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
    tryCatch({
      req(mean_factor(), 
          selected_journal())
      
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
  author_weights <- reactive({
    tryCatch({
      n_authors <- input$n_authors
      n_authors_foreign <- min(input$n_authors_foreign, n_authors)  # Ensure foreign authors ≤ total authors
      firstauthor_foreign <- input$firstauthor_foreign
      lastauthor_foreign <- input$lastauthor_foreign
      
      
      # Create author vector
      if (n_authors == 1) {
        authors <- "First author"
      } else if (n_authors == 2) {
        authors <- c("First author", "Last author")
      } else if (n_authors > 2) {
        authors <- c("First author", paste("Co-author", seq(1, n_authors - 2)), "Last author")
      }
      
      print("Authors vector created:")
      print(authors)
      
      
      # Create weights vector
      weights <- rep(1, n_authors)
      if (n_authors > 1) {
        weights[1] <- weights[1] * 2      # First author gets 2x weight
        weights[length(weights)] <- weights[length(weights)] * 1.5  # Last author gets 1.5x weight
      }
      
      
      print("Weights vector created:")
      print(weights)
      
      
      # Create foreign vector
      foreign <- rep(FALSE, n_authors)
      
      # Assign foreign authors
      if (n_authors_foreign > 0) {
        # Assign first author if applicable
        if (firstauthor_foreign) {
          foreign[1] <- TRUE
          n_authors_foreign <- n_authors_foreign - 1
        }
        
        # Assign last author if applicable
        if (lastauthor_foreign && n_authors_foreign > 0) {
          foreign[n_authors] <- TRUE
          n_authors_foreign <- n_authors_foreign - 1
        }
        
        # Assign remaining foreign authors to the middle
        if (n_authors_foreign > 0) {
          foreign[2:(1 + n_authors_foreign)] <- TRUE
        }
      }
      
      print("Foreign vector created:")
      print(foreign)
      
      
      # Update weights for foreign authors
      weights <- weights * ifelse(foreign, 0.5, 1)
      
      
      # Create author_weights tibble
      author_weights <- tibble(authors, weights, foreign)
      
      print("Author weights determined successfully!")
      
      
      print("Output: author_weights")
      print(author_weights)
      
      
      return(author_weights)
      
      
    }, error = function(e) {
      print(paste("Error creating author weights dataset:", e$message))
      return(NULL)
    })
  })
  
  
  
  
  
  
  # RIV POINTS PER AUTHOR----
  
  ## Calculate RIV points per author----
  riv_points_per_author <- reactive({
    tryCatch({
      req(author_weights(), 
          riv_points())
      
      author_weights <- author_weights()
      riv_points <- riv_points()
      
      riv_points_per_author <- author_weights %>%
        mutate(
          sum = sum(weights),
          prop = weights / sum,
          points_per_author = riv_points * prop
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
    tryCatch({
      
      req(riv_points_per_author())
      
      riv_overview <- riv_points_per_author() %>%
        mutate(foreign = ifelse(foreign == TRUE, "external", "FFPW")) %>%
        group_by(foreign) %>%
        summarise(riv_points = sum(points_per_author)) %>%
        rename(
          `Affiliation` = "foreign", 
          `Resulting RIV points` = "riv_points")
      
      
      print("RIV point overview table rendered successfully!")
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
    tryCatch({
      req(riv_points_per_author())
      
      riv_points_per_author_table <- riv_points_per_author() %>%
        select(-sum, -prop, -foreign) %>%
        rename(
          `Author` = "authors",
          `Resulting author weight` = "weights",
          `Resulting RIV points` = "points_per_author"
        )
      
      
      print("RIV points per author table rendered successfully!")
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
