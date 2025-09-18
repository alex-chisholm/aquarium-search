library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(DBI)
library(RPostgres)

# PostgreSQL configuration
# You can set these as environment variables for security
PG_HOST <- Sys.getenv("PG_HOST")
PG_PORT <- as.integer(Sys.getenv("PG_PORT"))
PG_DBNAME <- Sys.getenv("PG_DBNAME")
PG_USER <- Sys.getenv("PG_USER")
PG_PASSWORD <- Sys.getenv("PG_PASSWORD")

# Function to get PostgreSQL connection
get_pg_connection <- function() {
  tryCatch({
    dbConnect(
      RPostgres::Postgres(),
      host = PG_HOST,
      port = PG_PORT,
      dbname = PG_DBNAME,
      user = PG_USER,
      password = PG_PASSWORD
    )
  }, error = function(e) {
    cat("Error connecting to PostgreSQL:", e$message, "\n")
    return(NULL)
  })
}

# Function to initialize database table
initialize_ratings_table <- function() {
  con <- get_pg_connection()
  if (is.null(con)) return(FALSE)
  
  tryCatch({
    # Create ratings table if it doesn't exist
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS ratings (
        id SERIAL PRIMARY KEY,
        timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        session_id VARCHAR(255),
        animal_name VARCHAR(255),
        rating VARCHAR(50)
      )
    ")
    
    # Create index for better performance
    dbExecute(con, "
      CREATE INDEX IF NOT EXISTS idx_ratings_animal_rating 
      ON ratings(animal_name, rating)
    ")
    
    dbDisconnect(con)
    return(TRUE)
  }, error = function(e) {
    cat("Error initializing table:", e$message, "\n")
    if (!is.null(con)) dbDisconnect(con)
    return(FALSE)
  })
}

# Function to save rating to PostgreSQL
save_rating_to_postgres <- function(animal_name, rating, session_id) {
  con <- get_pg_connection()
  if (is.null(con)) return(FALSE)
  
  tryCatch({
    cat("Saving rating to PostgreSQL...\n")
    
    # Insert new rating
    dbExecute(con, "
      INSERT INTO ratings (session_id, animal_name, rating) 
      VALUES ($1, $2, $3)
    ", params = list(session_id, animal_name, rating))
    
    dbDisconnect(con)
    cat("Successfully saved rating to PostgreSQL\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("Error saving to PostgreSQL:", e$message, "\n")
    if (!is.null(con)) dbDisconnect(con)
    return(FALSE)
  })
}

# Initialize the database table on startup
if (!initialize_ratings_table()) {
  cat("Warning: Could not initialize PostgreSQL table. Check your database connection.\n")
}

# Read the aquarium data
aquarium_data <- read.csv("aquarium.csv", stringsAsFactors = FALSE)

# Select 8 diverse and appealing animals for the landing page
featured_animals <- aquarium_data[aquarium_data$name %in% c(
  "Sea Otter", "Beluga Whale", "Penguin", "Seahorse",
  "Sea Turtle", "Octopus", "Jellyfish", "Shark"
), ]

if (nrow(featured_animals) < 8) {
  featured_animals <- head(aquarium_data, 8)
}

# Define UI (unchanged from original)
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  div(class = "search-container",
    div(class = "logo-container",
      img(src = "conf2025.png", class = "logo", alt = "Conference 2025")
    ),
    h1("Georgia Aquarium Search", class = "search-title"),
    textInput("search_query", 
              label = NULL,
              placeholder = "Search for animals, habitats, diets, or characteristics...",
              width = "100%",
              value = "")
  ),
  
  # Landing page with featured animals and ratings summary
  conditionalPanel(
    condition = "input.search_query == ''",
    div(class = "landing-container",
      h2("Some of Our Favorites", class = "favorites-header"),
      uiOutput("featured_animals"),
      
      # Ratings summary section
      div(class = "ratings-summary-container",
        h2("Visitor Favorites", class = "ratings-header"),
        uiOutput("ratings_summary")
      )
    )
  ),
  
  # Search results
  div(class = "results-container",
    conditionalPanel(
      condition = "input.search_query != ''",
      div(id = "results-area",
        div(class = "result-count-container",
          div(class = "result-count", textOutput("result_count")),
          a(class = "clear-search-link", 
            href = "#",
            onclick = "Shiny.setInputValue('clear_search', Math.random(), {priority: 'event'}); return false;",
            "Clear search")
        ),
        uiOutput("search_results")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Generate unique session ID
  session_id <- paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", 
                       sample(1000:9999, 1))
  
  # Reactive values to track which animals have been rated
  rated_animals <- reactiveValues()
  
  # Reactive trigger for ratings updates
  ratings_trigger <- reactiveVal(0)
  
  # Function to read ratings summary from PostgreSQL
  get_ratings_summary <- reactive({
    # Depend on the trigger to refresh when ratings change
    ratings_trigger()
    
    con <- get_pg_connection()
    if (is.null(con)) return(NULL)
    
    tryCatch({
      cat("Reading ratings summary from PostgreSQL...\n")
      
      # Check if table exists and has data
      table_exists <- dbExistsTable(con, "ratings")
      if (!table_exists) {
        dbDisconnect(con)
        cat("No ratings table found\n")
        return(NULL)
      }
      
      # Check if table has data
      count_result <- dbGetQuery(con, "SELECT COUNT(*) as count FROM ratings")
      if (count_result$count == 0) {
        dbDisconnect(con)
        cat("No ratings data found\n")
        return(NULL)
      }
      
      # Get love summary
      love_summary <- dbGetQuery(con, "
        SELECT animal_name, COUNT(*) as n 
        FROM ratings 
        WHERE rating = 'Literally in love' 
        GROUP BY animal_name 
        ORDER BY n DESC 
        LIMIT 10
      ")
      
      # Get nope summary
      nope_summary <- dbGetQuery(con, "
        SELECT animal_name, COUNT(*) as n 
        FROM ratings 
        WHERE rating = 'Not my type' 
        GROUP BY animal_name 
        ORDER BY n DESC 
        LIMIT 10
      ")
      
      dbDisconnect(con)
      
      cat("Love summary rows:", nrow(love_summary), "\n")
      cat("Nope summary rows:", nrow(nope_summary), "\n")
      
      return(list(love = love_summary, nope = nope_summary))
      
    }, error = function(e) {
      cat("Error reading from PostgreSQL:", e$message, "\n")
      if (!is.null(con)) dbDisconnect(con)
      return(NULL)
    })
  })
  
  # Function to save rating
  save_rating <- function(animal_name, rating, session_id) {
    cat("Attempting to save rating:", animal_name, "-", rating, "\n")
    
    # Save to PostgreSQL
    pg_success <- save_rating_to_postgres(animal_name, rating, session_id)
    
    if (pg_success) {
      cat("Successfully saved rating to PostgreSQL\n")
    } else {
      # Fall back to local CSV
      rating_data <- data.frame(
        timestamp = as.character(Sys.time()),
        session_id = session_id,
        animal_name = animal_name,
        rating = rating,
        stringsAsFactors = FALSE
      )
      
      if (!file.exists("ratings_backup.csv")) {
        write.csv(rating_data, "ratings_backup.csv", row.names = FALSE)
      } else {
        write.table(rating_data, "ratings_backup.csv", 
                    sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
      }
      cat("Saved to backup CSV\n")
    }
  }
  
  # Clean up text fields helper function
  clean_text <- function(text) {
    if (is.na(text) || text == "") return("")
    text <- gsub('^"|"$', '', text)
    text <- gsub('""', '"', text)
    return(text)
  }
  
  # Handle clicks on featured animal cards
  observeEvent(input$featured_animal_click, {
    animal_name <- input$featured_animal_click
    updateTextInput(session, "search_query", value = animal_name)
  })
  
  # Handle clear search clicks
  observeEvent(input$clear_search, {
    updateTextInput(session, "search_query", value = "")
  })
  
  # Handle rating clicks
  observe({
    # Get all inputs that start with "love_" or "nope_"
    love_inputs <- names(input)[grepl("^love_", names(input))]
    nope_inputs <- names(input)[grepl("^nope_", names(input))]
    
    # Handle love ratings
    for (input_name in love_inputs) {
      if (!is.null(input[[input_name]]) && input[[input_name]] > 0) {
        animal_name <- gsub("^love_", "", input_name)
        animal_name <- gsub("_", " ", animal_name)
        
        # Only process if not already rated
        if (is.null(rated_animals[[animal_name]])) {
          save_rating(animal_name, "Literally in love", session_id)
          rated_animals[[animal_name]] <- "love"
          
          # Trigger ratings summary refresh
          ratings_trigger(ratings_trigger() + 1)
          
          # Show feedback
          showNotification(
            paste("💕 Recorded your love for", animal_name, "!"),
            type = "message",
            duration = 2
          )
        }
      }
    }
    
    # Handle nope ratings
    for (input_name in nope_inputs) {
      if (!is.null(input[[input_name]]) && input[[input_name]] > 0) {
        animal_name <- gsub("^nope_", "", input_name)
        animal_name <- gsub("_", " ", animal_name)
        
        # Only process if not already rated
        if (is.null(rated_animals[[animal_name]])) {
          save_rating(animal_name, "Not my type", session_id)
          rated_animals[[animal_name]] <- "nope"
          
          # Trigger ratings summary refresh
          ratings_trigger(ratings_trigger() + 1)
          
          # Show feedback
          showNotification(
            paste("👎 Noted that", animal_name, "is not your type"),
            type = "message",
            duration = 2
          )
        }
      }
    }
  })
  
  # Featured animals for landing page
  output$featured_animals <- renderUI({
    featured_cards <- lapply(1:nrow(featured_animals), function(i) {
      animal <- featured_animals[i, ]
      
      div(class = "featured-card",
          onclick = paste0("Shiny.setInputValue('featured_animal_click', '", animal$name, "', {priority: 'event'});"),
          
          # Animal image (placeholder if not available)
          if (!is.na(animal$image_url) && animal$image_url != "") {
            img(src = animal$image_url, class = "featured-image", 
                onerror = "this.src='data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMjAwIiBoZWlnaHQ9IjE0MCIgdmlld0JveD0iMCAwIDIwMCAxNDAiIGZpbGw9Im5vbmUiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+CjxyZWN0IHdpZHRoPSIyMDAiIGhlaWdodD0iMTQwIiBmaWxsPSIjRjJGMkYyIi8+CjxjaXJjbGUgY3g9IjEwMCIgY3k9IjcwIiByPSIzMCIgZmlsbD0iIzQxOTU5OSIvPgo8L3N2Zz4='")
          } else {
            div(class = "featured-image", 
                style = "background: linear-gradient(135deg, #447099 0%, #419599 100%); display: flex; align-items: center; justify-content: center; color: white; font-size: 24px;",
                "🐠")
          },
          
          div(class = "featured-name", animal$name)
      )
    })
    
    div(class = "favorites-grid", featured_cards)
  })
  
  # Ratings summary output
  output$ratings_summary <- renderUI({
    summary_data <- get_ratings_summary()
    
    if (is.null(summary_data)) {
      return(div(class = "no-ratings", "No ratings yet"))
    }
    
    # Create love and nope lists
    love_list <- if (nrow(summary_data$love) > 0) {
      div(class = "rating-column love-column",
        h3("💕 Most Loved", class = "rating-column-header"),
        div(class = "rating-list",
          lapply(1:nrow(summary_data$love), function(i) {
            animal <- summary_data$love[i, ]
            div(class = "rating-item",
              span(class = "rank", paste0(i, ".")),
              span(class = "animal-name-summary", 
                   onclick = paste0("Shiny.setInputValue('featured_animal_click', '", animal$animal_name, "', {priority: 'event'});"),
                   animal$animal_name),
              span(class = "vote-count", paste0("(", animal$n, " votes)"))
            )
          })
        )
      )
    } else {
      div(class = "rating-column love-column",
        h3("💕 Most Loved", class = "rating-column-header"),
        div(class = "no-votes", "No votes yet")
      )
    }
    
    nope_list <- if (nrow(summary_data$nope) > 0) {
      div(class = "rating-column nope-column",
        h3("👎 Not Their Type", class = "rating-column-header"),
        div(class = "rating-list",
          lapply(1:nrow(summary_data$nope), function(i) {
            animal <- summary_data$nope[i, ]
            div(class = "rating-item",
              span(class = "rank", paste0(i, ".")),
              span(class = "animal-name-summary",
                   onclick = paste0("Shiny.setInputValue('featured_animal_click', '", animal$animal_name, "', {priority: 'event'});"),
                   animal$animal_name),
              span(class = "vote-count", paste0("(", animal$n, " votes)"))
            )
          })
        )
      )
    } else {
      div(class = "rating-column nope-column",
        h3("👎 Not Their Type", class = "rating-column-header"),
        div(class = "no-votes", "No votes yet")
      )
    }
    
    div(class = "ratings-summary-grid", love_list, nope_list)
  })
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    query <- input$search_query
    
    if (is.null(query) || query == "") {
      return(data.frame())
    }
    
    # Convert query to lowercase for case-insensitive search
    query_lower <- tolower(query)
    
    # Search across multiple columns
    matches <- aquarium_data[
      grepl(query_lower, tolower(aquarium_data$name), fixed = TRUE) |
      grepl(query_lower, tolower(aquarium_data$scientific_name), fixed = TRUE) |
      grepl(query_lower, tolower(aquarium_data$diet), fixed = TRUE) |
      grepl(query_lower, tolower(aquarium_data$habitat), fixed = TRUE) |
      grepl(query_lower, tolower(aquarium_data$range), fixed = TRUE) |
      grepl(query_lower, tolower(aquarium_data$physical_characteristics), fixed = TRUE) |
      grepl(query_lower, tolower(aquarium_data$fun_fact), fixed = TRUE) |
      grepl(query_lower, tolower(aquarium_data$conservation_status), fixed = TRUE),
    ]
    
    return(matches)
  })
  
  # Output result count
  output$result_count <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0 && input$search_query != "") {
      return("No results found")
    } else if (nrow(data) == 1) {
      return("1 result")
    } else if (nrow(data) > 1) {
      return(paste(nrow(data), "results"))
    }
    return("")
  })
  
  # Output search results
  output$search_results <- renderUI({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      if (input$search_query != "") {
        return(div(class = "no-results", 
                  "No animals found matching your search. Try different keywords like 'shark', 'coral', 'endangered', or 'Pacific'."))
      }
      return(NULL)
    }
    
    # Create cards for each animal
    animal_cards <- lapply(1:nrow(data), function(i) {
      animal <- data[i, ]
      
      # Create unique button IDs (replace spaces with underscores)
      animal_id <- gsub(" ", "_", animal$name)
      
      # Determine conservation status color class
      conservation_class <- "conservation-stable"
      if (!is.na(animal$conservation_status)) {
        if (grepl("Endangered|Critically", animal$conservation_status, ignore.case = TRUE)) {
          conservation_class <- "conservation-endangered"
        } else if (grepl("Vulnerable|Near Threatened", animal$conservation_status, ignore.case = TRUE)) {
          conservation_class <- "conservation-vulnerable"
        }
      }
      
      # Check if this animal has been rated
      is_rated <- !is.null(rated_animals[[animal$name]])
      
      div(class = "animal-card",
        # Animal image (if available)
        if (!is.na(animal$image_url) && animal$image_url != "") {
          img(src = animal$image_url, class = "animal-image", 
              onerror = "this.style.display='none'")
        },
        
        # Animal name and scientific name
        div(class = "animal-name", 
            onclick = paste0("window.open('", animal$url, "', '_blank')"),
            animal$name,
            span(class = "external-link-icon", "↗")
        ),
        div(class = "scientific-name", animal$scientific_name),
        
        # Animal details
        div(class = "animal-details",
          if (!is.na(animal$size) && animal$size != "") {
            p(span(class = "detail-label", "Size: "), clean_text(animal$size))
          },
          if (!is.na(animal$diet) && animal$diet != "") {
            p(span(class = "detail-label", "Diet: "), clean_text(animal$diet))
          },
          if (!is.na(animal$habitat) && animal$habitat != "") {
            p(span(class = "detail-label", "Habitat: "), clean_text(animal$habitat))
          },
          if (!is.na(animal$range) && animal$range != "") {
            p(span(class = "detail-label", "Range: "), clean_text(animal$range))
          },
          if (!is.na(animal$conservation_status) && animal$conservation_status != "") {
            p(span(class = "detail-label", "Conservation Status: "), 
              span(class = conservation_class, clean_text(animal$conservation_status)))
          }
        ),
        
        # Fun fact
        if (!is.na(animal$fun_fact) && animal$fun_fact != "") {
          div(class = "fun-fact",
              strong("Fun Fact: "), clean_text(animal$fun_fact))
        },
        
        # Rating buttons or thank you message
        if (is_rated) {
          # Create personalized thank you message based on rating
          rating_type <- rated_animals[[animal$name]]
          if (rating_type == "love") {
            div(class = "thank-you-message love-message", "💕 Literally in love - Thanks for sharing!")
          } else {
            div(class = "thank-you-message nope-message", "👎 Not my type - Thanks for sharing!")
          }
        } else {
          div(class = "rating-buttons",
            actionButton(
              inputId = paste0("love_", animal_id),
              label = "💕 Literally in love",
              class = "love-button"
            ),
            actionButton(
              inputId = paste0("nope_", animal_id),
              label = "👎 Not my type", 
              class = "nope-button"
            )
          )
        }
      )
    })
    
    return(animal_cards)
  })
}

# Run the application
shinyApp(ui = ui, server = server)