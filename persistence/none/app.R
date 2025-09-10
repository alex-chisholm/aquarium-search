library(shiny)
library(DT)
library(dplyr)
library(stringr)

# Read the aquarium data
aquarium_data <- read.csv("aquarium.csv", stringsAsFactors = FALSE)

# Select 8 diverse and appealing animals for the landing page
featured_animals <- aquarium_data[aquarium_data$name %in% c(
  "Sea Otter", 
  "Beluga Whale", 
  "Penguin", 
  "Seahorse",
  "Sea Turtle",
  "Octopus",
  "Jellyfish",
  "Shark"
), ]

# If those specific animals aren't available, pick the first 8
if (nrow(featured_animals) < 8) {
  featured_animals <- head(aquarium_data, 8)
}

# Define UI
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
  
  # Landing page with featured animals
  conditionalPanel(
    condition = "input.search_query == ''",
    div(class = "landing-container",
      h2("Some of Our Favorites", class = "favorites-header"),
      uiOutput("featured_animals")
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
  
  # Function to save rating to CSV
  save_rating <- function(animal_name, rating, session_id) {
    rating_data <- data.frame(
      timestamp = Sys.time(),
      session_id = session_id,
      animal_name = animal_name,
      rating = rating,
      stringsAsFactors = FALSE
    )
    
    # Check if file exists, if not create with headers
    if (!file.exists("animal_ratings.csv")) {
      write.csv(rating_data, "animal_ratings.csv", row.names = FALSE)
    } else {
      write.table(rating_data, "animal_ratings.csv", 
                  sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
    }
  }
  
  # Clean up text fields helper function
  clean_text <- function(text) {
    if (is.na(text) || text == "") return("")
    # Remove extra quotes and clean up formatting
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
          div(class = "thank-you-message", "✨ Thanks for sharing!")
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