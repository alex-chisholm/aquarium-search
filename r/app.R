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
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #F2F2F2;
      }
      .search-container {
        max-width: 800px;
        margin: 30px auto;
        text-align: center;
      }
      .logo-container {
        text-align: center;
        margin-bottom: 20px;
      }
      .logo {
        max-height: 80px;
        width: auto;
      }
      .search-title {
        font-size: 48px;
        color: #447099;
        font-weight: 300;
        margin-bottom: 30px;
      }
      .search-box {
        width: 100%;
        padding: 15px 20px;
        font-size: 16px;
        border: 2px solid #419599;
        border-radius: 24px;
        outline: none;
        box-shadow: 0 2px 5px 1px rgba(68,112,153,.16);
        transition: box-shadow 0.3s ease, border-color 0.3s ease;
        background-color: white;
      }
      .search-box:focus {
        box-shadow: 0 2px 8px 1px rgba(68,112,153,.24);
        border-color: #447099;
      }
      .results-container {
        max-width: 1000px;
        margin: 20px auto;
        padding: 0 20px;
      }
      .landing-container {
        max-width: 1400px;
        margin: 40px auto;
        padding: 0 20px;
      }
      .favorites-header {
        text-align: center;
        font-size: 28px;
        color: #404041;
        font-weight: 400;
        margin-bottom: 30px;
      }
      .favorites-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        gap: 16px;
        margin-bottom: 40px;
      }
      .featured-card {
        background: white;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(64,64,65,0.1);
        padding: 16px;
        text-align: center;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
        cursor: pointer;
        aspect-ratio: 1;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        border: 1px solid rgba(65,149,153,0.2);
      }
      .featured-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 8px 25px rgba(68,112,153,0.15);
        border-color: #419599;
      }
      .featured-image {
        width: 100%;
        height: 140px;
        object-fit: cover;
        border-radius: 8px;
        margin-bottom: 12px;
        flex-grow: 1;
      }
      .featured-name {
        font-size: 16px;
        color: #447099;
        font-weight: 500;
        margin: 0;
        padding: 8px 0;
        line-height: 1.2;
      }
      .animal-card {
        background: white;
        border-radius: 8px;
        box-shadow: 0 1px 3px rgba(64,64,65,0.12);
        margin-bottom: 20px;
        padding: 20px;
        transition: box-shadow 0.3s ease;
        border-left: 4px solid #419599;
      }
      .animal-card:hover {
        box-shadow: 0 4px 8px rgba(64,64,65,0.16);
      }
      .animal-name {
        font-size: 24px;
        color: #447099;
        font-weight: 500;
        margin-bottom: 5px;
        cursor: pointer;
      }
      .animal-name:hover {
        text-decoration: underline;
        color: #EE6331;
      }
      .scientific-name {
        font-style: italic;
        color: #72994E;
        margin-bottom: 10px;
      }
      .animal-image {
        float: right;
        margin-left: 20px;
        margin-bottom: 10px;
        border-radius: 8px;
        max-width: 150px;
        height: auto;
      }
      .animal-details {
        color: #404041;
        line-height: 1.6;
      }
      .detail-label {
        font-weight: 600;
        color: #404041;
      }
      .fun-fact {
        background-color: #F2F2F2;
        border-left: 4px solid #EE6331;
        padding: 10px 15px;
        margin-top: 15px;
        border-radius: 0 4px 4px 0;
        font-style: italic;
        color: #404041;
      }
      .no-results {
        text-align: center;
        color: #72994E;
        font-size: 18px;
        margin-top: 50px;
      }
      .result-count-container {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 20px;
      }
      .result-count {
        color: #72994E;
        font-size: 14px;
      }
      .clear-search-link {
        color: #447099;
        font-size: 14px;
        text-decoration: none;
        cursor: pointer;
        padding: 4px 8px;
        border-radius: 4px;
        transition: background-color 0.2s ease, color 0.2s ease;
      }
      .clear-search-link:hover {
        background-color: rgba(68,112,153,0.1);
        text-decoration: underline;
        color: #EE6331;
      }
      .conservation-endangered {
        color: #D44000;
      }
      .conservation-vulnerable {
        color: #E7B10A;
      }
      .conservation-stable {
        color: #72994E;
      }
    "))
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
      
      # Determine conservation status color class
      conservation_class <- "conservation-stable"
      if (!is.na(animal$conservation_status)) {
        if (grepl("Endangered|Critically", animal$conservation_status, ignore.case = TRUE)) {
          conservation_class <- "conservation-endangered"
        } else if (grepl("Vulnerable|Near Threatened", animal$conservation_status, ignore.case = TRUE)) {
          conservation_class <- "conservation-vulnerable"
        }
      }
      
      div(class = "animal-card",
        # Animal image (if available)
        if (!is.na(animal$image_url) && animal$image_url != "") {
          img(src = animal$image_url, class = "animal-image", 
              onerror = "this.style.display='none'")
        },
        
        # Animal name and scientific name
        div(class = "animal-name", 
            onclick = paste0("window.open('", animal$url, "', '_blank')"),
            animal$name),
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
        }
      )
    })
    
    return(animal_cards)
  })
}

# Run the application
shinyApp(ui = ui, server = server)