from shiny import App, ui, render, reactive
import pandas as pd
import re
from pathlib import Path


# Read the aquarium data
aquarium_data = pd.read_csv("aquarium.csv")

# Select 8 diverse and appealing animals for the landing page
featured_animal_names = [
    "Sea Otter", "Beluga Whale", "Penguin", "Seahorse",
    "Sea Turtle", "Octopus", "Jellyfish", "Shark"
]

featured_animals_df = aquarium_data[aquarium_data['name'].isin(featured_animal_names)]

# If those specific animals aren't available, pick the first 8
if len(featured_animals_df) < 8:
    featured_animals_df = aquarium_data.head(8)

# Define UI
app_ui = ui.page_fluid(
    ui.head_content(
        ui.tags.link(rel="stylesheet", type="text/css", href="styles.css")
    ),
    
    ui.div(
        ui.div(
            ui.img(src="conf2025.png", class_="logo", alt="Conference 2025"),
            class_="logo-container"
        ),
        ui.h1("Georgia Aquarium Search", class_="search-title"),
        ui.input_text(
            "search_query",
            label=None,
            placeholder="Search for animals, habitats, diets, or characteristics...",
            width="100%",
            value=""
        ),
        class_="search-container"
    ),
    
    # Landing page with featured animals
    ui.panel_conditional(
        "input.search_query == ''",
        ui.div(
            ui.h2("Some of Our Favorites", class_="favorites-header"),
            ui.output_ui("featured_animals"),
            class_="landing-container"
        )
    ),
    
    # Search results
    ui.div(
        ui.panel_conditional(
            "input.search_query != ''",
            ui.div(
                ui.div(
                    ui.div(ui.output_text("result_count"), class_="result-count"),
                    ui.tags.a(
                        "Clear search",
                        href="#",
                        onclick="Shiny.setInputValue('clear_search', Math.random(), {priority: 'event'}); return false;",
                        class_="clear-search-link"
                    ),
                    class_="result-count-container"
                ),
                ui.output_ui("search_results"),
                id="results-area"
            )
        ),
        class_="results-container"
    )
)

def server(input, output, session):
    
    def clean_text(text):
        """Clean up text fields helper function"""
        if pd.isna(text) or text == "":
            return ""
        # Remove extra quotes and clean up formatting
        text = re.sub(r'^"|"$', '', str(text))
        text = re.sub(r'""', '"', text)
        return text
    
    # Handle clicks on featured animal cards
    @reactive.effect
    def _():
        if input.featured_animal_click():
            animal_name = input.featured_animal_click()
            ui.update_text("search_query", value=animal_name)
    
    # Handle clear search clicks
    @reactive.effect
    def _():
        if input.clear_search():
            ui.update_text("search_query", value="")
    
    # Featured animals for landing page
    @render.ui
    def featured_animals():
        featured_cards = []
        
        for idx, animal in featured_animals_df.iterrows():
            # Handle image
            if pd.notna(animal.get('image_url')) and animal.get('image_url') != "":
                image_element = ui.img(
                    src=animal['image_url'],
                    class_="featured-image",
                    onerror="this.src='data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMjAwIiBoZWlnaHQ9IjE0MCIgdmlld0JveD0iMCAwIDIwMCAxNDAiIGZpbGw9Im5vbmUiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+CjxyZWN0IHdpZHRoPSIyMDAiIGhlaWdodD0iMTQwIiBmaWxsPSIjRjJGMkYyIi8+CjxjaXJjbGUgY3g9IjEwMCIgY3k9IjcwIiByPSIzMCIgZmlsbD0iIzQxOTU5OSIvPgo8L3N2Zz4='"
                )
            else:
                image_element = ui.div(
                    "🐠",
                    class_="featured-image",
                    style="background: linear-gradient(135deg, #447099 0%, #419599 100%); display: flex; align-items: center; justify-content: center; color: white; font-size: 24px;"
                )
            
            card = ui.div(
                image_element,
                ui.div(animal['name'], class_="featured-name"),
                class_="featured-card",
                onclick=f"Shiny.setInputValue('featured_animal_click', '{animal['name']}', {{priority: 'event'}});"
            )
            featured_cards.append(card)
        
        return ui.div(*featured_cards, class_="favorites-grid")
    
    # Reactive expression for filtered data
    @reactive.calc
    def filtered_data():
        query = input.search_query()
        
        if not query or query == "":
            return pd.DataFrame()
        
        # Convert query to lowercase for case-insensitive search
        query_lower = query.lower()
        
        # Search across multiple columns
        mask = (
            aquarium_data['name'].str.lower().str.contains(query_lower, na=False) |
            aquarium_data['scientific_name'].str.lower().str.contains(query_lower, na=False) |
            aquarium_data['diet'].str.lower().str.contains(query_lower, na=False) |
            aquarium_data['habitat'].str.lower().str.contains(query_lower, na=False) |
            aquarium_data['range'].str.lower().str.contains(query_lower, na=False) |
            aquarium_data['physical_characteristics'].str.lower().str.contains(query_lower, na=False) |
            aquarium_data['fun_fact'].str.lower().str.contains(query_lower, na=False) |
            aquarium_data['conservation_status'].str.lower().str.contains(query_lower, na=False)
        )
        
        return aquarium_data[mask]
    
    # Output result count
    @render.text
    def result_count():
        data = filtered_data()
        if len(data) == 0 and input.search_query() != "":
            return "No results found"
        elif len(data) == 1:
            return "1 result"
        elif len(data) > 1:
            return f"{len(data)} results"
        return ""
    
    # Output search results
    @render.ui
    def search_results():
        data = filtered_data()
        
        if len(data) == 0:
            if input.search_query() != "":
                return ui.div(
                    "No animals found matching your search. Try different keywords like 'shark', 'coral', 'endangered', or 'Pacific'.",
                    class_="no-results"
                )
            return None
        
        # Create cards for each animal
        animal_cards = []
        
        for idx, animal in data.iterrows():
            # Determine conservation status color class
            conservation_class = "conservation-stable"
            if pd.notna(animal.get('conservation_status')):
                status = str(animal['conservation_status']).lower()
                if re.search(r'endangered|critically', status):
                    conservation_class = "conservation-endangered"
                elif re.search(r'vulnerable|near threatened', status):
                    conservation_class = "conservation-vulnerable"
            
            # Build card elements
            card_elements = []
            
            # Animal image (if available)
            if pd.notna(animal.get('image_url')) and animal.get('image_url') != "":
                card_elements.append(
                    ui.img(
                        src=animal['image_url'],
                        class_="animal-image",
                        onerror="this.style.display='none'"
                    )
                )
            
            # Animal name and scientific name
            card_elements.extend([
                ui.div(
                    animal['name'],
                    ui.tags.span("↗", class_="external-link-icon"),
                    class_="animal-name",
                    onclick=f"window.open('{animal.get('url', '#')}', '_blank')"
                ),
                ui.div(animal.get('scientific_name', ''), class_="scientific-name")
            ])
            
            # Animal details
            details = []
            if pd.notna(animal.get('size')) and animal.get('size') != "":
                details.append(
                    ui.p(
                        ui.tags.span("Size: ", class_="detail-label"),
                        clean_text(animal['size'])
                    )
                )
            
            if pd.notna(animal.get('diet')) and animal.get('diet') != "":
                details.append(
                    ui.p(
                        ui.tags.span("Diet: ", class_="detail-label"),
                        clean_text(animal['diet'])
                    )
                )
            
            if pd.notna(animal.get('habitat')) and animal.get('habitat') != "":
                details.append(
                    ui.p(
                        ui.tags.span("Habitat: ", class_="detail-label"),
                        clean_text(animal['habitat'])
                    )
                )
            
            if pd.notna(animal.get('range')) and animal.get('range') != "":
                details.append(
                    ui.p(
                        ui.tags.span("Range: ", class_="detail-label"),
                        clean_text(animal['range'])
                    )
                )
            
            if pd.notna(animal.get('conservation_status')) and animal.get('conservation_status') != "":
                details.append(
                    ui.p(
                        ui.tags.span("Conservation Status: ", class_="detail-label"),
                        ui.tags.span(
                            clean_text(animal['conservation_status']),
                            class_=conservation_class
                        )
                    )
                )
            
            if details:
                card_elements.append(ui.div(*details, class_="animal-details"))
            
            # Fun fact
            if pd.notna(animal.get('fun_fact')) and animal.get('fun_fact') != "":
                card_elements.append(
                    ui.div(
                        ui.tags.strong("Fun Fact: "),
                        clean_text(animal['fun_fact']),
                        class_="fun-fact"
                    )
                )
            
            # Create the complete card
            animal_card = ui.div(*card_elements, class_="animal-card")
            animal_cards.append(animal_card)
        
        return animal_cards

# Create the app
app = App(app_ui, server, static_assets=Path(__file__).parent / "www")

if __name__ == "__main__":
    app.run()