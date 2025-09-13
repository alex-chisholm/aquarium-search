from shiny import App, ui, render, reactive
import pandas as pd
import re
import duckdb
import uuid
import os
from datetime import datetime
from pathlib import Path

# DuckDB and MotherDuck configuration
MOTHERDUCK_TOKEN = os.getenv("MOTHERDUCK_TOKEN", "")
DATABASE_NAME = "aquarium"
LOCAL_DB_PATH = "local_ratings.duckdb"

def get_duckdb_connection():
    """Get DuckDB connection with MotherDuck fallback to local"""
    try:
        if MOTHERDUCK_TOKEN:
            # Try MotherDuck first
            conn = duckdb.connect(f"md:{DATABASE_NAME}?motherduck_token={MOTHERDUCK_TOKEN}")
            print("Connected to MotherDuck")
            return conn, "motherduck"
        else:
            # Fall back to local DuckDB
            conn = duckdb.connect(LOCAL_DB_PATH)
            print("Connected to local DuckDB")
            return conn, "local"
    except Exception as e:
        print(f"Error connecting to MotherDuck, using local: {e}")
        # Fall back to local DuckDB
        conn = duckdb.connect(LOCAL_DB_PATH)
        return conn, "local"

def initialize_ratings_table():
    """Initialize the ratings table"""
    try:
        conn, db_type = get_duckdb_connection()
        
        # Create ratings table if it doesn't exist
        conn.execute("""
            CREATE TABLE IF NOT EXISTS ratings (
                id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
                timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                session_id VARCHAR,
                animal_name VARCHAR,
                rating VARCHAR
            )
        """)
        
        # Create index for better performance
        conn.execute("""
            CREATE INDEX IF NOT EXISTS idx_ratings_animal_rating 
            ON ratings(animal_name, rating)
        """)
        
        conn.close()
        print(f"Successfully initialized ratings table in {db_type}")
        return True
        
    except Exception as e:
        print(f"Error initializing table: {e}")
        return False

def save_rating_to_duckdb(animal_name, rating, session_id):
    """Save rating to DuckDB/MotherDuck"""
    try:
        conn, db_type = get_duckdb_connection()
        
        print(f"Saving rating to {db_type}...")
        
        # Insert new rating
        conn.execute("""
            INSERT INTO ratings (session_id, animal_name, rating) 
            VALUES (?, ?, ?)
        """, [session_id, animal_name, rating])
        
        conn.close()
        print(f"Successfully saved rating to {db_type}")
        return True
        
    except Exception as e:
        print(f"Error saving to DuckDB: {e}")
        return False

def get_ratings_summary():
    """Get ratings summary from DuckDB/MotherDuck"""
    try:
        conn, db_type = get_duckdb_connection()
        
        print(f"Reading ratings summary from {db_type}...")
        
        # Check if table exists and has data
        try:
            count_result = conn.execute("SELECT COUNT(*) as count FROM ratings").fetchone()
            if count_result[0] == 0:
                conn.close()
                print("No ratings data found")
                return None
        except:
            conn.close()
            print("No ratings table found")
            return None
        
        # Get love summary
        love_summary = conn.execute("""
            SELECT animal_name, COUNT(*) as n 
            FROM ratings 
            WHERE rating = 'Literally in love' 
            GROUP BY animal_name 
            ORDER BY n DESC 
            LIMIT 10
        """).fetchdf()
        
        # Get nope summary
        nope_summary = conn.execute("""
            SELECT animal_name, COUNT(*) as n 
            FROM ratings 
            WHERE rating = 'Not my type' 
            GROUP BY animal_name 
            ORDER BY n DESC 
            LIMIT 10
        """).fetchdf()
        
        conn.close()
        
        print(f"Love summary rows: {len(love_summary)}")
        print(f"Nope summary rows: {len(nope_summary)}")
        
        return {"love": love_summary, "nope": nope_summary}
        
    except Exception as e:
        print(f"Error reading from DuckDB: {e}")
        return None

def sanitize_id(name):
    """Convert animal name to valid Shiny input ID"""
    # Replace any non-alphanumeric character with underscore
    sanitized = re.sub(r'[^a-zA-Z0-9]', '_', name)
    # Remove multiple consecutive underscores
    sanitized = re.sub(r'_+', '_', sanitized)
    # Remove leading/trailing underscores
    sanitized = sanitized.strip('_')
    return sanitized

# Initialize the database table on startup
if not initialize_ratings_table():
    print("Warning: Could not initialize DuckDB table. Check your database connection.")

# Read the aquarium data - use Path to ensure correct file location
app_dir = Path(__file__).parent
aquarium_csv_path = app_dir / "aquarium.csv"

try:
    aquarium_data = pd.read_csv(aquarium_csv_path)
    print(f"Successfully loaded aquarium data from {aquarium_csv_path}")
except FileNotFoundError:
    print(f"Could not find aquarium.csv at {aquarium_csv_path}")
    # Create a minimal fallback dataset
    aquarium_data = pd.DataFrame({
        'name': ['Sample Fish', 'Sample Shark', 'Sample Turtle'],
        'scientific_name': ['Fishus sampleus', 'Sharkus sampleus', 'Turtleus sampleus'],
        'diet': ['Omnivore', 'Carnivore', 'Herbivore'],
        'habitat': ['Ocean', 'Deep Sea', 'Coastal'],
        'range': ['Global', 'Pacific', 'Atlantic'],
        'physical_characteristics': ['Colorful', 'Large teeth', 'Hard shell'],
        'fun_fact': ['Very colorful', 'Apex predator', 'Long lived'],
        'conservation_status': ['Stable', 'Vulnerable', 'Endangered'],
        'image_url': ['', '', ''],
        'url': ['#', '#', '#']
    })
    print("Using fallback sample data")

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
    
    # Landing page with featured animals and ratings summary
    ui.panel_conditional(
        "input.search_query == ''",
        ui.div(
            ui.h2("Some of Our Favorites", class_="favorites-header"),
            ui.output_ui("featured_animals"),
            
            # Ratings summary section
            ui.div(
                ui.h2("Visitor Favorites", class_="ratings-header"),
                ui.output_ui("ratings_summary"),
                class_="ratings-summary-container"
            ),
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
    
    # Generate unique session ID
    session_id = f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}_{uuid.uuid4().hex[:8]}"
    
    # Reactive values to track which animals have been rated
    rated_animals = reactive.value({})
    
    # Reactive trigger for ratings updates
    ratings_trigger = reactive.value(0)
    
    # Store button observers to prevent garbage collection
    button_observers = {}
    
    def clean_text(text):
        """Clean up text fields helper function"""
        if pd.isna(text) or text == "":
            return ""
        # Remove extra quotes and clean up formatting
        text = re.sub(r'^"|"$', '', str(text))
        text = re.sub(r'""', '"', text)
        return text
    
    def save_rating(animal_name, rating, session_id):
        """Save rating with fallback to CSV"""
        print(f"Attempting to save rating: {animal_name} - {rating}")
        
        # Save to DuckDB/MotherDuck
        duckdb_success = save_rating_to_duckdb(animal_name, rating, session_id)
        
        if duckdb_success:
            print("Successfully saved rating to DuckDB")
        else:
            # Fall back to local CSV
            rating_data = pd.DataFrame({
                'timestamp': [datetime.now().isoformat()],
                'session_id': [session_id],
                'animal_name': [animal_name],
                'rating': [rating]
            })
            
            backup_file = app_dir / "ratings_backup.csv"
            if not backup_file.exists():
                rating_data.to_csv(backup_file, index=False)
            else:
                rating_data.to_csv(backup_file, mode='a', header=False, index=False)
            print("Saved to backup CSV")
    
    def create_rating_observer(animal_name, button_id, rating_type, rating_key):
        """Create an observer for a specific rating button"""
        def rating_handler():
            try:
                button_value = getattr(input, button_id)()
                if button_value and button_value > 0:
                    current_ratings = rated_animals.get()
                    if animal_name not in current_ratings:
                        save_rating(animal_name, rating_type, session_id)
                        
                        # Update rated animals
                        new_ratings = current_ratings.copy()
                        new_ratings[animal_name] = rating_key
                        rated_animals.set(new_ratings)
                        
                        # Trigger ratings summary refresh
                        ratings_trigger.set(ratings_trigger.get() + 1)
                        
                        # Show feedback notification
                        if rating_key == "love":
                            ui.notification_show(
                                f"💕 Recorded your love for {animal_name}!",
                                type="message",
                                duration=2
                            )
                        else:
                            ui.notification_show(
                                f"👎 Noted that {animal_name} is not your type",
                                type="message", 
                                duration=2
                            )
            except AttributeError:
                # Button doesn't exist yet, ignore
                pass
            except Exception as e:
                print(f"Error in rating handler: {e}")
        
        return reactive.effect(rating_handler)
    
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
    
    # Ratings summary output
    @render.ui
    def ratings_summary():
        # Depend on the trigger to refresh when ratings change
        ratings_trigger.get()
        
        summary_data = get_ratings_summary()
        
        if summary_data is None:
            return ui.div("No ratings yet", class_="no-ratings")
        
        # Create love and nope lists
        love_items = []
        if len(summary_data["love"]) > 0:
            for i, row in summary_data["love"].iterrows():
                love_items.append(
                    ui.div(
                        ui.tags.span(f"{i+1}.", class_="rank"),
                        ui.tags.span(
                            row['animal_name'],
                            class_="animal-name-summary",
                            onclick=f"Shiny.setInputValue('featured_animal_click', '{row['animal_name']}', {{priority: 'event'}});"
                        ),
                        ui.tags.span(f"({row['n']} votes)", class_="vote-count"),
                        class_="rating-item"
                    )
                )
            
            love_column = ui.div(
                ui.h3("💕 Most Loved", class_="rating-column-header"),
                ui.div(*love_items, class_="rating-list"),
                class_="rating-column love-column"
            )
        else:
            love_column = ui.div(
                ui.h3("💕 Most Loved", class_="rating-column-header"),
                ui.div("No votes yet", class_="no-votes"),
                class_="rating-column love-column"
            )
        
        nope_items = []
        if len(summary_data["nope"]) > 0:
            for i, row in summary_data["nope"].iterrows():
                nope_items.append(
                    ui.div(
                        ui.tags.span(f"{i+1}.", class_="rank"),
                        ui.tags.span(
                            row['animal_name'],
                            class_="animal-name-summary",
                            onclick=f"Shiny.setInputValue('featured_animal_click', '{row['animal_name']}', {{priority: 'event'}});"
                        ),
                        ui.tags.span(f"({row['n']} votes)", class_="vote-count"),
                        class_="rating-item"
                    )
                )
            
            nope_column = ui.div(
                ui.h3("👎 Not Their Type", class_="rating-column-header"),
                ui.div(*nope_items, class_="rating-list"),
                class_="rating-column nope-column"
            )
        else:
            nope_column = ui.div(
                ui.h3("👎 Not Their Type", class_="rating-column-header"),
                ui.div("No votes yet", class_="no-votes"),
                class_="rating-column nope-column"
            )
        
        return ui.div(love_column, nope_column, class_="ratings-summary-grid")
    
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
        current_ratings = rated_animals.get()
        
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
            # Create unique button IDs using proper sanitization
            animal_id = sanitize_id(animal['name'])
            love_button_id = f"love_{animal_id}"
            nope_button_id = f"nope_{animal_id}"
            
            # Create observers for the rating buttons if they don't exist
            if love_button_id not in button_observers:
                button_observers[love_button_id] = create_rating_observer(
                    animal['name'], love_button_id, "Literally in love", "love"
                )
            
            if nope_button_id not in button_observers:
                button_observers[nope_button_id] = create_rating_observer(
                    animal['name'], nope_button_id, "Not my type", "nope"
                )
            
            # Determine conservation status color class
            conservation_class = "conservation-stable"
            if pd.notna(animal.get('conservation_status')):
                status = str(animal['conservation_status']).lower()
                if re.search(r'endangered|critically', status):
                    conservation_class = "conservation-endangered"
                elif re.search(r'vulnerable|near threatened', status):
                    conservation_class = "conservation-vulnerable"
            
            # Check if this animal has been rated
            is_rated = animal['name'] in current_ratings
            
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
            
            # Rating buttons or thank you message
            if is_rated:
                # Create personalized thank you message based on rating
                rating_type = current_ratings[animal['name']]
                if rating_type == "love":
                    card_elements.append(
                        ui.div("💕 Literally in love - Thanks for sharing!", class_="thank-you-message love-message")
                    )
                else:
                    card_elements.append(
                        ui.div("👎 Not my type - Thanks for sharing!", class_="thank-you-message nope-message")
                    )
            else:
                card_elements.append(
                    ui.div(
                        ui.input_action_button(
                            love_button_id,
                            "💕 Literally in love",
                            class_="love-button"
                        ),
                        ui.input_action_button(
                            nope_button_id,
                            "👎 Not my type",
                            class_="nope-button"
                        ),
                        class_="rating-buttons"
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