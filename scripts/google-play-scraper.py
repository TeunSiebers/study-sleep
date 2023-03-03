from google_play_scraper import app

def scrape_app(x):
    try:
        result = app(
            x,
            lang = 'en', # defaults to 'en'
            country = 'us' # defaults to 'us'
        )
        return {"app_name": result["title"], "app_cat": result["genre"]}
    except:
        return {"app_name": None, "app_cat": None}

