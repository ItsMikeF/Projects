#scrape nfl injuries from espn.com
import requests
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np

def get_injuries(url):
    r = requests.get(url)
    soup = BeautifulSoup(r.text, 'html.parser')
    table = soup.find('table', {'class': 'tablehead'})
    rows = table.find_all('tr')
    rows = rows[1:]
    injuries = []
    for row in rows:
        cells = row.find_all('td')
        injuries.append([cell.text for cell in cells])
    return injuries
get_injuries('https://www.espn.com/nfl/injuries')