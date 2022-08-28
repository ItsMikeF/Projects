#scrape epsn site for player data
import requests
from bs4 import BeautifulSoup


def get_player_data(url):
    #get player data from espn site
    r = requests.get(url)
    soup = BeautifulSoup(r.text, 'html.parser')
    player_data = soup.find_all('tr', class_='player-	player-table-row')
    print(player_data)
    
get_player_data('https://www.espn.com/nba/stats/player/_/stat/scoring/sort/avgPointsPerGame/year/2019/seasontype/2')