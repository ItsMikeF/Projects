msg = "hello world"
print(msg)

for i in range(10):
    print(i)

import json

items = json.loads('[{"name": "John", "age": 30}, {"name": "Mary", "age": 28}]')

for item in items:
    print(item['name'])
    print(item['age'])

#write function to get google stock price
import requests
import json

def get_price(symbol):
    url = 'https://www.google.com/finance/info?q={}'.format(symbol)
    response = requests.get(url)
    content = response.text.split('=', 1)[1]
    data = json.loads(content)
    return data[0]['l']

get_price('GOOG')

#write a function to get stock price
def get_price(symbol):
    url = 'https://www.google.com/finance/info?q={}'.format(symbol)
    response = requests.get(url)
    content = response.text.split('=', 1)[1]
    data = json.loads(content)
    return data[0]['l']

#write a function to scrape espn
def get_espn_scores():
    url = 'http://www.espn.com/mens-college-basketball/scoreboard'
    response = requests.get(url)
    content = response.text
    return content

#cbioportal ap