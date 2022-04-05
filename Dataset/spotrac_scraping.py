from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd
import os 

# Function to download webpages as txt to save time

def get_page(base_url, page_name, path=''):
    page = urlopen(base_url)
    soup = BeautifulSoup(page, features='lxml')
    file = open(os.path.join(path,page_name + ".txt"), 'w')
    file.write(str(soup))
    file.close()

# Let's get all the players on spotrac and their personal webpages

base_url = "https://www.spotrac.com/nba/contracts/sort-value/all-time/limit-2000/"
page_name = "all_players"
path = "webpages"

get_page(base_url=base_url, page_name=page_name, path=path)

page = open(os.path.join(path,page_name) + ".txt", 'rb')
soup = BeautifulSoup(page, "html.parser")
players = soup.find_all("a", class_="team-name") # Class 'team-name' contains player_name-player_page pairs

# Create an empty dataframe to store player_name-player_page pairs
page_index = pd.DataFrame(columns=['player_name', 'player_page'])

for link in players:
    d = {'player_name' : link.get_text(),
         'player_page' : link.get('href')}
    page_index = page_index.append(d, ignore_index=True)

# Let's save the csv
# page_index.to_csv('spotrac_index.csv', index=False)

# Read the csv
page_index = pd.read_csv('spotrac_index.csv')

# Load our previously scraped dataset
train = pd.read_csv('data_Bplayers_2000_TRAIN.csv', encoding = 'unicode_escape')
test = pd.read_csv('data_Bplayers_2000_TEST.csv', encoding = 'unicode_escape')
df = train.append(test)


base_url = page_index['player_page'][0]
page_name = page_index['player_name'][0].replace(' ', '_')
path = "webpages"

get_page(base_url=base_url, page_name=page_name, path=path)

page = open(os.path.join(path,page_name) + ".txt", 'rb')
soup = BeautifulSoup(page, "html.parser")
