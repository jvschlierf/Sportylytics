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

base_url = "https://www.spotrac.com/nba/contracts/sort-value/all-time/limit-10000/"
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
dataset = train.append(test).reset_index(drop=True)

# Keep the columns we need to match with the contracts
df = dataset[['Player', 'season', 'Salary_Cap_Perc']]
df['season_year'] = df['season'].str[:4].apply(int)

# Create an empty dataframe to store all the contracts, one row for each season
player_contracts = pd.DataFrame(columns=['player_name', 'season', 'season_year', 'signed_using'])

path = "webpages" # Path where we dump the txts

# Download the txt file for each player (may take a long time, and it is going to save ... in webpages/)
for i, row in page_index.iterrows():
    player_name = row['player_name']
    base_url = row['player_page']
    page_name = player_name.replace(' ', '_')
    get_page(base_url=base_url, page_name=page_name, path=path)

# Iterate over each player (page)
for i, row in page_index.iterrows():
    player_name = row['player_name']
    page_name = player_name.replace(' ', '_')
    path = "webpages"
    page = open(os.path.join(path,page_name) + ".txt", 'rb')
    soup = BeautifulSoup(page, "html.parser")
    contracts = soup.find_all("span", class_="playerValue") # Contracts information
    page.close()

    # Create an empty dataframe for each player, to store all of his contracts (1 row = 1 contract)
    tmp_c = pd.DataFrame(columns=['player_name', 'expiring', 'signed_using'])

    # Scraping the class "playerValue"
    # 5 rows = 1 contract signed by the player
    # for each contract, we need its fourth and fifth rows
    for i in range(0, len(contracts), 5):
        d = {'player_name' : player_name,
            'expiring' : int(contracts[i-1].get_text()[:4]),
            'signed_using' : contracts[i-2].get_text()}
        tmp_c = tmp_c.append(d, ignore_index=True).sort_values('expiring', ascending=True).reset_index(drop=True)

    # Create an empty dataframe for each player, to store all of his seasons (1 row = 1 season)
    tmp_p = df[df['Player']==player_name].sort_values('season_year', ascending=True).reset_index(drop=True)

    ### We need to go from a row for each contract (tmp_c) to a row for each
    ### season (player_contracts) iterating one player at the time (tmp_p)

    # Iterate over all the contracts of a single player
    for ic, rowc in tmp_c.iterrows():
        cexp = rowc['expiring']
        ctype= rowc['signed_using']
        # Iterate over all the seasons of a single player
        for ip, rowp in tmp_p.iterrows():
            # Using the expiring date we check if the player was playing this season on this contract
            if cexp > rowp['season_year']: 
                d = {'player_name' : player_name,
                    'season' : rowp['season'],
                    'season_year' : rowp['season_year'],
                    'signed_using' : ctype}
                # If true, we append a season played on this contract
                player_contracts = player_contracts.append(d, ignore_index=True) 
            else: 
                # The contract has expired, we move to the next one
                break 

# Save to csv
player_contracts.to_csv('spotrac_contracts.csv', index=False)

""" # Let's cleanup from all the txt files
for file in os.listdir(path):
    if file == 'all_players.txt':
        continue
    else:
        os.remove(os.path.join(path, file)) """
