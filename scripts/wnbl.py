import requests
import pandas as pd
import re
import os
from bs4 import BeautifulSoup

def get_soup(url):
    """Wrapper function to Beautiful Soup.
        Customised to parse json response.
        Returns soup object.
    """
    page = requests.get(url)
    jsonResponse = page.json()
    html = jsonResponse['html']
    soup = BeautifulSoup(html, 'html.parser')
    return(soup)

def get_seasons(url):
    """Get a list of available seasons.
    Sample Record:
    ['WNBL 2020',
    '2020',
    'https://wnbl.basketball/stats/?&WHurl=%2Fcompetition%2F28356%2Fstatistics%2Fteam',
    '28356']
    """
    
    soup = get_soup(url)
    options = soup.find_all("option")

    seasons = []
    for o in options:

        # option HTML element
        season_uri = o['value']
        season_name = o.contents[0]

        if (season_uri.startswith('https')):
            # only save options with links
            
            # extract the year from the season name
            m = re.search('\d{4}',season_name)
            season_year = m.group(0) if m else 1900

            # competition ID
            x = season_uri.split('%2F')
            season_id = x[2]
            seasons.append([ season_name, season_year, season_uri, season_id ])
    
    return(seasons)

def get_player_data(html):
    """Get player data from table.
        Returns dataframe.
    """

    # gather data as list of lists
    data = []

    # column headings
    header = [ele.text.strip() for ele in html.find('thead').find_all('th')]

    # parse table body
    rows = html.find('tbody').find_all('tr')
    for row in rows:
        cols = row.find_all('td')
        cols = [ele.text.strip() for ele in cols]

        # save row
        # data.append([ele for ele in cols if ele]) # Get rid of empty values
        data.append(cols)

    # convert lists to dataframe
    df = pd.DataFrame(data,columns=header)

    return(df)

def get_team_data(html,colnum,posnum):
    """Get team data from table.
        Team ID is found in given column and position in url.
        Returns dataframe.
    """

    # gather data as list of lists
    data = []

    # column headings
    header = [ele.text.strip() for ele in html.find('thead').find_all('th')]
    header.append('TeamID')

    # parse table body
    rows = html.find('tbody').find_all('tr')
    for row in rows:
        cols = row.find_all('td')

        if len(cols) > 0:
            # extract team ID from URL
            # x = cols[0].findChild()['href'].split('%2F')[4].replace('%3F','')
            try:
                x = cols[colnum].findChild()['href'].split('%2F')[posnum].replace('%3F','')
            except:
                x = 0

        cols = [ele.text.strip() for ele in cols]
        cols.append(x)

        # save row
        # data.append([ele for ele in cols if ele]) # Get rid of empty values
        data.append(cols)

    # convert lists to dataframe
    df = pd.DataFrame(data,columns=header)

    return(df)

def get_standings(url):
    """Get standings data from table.
        Returns dataframe.
    """

    soup = get_soup(url)

    tables = soup.find_all("table")

    # get tabular data as panda dataframe
    standings_df = get_team_data(tables[0],2,4)

    # convert strings to numerica
    standings_df = standings_df.apply(pd.to_numeric, errors='ignore')
    # standings_df.info()

    return(standings_df)

def get_teams(url):
    """
    """
    soup = get_soup(url)

    # using same soup object for current season
    tables = soup.find_all("table")

    teams_full_df = get_team_data(tables[0],0,4)
    teams_full_df = teams_full_df.apply(pd.to_numeric, errors='ignore')
    teams_df = teams_full_df[['Team','TeamID']]

    return(teams_df)

def get_players(url):
    """
    """
    soup = get_soup(url)
    tables = soup.find_all("table")

    dfp = get_player_data(tables[2])
    dfp = dfp.apply(pd.to_numeric, errors='ignore')

    return(dfp)

def load_csv(data_path,prefix,include_id=False):
    """Load multiple csv files matching prefix.
        If include_id=True, competition ID is parsed from source filename
    """
    filepaths = [os.path.join(data_path,f) for f in os.listdir(data_path) if f.startswith(prefix)]
    df = pd.concat(map(lambda file: pd.read_csv(file).assign(filename=file), filepaths))

    # competition ID from filename
    if include_id:
        df['CompID'] = df['filename'].str.extract('(\d+)').astype(int)

    df = df.drop(columns=['filename']) 
    return(df)   