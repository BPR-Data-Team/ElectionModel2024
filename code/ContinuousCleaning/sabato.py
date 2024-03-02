import requests 
from bs4 import BeautifulSoup 
import re
import csv
  
URL = "https://centerforpolitics.org/crystalball/2024-rating-changes/" 
r = requests.get(URL) 
  
soup = BeautifulSoup(r.content, 'html.parser') 

# Function to extract state and district
def extract_info(row):
    position_rating = row.find_previous("h2").text.strip()
    position = position_rating.split(" ", 1)[0]
    name_party_state_district = row.find("td").text.strip()
    
    # Handling different formats of name_party_state_district
    match = re.match(r'(.+) \((.+)\)', name_party_state_district)
    if match:
        if not (match.string[2] == " " or (match.string[2] == "-" and match.string[3].isnumeric())):
            state_district = match.group(2)
            state_match = re.search(r'[A-Z]{2}', state_district)
            state = state_match.group() if state_match else ""
            district_match = re.search(r'\b\d+\b', state_district)
            district = district_match.group() if district_match else ""
        else:
            state_district = match.group(1)
            state = state_district[0:2]
            district = state_district[3] if state_district[2] == "-" else ""
    else:
        state = ""
        district = ""

    # Extracting the new rating
    new_rating = row.find_all("td")[2].text.strip()
    
    return [position, state, district, new_rating]

# Finding all tables containing rating changes
tables = soup.find_all("table")

# Extracting info from each table row and writing to CSV
with open("data/rating_changes.csv", "w", newline='') as csvfile:
    csvwriter = csv.writer(csvfile)
    csvwriter.writerow(["Position", "State", "District", "Rating"])
    for table in tables:
        rows = table.find_all("tr")[1:]  # Skipping header row
        for row in rows:
            data = extract_info(row)
            csvwriter.writerow(data)
