################################################################################

#API request to Senscape Hub using Phyton

################################################################################

import requests

API_KEY = "yourapikey"

requests.get('url', headers={"Authorization": API_KEY})
response = requests.get('https://senscape.eu/api/devices', headers={"Authorization": API_KEY})

print(response.json())  # Print the API response 

data = response.json()

# Extract device names
device_names = [device["name"] for device in data["devices"]]

# Print a list with the device names
print("List of devices:")
for i, name in enumerate(device_names, start=1):
    print(f"{i}. {name}")
    
#Download data of interest

import pandas as pd

# Configuration

API_KEY = "yourapikey""
URL_DATA = "https://senscape.eu/api/data"
PAGE_SIZE = 1000  #Number of records per page
DEVICE_NAME1 = "ASPB 1 (Sant Andreu)"  
DEVICE_NAME2 = "ASPB 2 (Horta)"  
DEVICE_NAME3 = "ASPB 3 (Pedralbes)"  
DEVICE_NAME4 = "ASPB 5 (Zoo)"  
START_DATE = "2021-01-01T00:00:00.000Z"  # Start date ISO format
END_DATE = "2024-12-31T23:59:59.999Z"  # End date ISO format
CSV_FILE1 = "data_ASPB1_SantAndreu.csv"  
CSV_FILE2 = "data_ASPB2_Horta.csv" 
CSV_FILE3 = "data_ASPB3_Pedralbes.csv" 
CSV_FILE4 = "data_ASPB5_Zoo.csv" 

# Get device IDs

response = requests.get(URL_DEVICES, headers={"Authorization": API_KEY})
devices = response.json()["devices"]
device_id1 = next((d["_id"] for d in devices if d["name"] == DEVICE_NAME1), None)
device_id2 = next((d["_id"] for d in devices if d["name"] == DEVICE_NAME2), None)
device_id3 = next((d["_id"] for d in devices if d["name"] == DEVICE_NAME3), None)
device_id4 = next((d["_id"] for d in devices if d["name"] == DEVICE_NAME4), None)

# Download data from device_id1 and save in a .csv file

page_number = 0
all_data = []

while True:
    url = f"{URL_DATA}?deviceId={device_id1}&filterStart={START_DATE}&filterEnd={END_DATE}&sortOrder=asc&sortField=record_time&pageSize={PAGE_SIZE}&pageNumber={page_number}"
    
    response = requests.get(url, headers={"Authorization": API_KEY})
    data = response.json()

    if "samples" not in data or not data["samples"]:
        break 

    all_data.extend(data["samples"])
    print(f"Downloading page {page_number + 1}, records: {len(data['samples'])}")
    
    if len(data["samples"]) < PAGE_SIZE:
        break

    page_number += 1

if all_data:
    df = pd.DataFrame(all_data)
    df.to_csv(CSV_FILE1, index=False)
    
# Download data from device_id2 and save in a .csv file

page_number = 0
all_data = []

while True:
    url = f"{URL_DATA}?deviceId={device_id2}&filterStart={START_DATE}&filterEnd={END_DATE}&sortOrder=asc&sortField=record_time&pageSize={PAGE_SIZE}&pageNumber={page_number}"
    
    response = requests.get(url, headers={"Authorization": API_KEY})
    data = response.json()

    if "samples" not in data or not data["samples"]:
        break 

    all_data.extend(data["samples"])
    print(f"Downloading page {page_number + 1}, records: {len(data['samples'])}")
    
    if len(data["samples"]) < PAGE_SIZE:
        break

    page_number += 1

if all_data:
    df = pd.DataFrame(all_data)
    df.to_csv(CSV_FILE2, index=False)
    
# Download data from device_id3 and save in a .csv file

page_number = 0
all_data = []

while True:
    url = f"{URL_DATA}?deviceId={device_id3}&filterStart={START_DATE}&filterEnd={END_DATE}&sortOrder=asc&sortField=record_time&pageSize={PAGE_SIZE}&pageNumber={page_number}"
    
    response = requests.get(url, headers={"Authorization": API_KEY})
    data = response.json()

    if "samples" not in data or not data["samples"]:
        break 

    all_data.extend(data["samples"])
    print(f"Downloading page {page_number + 1}, records: {len(data['samples'])}")
    
    if len(data["samples"]) < PAGE_SIZE:
        break

    page_number += 1

if all_data:
    df = pd.DataFrame(all_data)
    df.to_csv(CSV_FILE3, index=False)
    
# Download data from device_id4 and save in a .csv file

page_number = 0
all_data = []

while True:
    url = f"{URL_DATA}?deviceId={device_id4}&filterStart={START_DATE}&filterEnd={END_DATE}&sortOrder=asc&sortField=record_time&pageSize={PAGE_SIZE}&pageNumber={page_number}"
    
    response = requests.get(url, headers={"Authorization": API_KEY})
    data = response.json()

    if "samples" not in data or not data["samples"]:
        break 

    all_data.extend(data["samples"])
    print(f"Downloading page {page_number + 1}, records: {len(data['samples'])}")
    
    if len(data["samples"]) < PAGE_SIZE:
        break

    page_number += 1

if all_data:
    df = pd.DataFrame(all_data)
    df.to_csv(CSV_FILE4, index=False)
    
#Download only test-pulse data to get temperature and humidity values each 30 min

#Test-pulse for device_id1

page_number = 0
all_data = []

while True:
    url = f"{URL_DATA}?deviceId={device_id1}&filterStart={START_DATE}&filterEnd={END_DATE}&classification=Test pulse&sortOrder=asc&sortField=record_time&pageSize={PAGE_SIZE}&pageNumber={page_number}"
    
    response = requests.get(url, headers={"Authorization": API_KEY})
    data = response.json()

    if "samples" not in data or not data["samples"]:
        break 

    all_data.extend(data["samples"])
    print(f"🔄 Downloading page {page_number + 1}, records: {len(data['samples'])}")
    
    if len(data["samples"]) < PAGE_SIZE:
        break

    page_number += 1

if all_data:
    df = pd.DataFrame(all_data)
    df.to_csv("TestPulse_ASPB1.csv", index=False)

#Test-pulse for device_id2

page_number = 0
all_data = []

while True:
    url = f"{URL_DATA}?deviceId={device_id2}&filterStart={START_DATE}&filterEnd={END_DATE}&classification=Test pulse&sortOrder=asc&sortField=record_time&pageSize={PAGE_SIZE}&pageNumber={page_number}"
    
    response = requests.get(url, headers={"Authorization": API_KEY})
    data = response.json()

    if "samples" not in data or not data["samples"]:
        break 

    all_data.extend(data["samples"])
    print(f"🔄 Downloading page {page_number + 1}, records: {len(data['samples'])}")
    
    if len(data["samples"]) < PAGE_SIZE:
        break

    page_number += 1

if all_data:
    df = pd.DataFrame(all_data)
    df.to_csv("TestPulse_ASPB2.csv", index=False)
    
#Test-pulse for device_id3

page_number = 0
all_data = []

while True:
    url = f"{URL_DATA}?deviceId={device_id3}&filterStart={START_DATE}&filterEnd={END_DATE}&classification=Test pulse&sortOrder=asc&sortField=record_time&pageSize={PAGE_SIZE}&pageNumber={page_number}"
    
    response = requests.get(url, headers={"Authorization": API_KEY})
    data = response.json()

    if "samples" not in data or not data["samples"]:
        break 

    all_data.extend(data["samples"])
    print(f"🔄 Downloading page {page_number + 1}, records: {len(data['samples'])}")
    
    if len(data["samples"]) < PAGE_SIZE:
        break

    page_number += 1

if all_data:
    df = pd.DataFrame(all_data)
    df.to_csv("TestPulse_ASPB3.csv", index=False)
    
#Test-pulse for device_id4

page_number = 0
all_data = []

while True:
    url = f"{URL_DATA}?deviceId={device_id4}&filterStart={START_DATE}&filterEnd={END_DATE}&classification=Test pulse&sortOrder=asc&sortField=record_time&pageSize={PAGE_SIZE}&pageNumber={page_number}"
    
    response = requests.get(url, headers={"Authorization": API_KEY})
    data = response.json()

    if "samples" not in data or not data["samples"]:
        break 

    all_data.extend(data["samples"])
    print(f"🔄 Downloading page {page_number + 1}, records: {len(data['samples'])}")
    
    if len(data["samples"]) < PAGE_SIZE:
        break

    page_number += 1

if all_data:
    df = pd.DataFrame(all_data)
    df.to_csv("TestPulse_ASPB5.csv", index=False)
    

