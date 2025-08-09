################################################################################

#Gatger historical meteo data via API request from ERA5 
#(https://cds.climate.copernicus.eu/)

################################################################################

#Set up working directory

import os
os.chdir("/yourdirectory")

os.getcwd() #check wd

#Request hourly meteo data (temperature, solar radiation and precipitation)

import cdsapi

c = cdsapi.Client()

#Change "year" to download data from different years

year = '2004'

variables = [
    '2m_temperature',
    '2m_dewpoint_temperature',
    'surface_solar_radiation_downwards',
    'total_precipitation'
]

# Coordinates for a 0.01 degrees box (1km²) around XEMA [X8]
area = [41.37919, 2.10540, 41.37819, 2.10640]

# Loop for each month of the year
for month in range(1, 13):
    month_str = f'{month:02d}'
    print(f'Downloading {year}-{month_str}...')

    c.retrieve(
        'reanalysis-era5-single-levels',
        {
            'product_type': 'reanalysis',
            'format': 'netcdf',
            'variable': variables,
            'year': year,
            'month': month_str,
            'day': [f'{i:02d}' for i in range(1, 32)],
            'time': [f'{h:02d}:00' for h in range(24)],
            'area': area,
        },
        f'barcelona_era5_{year}_{month_str}.nc'
    )

print('Done')

