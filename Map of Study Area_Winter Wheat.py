# importing library

import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib_scalebar.scalebar import ScaleBar
from matplotlib.patches import FancyArrow
import contextily as ctx

# --- Loading Excel File ---
data_area = pd.read_excel("Analysis_21-22.xlsx", sheet_name="Sheet2")
data_state = pd.read_excel("Analysis_21-22.xlsx", sheet_name="Sheet12")

# --- Load German NUTS level 1 shapefile from giscoR equivalent (naturalearth or download from Eurostat/GISCO) ---

nuts_gdf = gpd.read_file("NUTS_RG_01M_2021_4326_LEVL_1.shp")

# Filter for Germany only (DE)
germany = nuts_gdf[nuts_gdf['CNTR_CODE'] == 'DE']

# Merge with state data
germany = germany.merge(data_state, left_on='NUTS_NAME', right_on='Federal State')

# --- Create GeoDataFrame for point data ---
gdf_points = gpd.GeoDataFrame(data_area,
                               geometry=gpd.points_from_xy(data_area['Longitude'], data_area['Latitude']),
                               crs='EPSG:4326')

# --- Plotting Map---
fig, ax = plt.subplots(figsize=(12, 10))
germany.plot(column='Data (%)', cmap='viridis', legend=True, ax=ax, edgecolor='black')

# Adding state names
germany.apply(lambda x: ax.annotate(text=x['NUTS_NAME'], xy=x.geometry.centroid.coords[0], ha='center', fontsize=8), axis=1)

# Adding point markers
gdf_points.plot(ax=ax, color='red', markersize=20)

# Adding scale bar
scalebar = ScaleBar(1, location='lower left')  # 1 degree ≈ 111 km (approx)
ax.add_artist(scalebar)

# Adding north arrow manually
ax.annotate('N', xy=(0.1, 0.15), xytext=(0.1, 0.25), 
            arrowprops=dict(facecolor='black', width=5, headwidth=15), 
            ha='center', va='center', fontsize=12, xycoords=ax.transAxes)

# Customizing labels and title
ax.set_title("Winter Wheat", fontsize=16)
ax.set_xlabel("Longitude (°E)")
ax.set_ylabel("Latitude (°N)")


plt.tight_layout()
plt.show()
