# Exploratory Data Analysis and PCA for Wheat Dataset in Python (Jupyter Notebook Style)

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.impute import SimpleImputer
from scipy.cluster.hierarchy import dendrogram, linkage, fcluster
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
import scipy.stats as stats

# Load dataset
file_path = "Analysis_21-22.xlsx"
df = pd.read_excel(file_path, sheet_name="Sheet1")

# Filter crop types
wheat = df[df['CropType_Long'] == 'Winter Soft Wheat']

# Remove 'No Nitrogen Fertilisation'
wheat = wheat[wheat['Pgl_Bez'] != 'ohne N']

# Drop rows with missing values
wheat_clean = wheat.dropna()

# Drop irrelevant columns
drop_cols = df.columns[[0,1,2,3,5,6,7,8,9,10,13,15,17,24,26,28,35,37,38,112,128,129,130]]
wheat_data = wheat_clean.drop(columns=drop_cols)

# Divide into soil and weather data
wheat_soil = wheat_data.iloc[:, 1:24]
wheat_weather = wheat_data.iloc[:, 24:89]

# One-hot encoding of categorical variables in soil
ohe = OneHotEncoder(sparse=False, handle_unknown='ignore')
encoded_soil = pd.DataFrame(ohe.fit_transform(wheat_soil.select_dtypes(include='object')))
encoded_soil.columns = ohe.get_feature_names_out(wheat_soil.select_dtypes(include='object').columns)
encoded_soil = pd.concat([encoded_soil, wheat_soil.select_dtypes(include=[np.number]).reset_index(drop=True)], axis=1)

# PCA on Soil Data
scaler = StandardScaler()
scaled_soil = scaler.fit_transform(encoded_soil)
pca_soil = PCA()
pca_soil_fit = pca_soil.fit(scaled_soil)

# Explained variance for Soil PCA
explained_variance_soil = pca_soil.explained_variance_ratio_
plt.figure(figsize=(10,6))
plt.plot(np.cumsum(explained_variance_soil[:10]), marker='o')
plt.title('Scree Plot - Soil Variables')
plt.xlabel('Number of Components')
plt.ylabel('Cumulative Explained Variance')
plt.grid(True)
plt.show()

# Biplot for Soil PCA
components = pca_soil.components_[:2]
plt.figure(figsize=(12,8))
plt.bar(range(len(components[0])), components[0], label='PC1')
plt.bar(range(len(components[1])), components[1], label='PC2', alpha=0.7)
plt.legend()
plt.title('PCA Loadings for First Two Principal Components - Soil')
plt.show()

# Hierarchical Clustering - Soil
Z_soil = linkage(scaled_soil.T, method='complete')
plt.figure(figsize=(15, 6))
dendrogram(Z_soil)
plt.title('Hierarchical Clustering Dendrogram (Soil Variables)')
plt.xlabel('Variables')
plt.ylabel('Distance')
plt.show()

# Correlation Heatmap - Soil
plt.figure(figsize=(14,10))
corr_matrix_soil = encoded_soil.corr()
sns.heatmap(corr_matrix_soil, cmap='coolwarm', annot=False)
plt.title('Correlation Heatmap - Soil Variables')
plt.show()

# --- PCA and Clustering for Weather Variables ---

# PCA on Weather Data
scaled_weather = scaler.fit_transform(wheat_weather)
pca_weather = PCA()
pca_weather_fit = pca_weather.fit(scaled_weather)

# Explained variance for Weather PCA
explained_variance_weather = pca_weather.explained_variance_ratio_
plt.figure(figsize=(10,6))
plt.plot(np.cumsum(explained_variance_weather[:10]), marker='o')
plt.title('Scree Plot - Weather Variables')
plt.xlabel('Number of Components')
plt.ylabel('Cumulative Explained Variance')
plt.grid(True)
plt.show()

# Biplot for Weather PCA
components_weather = pca_weather.components_[:2]
plt.figure(figsize=(12,8))
plt.bar(range(len(components_weather[0])), components_weather[0], label='PC1')
plt.bar(range(len(components_weather[1])), components_weather[1], label='PC2', alpha=0.7)
plt.legend()
plt.title('PCA Loadings for First Two Principal Components - Weather')
plt.show()

# Hierarchical Clustering - Weather
Z_weather = linkage(scaled_weather.T, method='complete')
plt.figure(figsize=(15, 6))
dendrogram(Z_weather)
plt.title('Hierarchical Clustering Dendrogram (Weather Variables)')
plt.xlabel('Variables')
plt.ylabel('Distance')
plt.show()

# Correlation Heatmap - Weather
plt.figure(figsize=(14,10))
corr_matrix_weather = pd.DataFrame(wheat_weather).corr()
sns.heatmap(corr_matrix_weather, cmap='coolwarm', annot=False)
plt.title('Correlation Heatmap - Weather Variables')
plt.show()
