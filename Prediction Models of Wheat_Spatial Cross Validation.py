# SECTION 1: Data Loading and Preprocessing

import pandas as pd

# Load Excel data
file_path = "Analysis_21-22.xlsx"
data_model = pd.read_excel(file_path, sheet_name="Sheet1")

# Filter for Winter Soft Wheat
data_model_wheat = data_model[data_model['CropType_Long'] == "Winter Soft Wheat"]

# Exclude rows with 'ohne N'
data_model_wheat_NoN = data_model_wheat[data_model_wheat['Pgl_Bez'] != 'ohne N']

# Check for missing values
print("Missing values by column:")
print(data_model_wheat_NoN.isnull().sum())

print("\nTotal missing values:")
print(data_model_wheat_NoN.isnull().sum().sum())

# Drop rows with missing values
wheat_model_1 = data_model_wheat_NoN.dropna()

print("\nShape after dropping NAs:", wheat_model_1.shape)

# SECTION 2: Relative Change Calculations

# Define a helper to get baseline values
def get_baseline(df, treatment, column_name, new_name):
    return (
        df[df['Pgl_Bez'] == treatment]
        .groupby('Attempt')[column_name]
        .first()
        .rename(new_name)
        .reset_index()
    )

# Pp baselines
baseline_Pp_yield = get_baseline(wheat_model_1, "Pp_35_35_30_T2_T3_T5", "Ert_dt_ha", "baseline_yield_Pp")
baseline_Pp_neff = get_baseline(wheat_model_1, "Pp_35_35_30_T2_T3_T5", "N_Efficiency", "baseline_neff_Pp")
baseline_Pp_rp = get_baseline(wheat_model_1, "Pp_35_35_30_T2_T3_T5", "RP_Go", "baseline_rp_Pp")

# Pi46 baselines
baseline_Pi46_yield = get_baseline(wheat_model_1, "Pi46_35_35_30_T2_T3_T5", "Ert_dt_ha", "baseline_yield_Pi46")
baseline_Pi46_neff = get_baseline(wheat_model_1, "Pi46_35_35_30_T2_T3_T5", "N_Efficiency", "baseline_neff_Pi46")
baseline_Pi46_rp = get_baseline(wheat_model_1, "Pi46_35_35_30_T2_T3_T5", "RP_Go", "baseline_rp_Pi46")

# Merge and calculate relative changes
wheat_RC = wheat_model_1.merge(baseline_Pp_yield, on='Attempt', how='left')
wheat_RC['RC_Pp_Yield'] = (wheat_RC['Ert_dt_ha'] - wheat_RC['baseline_yield_Pp']) / wheat_RC['baseline_yield_Pp']
wheat_RC.drop(columns='baseline_yield_Pp', inplace=True)

wheat_RC = wheat_RC.merge(baseline_Pp_neff, on='Attempt', how='left')
wheat_RC['RC_Pp_Neff'] = (wheat_RC['N_Efficiency'] - wheat_RC['baseline_neff_Pp']) / wheat_RC['baseline_neff_Pp']
wheat_RC.drop(columns='baseline_neff_Pp', inplace=True)

wheat_RC = wheat_RC.merge(baseline_Pp_rp, on='Attempt', how='left')
wheat_RC['RC_Pp_RP'] = (wheat_RC['RP_Go'] - wheat_RC['baseline_rp_Pp']) / wheat_RC['baseline_rp_Pp']
wheat_RC.drop(columns='baseline_rp_Pp', inplace=True)

wheat_RC = wheat_RC.merge(baseline_Pi46_yield, on='Attempt', how='left')
wheat_RC['RC_Pi46_Yield'] = (wheat_RC['Ert_dt_ha'] - wheat_RC['baseline_yield_Pi46']) / wheat_RC['baseline_yield_Pi46']
wheat_RC.drop(columns='baseline_yield_Pi46', inplace=True)

wheat_RC = wheat_RC.merge(baseline_Pi46_neff, on='Attempt', how='left')
wheat_RC['RC_Pi46_Neff'] = (wheat_RC['N_Efficiency'] - wheat_RC['baseline_neff_Pi46']) / wheat_RC['baseline_neff_Pi46']
wheat_RC.drop(columns='baseline_neff_Pi46', inplace=True)

wheat_RC = wheat_RC.merge(baseline_Pi46_rp, on='Attempt', how='left')
wheat_RC['RC_Pi46_RP'] = (wheat_RC['RP_Go'] - wheat_RC['baseline_rp_Pi46']) / wheat_RC['baseline_rp_Pi46']
wheat_RC.drop(columns='baseline_rp_Pi46', inplace=True)

# Final checks
print("Missing values after RC calculation:", wheat_RC.isnull().sum().sum())

# SECTION 3: Aggregation and Variance Calculation per Experimental Site

# 📌 Columns for average calculation
rc_columns = ['RC_Pp_Yield', 'RC_Pi46_Yield']

# ✅ Average RC per location (Attempt)
avg_per_trial = wheat_RC.groupby('Attempt')[rc_columns].mean().reset_index()
avg_per_trial.columns = ['Attempt', 'Avg_RC_Pp_Yield', 'Avg_RC_Pi46_Yield']

print("Average RC per location:")
print(avg_per_trial.head())

# Optionally save
# avg_per_trial.to_csv("average_of_relative_change_per_locations.csv", index=False)

# ✅ Variance of RC per location
var_RC_Pp_Yield = wheat_RC.groupby('Attempt')['RC_Pp_Yield'].var().reset_index()
var_RC_Pp_Yield.columns = ['Attempt', 'Var_RC_Pp_Yield']

var_RC_Pi46_Yield = wheat_RC.groupby('Attempt')['RC_Pi46_Yield'].var().reset_index()
var_RC_Pi46_Yield.columns = ['Attempt', 'Var_RC_Pi46_Yield']

print("\nVariance RC_Pp_Yield:")
print(var_RC_Pp_Yield.head())

print("\nVariance RC_Pi46_Yield:")
print(var_RC_Pi46_Yield.head())

# SECTION 4: Feature Encoding and Dataset Preparation

from sklearn.preprocessing import OneHotEncoder

# First, make a copy of the cleaned data
wheat_RC_analysis = wheat_RC.copy()

# One-hot encode 'Soil Group' and 'fertilizer_type'
wheat_RC_analysis = pd.get_dummies(wheat_RC_analysis, columns=['Soil Group', 'fertilizer_type'], drop_first=False)

# List of soil/fertilizer dummy variables to convert to category
soil_fertilizer_vars = [col for col in wheat_RC_analysis.columns if col.startswith("Soil Group.") or col.startswith("fertilizer_type.")]

wheat_RC_analysis[soil_fertilizer_vars] = wheat_RC_analysis[soil_fertilizer_vars].astype('category')

# Convert strategy variables to categorical
strategy_vars = ["urea", "NI", "UI", "UI_T1", "UI_T2", "UI_T3", "UI_T4", "UI_T5", 
                 "NI_T1", "NI_T2", "NI_T3", "NI_T4"]

for col in strategy_vars:
    if col in wheat_RC_analysis.columns:
        wheat_RC_analysis[col] = wheat_RC_analysis[col].astype('category')

# Repeat for RC_Pi46_Yield

# Extract column ranges by inspection (these indexes may vary — update based on your actual dataframe)
soil_cols = list(wheat_RC_analysis.columns[0:23])
weather_cols = list(wheat_RC_analysis.columns[23:108])
strategy_and_coords = ['Longitude', 'Latitude']

# Add dummy variables and target column for each dataset
target_pp = 'RC_Pp_Yield'
target_pi46 = 'RC_Pi46_Yield'

# Soil features + target
soil_wheat_RC_Pp_Yield = wheat_RC_analysis[soil_cols + weather_cols[66:] + [target_pp] + strategy_and_coords].dropna()
soil_wheat_RC_Pi46_Yield = wheat_RC_analysis[soil_cols + weather_cols[66:] + [target_pi46] + strategy_and_coords].dropna()

# Weather features + target
weather_wheat_RC_Pp_Yield = wheat_RC_analysis[weather_cols + [target_pp] + strategy_and_coords].dropna()
weather_wheat_RC_Pi46_Yield = wheat_RC_analysis[weather_cols + [target_pi46] + strategy_and_coords].dropna()

# Soil + weather + target
soil_weather_wheat_RC_Pp_Yield = wheat_RC_analysis[soil_cols + weather_cols + [target_pp] + strategy_and_coords].dropna()
soil_weather_wheat_RC_Pi46_Yield = wheat_RC_analysis[soil_cols + weather_cols + [target_pi46] + strategy_and_coords].dropna()

# Show dimensions
print("soil_wheat_RC_Pp_Yield:", soil_wheat_RC_Pp_Yield.shape)
print("weather_wheat_RC_Pp_Yield:", weather_wheat_RC_Pp_Yield.shape)
print("soil_weather_wheat_RC_Pp_Yield:", soil_weather_wheat_RC_Pp_Yield.shape)

print("soil_wheat_RC_Pi46_Yield:", soil_wheat_RC_Pi46_Yield.shape)
print("weather_wheat_RC_Pi46_Yield:", weather_wheat_RC_Pi46_Yield.shape)
print("soil_weather_wheat_RC_Pi46_Yield:", soil_weather_wheat_RC_Pi46_Yield.shape)

# SECTION 5: Spatial Cross-Validation

import geopandas as gpd
import matplotlib.pyplot as plt
from shapely.geometry import Point
from sklearn.model_selection import KFold

# Load Germany shapefile or GeoJSON
# Replace with actual path to your Germany boundaries geojson/shapefile
germany = gpd.read_file("D:/ERM/Thesis/dataset/Principal Component Analysis/Germany.json")

# Convert training datasets to GeoDataFrames for spatial plotting
def convert_to_gdf(df):
    return gpd.GeoDataFrame(
        df.copy(), 
        geometry=gpd.points_from_xy(df.Longitude, df.Latitude),
        crs=germany.crs
    )

spatial_soil_wheat_RC_Pp_Yield = convert_to_gdf(soil_wheat_RC_Pp_Yield)
spatial_soil_wheat_RC_Pi46_Yield = convert_to_gdf(soil_wheat_RC_Pi46_Yield)

# Plot sample locations for Winter Wheat (RC_Pp_Yield)
fig, ax = plt.subplots(figsize=(8, 8))
germany.plot(ax=ax, color='white', edgecolor='black')
spatial_soil_wheat_RC_Pp_Yield.plot(ax=ax, color='darkred', markersize=20, alpha=0.6)
plt.title("Sample Locations of Winter Wheat")
plt.xlabel("Longitude")
plt.ylabel("Latitude")
plt.grid(True)
plt.show()

# Function to create spatial folds using unique coordinates
def create_spatial_folds(df, k=5):
    unique_locations = df[['Longitude', 'Latitude']].drop_duplicates().reset_index(drop=True)
    kf = KFold(n_splits=k, shuffle=True, random_state=123)
    folds = []

    for train_idx, test_idx in kf.split(unique_locations):
        train_coords = unique_locations.iloc[train_idx]
        test_coords = unique_locations.iloc[test_idx]

        train_indices = df[df[['Longitude', 'Latitude']].apply(tuple, axis=1).isin(train_coords.apply(tuple, axis=1))].index.tolist()
        test_indices = df[df[['Longitude', 'Latitude']].apply(tuple, axis=1).isin(test_coords.apply(tuple, axis=1))].index.tolist()

        folds.append({
            'train_idx': train_indices,
            'test_idx': test_indices
        })
    return folds

# Create spatial cross-validation folds for each dataset
k = 5
folds_soil_wheat_RC_Pp_Yield = create_spatial_folds(soil_wheat_RC_Pp_Yield, k)
folds_weather_wheat_RC_Pp_Yield = create_spatial_folds(weather_wheat_RC_Pp_Yield, k)
folds_soil_weather_wheat_RC_Pp_Yield = create_spatial_folds(soil_weather_wheat_RC_Pp_Yield, k)
folds_soil_wheat_RC_Pi46_Yield = create_spatial_folds(soil_wheat_RC_Pi46_Yield, k)
folds_weather_wheat_RC_Pi46_Yield = create_spatial_folds(weather_wheat_RC_Pi46_Yield, k)
folds_soil_weather_wheat_RC_Pi46_Yield = create_spatial_folds(soil_weather_wheat_RC_Pi46_Yield, k)

# (Optional) Plot training and testing points for one of the folds
fold_number = 0
train_idx = folds_soil_wheat_RC_Pp_Yield[fold_number]['train_idx']
test_idx = folds_soil_wheat_RC_Pp_Yield[fold_number]['test_idx']

fig, ax = plt.subplots(figsize=(8, 8))
germany.plot(ax=ax, color='white', edgecolor='black')
plt.scatter(soil_wheat_RC_Pp_Yield.iloc[train_idx]['Longitude'], soil_wheat_RC_Pp_Yield.iloc[train_idx]['Latitude'], 
            c='blue', label='Train', s=20)
plt.scatter(soil_wheat_RC_Pp_Yield.iloc[test_idx]['Longitude'], soil_wheat_RC_Pp_Yield.iloc[test_idx]['Latitude'], 
            c='red', label='Test', s=20)
plt.title(f"Spatial CV Fold {fold_number+1} - RC_Pp_Yield (Soil)")
plt.xlabel("Longitude")
plt.ylabel("Latitude")
plt.legend()
plt.grid(True)
plt.show()

# SECTION 6: Model Building (Ridge, Lasso, RFE)

import numpy as np
from sklearn.linear_model import Ridge, Lasso
from sklearn.feature_selection import RFE
from sklearn.ensemble import RandomForestRegressor
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import GridSearchCV

# Define lambda grid for regularization
lambda_grid = np.logspace(-3, 3, 100)

# Extract training sets for modeling (example for soil_wheat_RC_Pp_Yield)
train_df = soil_wheat_RC_Pp_Yield.iloc[train_idx].drop(columns=['Longitude', 'Latitude'])
test_df = soil_wheat_RC_Pp_Yield.iloc[test_idx].drop(columns=['Longitude', 'Latitude'])

X_train = train_df.drop(columns=['RC_Pp_Yield'])
y_train = train_df['RC_Pp_Yield']
X_test = test_df.drop(columns=['RC_Pp_Yield'])
y_test = test_df['RC_Pp_Yield']

# Ridge Regression
ridge_pipeline = Pipeline([
    ('scaler', StandardScaler()),
    ('ridge', Ridge())
])

ridge_model = GridSearchCV(ridge_pipeline, param_grid={'ridge__alpha': lambda_grid}, cv=5)
ridge_model.fit(X_train, y_train)

# Lasso Regression
lasso_pipeline = Pipeline([
    ('scaler', StandardScaler()),
    ('lasso', Lasso(max_iter=10000))
])

lasso_model = GridSearchCV(lasso_pipeline, param_grid={'lasso__alpha': lambda_grid}, cv=5)
lasso_model.fit(X_train, y_train)

# Recursive Feature Elimination with Random Forest
rfe_selector = RFE(estimator=RandomForestRegressor(n_estimators=100, random_state=123), n_features_to_select=10)
rfe_pipeline = Pipeline([
    ('scaler', StandardScaler()),
    ('rfe', rfe_selector)
])

rfe_pipeline.fit(X_train, y_train)

# Print model coefficients or feature importances
print("Best Ridge alpha:", ridge_model.best_params_)
print("Best Lasso alpha:", lasso_model.best_params_)
print("Top features from RFE:", X_train.columns[rfe_selector.get_support()].tolist())

# Section 7: Variable Importance

import shap
import eli5
from sklearn.inspection import permutation_importance
import matplotlib.pyplot as plt

# Variable importance using permutation (LASSO)
result_lasso = permutation_importance(
    lasso_soil_weather_model,
    train_data_soil_weather.drop(columns=["RC_Pi46_Yield"]),
    train_data_soil_weather["RC_Pi46_Yield"],
    n_repeats=30,
    random_state=123,
    scoring="r2"
)

sorted_idx_lasso = result_lasso.importances_mean.argsort()[::-1][:10]
plt.figure(figsize=(10, 6))
plt.barh(
    np.array(train_data_soil_weather.drop(columns=["RC_Pi46_Yield"]).columns)[sorted_idx_lasso],
    result_lasso.importances_mean[sorted_idx_lasso],
    color="orange"
)
plt.xlabel("Permutation Importance (R² drop)")
plt.title("Top 10 Important Features - LASSO")
plt.gca().invert_yaxis()
plt.grid(True)
plt.tight_layout()
plt.show()

# Variable importance using permutation (RFE)
result_rfe = permutation_importance(
    rfe_soil_weather_model,
    train_data_soil_weather.drop(columns=["RC_Pi46_Yield"]),
    train_data_soil_weather["RC_Pi46_Yield"],
    n_repeats=30,
    random_state=123,
    scoring="r2"
)

sorted_idx_rfe = result_rfe.importances_mean.argsort()[::-1][:10]
plt.figure(figsize=(10, 6))
plt.barh(
    np.array(train_data_soil_weather.drop(columns=["RC_Pi46_Yield"]).columns)[sorted_idx_rfe],
    result_rfe.importances_mean[sorted_idx_rfe],
    color="darkgreen"
)
plt.xlabel("Permutation Importance (R² drop)")
plt.title("Top 10 Important Features - RFE (Random Forest)")
plt.gca().invert_yaxis()
plt.grid(True)
plt.tight_layout()
plt.show()

# SECTION 8: Partial Dependence Plots and Interaction Analysis

# Partial Dependence Plots for top features

pdp_features = [
    "T3", "UI_T4", "fertilizer_type.Urea", "Corg", "Cu",
    "BOW_0-30_T2_10d", "BOW_0-60_T2_10d", "BOF_0-60_T4_10d",
    "Zn", "NS_Sum_T4_10d"
]

pdp_plots = []

for feature in pdp_features:
    fig, ax = plt.subplots(figsize=(6, 4))
    display = PartialDependenceDisplay.from_estimator(
        rfe_soil_weather_model, train_data_soil_weather, [feature], ax=ax
    )
    ax.set_title(f"Partial Dependence of {feature}")
    pdp_plots.append(fig)
    plt.tight_layout()
    plt.show()

# Interaction effects (2-way partial dependence) for selected feature pairs
interaction_pairs = [
    ("Cu", "fertilizer_type.Urea"),
    ("Corg", "fertilizer_type.Urea"),
    ("BOF_0-60_T4_10d", "fertilizer_type.Urea"),
    ("fertilizer_type.Alzon+Piagran pro", "Zn")
]

interaction_plots = []

for pair in interaction_pairs:
    fig, ax = plt.subplots(figsize=(6, 5))
    display = PartialDependenceDisplay.from_estimator(
        rfe_soil_weather_model, train_data_soil_weather, [pair], ax=ax
    )
    ax.set_title(f"Interaction between {pair[0]} and {pair[1]}")
    interaction_plots.append(fig)
    plt.tight_layout()
    plt.show()

# SECTION 9: Coefficient Extraction and Visualization (LASSO)

# Extract LASSO coefficients
lasso_coefs = lasso_soil_weather_model.named_steps['regressor'].coef_
features = train_data_soil_weather.drop(columns=["RC_Pi46_Yield"]).columns
intercept = lasso_soil_weather_model.named_steps['regressor'].intercept_

coef_df = pd.DataFrame({
    "feature": features,
    "estimate": lasso_coefs
})

# Add intercept if needed
coef_df = coef_df[coef_df.estimate != 0].sort_values(by="estimate", ascending=False)

# Save coefficients
coef_df.to_csv("Model_Coefficients_LASSO_Soil_Weather_RC_Pi46_Yield.csv", index=False)

# Plot coefficients
plt.figure(figsize=(10, 8))
sns.barplot(y="feature", x="estimate", data=coef_df, palette="Blues_r")
plt.title("LASSO Coefficients (Non-zero)")
plt.xlabel("Coefficient Estimate")
plt.ylabel("Feature")
plt.tight_layout()
plt.show()

# Optional smaller version
coef_df_top = coef_df.head(20)
plt.figure(figsize=(10, 6))
sns.barplot(y="feature", x="estimate", data=coef_df_top, palette="Blues_r")
plt.title("Top 20 LASSO Coefficients")
plt.xlabel("Coefficient Estimate")
plt.ylabel("Feature")
plt.tight_layout()
plt.show()

# SECTION 10: LASSO Models with Interaction Terms
from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import Pipeline

# Add interaction terms to the model (LASSO with PolynomialFeatures)
interaction_pipeline = Pipeline([
    ("poly", PolynomialFeatures(degree=2, interaction_only=True, include_bias=False)),
    ("scaler", StandardScaler()),
    ("regressor", LassoCV(alphas=lambda_range, cv=5, random_state=123, max_iter=10000))
])

interaction_pipeline.fit(
    train_data_soil_weather.drop(columns=["RC_Pi46_Yield"]),
    train_data_soil_weather["RC_Pi46_Yield"]
)

# Predictions and evaluation
preds_lasso_interact = interaction_pipeline.predict(test_data_soil_weather.drop(columns=["RC_Pi46_Yield"]))

r2_lasso_interact = r2_score(test_data_soil_weather["RC_Pi46_Yield"], preds_lasso_interact)
rmse_lasso_interact = np.sqrt(mean_squared_error(test_data_soil_weather["RC_Pi46_Yield"], preds_lasso_interact))

print("R² (LASSO with Interactions):", round(r2_lasso_interact, 3))
print("RMSE (LASSO with Interactions):", round(rmse_lasso_interact, 3))

# Extract coefficients
feature_names_poly = interaction_pipeline.named_steps['poly'].get_feature_names_out(
    input_features=train_data_soil_weather.drop(columns=["RC_Pi46_Yield"]).columns
)

coef_df_interaction = pd.DataFrame({
    "feature": feature_names_poly,
    "estimate": interaction_pipeline.named_steps['regressor'].coef_
})

coef_df_interaction = coef_df_interaction[coef_df_interaction.estimate != 0]
coef_df_interaction.to_csv("Model_Coefficients_LASSO_Interactions_Soil_Weather_RC_Pi46_Yield.csv", index=False)

# Plot top interaction terms
coef_df_top_interactions = coef_df_interaction.sort_values(by="estimate", key=abs, ascending=False).head(20)
plt.figure(figsize=(10, 8))
sns.barplot(y="feature", x="estimate", data=coef_df_top_interactions, palette="coolwarm")
plt.title("Top 20 LASSO Coefficients with Interaction Terms")
plt.xlabel("Estimate")
plt.ylabel("Feature")
plt.tight_layout()
plt.show()

# SECTION 11: Permutation-Based Variable Importance
from sklearn.inspection import permutation_importance

# Calculate permutation importance for LASSO with interaction terms
perm_importance = permutation_importance(
    interaction_pipeline,
    test_data_soil_weather.drop(columns=["RC_Pi46_Yield"]),
    test_data_soil_weather["RC_Pi46_Yield"],
    scoring="r2",
    n_repeats=30,
    random_state=123,
    n_jobs=-1
)

importance_df = pd.DataFrame({
    "feature": feature_names_poly,
    "importance": perm_importance.importances_mean
})

importance_df = importance_df.sort_values(by="importance", ascending=False)

# Plot top 20 important features
plt.figure(figsize=(10, 8))
sns.boxplot(
    y="feature", x="importance",
    data=importance_df.head(20), palette="Oranges_r", orient="h"
)
plt.title("Top 20 Permutation Importances - LASSO with Interaction Terms")
plt.xlabel("Mean Importance")
plt.ylabel("Feature")
plt.tight_layout()
plt.show()

# SECTION 12: Variable Interaction Strengths (Proxy Using SHAP Interaction Values)
import shap

# Create explainer using SHAP (KernelExplainer for interpretability)
X_train_sample = train_data_soil_weather.drop(columns=["RC_Pi46_Yield"]).sample(n=100, random_state=123)
explainer = shap.Explainer(interaction_pipeline.predict, X_train_sample)

# Compute SHAP interaction values for a sample
X_test_sample = test_data_soil_weather.drop(columns=["RC_Pi46_Yield"]).sample(n=50, random_state=123)
interaction_values = shap.Explanation(values=explainer(X_test_sample))

# Summarize interaction strengths
interaction_matrix = shap.utils.approximate_interactions(0, interaction_values.values, X_test_sample)
interaction_strengths = pd.DataFrame({
    "Variables": [f"{X_test_sample.columns[0]} & {X_test_sample.columns[i]}" for i in interaction_matrix],
    "Interaction": interaction_values.values[0, interaction_matrix]
})

interaction_strengths = interaction_strengths.sort_values(by="Interaction", ascending=False).head(10)

# Plot interaction strengths
plt.figure(figsize=(10, 6))
sns.barplot(x="Interaction", y="Variables", data=interaction_strengths, palette="flare")
plt.title("Top Variable Interactions (SHAP Approximation)")
plt.xlabel("Interaction Strength")
plt.ylabel("Variable Pair")
plt.tight_layout()
plt.show()

# SECTION 13: Partial Dependence Plots
from sklearn.inspection import PartialDependenceDisplay

# Top 4 features from the permutation importance for plotting
top_features = importance_df.head(4)["feature"].tolist()

# Refit the pipeline to include the feature names correctly
interaction_pipeline.fit(
    train_data_soil_weather.drop(columns=["RC_Pi46_Yield"]),
    train_data_soil_weather["RC_Pi46_Yield"]
)

# Plot partial dependence for the top 4 features
fig, ax = plt.subplots(figsize=(16, 10))
PartialDependenceDisplay.from_estimator(
    interaction_pipeline,
    test_data_soil_weather.drop(columns=["RC_Pi46_Yield"]),
    features=top_features,
    grid_resolution=30,
    ax=ax
)
plt.suptitle("Partial Dependence Plots for Top 4 Features", fontsize=16)
plt.tight_layout()
plt.show()

# SECTION 14: 2-Way Interactions Visualization
from sklearn.inspection import partial_dependence

# Define selected 2-way interaction feature pairs
interaction_pairs = [
    ("Cu", "fertilizer_type.Urea"),
    ("Corg", "fertilizer_type.Urea"),
    ("BOF_0-60_T4_10d", "fertilizer_type.Urea"),
    ("fertilizer_type.Alzon+Piagran pro", "Zn")
]

# Refit the pipeline if needed
interaction_pipeline.fit(
    train_data_soil_weather.drop(columns=["RC_Pi46_Yield"]),
    train_data_soil_weather["RC_Pi46_Yield"]
)

# Plot 2D partial dependence plots for selected interactions
fig, ax = plt.subplots(2, 2, figsize=(14, 12))
for i, pair in enumerate(interaction_pairs):
    disp = PartialDependenceDisplay.from_estimator(
        interaction_pipeline,
        test_data_soil_weather.drop(columns=["RC_Pi46_Yield"]),
        features=[pair],
        ax=ax[i // 2][i % 2],
        grid_resolution=20
    )

plt.suptitle("2-Way Partial Dependence Plots", fontsize=16)
plt.tight_layout()
plt.show()

# SECTION 15: LASSO Coefficient Extraction and Plotting
import pandas as pd

# Get LASSO coefficients from the trained model
lasso_model = interaction_pipeline.named_steps["lasso"]
feature_names = interaction_pipeline.named_steps["preprocessor"].get_feature_names_out()
coefs = lasso_model.coef_

# Create a dataframe of non-zero coefficients
coef_df = pd.DataFrame({
    "feature": feature_names,
    "estimate": coefs
})

coef_df = coef_df[coef_df["estimate"] != 0].sort_values(by="estimate")

# Plot non-zero coefficients
plt.figure(figsize=(10, 12))
sns.barplot(data=coef_df, x="estimate", y="feature", palette="viridis")
plt.title("LASSO Coefficients for RC_Pi46_Yield Prediction")
plt.xlabel("Coefficient Estimate")
plt.ylabel("Feature")
plt.tight_layout()
plt.show()

# Save coefficients to CSV
coef_df.to_csv("Model_Coefficients_LASSO_soil_weather_RC_Pi46_Yield.csv", index=False)





