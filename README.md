# Comparing Machine Learning Variable Selection Approaches for Agricultural Prediction in Germany

## Introduction
This thesis is part of the **StaPrax-Regio Project**, a collaborative initiative that aims to improve agricultural outcomes through innovative fertilization strategies tailored to site-specific soil and weather conditions. The project involves contributions from various partners, including **SKW Stickstoffwerke Piesteritz GmbH**, whose R&D team played a critical role in organizing field trials, conducting lab experiments, and integrating the data used for this research.

## Background
- Winter wheat is one of the major cereals in Germany occupying about 46% of arable land area of cereals in Germany.
- Nitrogen fertilization is one of the major factors of winter wheat yield.
- Staprax-Regio Project has been taken over to study the effects of a new fertilization strategy on the crop yield based on site-specific soil and weather conditions.
- Machine learning feature selection approaches have been utilized to predict crop yield from a high dimensional dataset with better accuracy.
  
## Research Questions
1. What are the major characteristics representing soil and weather conditions over the experimental field sites?
2. Which machine learning method with feature selection is the best to predict an expected effect of a new fertilization strategy on key agronomic parameters in comparison to the baseline fertilization strategies? How well can machine learning models predict expected effects?
3. Which soil and weather conditions can explain an expected effect of a new fertilization strategy?

## Dataset
The dataset was prepared by the** R&D team of SKW Stickstoffwerke Piesteritz GmbH** and includes field trial data from **2021** and **2022** across **11 federal states of Germany**. Key features of the dataset include:
- 41 trial locations with data distributed unevenly across states.
- Temporal resolution:
  - Soil and field experiment variables: measured once per year.
  - Weather variables: measured five times annually during key phenological stages of winter wheat.


## Methodology
### **Workflow**

![Workflow_Methodology](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/e80db24af014c4496773b44d600b7ff35179f86c/Workflow_Methodology.PNG)

### **Prediction Modeling**

![Modeling_Methodology](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/e80db24af014c4496773b44d600b7ff35179f86c/Modeling_Methodology.PNG)

### Spatial Cross-Validation
To avoid the **data leakage problem** (Using the same site data for train and test dataset) and **over-optimistic results**, spatial cross-validation was applied. Moreover, spatial cross-validation allow the prediction models to be applied to new locations with more accurate results.


## Results
### Model Performance
![Model Performance](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/e80db24af014c4496773b44d600b7ff35179f86c/Model%20Performance.png)

### Variable Importance
Both Lasso and Random Forest-Recursive Feature Elimination methods exhibits the following 3 important variables for predicting the target variable (RC_Pi46_Yield):
- Zinc content (Zn)
- Copper content (Cu)
- Average soil moisture of T4 time frame at 0-60cm soil depth (BOF_0-60_T4_10d)

### Partial Dependence Plot


