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
  
![Map_Study Area](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/3fcea00696f3436b97b36aebf44e5298ddb91029/Map%20of%20Study%20Area.PNG)

## Methodology
### **Workflow**

![Workflow_Methodology](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/e80db24af014c4496773b44d600b7ff35179f86c/Workflow_Methodology.PNG)

### **Prediction Modeling**

![Modeling_Methodology](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/e80db24af014c4496773b44d600b7ff35179f86c/Modeling_Methodology.PNG)

### Spatial Cross-Validation
To avoid the **data leakage problem** (Using the same site data for train and test dataset) and **over-optimistic results**, spatial cross-validation was applied. Moreover, spatial cross-validation allows the prediction models to be applied to new locations with more accurate results.
![Training and Test Location](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/3fcea00696f3436b97b36aebf44e5298ddb91029/Training%20and%20Test%20Locations.PNG)


## Results
### Model Performance
![Model Performance](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/e80db24af014c4496773b44d600b7ff35179f86c/Model%20Performance.png)

### Variable Importance
Both Lasso and Random Forest-Recursive Feature Elimination methods exhibits the following 3 important variables for predicting the target variable (RC_Pi46_Yield):
- Zinc content (Zn)
- Copper content (Cu)
- Average soil moisture of T4 time frame at 0-60cm soil depth (BOF_0-60_T4_10d)

![Variable Importance](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/3fcea00696f3436b97b36aebf44e5298ddb91029/Variable%20Importance.PNG)

### Partial Dependence Plot

![Partial Dependence Plot](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/3fcea00696f3436b97b36aebf44e5298ddb91029/Partial%20Dependence%20Plot%20of%20Important%20Variables.PNG)

### Pairwise Variable Interaction

![Pairwise Variable Interaction](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/3fcea00696f3436b97b36aebf44e5298ddb91029/Pairwise%20Variable%20Interaction%20Importance.PNG)

### Two-Dimensional Partial Dependence Plot

![Two-Dimensional Partial Dependence Plot](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/b7462ebf3116e5550e7ddbfdc2e6a5958f4375da/Two-Dimensional%20Partial%20Dependence%20Plot.PNG)

## Key Findings
### Best Prediction Model and Predictability
- RC_Pp_Yield could not predict well
- Ridge, LASSO, and RF-RFE treated multicollinearity effectively
- LASSO performed better than Ridge 
- RF-RFE showed best prediction accuracy in predicting RC_Pi46_Yield -> captured non-linear relationships and interactions
- Better predicability when soil and weather variables were analyzed together
- Predictability: LASSO (28.7%) and RF-RFE (42%)

### Expected Better Yield Conditions
- 0% fertilizer applied at earlier stage of elongation (T3)
- Presence of urease inhibitor (UI) at later stage of elongation (T4)
- Soil and Weather: 2 mg/kg Cu, 2.5 mg/kg Zn, 20% soil moisture of T4 at 0-60cm soil depth
  
Based on these conditions found from the analysis, the following nitrogen fertilization strategies can be recommended.

![Recommended Fertilization Strategies](https://github.com/binita-roy/Comparing-Machine-Learning-Variable-Selection-Approaches-for-Agricultural-Prediction-in-Germany/blob/10b07177aa478fb0cd444658b8270028e45e7478/Recommended%20Fertilization%20Strategies.png)

## Conclusion
- Random Forest-Recursive Feature Elimination can be adopted for predicting the winter wheat yield with 42% predictability.
- Copper (Cu), zinc (Zn), and soil moisture are important  soil and weather conditions affected the relative change in yield negatively and should be restricted to a limited value.
- 5 variants out of 11 variants of nitrogen fertilizations strategies can be expected to produce higher amount of yield comparing to the baseline strategy.
- The findings of the prediction models can be utilized in new locations within Germany for winter wheat.

## Acknowledgement
- My heartiest gratitude to my supervisors Prof. Dr. Masahiro Ryo and my co-supervisor Stefan Stiller.
- I am grateful to my work group of "Artificial Intelligence" at Leibniz Centre for Agricultural Landscape Research (ZALF) e. V.
- I am also thankful to the other project partners of StaPrax Project, specially the R&D Team of SKW Stickstoffwerke Piesteritz GmbH and Dr. Enrico Thiel for sharing the dataset and the valuable insights and knowledge about the dataset.

## Author
Binita Roy
[LinkedIn](https://www.linkedin.com/in/binita-roy/)|[Email](mailto:binitaroy1312@gmail.com)









