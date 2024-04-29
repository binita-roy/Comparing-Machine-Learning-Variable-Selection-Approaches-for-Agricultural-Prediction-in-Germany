
rm(list = ls())
set.seed(124567)

# Loading the libraries

library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(caret)
library (knitr)
library(vip)
library(raster)
library(sf)
library(lattice)
library(ggplot2)
library(patchwork)
library(spdep)

# Loading the dataset

data_model <- read_excel("Analysis_21-22.xlsx", sheet = "Sheet1", col_types = c("text", "text", "text", "text",
                                                                                "numeric", "numeric", "numeric", "numeric", "text", "text",
                                                                                "text", "numeric", "numeric", "text", "numeric", "text", "numeric", "text", "numeric",
                                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "text",
                                                                                "numeric", "text", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                "numeric", "numeric", "numeric", "text", "text", "text",
                                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",                                                   
                                                                                "numeric", "text", "numeric", "numeric", "numeric", "numeric",
                                                                                "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                "numeric", "numeric", "numeric", "numeric",                                                                                                   
                                                                      "numeric", "numeric", "numeric", "numeric"))
View(data_model)

# Creating Dataset for Wheat

data_model_wheat <-  filter(data_model, CropType_Long == "Winter Soft Wheat")          

# Excluding the rows with `No Nitrogen Fertilisation`'

data_model_wheat_NoN <- data_model_wheat[data_model_wheat$Pgl_Bez != 'ohne N', ]
dim(data_model_wheat_NoN)

## Checking for missing values

colSums(is.na(data_model_wheat_NoN))

sum(is.na(data_model_wheat_NoN))


## Excluding the rows with missing values

wheat_model_1 <- na.exclude(data_model_wheat_NoN)
dim(wheat_model_1)


# Calculating Relative Changes

## Calculating Relative Changes with respect to Pp_35_35_30_T2_T3_T5 and Pi46_35_35_30_T2_T3_T5

# Pp_35_35_30_T2_T3_T5

wheat_baseline_data_Pp_Yield <- wheat_model_1 %>%
  group_by(Attempt) %>%
  filter(Pgl_Bez == "Pp_35_35_30_T2_T3_T5") %>%
  dplyr::select(Attempt, baseline_value = Ert_dt_ha)

wheat_baseline_data_Pp_Neff <- wheat_model_1 %>%
  group_by(Attempt) %>%
  filter(Pgl_Bez == "Pp_35_35_30_T2_T3_T5") %>%
  dplyr::select(Attempt, baseline_value = N_Efficiency)

wheat_baseline_data_Pp_RP <- wheat_model_1 %>%
  group_by(Attempt) %>%
  filter(Pgl_Bez == "Pp_35_35_30_T2_T3_T5") %>%
  dplyr::select(Attempt, baseline_value = RP_Go)

# Pi46_35_35_30_T2_T3_T5

wheat_baseline_data_Pi46_Yield <- wheat_model_1 %>%
  group_by(Attempt) %>%
  filter(Pgl_Bez == "Pi46_35_35_30_T2_T3_T5") %>%
  dplyr::select(Attempt, baseline_value = Ert_dt_ha)

wheat_baseline_data_Pi46_Neff <- wheat_model_1 %>%
  group_by(Attempt) %>%
  filter(Pgl_Bez == "Pi46_35_35_30_T2_T3_T5") %>%
  dplyr::select(Attempt, baseline_value = N_Efficiency)

wheat_baseline_data_Pi46_RP <- wheat_model_1 %>%
  group_by(Attempt) %>%
  filter(Pgl_Bez == "Pi46_35_35_30_T2_T3_T5") %>%
  dplyr::select(Attempt, baseline_value = RP_Go)


wheat_RC_1 <- wheat_model_1 %>%
  left_join(wheat_baseline_data_Pp_Yield, by = "Attempt") %>%
  mutate(RC_Pp_Yield = (Ert_dt_ha - baseline_value) / baseline_value) %>%
  dplyr::select(-baseline_value)

wheat_RC_2 <- wheat_RC_1 %>%
  left_join(wheat_baseline_data_Pp_Neff, by = "Attempt") %>%
  mutate(RC_Pp_Neff = (N_Efficiency - baseline_value) / baseline_value) %>%
  dplyr::select(-baseline_value)

wheat_RC_3 <- wheat_RC_2 %>%
  left_join(wheat_baseline_data_Pp_RP, by = "Attempt") %>%
  mutate(RC_Pp_RP = (RP_Go - baseline_value) / baseline_value) %>%
  dplyr::select(-baseline_value)


wheat_RC_4 <- wheat_RC_3 %>%
  left_join(wheat_baseline_data_Pi46_Yield, by = "Attempt") %>%
  mutate(RC_Pi46_Yield = (Ert_dt_ha - baseline_value) / baseline_value) %>%
  dplyr::select(-baseline_value)

wheat_RC_5 <- wheat_RC_4 %>%
  left_join(wheat_baseline_data_Pi46_Neff, by = "Attempt") %>%
  mutate(RC_Pi46_Neff = (N_Efficiency - baseline_value) / baseline_value) %>%
  dplyr::select(-baseline_value)

wheat_RC <- wheat_RC_5 %>%
  left_join(wheat_baseline_data_Pi46_RP, by = "Attempt") %>%
  mutate(RC_Pi46_RP = (RP_Go - baseline_value) / baseline_value) %>%
  dplyr::select(-baseline_value)

view(wheat_RC)

colSums(is.na(wheat_RC))

write.csv(wheat_RC, "Dataset with Relative Changes.csv")

### Calculating Average RC_Pp_Yield and RC_Pi46_Yield in each experimental site

View(wheat_RC[, 135])

avg_per_trial <- aggregate(wheat_RC[, 132:135], list(wheat_RC$Attempt), mean)

View(avg_per_trial)

write.csv(avg_per_trial, file = "average of relative change per locations.csv")

### Calculating variance of RC_Pp_Yield and RC_Pi46_Yield

var_RC_Pp_Yield <- wheat_RC %>%
  group_by(Attempt) %>%
  summarize(var = var(RC_Pp_Yield))

write.csv(var_RC_Pp_Yield, file = "Variance of RC_Pp_Yield.csv")

var_RC_Pi46_Yield <- wheat_RC %>%
  group_by(Attempt) %>%
  summarize(var = var(RC_Pi46_Yield))

write.csv(var_RC_Pi46_Yield, file = "Variance of RC_Pi46_Yield.csv")


wheat_RC_analysis <- wheat_RC[, c(5, 12, 13, 15, 17, 19:24, 26, 28, 30:35, 37, 40:112, 114:128, 130:137)]
View(wheat_RC_analysis)

wheat_RC_analysis$`Soil Group` <- as.factor(wheat_RC_analysis$`Soil Group`)
wheat_RC_analysis$fertilizer_type <- as.factor(wheat_RC_analysis$fertilizer_type)

print(levels(wheat_RC_analysis$`Soil Group`))
print(levels(wheat_RC_analysis$fertilizer_type))

# One hot encoding of the categorical variables Soil Group and Fertilizer type

## Creating a function for one hot encoding

one_hot_encoding = function(df, columns="season"){
  # create a copy of the original data.frame for not modifying the original
  df = cbind(df)
  # convert the columns to vector in case it is a string
  columns = c(columns)
  # for each variable perform the One hot encoding
  for (column in columns){
    unique_values = sort(unique(df[column])[,column])
    # to be the reference by default
    for (value in unique_values){
      # the new dummy column name
      new_col_name = paste0(column,'.',value)
      # create new dummy column for each value of the non_reference_values
      df[new_col_name] <- with(df, ifelse(df[,column] == value, 1, 0))
    }
    # delete the one hot encoded column
    df[column] = NULL
    
  }
  return(df)
}

wheat_analysis <- one_hot_encoding(wheat_RC_analysis, c("Soil Group", "fertilizer_type"))
View(wheat_analysis)

soil_fertilizer_var <- c("Soil Group.2", "Soil Group.3", "Soil Group.4", "Soil Group.5",
                         "fertilizer_type.Alzon", "fertilizer_type.Alzon+Piagran pro",
                         "fertilizer_type.CAN", "fertilizer_type.Piagran pro", 
                         "fertilizer_type.Piagran pro+Alzon", "fertilizer_type.Urea")

wheat_analysis <- wheat_analysis %>%
  mutate_at(vars(soil_fertilizer_var), as.factor)

Strategy_var <- c("urea", "NI", "UI", "UI_T1", "UI_T2", "UI_T3", "UI_T4", "UI_T5", "NI_T1", "NI_T2", 
                  "NI_T3", "NI_T4")

wheat_analysis <- wheat_analysis %>%
  mutate_at(vars(Strategy_var), as.factor)

View(wheat_analysis)



# Creating dataset with only one target variable in each dataset

## Yield

### RC_Pp_Yield

soil_wheat_RC_Pp_Yield <- data.frame(wheat_analysis[, c(1:23, 89:108, 109, 115:124)]) %>% na.exclude()
weather_wheat_RC_Pp_Yield <- data.frame(wheat_analysis[, c(24:108, 109, 119:124)]) %>% na.exclude()
soil_weather_wheat_RC_Pp_Yield <- wheat_analysis[, c(1:108, 109, 115:124)] %>% na.exclude()

dim(soil_wheat_RC_Pp_Yield)
dim(weather_wheat_RC_Pp_Yield)
dim(soil_weather_wheat_RC_Pp_Yield)

### RC_Pi46_Yield

soil_wheat_RC_Pi46_Yield <- wheat_analysis[, c(1:23, 89:108, 112, 115:124)] %>% na.exclude()
weather_wheat_RC_Pi46_Yield <- wheat_analysis[, c(24:108, 112, 119:124)] %>% na.exclude()
soil_weather_wheat_RC_Pi46_Yield <- wheat_analysis[, c(1:108, 112, 115:124)] %>% na.exclude()

dim(soil_wheat_RC_Pi46_Yield)
dim(weather_wheat_RC_Pi46_Yield)
dim(soil_weather_wheat_RC_Pi46_Yield)
view(soil_weather_wheat_RC_Pi46_Yield)

# Creating Training and Test Dataset based on Random Spatial Cross Validation

germany <- st_read("D:/ERM/Thesis/dataset/Principal Component Analysis/Germany.json", quiet = TRUE)

## Soil_Wheat_RC_Pp_Yield

spatial_soil_wheat_RC_Pp_Yield <- st_as_sf(soil_wheat_RC_Pp_Yield, coords = c("Longitude", "Latitude"),
                                           crs = st_crs(germany))


ggplot() +
  geom_sf(data = germany, fill = "white", color = "gray50") +
  geom_sf(data = spatial_soil_wheat_RC_Pp_Yield, color = "darkred", size = 2, alpha =0.6) +
  labs(title = "Sample Locations of Winter Wheat", x = "Longitude", y = "Latitude")


set.seed(123)
unique_locations_soil_wheat_RC_Pp_Yield <- unique(soil_wheat_RC_Pp_Yield[, c("Longitude", "Latitude")])

k=5

folds_soil_wheat_RC_Pp_Yield <- lapply(1:k, function(i) {
  test_indices_soil_wheat_RC_Pp_Yield <- sample(1:nrow(unique_locations_soil_wheat_RC_Pp_Yield), size = floor(nrow(unique_locations_soil_wheat_RC_Pp_Yield) / k))
  train_indices_soil_wheat_RC_Pp_Yield <- setdiff(1:nrow(unique_locations_soil_wheat_RC_Pp_Yield), test_indices_soil_wheat_RC_Pp_Yield)
  
  
  train_indices_soil_wheat_RC_Pp_Yield <- which(soil_wheat_RC_Pp_Yield$Longitude %in% unique_locations_soil_wheat_RC_Pp_Yield$Longitude[train_indices_soil_wheat_RC_Pp_Yield] &
                                                  soil_wheat_RC_Pp_Yield$Latitude %in% unique_locations_soil_wheat_RC_Pp_Yield$Latitude[train_indices_soil_wheat_RC_Pp_Yield])
  
  test_indices_soil_wheat_RC_Pp_Yield <- which(soil_wheat_RC_Pp_Yield$Longitude %in% unique_locations_soil_wheat_RC_Pp_Yield$Longitude[test_indices_soil_wheat_RC_Pp_Yield] &
                                                 soil_wheat_RC_Pp_Yield$Latitude %in% unique_locations_soil_wheat_RC_Pp_Yield$Latitude[test_indices_soil_wheat_RC_Pp_Yield])
  
  return(list(train_soil_wheat_RC_Pp_Yield = train_indices_soil_wheat_RC_Pp_Yield, test_soil_wheat_RC_Pp_Yield = test_indices_soil_wheat_RC_Pp_Yield))
})

plots_soil_wheat_RC_Pp_Yield <- list()

for (i in 1:k) {
  test_indices_soil_wheat_RC_Pp_Yield <- unlist(folds_soil_wheat_RC_Pp_Yield[[i]]$test_soil_wheat_RC_Pp_Yield)
  train_indices_soil_wheat_RC_Pp_Yield <- unlist(folds_soil_wheat_RC_Pp_Yield[[i]]$train_soil_wheat_RC_Pp_Yield)
  
  
  p_soil_wheat_RC_Pp_Yield <- ggplot() +
    geom_sf(data = germany, fill = "white", color = "gray50") +
    labs(title = paste("Fold", i), x = "Longitude", y = "Latitude")
  
  p_soil_wheat_RC_Pp_Yield <- p_soil_wheat_RC_Pp_Yield +
    geom_point(data = soil_wheat_RC_Pp_Yield[test_indices_soil_wheat_RC_Pp_Yield, ], aes(x = Longitude, y = Latitude), color = "red", size = 2) +
    geom_point(data = soil_wheat_RC_Pp_Yield[train_indices_soil_wheat_RC_Pp_Yield, ], aes(x = Longitude, y = Latitude), color = "blue", size = 2) 
    
  
  
   plots_soil_wheat_RC_Pp_Yield[[i]] <- p_soil_wheat_RC_Pp_Yield
   print (p_soil_wheat_RC_Pp_Yield)
   
}

plots_soil_wheat_RC_Pp_Yield[[1]] + plots_soil_wheat_RC_Pp_Yield[[2]] + plots_soil_wheat_RC_Pp_Yield[[3]] + 
  plots_soil_wheat_RC_Pp_Yield[[4]] + plots_soil_wheat_RC_Pp_Yield[[5]] + 
  plot_annotation(title = "Sample Locations of Soil Variable Models for Predicting RC_Pp_Yield")


set.seed(123)
for (fold_soil_wheat_RC_Pp_Yield in seq_along(folds_soil_wheat_RC_Pp_Yield)) {
  train_indices_soil_wheat_RC_Pp_Yield <- unlist(folds_soil_wheat_RC_Pp_Yield[[fold_soil_wheat_RC_Pp_Yield]]$train_soil_wheat_RC_Pp_Yield)
  test_indices_soil_wheat_RC_Pp_Yield <- unlist(folds_soil_wheat_RC_Pp_Yield[[fold_soil_wheat_RC_Pp_Yield]]$test_soil_wheat_RC_Pp_Yield)
  
  train_data_soil_wheat_RC_Pp_Yield <- soil_wheat_RC_Pp_Yield[train_indices_soil_wheat_RC_Pp_Yield, ]
  test_data_soil_wheat_RC_Pp_Yield <- soil_wheat_RC_Pp_Yield[test_indices_soil_wheat_RC_Pp_Yield, ]
  
  train_data_soil_wheat_RC_Pp_Yield$Latitude <- NULL
  train_data_soil_wheat_RC_Pp_Yield$Longitude <- NULL
  test_data_soil_wheat_RC_Pp_Yield$Latitude <- NULL
  test_data_soil_wheat_RC_Pp_Yield$Longitude <- NULL
  
  return()
}

dim(train_data_soil_wheat_RC_Pp_Yield)
dim(test_data_soil_wheat_RC_Pp_Yield)
view(train_data_soil_wheat_RC_Pp_Yield)
## weather_wheat_RC_Pp_Yield

set.seed(123)
unique_locations_weather_wheat_RC_Pp_Yield <- unique(weather_wheat_RC_Pp_Yield[, c("Longitude", "Latitude")])

k=5

folds_weather_wheat_RC_Pp_Yield <- lapply(1:k, function(i) {
  test_indices_weather_wheat_RC_Pp_Yield <- sample(1:nrow(unique_locations_weather_wheat_RC_Pp_Yield), size = floor(nrow(unique_locations_weather_wheat_RC_Pp_Yield) / k))
  train_indices_weather_wheat_RC_Pp_Yield <- setdiff(1:nrow(unique_locations_weather_wheat_RC_Pp_Yield), test_indices_weather_wheat_RC_Pp_Yield)
  
  
  train_indices_weather_wheat_RC_Pp_Yield <- which(weather_wheat_RC_Pp_Yield$Longitude %in% unique_locations_weather_wheat_RC_Pp_Yield$Longitude[train_indices_weather_wheat_RC_Pp_Yield] &
                                                     weather_wheat_RC_Pp_Yield$Latitude %in% unique_locations_weather_wheat_RC_Pp_Yield$Latitude[train_indices_weather_wheat_RC_Pp_Yield])
  
  test_indices_weather_wheat_RC_Pp_Yield <- which(weather_wheat_RC_Pp_Yield$Longitude %in% unique_locations_weather_wheat_RC_Pp_Yield$Longitude[test_indices_weather_wheat_RC_Pp_Yield] &
                                                    weather_wheat_RC_Pp_Yield$Latitude %in% unique_locations_weather_wheat_RC_Pp_Yield$Latitude[test_indices_weather_wheat_RC_Pp_Yield])
  
  return(list(train_weather_wheat_RC_Pp_Yield = train_indices_weather_wheat_RC_Pp_Yield, test_weather_wheat_RC_Pp_Yield = test_indices_weather_wheat_RC_Pp_Yield))
})

plots_weather_wheat_RC_Pp_Yield <- list()

for (i in 1:k) {
  test_indices_weather_wheat_RC_Pp_Yield <- unlist(folds_weather_wheat_RC_Pp_Yield[[i]]$test_weather_wheat_RC_Pp_Yield)
  train_indices_weather_wheat_RC_Pp_Yield <- unlist(folds_weather_wheat_RC_Pp_Yield[[i]]$train_weather_wheat_RC_Pp_Yield)
  
  
  p_weather_wheat_RC_Pp_Yield <- ggplot() +
    geom_sf(data = germany, fill = "white", color = "gray50") +
    labs(title = paste("Fold", i), x = "Longitude", y = "Latitude")
  
  p_weather_wheat_RC_Pp_Yield <- p_weather_wheat_RC_Pp_Yield +
    geom_point(data = weather_wheat_RC_Pp_Yield[test_indices_weather_wheat_RC_Pp_Yield, ], aes(x = Longitude, y = Latitude), color = "red", size = 2) +
    geom_point(data = weather_wheat_RC_Pp_Yield[train_indices_weather_wheat_RC_Pp_Yield, ], aes(x = Longitude, y = Latitude), color = "blue", size = 2) 
  
  
  
  plots_weather_wheat_RC_Pp_Yield[[i]] <- p_weather_wheat_RC_Pp_Yield
  print (p_weather_wheat_RC_Pp_Yield)
  
}

plots_weather_wheat_RC_Pp_Yield[[1]] + plots_weather_wheat_RC_Pp_Yield[[2]] + plots_weather_wheat_RC_Pp_Yield[[3]] + 
  plots_weather_wheat_RC_Pp_Yield[[4]] + plots_weather_wheat_RC_Pp_Yield[[5]] + 
  plot_annotation(title = "Sample Locations of Weather Variable Models for Predicting RC_Pp_Yield")


set.seed(123)
for (fold_weather_wheat_RC_Pp_Yield in seq_along(folds_weather_wheat_RC_Pp_Yield)) {
  train_indices_weather_wheat_RC_Pp_Yield <- unlist(folds_weather_wheat_RC_Pp_Yield[[fold_weather_wheat_RC_Pp_Yield]]$train_weather_wheat_RC_Pp_Yield)
  test_indices_weather_wheat_RC_Pp_Yield <- unlist(folds_weather_wheat_RC_Pp_Yield[[fold_weather_wheat_RC_Pp_Yield]]$test_weather_wheat_RC_Pp_Yield)
  
  train_data_weather_wheat_RC_Pp_Yield <- weather_wheat_RC_Pp_Yield[train_indices_weather_wheat_RC_Pp_Yield, ]
  test_data_weather_wheat_RC_Pp_Yield <- weather_wheat_RC_Pp_Yield[test_indices_weather_wheat_RC_Pp_Yield, ]
  
  train_data_weather_wheat_RC_Pp_Yield$Latitude <- NULL
  train_data_weather_wheat_RC_Pp_Yield$Longitude <- NULL
  test_data_weather_wheat_RC_Pp_Yield$Latitude <- NULL
  test_data_weather_wheat_RC_Pp_Yield$Longitude <- NULL
  
  return()
}


## soil_weather_wheat_RC_Pp_Yield


set.seed(123)
unique_locations_soil_weather_wheat_RC_Pp_Yield <- unique(soil_weather_wheat_RC_Pp_Yield[, c("Longitude", "Latitude")])

k=5

folds_soil_weather_wheat_RC_Pp_Yield <- lapply(1:k, function(i) {
  test_indices_soil_weather_wheat_RC_Pp_Yield <- sample(1:nrow(unique_locations_soil_weather_wheat_RC_Pp_Yield), size = floor(nrow(unique_locations_soil_weather_wheat_RC_Pp_Yield) / k))
  train_indices_soil_weather_wheat_RC_Pp_Yield <- setdiff(1:nrow(unique_locations_soil_weather_wheat_RC_Pp_Yield), test_indices_soil_weather_wheat_RC_Pp_Yield)
  
  
  train_indices_soil_weather_wheat_RC_Pp_Yield <- which(soil_weather_wheat_RC_Pp_Yield$Longitude %in% unique_locations_soil_weather_wheat_RC_Pp_Yield$Longitude[train_indices_soil_weather_wheat_RC_Pp_Yield] &
                                                          soil_weather_wheat_RC_Pp_Yield$Latitude %in% unique_locations_soil_weather_wheat_RC_Pp_Yield$Latitude[train_indices_soil_weather_wheat_RC_Pp_Yield])
  
  test_indices_soil_weather_wheat_RC_Pp_Yield <- which(soil_weather_wheat_RC_Pp_Yield$Longitude %in% unique_locations_soil_weather_wheat_RC_Pp_Yield$Longitude[test_indices_soil_weather_wheat_RC_Pp_Yield] &
                                                         soil_weather_wheat_RC_Pp_Yield$Latitude %in% unique_locations_soil_weather_wheat_RC_Pp_Yield$Latitude[test_indices_soil_weather_wheat_RC_Pp_Yield])
  
  return(list(train_soil_weather_wheat_RC_Pp_Yield = train_indices_soil_weather_wheat_RC_Pp_Yield, test_soil_weather_wheat_RC_Pp_Yield = test_indices_soil_weather_wheat_RC_Pp_Yield))
})

plots_soil_weather_wheat_RC_Pp_Yield <- list()

for (i in 1:k) {
  test_indices_soil_weather_wheat_RC_Pp_Yield <- unlist(folds_soil_weather_wheat_RC_Pp_Yield[[i]]$test_soil_weather_wheat_RC_Pp_Yield)
  train_indices_soil_weather_wheat_RC_Pp_Yield <- unlist(folds_soil_weather_wheat_RC_Pp_Yield[[i]]$train_soil_weather_wheat_RC_Pp_Yield)
  
  
  p_soil_weather_wheat_RC_Pp_Yield <- ggplot() +
    geom_sf(data = germany, fill = "white", color = "gray50") +
    labs(title = paste("Fold", i), x = "Longitude", y = "Latitude")
  
  p_soil_weather_wheat_RC_Pp_Yield <- p_soil_weather_wheat_RC_Pp_Yield +
    geom_point(data = soil_weather_wheat_RC_Pp_Yield[test_indices_soil_weather_wheat_RC_Pp_Yield, ], aes(x = Longitude, y = Latitude), color = "red", size = 2) +
    geom_point(data = soil_weather_wheat_RC_Pp_Yield[train_indices_soil_weather_wheat_RC_Pp_Yield, ], aes(x = Longitude, y = Latitude), color = "blue", size = 2) 
  
  
  
  plots_soil_weather_wheat_RC_Pp_Yield[[i]] <- p_soil_weather_wheat_RC_Pp_Yield
  print (p_soil_weather_wheat_RC_Pp_Yield)
  
}

plots_soil_weather_wheat_RC_Pp_Yield[[1]] + plots_soil_weather_wheat_RC_Pp_Yield[[2]] + plots_soil_weather_wheat_RC_Pp_Yield[[3]] + 
  plots_soil_weather_wheat_RC_Pp_Yield[[4]] + plots_soil_weather_wheat_RC_Pp_Yield[[5]] + 
  plot_annotation(title = "Sample Locations of Soil and Weather Variable Models for Predicting RC_Pp_Yield")


set.seed(123)
for (fold_soil_weather_wheat_RC_Pp_Yield in seq_along(folds_soil_weather_wheat_RC_Pp_Yield)) {
  train_indices_soil_weather_wheat_RC_Pp_Yield <- unlist(folds_soil_weather_wheat_RC_Pp_Yield[[fold_soil_weather_wheat_RC_Pp_Yield]]$train_soil_weather_wheat_RC_Pp_Yield)
  test_indices_soil_weather_wheat_RC_Pp_Yield <- unlist(folds_soil_weather_wheat_RC_Pp_Yield[[fold_soil_weather_wheat_RC_Pp_Yield]]$test_soil_weather_wheat_RC_Pp_Yield)
  
  train_data_soil_weather_wheat_RC_Pp_Yield <- soil_weather_wheat_RC_Pp_Yield[train_indices_soil_weather_wheat_RC_Pp_Yield, ]
  test_data_soil_weather_wheat_RC_Pp_Yield <- soil_weather_wheat_RC_Pp_Yield[test_indices_soil_weather_wheat_RC_Pp_Yield, ]
  
  train_data_soil_weather_wheat_RC_Pp_Yield$Latitude <- NULL
  train_data_soil_weather_wheat_RC_Pp_Yield$Longitude <- NULL
  test_data_soil_weather_wheat_RC_Pp_Yield$Latitude <- NULL
  test_data_soil_weather_wheat_RC_Pp_Yield$Longitude <- NULL
  
  return()
}


## Soil_Wheat_RC_Pi46_Yield

spatial_soil_wheat_RC_Pi46_Yield <- st_as_sf(soil_wheat_RC_Pi46_Yield, coords = c("Longitude", "Latitude"),
                                           crs = st_crs(germany))


ggplot() +
  geom_sf(data = germany, fill = "white", color = "gray50") +
  geom_sf(data = spatial_soil_wheat_RC_Pi46_Yield, color = "darkred", size = 2, alpha =0.6) +
  labs(title = "Sample Locations of Winter Wheat for Predicting RC_Pi46_Yield", x = "Longitude", y = "Latitude")

set.seed(123)
unique_locations_soil_wheat_RC_Pi46_Yield <- unique(soil_wheat_RC_Pi46_Yield[, c("Longitude", "Latitude")])

k=5

folds_soil_wheat_RC_Pi46_Yield <- lapply(1:k, function(i) {
  test_indices_soil_wheat_RC_Pi46_Yield <- sample(1:nrow(unique_locations_soil_wheat_RC_Pi46_Yield), size = floor(nrow(unique_locations_soil_wheat_RC_Pi46_Yield) / k))
  train_indices_soil_wheat_RC_Pi46_Yield <- setdiff(1:nrow(unique_locations_soil_wheat_RC_Pi46_Yield), test_indices_soil_wheat_RC_Pi46_Yield)
  
  
  train_indices_soil_wheat_RC_Pi46_Yield <- which(soil_wheat_RC_Pi46_Yield$Longitude %in% unique_locations_soil_wheat_RC_Pi46_Yield$Longitude[train_indices_soil_wheat_RC_Pi46_Yield] &
                                                  soil_wheat_RC_Pi46_Yield$Latitude %in% unique_locations_soil_wheat_RC_Pi46_Yield$Latitude[train_indices_soil_wheat_RC_Pi46_Yield])
  
  test_indices_soil_wheat_RC_Pi46_Yield <- which(soil_wheat_RC_Pi46_Yield$Longitude %in% unique_locations_soil_wheat_RC_Pi46_Yield$Longitude[test_indices_soil_wheat_RC_Pi46_Yield] &
                                                 soil_wheat_RC_Pi46_Yield$Latitude %in% unique_locations_soil_wheat_RC_Pi46_Yield$Latitude[test_indices_soil_wheat_RC_Pi46_Yield])
  
  return(list(train_soil_wheat_RC_Pi46_Yield = train_indices_soil_wheat_RC_Pi46_Yield, test_soil_wheat_RC_Pi46_Yield = test_indices_soil_wheat_RC_Pi46_Yield))
})

plots_soil_wheat_RC_Pi46_Yield <- list()

for (i in 1:k) {
  test_indices_soil_wheat_RC_Pi46_Yield <- unlist(folds_soil_wheat_RC_Pi46_Yield[[i]]$test_soil_wheat_RC_Pi46_Yield)
  train_indices_soil_wheat_RC_Pi46_Yield <- unlist(folds_soil_wheat_RC_Pi46_Yield[[i]]$train_soil_wheat_RC_Pi46_Yield)
  
  
  p_soil_wheat_RC_Pi46_Yield <- ggplot() +
    geom_sf(data = germany, fill = "white", color = "gray50") +
    labs(title = paste("Fold", i), x = "Longitude", y = "Latitude")
  
  p_soil_wheat_RC_Pi46_Yield <- p_soil_wheat_RC_Pi46_Yield +
    geom_point(data = soil_wheat_RC_Pi46_Yield[test_indices_soil_wheat_RC_Pi46_Yield, ], aes(x = Longitude, y = Latitude), color = "red", size = 2) +
    geom_point(data = soil_wheat_RC_Pi46_Yield[train_indices_soil_wheat_RC_Pi46_Yield, ], aes(x = Longitude, y = Latitude), color = "blue", size = 2) 
  
  
  
  plots_soil_wheat_RC_Pi46_Yield[[i]] <- p_soil_wheat_RC_Pi46_Yield
  print (p_soil_wheat_RC_Pi46_Yield)
  
}

plots_soil_wheat_RC_Pi46_Yield[[1]] + plots_soil_wheat_RC_Pi46_Yield[[2]] + plots_soil_wheat_RC_Pi46_Yield[[3]] + 
  plots_soil_wheat_RC_Pi46_Yield[[4]] + plots_soil_wheat_RC_Pi46_Yield[[5]] + 
  plot_annotation(title = "Sample Locations of Soil Variable Models for Predicting RC_Pi46_Yield")


set.seed(123)
for (fold_soil_wheat_RC_Pi46_Yield in seq_along(folds_soil_wheat_RC_Pi46_Yield)) {
  train_indices_soil_wheat_RC_Pi46_Yield <- unlist(folds_soil_wheat_RC_Pi46_Yield[[fold_soil_wheat_RC_Pi46_Yield]]$train_soil_wheat_RC_Pi46_Yield)
  test_indices_soil_wheat_RC_Pi46_Yield <- unlist(folds_soil_wheat_RC_Pi46_Yield[[fold_soil_wheat_RC_Pi46_Yield]]$test_soil_wheat_RC_Pi46_Yield)
  
  train_data_soil_wheat_RC_Pi46_Yield <- soil_wheat_RC_Pi46_Yield[train_indices_soil_wheat_RC_Pi46_Yield, ]
  test_data_soil_wheat_RC_Pi46_Yield <- soil_wheat_RC_Pi46_Yield[test_indices_soil_wheat_RC_Pi46_Yield, ]
  
  train_data_soil_wheat_RC_Pi46_Yield$Latitude <- NULL
  train_data_soil_wheat_RC_Pi46_Yield$Longitude <- NULL
  test_data_soil_wheat_RC_Pi46_Yield$Latitude <- NULL
  test_data_soil_wheat_RC_Pi46_Yield$Longitude <- NULL
  
  return()
}


## weather_wheat_RC_Pi46_Yield

set.seed(123)
unique_locations_weather_wheat_RC_Pi46_Yield <- unique(weather_wheat_RC_Pi46_Yield[, c("Longitude", "Latitude")])

k=5

folds_weather_wheat_RC_Pi46_Yield <- lapply(1:k, function(i) {
  test_indices_weather_wheat_RC_Pi46_Yield <- sample(1:nrow(unique_locations_weather_wheat_RC_Pi46_Yield), size = floor(nrow(unique_locations_weather_wheat_RC_Pi46_Yield) / k))
  train_indices_weather_wheat_RC_Pi46_Yield <- setdiff(1:nrow(unique_locations_weather_wheat_RC_Pi46_Yield), test_indices_weather_wheat_RC_Pi46_Yield)
  
  
  train_indices_weather_wheat_RC_Pi46_Yield <- which(weather_wheat_RC_Pi46_Yield$Longitude %in% unique_locations_weather_wheat_RC_Pi46_Yield$Longitude[train_indices_weather_wheat_RC_Pi46_Yield] &
                                                     weather_wheat_RC_Pi46_Yield$Latitude %in% unique_locations_weather_wheat_RC_Pi46_Yield$Latitude[train_indices_weather_wheat_RC_Pi46_Yield])
  
  test_indices_weather_wheat_RC_Pi46_Yield <- which(weather_wheat_RC_Pi46_Yield$Longitude %in% unique_locations_weather_wheat_RC_Pi46_Yield$Longitude[test_indices_weather_wheat_RC_Pi46_Yield] &
                                                    weather_wheat_RC_Pi46_Yield$Latitude %in% unique_locations_weather_wheat_RC_Pi46_Yield$Latitude[test_indices_weather_wheat_RC_Pi46_Yield])
  
  return(list(train_weather_wheat_RC_Pi46_Yield = train_indices_weather_wheat_RC_Pi46_Yield, test_weather_wheat_RC_Pi46_Yield = test_indices_weather_wheat_RC_Pi46_Yield))
})

plots_weather_wheat_RC_Pi46_Yield <- list()

for (i in 1:k) {
  test_indices_weather_wheat_RC_Pi46_Yield <- unlist(folds_weather_wheat_RC_Pi46_Yield[[i]]$test_weather_wheat_RC_Pi46_Yield)
  train_indices_weather_wheat_RC_Pi46_Yield <- unlist(folds_weather_wheat_RC_Pi46_Yield[[i]]$train_weather_wheat_RC_Pi46_Yield)
  
  
  p_weather_wheat_RC_Pi46_Yield <- ggplot() +
    geom_sf(data = germany, fill = "white", color = "gray50") +
    labs(title = paste("Fold", i), x = "Longitude", y = "Latitude")
  
  p_weather_wheat_RC_Pi46_Yield <- p_weather_wheat_RC_Pi46_Yield +
    geom_point(data = weather_wheat_RC_Pi46_Yield[test_indices_weather_wheat_RC_Pi46_Yield, ], aes(x = Longitude, y = Latitude), color = "red", size = 2) +
    geom_point(data = weather_wheat_RC_Pi46_Yield[train_indices_weather_wheat_RC_Pi46_Yield, ], aes(x = Longitude, y = Latitude), color = "blue", size = 2) 
  
  
  
  plots_weather_wheat_RC_Pi46_Yield[[i]] <- p_weather_wheat_RC_Pi46_Yield
  print (p_weather_wheat_RC_Pi46_Yield)
  
}

plots_weather_wheat_RC_Pi46_Yield[[1]] + plots_weather_wheat_RC_Pi46_Yield[[2]] + plots_weather_wheat_RC_Pi46_Yield[[3]] + 
  plots_weather_wheat_RC_Pi46_Yield[[4]] + plots_weather_wheat_RC_Pi46_Yield[[5]] + 
  plot_annotation(title = "Sample Locations of Weather Variable Models for Predicting RC_Pi46_Yield")


set.seed(123)
for (fold_weather_wheat_RC_Pi46_Yield in seq_along(folds_weather_wheat_RC_Pi46_Yield)) {
  train_indices_weather_wheat_RC_Pi46_Yield <- unlist(folds_weather_wheat_RC_Pi46_Yield[[fold_weather_wheat_RC_Pi46_Yield]]$train_weather_wheat_RC_Pi46_Yield)
  test_indices_weather_wheat_RC_Pi46_Yield <- unlist(folds_weather_wheat_RC_Pi46_Yield[[fold_weather_wheat_RC_Pi46_Yield]]$test_weather_wheat_RC_Pi46_Yield)
  
  train_data_weather_wheat_RC_Pi46_Yield <- weather_wheat_RC_Pi46_Yield[train_indices_weather_wheat_RC_Pi46_Yield, ]
  test_data_weather_wheat_RC_Pi46_Yield <- weather_wheat_RC_Pi46_Yield[test_indices_weather_wheat_RC_Pi46_Yield, ]
  
  train_data_weather_wheat_RC_Pi46_Yield$Latitude <- NULL
  train_data_weather_wheat_RC_Pi46_Yield$Longitude <- NULL
  test_data_weather_wheat_RC_Pi46_Yield$Latitude <- NULL
  test_data_weather_wheat_RC_Pi46_Yield$Longitude <- NULL
  
  return()
}


## soil_weather_wheat_RC_Pi46_Yield

set.seed(123)
unique_locations_soil_weather_wheat_RC_Pi46_Yield <- unique(soil_weather_wheat_RC_Pi46_Yield[, c("Longitude", "Latitude")])

k=5

folds_soil_weather_wheat_RC_Pi46_Yield <- lapply(1:k, function(i) {
  test_indices_soil_weather_wheat_RC_Pi46_Yield <- sample(1:nrow(unique_locations_soil_weather_wheat_RC_Pi46_Yield), size = floor(nrow(unique_locations_soil_weather_wheat_RC_Pi46_Yield) / k))
  train_indices_soil_weather_wheat_RC_Pi46_Yield <- setdiff(1:nrow(unique_locations_soil_weather_wheat_RC_Pi46_Yield), test_indices_soil_weather_wheat_RC_Pi46_Yield)
  
  
  train_indices_soil_weather_wheat_RC_Pi46_Yield <- which(soil_weather_wheat_RC_Pi46_Yield$Longitude %in% unique_locations_soil_weather_wheat_RC_Pi46_Yield$Longitude[train_indices_soil_weather_wheat_RC_Pi46_Yield] &
                                                          soil_weather_wheat_RC_Pi46_Yield$Latitude %in% unique_locations_soil_weather_wheat_RC_Pi46_Yield$Latitude[train_indices_soil_weather_wheat_RC_Pi46_Yield])
  
  test_indices_soil_weather_wheat_RC_Pi46_Yield <- which(soil_weather_wheat_RC_Pi46_Yield$Longitude %in% unique_locations_soil_weather_wheat_RC_Pi46_Yield$Longitude[test_indices_soil_weather_wheat_RC_Pi46_Yield] &
                                                         soil_weather_wheat_RC_Pi46_Yield$Latitude %in% unique_locations_soil_weather_wheat_RC_Pi46_Yield$Latitude[test_indices_soil_weather_wheat_RC_Pi46_Yield])
  
  return(list(train_soil_weather_wheat_RC_Pi46_Yield = train_indices_soil_weather_wheat_RC_Pi46_Yield, test_soil_weather_wheat_RC_Pi46_Yield = test_indices_soil_weather_wheat_RC_Pi46_Yield))
})

plots_soil_weather_wheat_RC_Pi46_Yield <- list()

for (i in 1:k) {
  test_indices_soil_weather_wheat_RC_Pi46_Yield <- unlist(folds_soil_weather_wheat_RC_Pi46_Yield[[i]]$test_soil_weather_wheat_RC_Pi46_Yield)
  train_indices_soil_weather_wheat_RC_Pi46_Yield <- unlist(folds_soil_weather_wheat_RC_Pi46_Yield[[i]]$train_soil_weather_wheat_RC_Pi46_Yield)
  
  
  p_soil_weather_wheat_RC_Pi46_Yield <- ggplot() +
    geom_sf(data = germany, fill = "white", color = "gray50") + scale_x_continuous(labels = function(x) paste0(x, '\u00B0', "E")) +
    scale_y_continuous(labels = function(x) paste0(x, '\u00B0', "N")) +
    labs(title = paste("Fold", i), x = "Longitude", y = "Latitude") +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.02, "in"), pad_y = unit(0.15, "in"),
                           style = north_arrow_fancy_orienteering)
  
  
  p_soil_weather_wheat_RC_Pi46_Yield <- p_soil_weather_wheat_RC_Pi46_Yield +
    geom_point(data = soil_weather_wheat_RC_Pi46_Yield[test_indices_soil_weather_wheat_RC_Pi46_Yield, ], aes(x = Longitude, y = Latitude), color = "red", size = 2) +
    geom_point(data = soil_weather_wheat_RC_Pi46_Yield[train_indices_soil_weather_wheat_RC_Pi46_Yield, ], aes(x = Longitude, y = Latitude), color = "blue", size = 2) 
  
  
  
  plots_soil_weather_wheat_RC_Pi46_Yield[[i]] <- p_soil_weather_wheat_RC_Pi46_Yield 
  print (p_soil_weather_wheat_RC_Pi46_Yield)
  
}

plots_soil_weather_wheat_RC_Pi46_Yield[[1]] + plots_soil_weather_wheat_RC_Pi46_Yield[[2]] + plots_soil_weather_wheat_RC_Pi46_Yield[[3]] + 
  plots_soil_weather_wheat_RC_Pi46_Yield[[4]] + plots_soil_weather_wheat_RC_Pi46_Yield[[5]] + 
  plot_annotation(title = "Sample Locations of Soil and Weather Variable Models for Predicting RC_Pi46_Yield")


set.seed(123)
for (fold_soil_weather_wheat_RC_Pi46_Yield in seq_along(folds_soil_weather_wheat_RC_Pi46_Yield)) {
  train_indices_soil_weather_wheat_RC_Pi46_Yield <- unlist(folds_soil_weather_wheat_RC_Pi46_Yield[[fold_soil_weather_wheat_RC_Pi46_Yield]]$train_soil_weather_wheat_RC_Pi46_Yield)
  test_indices_soil_weather_wheat_RC_Pi46_Yield <- unlist(folds_soil_weather_wheat_RC_Pi46_Yield[[fold_soil_weather_wheat_RC_Pi46_Yield]]$test_soil_weather_wheat_RC_Pi46_Yield)
  
  train_data_soil_weather_wheat_RC_Pi46_Yield <- soil_weather_wheat_RC_Pi46_Yield[train_indices_soil_weather_wheat_RC_Pi46_Yield, ]
  test_data_soil_weather_wheat_RC_Pi46_Yield <- soil_weather_wheat_RC_Pi46_Yield[test_indices_soil_weather_wheat_RC_Pi46_Yield, ]
  
  train_data_soil_weather_wheat_RC_Pi46_Yield$Latitude <- NULL
  train_data_soil_weather_wheat_RC_Pi46_Yield$Longitude <- NULL
  test_data_soil_weather_wheat_RC_Pi46_Yield$Latitude <- NULL
  test_data_soil_weather_wheat_RC_Pi46_Yield$Longitude <- NULL
  
  return()
}


# Building Models

lambda <- 10^seq(-3, 3, length = 100)
model_control <- trainControl(method = "cv", number = 5)
control_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

## Yield

### RC_Pp_Yield


set.seed(123)
ridge_soil_wheat_RC_Pp_Yield <- train(RC_Pp_Yield ~.,
                                      data = train_data_soil_wheat_RC_Pp_Yield,
                                      method = "glmnet",
                                      trcontrol = model_control,
                                      tuneGrid = expand.grid(alpha = 0, lambda = lambda),
                                      preProcess = 'scale')

print(ridge_soil_wheat_RC_Pp_Yield)

ridge_soil_wheat_RC_Pp_Yield$finalModel$xNames

set.seed(123)
lasso_soil_wheat_RC_Pp_Yield <- train(RC_Pp_Yield ~.,
                                      data = train_data_soil_wheat_RC_Pp_Yield,
                                      method = "glmnet",
                                      trcontrol = model_control,
                                      tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                                      preProcess = 'scale')
print(lasso_soil_wheat_RC_Pp_Yield)

lasso_soil_wheat_RC_Pp_Yield$finalModel$xNames

set.seed(123)
rfe_soil_wheat_RC_Pp_Yield <- rfe(RC_Pp_Yield ~.,
                                  data = train_data_soil_wheat_RC_Pp_Yield,
                                  sizes = c(1:51),
                                  rfeControl = control_rfe,
                                  preProcess = 'scale')
print(rfe_soil_wheat_RC_Pp_Yield)



##### RC_Pi46_Yield

set.seed(123)
ridge_soil_wheat_RC_Pi46_Yield <- train(RC_Pi46_Yield ~.,
                                        data = train_data_soil_wheat_RC_Pi46_Yield,
                                        method = "glmnet",
                                        trcontrol = model_control,
                                        tuneGrid = expand.grid(alpha = 0, lambda = lambda),
                                        preProcess = 'scale')

set.seed(123)
lasso_soil_wheat_RC_Pi46_Yield <- train(RC_Pi46_Yield ~.,
                                        data = train_data_soil_wheat_RC_Pi46_Yield,
                                        method = "glmnet",
                                        trcontrol = model_control,
                                        tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                                        preProcess = 'scale')
set.seed(123)
rfe_soil_wheat_RC_Pi46_Yield <- rfe(RC_Pi46_Yield ~.,
                                    data = train_data_soil_wheat_RC_Pi46_Yield,
                                    sizes = c(1:51),
                                    rfeControl = control_rfe,
                                    preProcess = 'scale')
print(rfe_soil_wheat_RC_Pp_Yield)

plot(rfe_soil_wheat_RC_Pp_Yield)

#### Weather Variables

##### RC_Pp_Yield

set.seed(123)
ridge_weather_wheat_RC_Pp_Yield <- train(RC_Pp_Yield ~.,
                                         data = train_data_weather_wheat_RC_Pp_Yield,
                                         method = "glmnet",
                                         trcontrol = model_control,
                                         tuneGrid = expand.grid(alpha = 0, lambda = lambda),
                                         preProcess = 'scale')


set.seed(123)
lasso_weather_wheat_RC_Pp_Yield <- train(RC_Pp_Yield ~.,
                                         data = train_data_weather_wheat_RC_Pp_Yield,
                                         method = "glmnet",
                                         trcontrol = model_control,
                                         tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                                         preProcess = 'scale')
print(lasso_weather_wheat_RC_Pp_Yield)

set.seed(123)
rfe_weather_wheat_RC_Pp_Yield <- rfe(RC_Pp_Yield ~.,
                                     data = train_data_weather_wheat_RC_Pp_Yield,
                                     sizes = c(1:89),
                                     rfeControl = control_rfe,
                                     preProcess = 'scale')
print(rfe_weather_wheat_RC_Pp_Yield)


##### RC_Pi46_Yield

set.seed(123)
ridge_weather_wheat_RC_Pi46_Yield <- train(RC_Pi46_Yield ~.,
                                           data = train_data_weather_wheat_RC_Pi46_Yield,
                                           method = "glmnet",
                                           trcontrol = model_control,
                                           tuneGrid = expand.grid(alpha = 0, lambda = lambda),
                                           preProcess = 'scale')


set.seed(123)
lasso_weather_wheat_RC_Pi46_Yield <- train(RC_Pi46_Yield ~.,
                                           data = train_data_weather_wheat_RC_Pi46_Yield,
                                           method = "glmnet",
                                           trcontrol = model_control,
                                           tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                                           preProcess = 'scale')
set.seed(123)
rfe_weather_wheat_RC_Pi46_Yield <- rfe(RC_Pi46_Yield ~.,
                                       data = train_data_weather_wheat_RC_Pi46_Yield,
                                       sizes = c(1:89),
                                       rfeControl = control_rfe,
                                       preProcess = 'scale')
print(rfe_weather_wheat_RC_Pi46_Yield)

#### soil and Weather Variables

##### RC_Pp_Yield

set.seed(123)
ridge_soil_weather_wheat_RC_Pp_Yield <- train(RC_Pp_Yield ~.,
                                              data = train_data_soil_weather_wheat_RC_Pp_Yield,
                                              method = "glmnet",
                                              trcontrol = model_control,
                                              tuneGrid = expand.grid(alpha = 0, lambda = lambda),
                                              preProcess = 'scale')


set.seed(123)
lasso_soil_weather_wheat_RC_Pp_Yield <- train(RC_Pp_Yield ~.,
                                              data = train_data_soil_weather_wheat_RC_Pp_Yield,
                                              method = "glmnet",
                                              trcontrol = model_control,
                                              tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                                              preProcess = 'scale')
print(lasso_soil_weather_wheat_RC_Pp_Yield)

set.seed(123)
rfe_soil_weather_wheat_RC_Pp_Yield <- rfe(RC_Pp_Yield ~.,
                                          data = train_data_soil_weather_wheat_RC_Pp_Yield,
                                          sizes = c(1:116),
                                          rfeControl = control_rfe,
                                          preProcess = 'scale')
print(rfe_soil_weather_wheat_RC_Pp_Yield)

##### RC_Pi46_Yield

set.seed(123)
ridge_soil_weather_wheat_RC_Pi46_Yield <- train(RC_Pi46_Yield ~.,
                                                data = train_data_soil_weather_wheat_RC_Pi46_Yield,
                                                method = "glmnet",
                                                trcontrol = model_control,
                                                tuneGrid = expand.grid(alpha = 0, lambda = lambda),
                                                preProcess = 'scale')

print(ridge_soil_weather_wheat_RC_Pi46_Yield)
set.seed(123)
lasso_soil_weather_wheat_RC_Pi46_Yield <- train(RC_Pi46_Yield ~.,
                                                data = train_data_soil_weather_wheat_RC_Pi46_Yield,
                                                method = "glmnet",
                                                trcontrol = model_control,
                                                tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                                                preProcess = 'scale')
print(lasso_soil_weather_wheat_RC_Pi46_Yield)

set.seed(123)
rfe_soil_weather_wheat_RC_Pi46_Yield <- rfe(RC_Pi46_Yield ~.,
                                            data = train_data_soil_weather_wheat_RC_Pi46_Yield,
                                            sizes = c(1:116),
                                            rfeControl = control_rfe,
                                            preProcess = 'scale')
print(rfe_soil_weather_wheat_RC_Pi46_Yield)

view(train_data_soil_weather_wheat_RC_Pi46_Yield)

rfe_soil_weather_wheat_RC_Pi46_Yield$optVariables

# Model Performance

## Soil Variables Models

### RC_Pp_Yield

predictions_ridge_soil_wheat_RC_Pp_Yield <- predict(ridge_soil_wheat_RC_Pp_Yield, test_data_soil_wheat_RC_Pp_Yield)

ridge_soil_wheat_RC_Pp_Yield_R2 <- R2(predictions_ridge_soil_wheat_RC_Pp_Yield, test_data_soil_wheat_RC_Pp_Yield$RC_Pp_Yield)
ridge_soil_wheat_RC_Pp_Yield_RMSE <- RMSE(predictions_ridge_soil_wheat_RC_Pp_Yield, test_data_soil_wheat_RC_Pp_Yield$RC_Pp_Yield)


predictions_lasso_soil_wheat_RC_Pp_Yield <- predict(lasso_soil_wheat_RC_Pp_Yield, test_data_soil_wheat_RC_Pp_Yield)

lasso_soil_wheat_RC_Pp_Yield_R2 <- R2(predictions_lasso_soil_wheat_RC_Pp_Yield, test_data_soil_wheat_RC_Pp_Yield$RC_Pp_Yield)
lasso_soil_wheat_RC_Pp_Yield_RMSE <- RMSE(predictions_lasso_soil_wheat_RC_Pp_Yield, test_data_soil_wheat_RC_Pp_Yield$RC_Pp_Yield)


predictions_rfe_soil_wheat_RC_Pp_Yield <- predict(rfe_soil_wheat_RC_Pp_Yield, test_data_soil_wheat_RC_Pp_Yield)

rfe_soil_wheat_RC_Pp_Yield_R2 <- R2(predictions_rfe_soil_wheat_RC_Pp_Yield, test_data_soil_wheat_RC_Pp_Yield$RC_Pp_Yield)
rfe_soil_wheat_RC_Pp_Yield_RMSE <- RMSE(predictions_rfe_soil_wheat_RC_Pp_Yield, test_data_soil_wheat_RC_Pp_Yield$RC_Pp_Yield)

##### Creating dataframe of R2 and RMSE and making plots

R2_soil_wheat_RC_Pp_Yield <- data.frame(r2 = c(ridge_soil_wheat_RC_Pp_Yield_R2,lasso_soil_wheat_RC_Pp_Yield_R2,rfe_soil_wheat_RC_Pp_Yield_R2), 
                                        algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                          factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


RMSE_soil_wheat_RC_Pp_Yield <- data.frame(rmse = c(ridge_soil_wheat_RC_Pp_Yield_RMSE,lasso_soil_wheat_RC_Pp_Yield_RMSE,rfe_soil_wheat_RC_Pp_Yield_RMSE), 
                                          algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                            factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


Fig_R2_soil_wheat_RC_Pp_Yield <- 
  ggplot(R2_soil_wheat_RC_Pp_Yield, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("R-squared") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(r2, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  theme_bw() + labs(title="Soil Variable Model for Predicting RC_Pp_Yield") +
  theme(legend.position = "none")

Fig_R2_soil_wheat_RC_Pp_Yield

Fig_RMSE_soil_wheat_RC_Pp_Yield <- 
  ggplot(RMSE_soil_wheat_RC_Pp_Yield, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("RMSE") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(rmse, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  theme_bw() + labs(title="Soil Variable Model for Predicting RC_Pp_Yield") +
  theme(legend.position = "none")

Fig_RMSE_soil_wheat_RC_Pp_Yield

##### RC_Pi46_Yield

predictions_ridge_soil_wheat_RC_Pi46_Yield <- predict(ridge_soil_wheat_RC_Pi46_Yield, test_data_soil_wheat_RC_Pi46_Yield)

ridge_soil_wheat_RC_Pi46_Yield_R2 <- R2(predictions_ridge_soil_wheat_RC_Pi46_Yield, test_data_soil_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
ridge_soil_wheat_RC_Pi46_Yield_RMSE <- RMSE(predictions_ridge_soil_wheat_RC_Pi46_Yield, test_data_soil_wheat_RC_Pi46_Yield$RC_Pi46_Yield)


predictions_lasso_soil_wheat_RC_Pi46_Yield <- predict(lasso_soil_wheat_RC_Pi46_Yield, test_data_soil_wheat_RC_Pi46_Yield)

lasso_soil_wheat_RC_Pi46_Yield_R2 <- R2(predictions_lasso_soil_wheat_RC_Pi46_Yield, test_data_soil_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
lasso_soil_wheat_RC_Pi46_Yield_RMSE <- RMSE(predictions_lasso_soil_wheat_RC_Pi46_Yield, test_data_soil_wheat_RC_Pi46_Yield$RC_Pi46_Yield)


predictions_rfe_soil_wheat_RC_Pi46_Yield <- predict(rfe_soil_wheat_RC_Pi46_Yield, test_data_soil_wheat_RC_Pi46_Yield)

rfe_soil_wheat_RC_Pi46_Yield_R2 <- R2(predictions_rfe_soil_wheat_RC_Pi46_Yield, test_data_soil_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
rfe_soil_wheat_RC_Pi46_Yield_RMSE <- RMSE(predictions_rfe_soil_wheat_RC_Pi46_Yield, test_data_soil_wheat_RC_Pi46_Yield$RC_Pi46_Yield)


##### Creating dataframe of R2 and RMSE and making plots

R2_soil_wheat_RC_Pi46_Yield <- data.frame(r2 = c(ridge_soil_wheat_RC_Pi46_Yield_R2,lasso_soil_wheat_RC_Pi46_Yield_R2,rfe_soil_wheat_RC_Pi46_Yield_R2), 
                                          algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                            factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


RMSE_soil_wheat_RC_Pi46_Yield <- data.frame(rmse = c(ridge_soil_wheat_RC_Pi46_Yield_RMSE,lasso_soil_wheat_RC_Pi46_Yield_RMSE,rfe_soil_wheat_RC_Pi46_Yield_RMSE), 
                                            algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                              factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


Fig_R2_soil_wheat_RC_Pi46_Yield <- 
  ggplot(R2_soil_wheat_RC_Pi46_Yield, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("R-squared") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(r2, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  labs(title = "Soil Variable Model for Predicting RC_Pi46_Yield") +
  theme_bw() +
  theme(legend.position = "none")

Fig_R2_soil_wheat_RC_Pi46_Yield

Fig_RMSE_soil_wheat_RC_Pi46_Yield <- 
  ggplot(RMSE_soil_wheat_RC_Pi46_Yield, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("RMSE") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(rmse, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  labs(title = "Soil Variable Model for Predicting RC_Pi46_Yield") +
  theme_bw() +
  theme(legend.position = "none")
Fig_RMSE_soil_wheat_RC_Pi46_Yield

#### Weather Variables Models

##### RC_Pp_Yield

predictions_ridge_weather_wheat_RC_Pp_Yield <- predict(ridge_weather_wheat_RC_Pp_Yield,  test_data_weather_wheat_RC_Pp_Yield)

ridge_weather_wheat_RC_Pp_Yield_R2 <- R2(predictions_ridge_weather_wheat_RC_Pp_Yield,  test_data_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)
ridge_weather_wheat_RC_Pp_Yield_RMSE <- RMSE(predictions_ridge_weather_wheat_RC_Pp_Yield,  test_data_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)


predictions_lasso_weather_wheat_RC_Pp_Yield <- predict(lasso_weather_wheat_RC_Pp_Yield,  test_data_weather_wheat_RC_Pp_Yield)

lasso_weather_wheat_RC_Pp_Yield_R2 <- R2(predictions_lasso_weather_wheat_RC_Pp_Yield,  test_data_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)
lasso_weather_wheat_RC_Pp_Yield_RMSE <- RMSE(predictions_lasso_weather_wheat_RC_Pp_Yield,  test_data_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)


predictions_rfe_weather_wheat_RC_Pp_Yield <- predict(rfe_weather_wheat_RC_Pp_Yield,  test_data_weather_wheat_RC_Pp_Yield)

rfe_weather_wheat_RC_Pp_Yield_R2 <- R2(predictions_rfe_weather_wheat_RC_Pp_Yield,  test_data_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)
rfe_weather_wheat_RC_Pp_Yield_RMSE <- RMSE(predictions_rfe_weather_wheat_RC_Pp_Yield,  test_data_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)

##### Creating dataframe of R2 and RMSE and making plots

R2_weather_wheat_RC_Pp_Yield <- data.frame(r2 = c(ridge_weather_wheat_RC_Pp_Yield_R2,lasso_weather_wheat_RC_Pp_Yield_R2,rfe_weather_wheat_RC_Pp_Yield_R2), 
                                           algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                             factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


RMSE_weather_wheat_RC_Pp_Yield <- data.frame(rmse = c(ridge_weather_wheat_RC_Pp_Yield_RMSE,lasso_weather_wheat_RC_Pp_Yield_RMSE,rfe_weather_wheat_RC_Pp_Yield_RMSE), 
                                             algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                               factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


Fig_R2_weather_wheat_RC_Pp_Yield <- 
  ggplot(R2_weather_wheat_RC_Pp_Yield, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("R-squared") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(r2, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  labs(title = "Weather Variable Model for Predicting RC_Pp_Yield") +
  theme_bw() +
  theme(legend.position = "none")
Fig_R2_weather_wheat_RC_Pp_Yield

Fig_RMSE_weather_wheat_RC_Pp_Yield <- 
  ggplot(RMSE_weather_wheat_RC_Pp_Yield, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("RMSE") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(rmse, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  labs(title = "Weather Variable Model for Predicting RC_Pp_Yield") +
  theme_bw() + 
  theme(legend.position = "none")
Fig_RMSE_weather_wheat_RC_Pp_Yield

##### RC_Pi46_Yield

predictions_ridge_weather_wheat_RC_Pi46_Yield <- predict(ridge_weather_wheat_RC_Pi46_Yield, test_data_weather_wheat_RC_Pi46_Yield)

ridge_weather_wheat_RC_Pi46_Yield_R2 <- R2(predictions_ridge_weather_wheat_RC_Pi46_Yield, test_data_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
ridge_weather_wheat_RC_Pi46_Yield_RMSE <- RMSE(predictions_ridge_weather_wheat_RC_Pi46_Yield, test_data_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)


predictions_lasso_weather_wheat_RC_Pi46_Yield <- predict(lasso_weather_wheat_RC_Pi46_Yield, test_data_weather_wheat_RC_Pi46_Yield)

lasso_weather_wheat_RC_Pi46_Yield_R2 <- R2(predictions_lasso_weather_wheat_RC_Pi46_Yield, test_data_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
lasso_weather_wheat_RC_Pi46_Yield_RMSE <- RMSE(predictions_lasso_weather_wheat_RC_Pi46_Yield, test_data_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)


predictions_rfe_weather_wheat_RC_Pi46_Yield <- predict(rfe_weather_wheat_RC_Pi46_Yield, test_data_weather_wheat_RC_Pi46_Yield)

rfe_weather_wheat_RC_Pi46_Yield_R2 <- R2(predictions_rfe_weather_wheat_RC_Pi46_Yield, test_data_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
rfe_weather_wheat_RC_Pi46_Yield_RMSE <- RMSE(predictions_rfe_weather_wheat_RC_Pi46_Yield, test_data_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)

##### Creating dataframe of R2 and RMSE and making plots

R2_weather_wheat_RC_Pi46_Yield <- data.frame(r2 = c(ridge_weather_wheat_RC_Pi46_Yield_R2,lasso_weather_wheat_RC_Pi46_Yield_R2,rfe_weather_wheat_RC_Pi46_Yield_R2), 
                                             algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                               factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


RMSE_weather_wheat_RC_Pi46_Yield <- data.frame(rmse = c(ridge_weather_wheat_RC_Pi46_Yield_RMSE,lasso_weather_wheat_RC_Pi46_Yield_RMSE,rfe_weather_wheat_RC_Pi46_Yield_RMSE), 
                                               algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                                 factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


Fig_R2_weather_wheat_RC_Pi46_Yield <- 
  ggplot(R2_weather_wheat_RC_Pi46_Yield, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("R-squared") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(r2, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  labs(title = "Weather Variable Model for Predicting RC_Pi46_Yield") +
  theme_bw() +
  theme(legend.position = "none")

Fig_R2_weather_wheat_RC_Pi46_Yield

Fig_RMSE_weather_wheat_RC_Pi46_Yield <- 
  ggplot(RMSE_weather_wheat_RC_Pi46_Yield, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("RMSE") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(rmse, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  labs(title = "Weather Variable Model for Predicting RC_Pi46_Yield") +
  theme_bw() +
  theme(legend.position = "none")

Fig_RMSE_weather_wheat_RC_Pi46_Yield

#### Soil and Weather Variables Models

##### RC_Pp_Yield

predictions_ridge_soil_weather_wheat_RC_Pp_Yield <- predict(ridge_soil_weather_wheat_RC_Pp_Yield, test_data_soil_weather_wheat_RC_Pp_Yield)

ridge_soil_weather_wheat_RC_Pp_Yield_R2 <- R2(predictions_ridge_soil_weather_wheat_RC_Pp_Yield, test_data_soil_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)
ridge_soil_weather_wheat_RC_Pp_Yield_RMSE <- RMSE(predictions_ridge_soil_weather_wheat_RC_Pp_Yield, test_data_soil_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)


predictions_lasso_soil_weather_wheat_RC_Pp_Yield <- predict(lasso_soil_weather_wheat_RC_Pp_Yield, test_data_soil_weather_wheat_RC_Pp_Yield)

lasso_soil_weather_wheat_RC_Pp_Yield_R2 <- R2(predictions_lasso_soil_weather_wheat_RC_Pp_Yield, test_data_soil_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)
lasso_soil_weather_wheat_RC_Pp_Yield_RMSE <- RMSE(predictions_lasso_soil_weather_wheat_RC_Pp_Yield, test_data_soil_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)


predictions_rfe_soil_weather_wheat_RC_Pp_Yield <- predict(rfe_soil_weather_wheat_RC_Pp_Yield, test_data_soil_weather_wheat_RC_Pp_Yield)

rfe_soil_weather_wheat_RC_Pp_Yield_R2 <- R2(predictions_rfe_soil_weather_wheat_RC_Pp_Yield, test_data_soil_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)
rfe_soil_weather_wheat_RC_Pp_Yield_RMSE <- RMSE(predictions_rfe_soil_weather_wheat_RC_Pp_Yield, test_data_soil_weather_wheat_RC_Pp_Yield$RC_Pp_Yield)

##### Creating dataframe of R2 and RMSE and making plots

R2_soil_weather_wheat_RC_Pp_Yield <- data.frame(r2 = c(ridge_soil_weather_wheat_RC_Pp_Yield_R2,lasso_soil_weather_wheat_RC_Pp_Yield_R2,rfe_soil_weather_wheat_RC_Pp_Yield_R2), 
                                                algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                                  factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


RMSE_soil_weather_wheat_RC_Pp_Yield <- data.frame(rmse = c(ridge_soil_weather_wheat_RC_Pp_Yield_RMSE,lasso_soil_weather_wheat_RC_Pp_Yield_RMSE,rfe_soil_weather_wheat_RC_Pp_Yield_RMSE), 
                                                  algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                                    factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


Fig_R2_soil_weather_wheat_RC_Pp_Yield <- 
  ggplot(R2_soil_weather_wheat_RC_Pp_Yield, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("R-squared") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(r2, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  labs(title = "Soil and Weather Variable Model for Predicting RC_Pp_Yield") +
  theme_bw() +
  theme(legend.position = "none")
Fig_R2_soil_weather_wheat_RC_Pp_Yield

Fig_RMSE_soil_weather_wheat_RC_Pp_Yield <- 
  ggplot(RMSE_soil_weather_wheat_RC_Pp_Yield, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("RMSE") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(rmse, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  labs(title = "Soil and Weather Variable Model for Predicting RC_Pp_Yield") +
  theme_bw() +
  theme(legend.position = "none")
Fig_RMSE_soil_weather_wheat_RC_Pp_Yield

##### RC_Pi46_Yield

predictions_ridge_soil_weather_wheat_RC_Pi46_Yield <- predict(ridge_soil_weather_wheat_RC_Pi46_Yield, test_data_soil_weather_wheat_RC_Pi46_Yield)

ridge_soil_weather_wheat_RC_Pi46_Yield_R2 <- R2(predictions_ridge_soil_weather_wheat_RC_Pi46_Yield, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
ridge_soil_weather_wheat_RC_Pi46_Yield_RMSE <- RMSE(predictions_ridge_soil_weather_wheat_RC_Pi46_Yield, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)


predictions_lasso_soil_weather_wheat_RC_Pi46_Yield <- predict(lasso_soil_weather_wheat_RC_Pi46_Yield, test_data_soil_weather_wheat_RC_Pi46_Yield)

lasso_soil_weather_wheat_RC_Pi46_Yield_R2 <- R2(predictions_lasso_soil_weather_wheat_RC_Pi46_Yield, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
lasso_soil_weather_wheat_RC_Pi46_Yield_RMSE <- RMSE(predictions_lasso_soil_weather_wheat_RC_Pi46_Yield, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)


predictions_rfe_soil_weather_wheat_RC_Pi46_Yield <- predict(rfe_soil_weather_wheat_RC_Pi46_Yield, test_data_soil_weather_wheat_RC_Pi46_Yield)

rfe_soil_weather_wheat_RC_Pi46_Yield_R2 <- R2(predictions_rfe_soil_weather_wheat_RC_Pi46_Yield, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
rfe_soil_weather_wheat_RC_Pi46_Yield_RMSE <- RMSE(predictions_rfe_soil_weather_wheat_RC_Pi46_Yield, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)

##### Creating dataframe of R2 and RMSE and making plots

R2_soil_weather_wheat_RC_Pi46_Yield <- data.frame(r2 = c(ridge_soil_weather_wheat_RC_Pi46_Yield_R2,lasso_soil_weather_wheat_RC_Pi46_Yield_R2,rfe_soil_weather_wheat_RC_Pi46_Yield_R2), 
                                                  algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                                    factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


RMSE_soil_weather_wheat_RC_Pi46_Yield <- data.frame(rmse = c(ridge_soil_weather_wheat_RC_Pi46_Yield_RMSE,lasso_soil_weather_wheat_RC_Pi46_Yield_RMSE,rfe_soil_weather_wheat_RC_Pi46_Yield_RMSE), 
                                                    algorithm = c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination") %>% 
                                                      factor(.,levels=c("Ridge Regression","LASSO","Random Forest-Recursive Feature Elimination")))


Fig_R2_soil_weather_wheat_RC_Pi46_Yield <- 
  ggplot(R2_soil_weather_wheat_RC_Pi46_Yield, aes(x=algorithm, y=r2, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("R-squared") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(r2, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  labs(title = "Soil and Weather Variable Model for Predicting RC_Pi46_Yield") +
  theme_bw() +
  theme(legend.position = "none")
Fig_R2_soil_weather_wheat_RC_Pi46_Yield

Fig_RMSE_soil_weather_wheat_RC_Pi46_Yield <- 
  ggplot(RMSE_soil_weather_wheat_RC_Pi46_Yield, aes(x=algorithm, y=rmse, fill=algorithm)) + 
  geom_bar(stat="identity") + 
  ylab("RMSE") +
  scale_fill_manual(values=c("Ridge Regression"="darkblue",
                             "LASSO"="orange",
                             "Random Forest-Recursive Feature Elimination"="darkgreen"
  )) + geom_text(aes(label=round(rmse, 3)), vjust=-0.5, color="black", position = position_dodge(0.9), size=3.5) +
  labs(title = "Soil and Weather Variable Model for Predicting RC_Pi46_Yield") +
  theme_bw() +
  theme(legend.position = "none")

Fig_RMSE_soil_weather_wheat_RC_Pi46_Yield


# Variable Importance

## As the LASSO and Random Forest-Recursive Feature Elimination has given good model performance for
## soil_weather_wheat_RC_Pi46_Yield model. From now on, the main focus will be on this model and 
## LASSO and Random Forest-Recursive Feature Elimination.

### soil_weather_wheat_RC_Pi46_Yield

#### LASSO

pvip_lasso_soil_weather_wheat_RC_Pi46_Yield <- vip(lasso_soil_weather_wheat_RC_Pi46_Yield, method="permute", train=train_data_soil_weather_wheat_RC_Pi46_Yield, target="RC_Pi46_Yield", metric="rsquared", 
                                                   pred_wrapper=predict, nsim=30, geom="boxplot",aesthetics = list(fill = "orange", color="black")) + labs(title="LASSO") +theme_bw()
pvip_lasso_soil_weather_wheat_RC_Pi46_Yield

#### Random Forest-Recursive Feature Elimination

pvip_rfe_soil_weather_wheat_RC_Pi46_Yield <- vip(rfe_soil_weather_wheat_RC_Pi46_Yield, method="permute", train=train_data_soil_weather_wheat_RC_Pi46_Yield, target="RC_Pi46_Yield", metric="rsquared", 
                                                 pred_wrapper=predict, nsim=30, geom="boxplot",aesthetics = list(fill = "darkgreen", color="black")) + labs(title="Random Forest-Recursive Feature Elimination") +theme_bw()
pvip_rfe_soil_weather_wheat_RC_Pi46_Yield


#### Combining the Plots

pvip_soil_weather_wheat_RC_Pi46_Yield <-  pvip_lasso_soil_weather_wheat_RC_Pi46_Yield + pvip_rfe_soil_weather_wheat_RC_Pi46_Yield + 
  plot_annotation(tag_levels = 'a', title = "Variable Importance of Soil and Weather Variable Models for Predicting RC_Pi46_Yield") + plot_layout(nrow = 1)
pvip_soil_weather_wheat_RC_Pi46_Yield

# Variable Interaction

## RC_Pi46_Yield

### LASSO

int_lasso_soil_weather_wheat_RC_Pi46_Yield <- vint(
  object = lasso_soil_weather_wheat_RC_Pi46_Yield,                    # fitted model object
  feature_names = c("BOF_0-60_T4_10d", "Zn", "BOF_0-100_T3_10d", "NS_Sum_T4_10d", "T_Bo_20-30_T3_10d",
                    "T_Bo_20-30_T5_10d", "Cu", "vW_6-18_T1_10d", "Mg", "NO3_N"),  
  parallel = TRUE
)

int_lasso_soil_weather_wheat_RC_Pi46_Yield

plot_int_lasso_soil_weather_wheat_RC_Pi46_Yield <-
ggplot(int_lasso_soil_weather_wheat_RC_Pi46_Yield[1:10, ], aes(reorder(Variables, Interaction), Interaction)) +
  geom_bar(stat="identity", fill="orange") +
  labs(x = "", y = "Interaction strength") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1)) +
  coord_flip()  +
  theme_bw() + labs(title="LASSO")
plot_int_lasso_soil_weather_wheat_RC_Pi46_Yield

### Random Forest-Recursive Feature Elimination
rfe_soil_weather_wheat_RC_Pi46_Yield$bestSubset

rfe_soil_weather_wheat_RC_Pi46_Yield$optVariables

int_rfe_soil_weather_wheat_RC_Pi46_Yield <- vint(
  object = rfe_soil_weather_wheat_RC_Pi46_Yield,                    # fitted model object
  feature_names = c("T3", "UI_T4", "fertilizer_type.Urea", "Corg", "Cu", "BOW_0-30_T2_10d", "BOW_0-60_T2_10d",
                    "BOF_0-60_T4_10d", "Zn", "NS_Sum_T4_10d"),  
  parallel = TRUE
)

int_rfe_soil_weather_wheat_RC_Pi46_Yield

plot_int_rfe_soil_weather_wheat_RC_Pi46_Yield <-
ggplot(int_rfe_soil_weather_wheat_RC_Pi46_Yield[1:10, ], aes(reorder(Variables, Interaction), Interaction)) +
  geom_bar(stat="identity", fill="darkgreen") +
  labs(x = "", y = "Interaction strength") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1)) +
  coord_flip()  +
  theme_bw() + labs(title="Random Forest-Recursive Feature Elimination")

plot_int_rfe_soil_weather_wheat_RC_Pi46_Yield


# Partial Dependence Plot

## Soil and Weather_RC_Pi46_Yield

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_1 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = "T3", aprox = "T")) +
  geom_line(color="darkblue", size = 1) + theme(legend.position="none")
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_1

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_2 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = "UI_T4", aprox = "T")) +
  geom_line(color="orange", size = 1) + theme(legend.position="none")
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_2

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_3 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = "fertilizer_type.Urea", aprox = "T")) +
  geom_line(color="darkgreen", size = 1) + theme(legend.position="none")
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_3

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_4 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = "Corg", aprox = "T")) +
  geom_line(color="red", size = 1) + theme(legend.position="none")
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_4

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_5 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = "Cu", aprox = "T")) +
  geom_line(color="green", size = 1) + theme(legend.position="none")
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_5

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_6 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = "BOW_0-30_T2_10d", aprox = "T")) +
  geom_line(color="darkred", size = 1) + theme(legend.position="none")
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_6

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_7 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = "BOW_0-60_T2_10d", aprox = "T")) +
  geom_line(color="black", size = 1) + theme(legend.position="none")
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_7

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_8 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = "BOF_0-60_T4_10d", aprox = "T")) +
  geom_line(color="magenta", size = 1) + theme(legend.position="none")
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_8

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_9 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = "Zn", aprox = "T")) +
  geom_line(color="purple", size = 1) + theme(legend.position="none")
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_9

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_10 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = "NS_Sum_T4_10d", aprox = "T")) +
  geom_line(color="violet", size = 1) + theme(legend.position="none")
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_10

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield <- pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_1 + pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_2 + pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_3 +
  pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_4 + pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_5 + pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_6 +
  pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_7 + pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_8 + pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_9 +
  pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_10 +
  plot_annotation(tag_levels = 'a', title = "Partial Dependence Plot of Soil and Weather Variable Model (RC_Pi46_Yield Prediction)") + plot_layout(nrow = 5)
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_combined <- pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_1 + pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_2 + pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_3 +
  pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_4 +
  plot_annotation(tag_levels = 'a', title = "Partial Dependence Plot of Soil and Weather Variable Model (RC_Pi46_Yield Prediction)") + plot_layout(nrow = 2)
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_combined

pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_combined_2 <- pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_1 + pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_2 + pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_3 +
  plot_annotation(tag_levels = 'a', title = "Partial Dependence Plot of Soil and Weather Variable Model (RC_Pi46_Yield Prediction)") + plot_layout(nrow = 2)
pdp_rfe_soil_weather_wheat_RC_Pi46_Yield_combined_2


# 2-way interactions

two_way_interaction_1 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = c("Cu", "fertilizer_type.Urea"), aprox = "T"))


two_way_interaction_1


two_way_interaction_2 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = c("Corg", "fertilizer_type.Urea"), aprox = "T"))

two_way_interaction_2

two_way_interaction_3 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = c("BOF_0-60_T4_10d", "fertilizer_type.Urea"), aprox = "T"))

two_way_interaction_3

two_way_interaction_4 <- autoplot(pdp::partial(rfe_soil_weather_wheat_RC_Pi46_Yield, pred.var = c("fertilizer_type.Alzon+Piagran pro", "Zn"), aprox = "T"))

two_way_interaction_4


two_way_interaction <- two_way_interaction_1 + two_way_interaction_2 + two_way_interaction_3 + two_way_interaction_4 +
  plot_annotation(tag_levels = 'a')
two_way_interaction

# Coefficients of Soil and Weather_RC_Pi46_Yield Model

## LASSO

coef_lasso_soil_weather_wheat_RC_Pi46_Yield  <- coef(lasso_soil_weather_wheat_RC_Pi46_Yield$finalModel, lasso_soil_weather_wheat_RC_Pi46_Yield$bestTune$lambda)
coef_lasso_soil_weather_wheat_RC_Pi46_Yield  <- as_tibble(as.matrix(coef_lasso_soil_weather_wheat_RC_Pi46_Yield), rownames='feature') %>%
  rename(estimate = 's1') 

coef_lasso_soil_weather_wheat_RC_Pi46_Yield


coef_compare_soil_weather_wheat_RC_Pi46_Yield <- bind_rows(coef_lasso_soil_weather_wheat_RC_Pi46_Yield %>% mutate(model = 'LASSO'))

coef_compare_soil_weather_wheat_RC_Pi46_Yield_1 <- coef_compare_soil_weather_wheat_RC_Pi46_Yield[coef_compare_soil_weather_wheat_RC_Pi46_Yield$estimate != '0', ]

write.csv(coef_compare_soil_weather_wheat_RC_Pi46_Yield, file = "Model Coefficients_Wheat_LASSO_soil_Weather_RC_Pi46_Yield.csv")

coef_compare_soil_weather_wheat_RC_Pi46_Yield %>%
  ggplot(aes(x=reorder(feature, estimate),
             y=estimate,
             fill=model)) +
  geom_bar(stat = 'identity',position = 'dodge', show.legend = FALSE) + theme(axis.text.y = element_text(size = 7)) +
  coord_flip() + xlab("Features") + ylab("Estimate") + labs(title = "Estimates of LASSO") +
  theme(plot.title = element_text(size = 10.0), axis.text.y = element_text(size = 6.0)) +
  geom_text(aes(label=round(estimate, 3)), hjust=-0.8, color="black", position = position_dodge(0.9), size=1.8)


coef_compare_soil_weather_wheat_RC_Pi46_Yield_1 %>%
  ggplot(aes(x=reorder(feature, estimate),
             y=estimate,
             fill=model)) +
  geom_bar(stat = 'identity',position = 'dodge', show.legend = FALSE) + theme(axis.text.y = element_text(size = 9)) +
  coord_flip() + xlab("Features") + ylab("Estimate") + labs(title = "Estimates of LASSO") +
  theme(plot.title = element_text(size = 10.0), axis.text.y = element_text(size = 6.0)) +
  geom_text(aes(label=round(estimate, 3)), hjust=-0.8, color="black", position = position_dodge(0.9), size = 2.8)


# Adding interaction terms to the LASSO Models

set.seed(123)
ridge_soil_weather_wheat_RC_Pi46_Yield_interaction <- train(RC_Pi46_Yield ~.^2,
                                                data = train_data_soil_weather_wheat_RC_Pi46_Yield,
                                                method = "glmnet",
                                                trcontrol = model_control,
                                                tuneGrid = expand.grid(alpha = 0, lambda = lambda),
                                                preProcess = 'scale')

print(ridge_soil_weather_wheat_RC_Pi46_Yield_interaction)

set.seed(123)
lasso_soil_weather_wheat_RC_Pi46_Yield_interaction <- train(RC_Pi46_Yield ~.^2,
                                                data = train_data_soil_weather_wheat_RC_Pi46_Yield,
                                                method = "glmnet",
                                                trcontrol = model_control,
                                                tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                                                preProcess = 'scale')
print(lasso_soil_weather_wheat_RC_Pi46_Yield_interaction)

coef_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction  <- coef(lasso_soil_weather_wheat_RC_Pi46_Yield_interaction$finalModel, lasso_soil_weather_wheat_RC_Pi46_Yield_interaction$bestTune$lambda)
coef_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction  <- as_tibble(as.matrix(coef_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction), rownames='feature') %>%
  rename(estimate = 's1') 

coef_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction


coef_compare_soil_weather_wheat_RC_Pi46_Yield_interaction <- bind_rows(coef_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction %>% mutate(model = 'LASSO'))

write.csv(coef_compare_soil_weather_wheat_RC_Pi46_Yield_interaction, file = "Model Coefficients_Wheat_LASSO_interaction_soil_Weather_RC_Pi46_Yield.csv")


coef_compare_soil_weather_wheat_RC_Pi46_Yield_interaction %>%
  ggplot(aes(x=reorder(feature, estimate),
             y=estimate,
             fill=model)) +
  geom_bar(stat = 'identity',position = 'dodge', show.legend = FALSE) + theme(axis.text.y = element_text(size = 7)) +
  coord_flip() + xlab("Features") + ylab("Estimate") + labs(title = "Estimates of LASSO with interaction terms") +
  theme(plot.title = element_text(size = 10.0), axis.text.y = element_text(size = 6.0)) +
  geom_text(aes(label=round(estimate, 3)), hjust=-0.8, color="black", position = position_dodge(0.9), size=1.8)



## Model Performance of the lasso and ridge models with interactions

predictions_ridge_soil_weather_wheat_RC_Pi46_Yield_interaction <- predict(ridge_soil_weather_wheat_RC_Pi46_Yield_interaction, test_data_soil_weather_wheat_RC_Pi46_Yield)

ridge_soil_weather_wheat_RC_Pi46_Yield_R2_interaction <- R2(predictions_ridge_soil_weather_wheat_RC_Pi46_Yield_interaction, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
ridge_soil_weather_wheat_RC_Pi46_Yield_RMSE_interaction <- RMSE(predictions_ridge_soil_weather_wheat_RC_Pi46_Yield_interaction, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)

ridge_soil_weather_wheat_RC_Pi46_Yield_R2_interaction

predictions_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction <- predict(lasso_soil_weather_wheat_RC_Pi46_Yield_interaction, test_data_soil_weather_wheat_RC_Pi46_Yield)

lasso_soil_weather_wheat_RC_Pi46_Yield_R2_interaction <- R2(predictions_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
lasso_soil_weather_wheat_RC_Pi46_Yield_RMSE_interaction <- RMSE(predictions_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)

lasso_soil_weather_wheat_RC_Pi46_Yield_R2_interaction


## Variable Importance

pvip_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction <- vip(lasso_soil_weather_wheat_RC_Pi46_Yield_interaction, method="permute", train=train_data_soil_weather_wheat_RC_Pi46_Yield, target="RC_Pi46_Yield", metric="rsquared", 
                                                   pred_wrapper=predict, nsim=30, geom="boxplot",aesthetics = list(fill = "orange", color="black")) + labs(title="LASSO with Interaction Terms") +theme_bw()
pvip_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction


## Interactions

int_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction <- vint(
  object = lasso_soil_weather_wheat_RC_Pi46_Yield_interaction,                    # fitted model object
  feature_names = c("BOF_0-100_T3_10d", "Zn", "T_Air_T2_10d", "BOF_0-60_T4_10d", "BOF_0-100_T4_10d", 
                    "NS_Sum_T4_10d", "BOW_0-30_T5_10d", "T_Bo_20-30_T3_10d",
                    "vW_6-18_T1_10d", "T_Bo_10-20_T3_10d"),  
  parallel = TRUE
) 

int_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction

plot_int_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction <-
  ggplot(int_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction[1:10, ], aes(reorder(Variables, Interaction), Interaction)) +
  geom_bar(stat="identity", fill="orange") +
  labs(x = "", y = "Interaction strength") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1)) +
  coord_flip()  +
  theme_bw() + labs(title="LASSO with Interaction Terms")
plot_int_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction


# Adding only important interaction terms from the random forest-recursive feature elimination in the LASSO model


set.seed(123)
lasso_soil_weather_wheat_RC_Pi46_Yield_interaction_2 <- train(RC_Pi46_Yield ~ N_Fertilization_kg_ha + pH + K + P + Mg + Corg + NH4_N + NO3_N + Nges + Smin + B + Mn + Cu + Zn + Mo + KAK_eff +
                                                              Ca_eff + Mg_eff + K_eff + BS + Sand + Silt + Clay + NS_Sum_T1_10d + NS_Sum_T2_10d + NS_Sum_T3_10d + NS_Sum_T4_10d + NS_Sum_T5_10d +
                                                              T_Air_T1_10d + T_Air_T2_10d + T_Air_T3_10d + T_Air_T4_10d + T_Air_T5_10d + `T_Bo_0-10_T1_10d` + `T_Bo_0-10_T2_10d` + `T_Bo_0-10_T3_10d` +
                                                              `T_Bo_0-10_T4_10d` + `T_Bo_0-10_T5_10d` + `T_Bo_10-20_T1_10d` + `T_Bo_10-20_T2_10d` + `T_Bo_10-20_T3_10d` + `T_Bo_10-20_T4_10d` + `T_Bo_10-20_T5_10d` +
                                                              `T_Bo_20-30_T1_10d` + `T_Bo_20-30_T2_10d` + `T_Bo_20-30_T3_10d` + `T_Bo_20-30_T4_10d` + `T_Bo_20-30_T5_10d` + `T_Bo_0-30_T1_10d` + `T_Bo_0-30_T2_10d` +
                                                              `T_Bo_0-30_T3_10d` + `T_Bo_0-30_T4_10d` + `T_Bo_0-30_T5_10d` + `BOF_0-30_T1_10d` + `BOF_0-30_T2_10d` + `BOF_0-30_T3_10d` + `BOF_0-30_T4_10d` +
                                                              `BOF_0-30_T5_10d` + `BOF_0-60_T1_10d` + `BOF_0-60_T2_10d` + `BOF_0-60_T3_10d` + `BOF_0-60_T4_10d` + `BOF_0-60_T5_10d` + `BOF_0-100_T1_10d` + `BOF_0-100_T2_10d` +
                                                              `BOF_0-100_T3_10d` + `BOF_0-100_T4_10d` + `BOF_0-100_T5_10d` + `BOW_0-30_T1_10d` + `BOW_0-30_T2_10d` + `BOW_0-30_T3_10d` + `BOW_0-30_T4_10d` +
                                                              `BOW_0-30_T5_10d` + `BOW_0-60_T1_10d` + `BOW_0-60_T2_10d` + `BOW_0-60_T3_10d` + `BOW_0-60_T4_10d` + `BOW_0-60_T5_10d` + `BOW_0-100_T1_10d` + `BOW_0-100_T2_10d` +
                                                              `BOW_0-100_T3_10d` + `BOW_0-100_T4_10d` + `BOW_0-100_T5_10d` + `vW_6-18_T1_10d` + `vW_6-18_T2_10d` + `vW_6-18_T3_10d` + `vW_6-18_T4_10d` + `vW_6-18_T5_10d` +
                                                              urea + NI + UI + No_of_applications + T1 + T2 + T3 + T4 + T5 + UI_T1 + UI_T2 + UI_T3 + UI_T4 + UI_T5 + NI_T1 + NI_T2 + NI_T3 + NI_T4 +
                                                              `Soil Group.2` + `Soil Group.3` + `Soil Group.4` + `Soil Group.5` + fertilizer_type.Alzon + `fertilizer_type.Alzon+Piagran pro` + fertilizer_type.CAN +
                                                              `fertilizer_type.Piagran pro` + `fertilizer_type.Piagran pro+Alzon` + fertilizer_type.Urea + fertilizer_type.Urea*Cu + fertilizer_type.Urea*Corg +
                                                              fertilizer_type.Urea*`BOF_0-60_T4_10d` + fertilizer_type.Urea*Zn,
                                                            data = train_data_soil_weather_wheat_RC_Pi46_Yield,
                                                            method = "glmnet",
                                                            trcontrol = model_control,
                                                            tuneGrid = expand.grid(alpha = 1, lambda = lambda),
                                                            preProcess = 'scale')
print(lasso_soil_weather_wheat_RC_Pi46_Yield_interaction_2)


predictions_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction_2 <- predict(lasso_soil_weather_wheat_RC_Pi46_Yield_interaction_2, test_data_soil_weather_wheat_RC_Pi46_Yield)

lasso_soil_weather_wheat_RC_Pi46_Yield_R2_interaction_2 <- R2(predictions_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction_2, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)
lasso_soil_weather_wheat_RC_Pi46_Yield_RMSE_interaction_2 <- RMSE(predictions_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction_2, test_data_soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield)

lasso_soil_weather_wheat_RC_Pi46_Yield_R2_interaction_2


pvip_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction_2 <- vip(lasso_soil_weather_wheat_RC_Pi46_Yield_interaction_2, method="permute", train=train_data_soil_weather_wheat_RC_Pi46_Yield, target="RC_Pi46_Yield", metric="rsquared", 
                                                               pred_wrapper=predict, nsim=30, geom="boxplot",aesthetics = list(fill = "orange", color="black")) + labs(title="LASSO") +theme_bw()
pvip_lasso_soil_weather_wheat_RC_Pi46_Yield_interaction_2


### Moran's I
soil_weather_wheat_RC_Pp_Yield
soil_weather_wheat_RC_Pi46_Yield

coordinates(soil_weather_wheat_RC_Pp_Yield) <- ~Longitude + Latitude 

neighbours_1 <- knn2nb(knearneigh(coordinates(soil_weather_wheat_RC_Pp_Yield), k = 5))
weights_1 <- nb2listw(neighbours_1, style = "W")

moran_test_RC_Pp_Yield <- moran.test(soil_weather_wheat_RC_Pp_Yield$RC_Pp_Yield, weights_1)
print(moran_test_RC_Pp_Yield)


coordinates(soil_weather_wheat_RC_Pi46_Yield) <- ~Longitude + Latitude 

neighbours_2 <- knn2nb(knearneigh(coordinates(soil_weather_wheat_RC_Pi46_Yield), k = 5))
weights_2 <- nb2listw(neighbours_2, style = "W")

moran_test_RC_Pi46_Yield <- moran.test(soil_weather_wheat_RC_Pi46_Yield$RC_Pi46_Yield, weights_2)
print(moran_test_RC_Pi46_Yield)