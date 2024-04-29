# Loading the libraries
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(factoextra)
library(caret)
library (knitr)
library(data.table)
library(XLConnect)
library(htmlTable)
library(corrplot)
library(pheatmap)
library(lares)


# Loading the dataset containing both 2021 & 2022 data

data_analysis <-read_excel("Analysis_21-22.xlsx", sheet = "Sheet1", col_types = c("text", "text", "text", "text",
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
View(data_analysis)
head(data_analysis)

summary(data_analysis)

# Creating dataset for wheat

data_analysis_wheat <-  filter(data_analysis, CropType_Long == "Winter Soft Wheat")
View(data_analysis_wheat)

data_analysis_rye <-  filter(data_analysis, CropType_Long == "Winter Rye")
View(data_analysis_rye)

data_analysis_barley <-  filter(data_analysis, CropType_Long == "Winter Barley")
View(data_analysis_barley)

# Excluding the rows with `No Nitrogen Fertilisation`'

data_analysis_wheat_NoN <- data_analysis_wheat[data_analysis_wheat$Pgl_Bez != 'ohne N', ]
View(data_analysis_wheat_NoN)

data_analysis_rye_NoN <- data_analysis_rye[data_analysis_rye$Pgl_Bez != 'ohne N', ]
View(data_analysis_rye_NoN)

data_analysis_barley_NoN <- data_analysis_barley[data_analysis_barley$Pgl_Bez != 'ohne N', ]
View(data_analysis_barley_NoN)

## Checking for missing values

colSums(is.na(data_analysis_wheat_NoN))

sum(is.na(data_analysis_wheat_NoN))

colSums(is.na(data_analysis_rye_NoN))
sum(is.na(data_analysis_rye_NoN))

## Excluding the rows with missing values

wheat_1 <- na.exclude(data_analysis_wheat_NoN)
View(wheat_1)

rye_1 <- na.exclude(data_analysis_rye_NoN)
View(rye_1)

barley_1 <- na.exclude(data_analysis_barley_NoN)
View(barley_1)

sum(is.na(wheat_1))

## Excluding the columns containing IDs, unnecessary text values, constant values, and Nutrient Supply Values

wheat <- wheat_1[, -c(1:4, 6:9, 10, 11, 14, 16, 18, 25, 27, 29, 36, 38:39, 113, 129:131)]
View(wheat)

rye <- data_analysis_rye[, -c(1:4, 6:9, 10, 11, 14, 16, 18, 25, 27, 29, 36, 38:39, 113, 129)]
View(rye)

barley <- barley_1[, -c(1:4, 6:9, 10, 11, 14, 16, 18, 25, 27, 29, 36, 38:39, 113, 129)]
View(barley)

## Dividing the dataset into Soil and Weather Variables

wheat_soil <- wheat[, c(2:24)]
View(wheat_soil)

wheat_weather <- wheat[, c(25:89)]
View(wheat_weather)

rye_soil <- rye[, c(2:24)]
View(rye_soil)

rye_weather <- rye[, c(25:89)]
View(rye_weather)

barley_soil <- barley[, c(2:24)]
View(barley_soil)

barley_weather <- barley[, c(25:89)]
View(barley_weather)


## one hot encoding of the categorical variables

# Soil Dataset

dummy_wheat_soil <- dummyVars(~ ., data=wheat_soil)
encoded_wheat_soil <- data.frame(predict(dummy_wheat_soil, newdata=wheat_soil))
View(encoded_wheat_soil)

dummy_rye_soil <- dummyVars(~ ., data=rye_soil)
encoded_rye_soil <- data.frame(predict(dummy_rye_soil, newdata=rye_soil))
View(encoded_rye_soil)

dummy_barley_soil <- dummyVars(~ ., data=barley_soil)
encoded_barley_soil <- data.frame(predict(dummy_barley_soil, newdata=barley_soil))
View(encoded_barley_soil)


encoded_soil <- wheat_soil%>%
  mutate(rowid = row_number(), value = 1)%>%
  separate_rows(`Soil Group`)%>%
  filter(nzchar(`Soil Group`)) %>%
  pivot_wider(rowid, names_from = `Soil Group`, 
              values_fn = sum, names_prefix = 'Soil Group ', 
              values_fill = 0)
View(encoded_soil)

encoded_soil_rye <- rye_soil%>%
  mutate(rowid = row_number(), value = 1)%>%
  separate_rows(`Soil Group`)%>%
  filter(nzchar(`Soil Group`)) %>%
  pivot_wider(rowid, names_from = `Soil Group`, 
              values_fn = sum, names_prefix = 'Soil Group ', 
              values_fill = 0)
View(encoded_soil_rye)

encoded_soil_barley <- barley_soil%>%
  mutate(rowid = row_number(), value = 1)%>%
  separate_rows(`Soil Group`)%>%
  filter(nzchar(`Soil Group`)) %>%
  pivot_wider(rowid, names_from = `Soil Group`, 
              values_fn = sum, names_prefix = 'Soil Group ', 
              values_fill = 0)
View(encoded_soil_barley)

head(encoded_wheat_soil)
summary(encoded_wheat_soil)

encoding_rye_soil <- one_hot(rye_soil)
view(encoding_rye_soil)



which(apply(wheat_soil, 2, var)==0)

## Normality Test

shapiro.test(encoded_wheat_soil$Nges)



## Performing Principal Component Analysis by normalizing variables

# Soil Dataset

wheat_princomp_soil <- prcomp(encoded_wheat_soil, scale. = TRUE)
summary(wheat_princomp_soil)
output_soil <-  get_eigenvalue(wheat_princomp_soil)


write.table(output_soil, file = "Summary of Soil.csv", quote = TRUE, sep = ",", dec = ".", col.names = TRUE, row.names = TRUE)


  
as.data.frame(apply(wheat_princomp_soil, 2, summary))
print(wheat_princomp_soil)


writeWorksheetToFile("Summary.xlsx",
                     data = a,
                     sheet = "Summary of Soil",
                     header = TRUE,
                     clearSheets = TRUE)

writeWorksheetToFile("Rotation.xlsx",
                     data = wheat_princomp_soil$rotation[, 0:5],
                     sheet = "Rotation of Soil",
                     header = TRUE,
                     clearSheets = TRUE)

a <- get_eigenvalue(wheat_princomp_soil)

wheat_princomp_soil$rotation[, 1:5]

wheat_princomp_soil$loadings[, 1:5]

write.csv(a, file = "Summary of PCA of Soil Variables.csv")

htmlTable(a)

## Visualizing the percentage of variances explained by each principal component

fviz_eig(wheat_princomp_soil, addlabels = TRUE, ncp = 10, title = "Scree Plot of Soil Variables")

## Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph

fviz_pca_var(wheat_princomp_soil, geom = c("arrow", "text"),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("black", "red", "green"), alpha.var = 1,
             repel = TRUE # Avoid text overlapping
             
)

fviz_pca_var(wheat_princomp_soil, axes = 1:2,
             col.var = "contrib", 
             gradient.cols = c("black", "red", "green"),
             repel = TRUE,
             title = "Biplot of Soil Variables")



## Visualizing contribution of each variable

fviz_cos2(wheat_princomp_soil, choice = "var", axes = 1:2, top = 10)

fviz_contrib(wheat_princomp_soil, choice = "var", axes = 1, top = 10, title = "Contribution of Soil Variables to Dim 1")
fviz_contrib(wheat_princomp_soil, choice = "var", axes = 2, top = 10, title = "Contribution of Soil Variables to Dim 2")
fviz_contrib(wheat_princomp_soil, choice = "var", axes = 3, top = 10)
fviz_contrib(wheat_princomp_soil, choice = "var", axes = 4, top = 10)

fviz_cos2(wheat_princomp_soil, choice = "var", axes = 1, top = 10)

# Loading Plots
color_soil_PC1 <- ifelse(wheat_princomp_soil$rotation [,1] > 0, yes="green", no="red")

par(mar=c(8,3,2,1)) # Set margins
barplot_soil_PC1 <- barplot(wheat_princomp_soil$rotation [,1], main="Loading Plot of PC1", col=color_soil_PC1, las=2, axisnames=FALSE)
abline(h=0) # Add horizontal line
text(x=barplot_soil_PC1, y=color_soil_PC1, labels=names(wheat_princomp_soil$rotation [,1]), adj=1, srt=45, xpd=TRUE) # Add variable names

# Weather Dataset

wheat_princomp_weather <- prcomp(wheat_weather, scale. = TRUE)
summary(wheat_princomp_weather)
print(wheat_princomp_weather)

wheat_princomp$rotation[, 1:5]

output_weather <-  get_eigenvalue(wheat_princomp_weather)

write.table(output_rotation_weather, file = "Rotation of Weather Variables.csv", quote = TRUE, sep = ",", dec = ".", col.names = TRUE, row.names = TRUE)

output_rotation_weather <- wheat_princomp_weather$rotation[, 1:5]


## Visualizing the percentage of variances explained by each principal component

fviz_eig(wheat_princomp_weather, addlabels = TRUE, ncp = 10, title = "Scree Plot of Weather Variables")

## Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph

fviz_pca_var(wheat_princomp_weather, geom = c("arrow", "text"),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("black", "red", "green"), alpha.var = 1,
             repel = TRUE # Avoid text overlapping
             
)

fviz_pca_var(wheat_princomp_weather,
             col.var = "contrib", 
             gradient.cols = c("black", "red", "green"),
             labelsize = 2,
             repel = TRUE, title = "Biplot Weather Variables")



## Visualizing contribution of each variable

fviz_cos2(wheat_princomp_weather, choice = "var", axes = 1:2, top = 20)

fviz_contrib(wheat_princomp_weather, choice = "var", axes = 1:2, top = 20)

fviz_contrib(wheat_princomp_weather, choice = "var", axes = 1, top = 10, title = "Contribution of Weather Variables to Dim-1")
fviz_contrib(wheat_princomp_weather, choice = "var", axes = 2, top = 10, title = "Contribution of Weather Variables to Dim-2")
fviz_contrib(wheat_princomp_weather, choice = "var", axes = 3, top = 10)



### Hierarchical Clustering Dendogram

## Soil Dataset

# Transposing the dataset for creating clusters of the variables

transposed_soil <- t(encoded_wheat_soil)
view(transposed_soil)

# Scaling the soil dataset

scale_soil <- scale(transposed_soil)
summary(scale_soil)


# Creating the distance matrix by euclidean method

dist_mat_soil <- dist(scale_soil, method = 'euclidean')
View(dist_mat_soil)

# Building dendogram by plotting the hierarchical cluster object

hclust_soil_avg <- hclust(dist_mat_soil, method = 'complete')
plot(hclust_soil_avg, cex = 0.6, hang = -1)
rect.hclust(hclust_soil_avg , k = 6)
abline(h = 10, col = 'red')

hclust_soil_av$order

# Plotting a colorful graph of this cluster

fviz_dend(hclust_soil_avg, cex = 0.7, lwd = 0.8, k = 6,
          color_labels_by_k = TRUE,
          rect = TRUE,
          k_colors = "lancet",
          rect_border = "lancet", 
          ggtheme = theme_gray(),
          main = "Cluster Dendrogram of Soil Variables")

cut_soil <- cutree(hclust_soil_avg, k = 6)


## Weather dataset

transposed_weather <- t(wheat_weather)
view(transposed_weather)

# Scaling the weather dataset

scale_weather <- scale(transposed_weather)
summary(scale_weather)
view(scale_weather)

# Creating the distance matrix by euclidean method

dist_mat_weather <- dist(scale_weather, method = 'euclidean')
View(dist_mat_weather)

# Building dendogram by plotting the hierarchical cluster object

hclust_weather_avg <- hclust(dist_mat_weather, method = 'complete')
plot(hclust_weather_avg, cex = 0.6, hang = -1)
rect.hclust(hclust_weather_avg , k = 10)

abline(h = 15, col = 'red')

# Plotting a colorful graph of this cluster

fviz_dend(hclust_weather_avg, cex = 0.5, lwd = 0.5, k = 10,
          color_labels_by_k = TRUE,
          rect = TRUE,
          k_colors = "lancet",
          rect_border = "lancet", 
          ggtheme = theme_gray(),
          main = "Cluster Dendrogram of Weather Variables")

## Correlation Heatmap

# Creating Function for Correlation Heatmap

corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficients on the diagonal
           diag = diag
  )
}


# Correlation Heatmap/Correlogram of Soil Dataset

corrplot2(
  data = encoded_wheat_soil,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75,
  number.cex = 0.60,
  number.font = 0.5
)

# Correlation Heatmap/Correlogram of Weather Dataset

 corrplot2(
  data = wheat_weather,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 45,
  number.cex = 0.30,
  number.font = 0.1
)

a <- cor(wheat_weather)
a
write.csv(a, file ="correlation matrix of weather dataset.csv")
heatmap(a, cexRow = 0.55, cexCol = 0.55)

pheatmap(a, cutree_rows = 6, cutree_cols = 6)

## Soil Dataset

corr_wheat_soil <- cor(encoded_wheat_soil, method = "pearson")

pheatmap(corr_wheat_soil, cutree_rows = 4, cutree_cols = 4, display_numbers = TRUE, clustering_method = "complete")

## Weather Dataset

corr_wheat_weather <- cor(wheat_weather, method = "pearson")

write_csv(as.data.frame(corr_wheat_weather), file= "Correlation of Weather Variables.csv")

pheatmap(corr_wheat_weather, cutree_rows = 6, cutree_cols = 6, display_numbers = TRUE, clustering_method = "complete", fontsize_number = 6, fontsize_row = 6, fontsize_col = 8)


## Detailed Correlation Analysis

corr_cross(encoded_wheat_soil, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 11# display top 10 couples of variables (by correlation coefficient)
)

corr_cross(wheat_weather, # name of dataset
           type = 1,
            # display only significant correlations (at 5% level)
           top = 30# display top 10 couples of variables (by correlation coefficient)
)
