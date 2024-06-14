# Clean environment
rm(list=ls())

# Load packages
library(ggplot2)

# Load data
Shapann <- read.csv("your directory")
ShapRf <- read.csv("your directory")

# Set column names
colnames(Shapann) <- c("Feature", "Shap")
colnames(Shaprf) <- c("Feature", "Shap")

# Merge scores
Shap <- merge(Shapann, Shaprf, by = "Feature")

# Set column names
colnames(Shap) <- c("Feature", "ShapANN", "ShapRF")

# All points higher than 1 SHAP for both will be labelled
pointstolabel <- filter(Shap, ShapANN > 1  | ShapRF > 1)

# Make plot
Shap |>
  ggplot(aes(x = ShapANN, y = ShapRF)) +
  geom_point() +
  geom_text(aes(ShapANN, ShapRF, label = Feature), data = filter(Shap, ShapANN > 1 | ShapRF > 1),
            position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  xlab("SHAP ANN") +
  ylab("SHAP RF") +
  geom_abline() +
  theme_bw(base_size = 22)

# Get correlation
cor(Shap$ShapANN, Shap$ShapRF)
