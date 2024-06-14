# Multiple linear regression

# Clean environment
rm(list=ls())

# Load data
df <- read.csv("your directory")

# Load packages
library(tidymodels)
library(tidyverse)
library(corrplot)
library(doParallel)
library(ranger)
library(ggplot2)
library(farver)
library(vip)
library(fastshap)
library(stargazer)
library(car)
library(hstats)
library(kernelshap)
library(shapviz)
library(patchwork)
library(fastDummies)
library(lm.beta)
library(lmtest)
library(nlme)
library(sandwich)

stargazer(df, type = "html", out="your directory")


# Split data
set.seed(912341)
split <- initial_split(data = df, prop = 0.8, strata = ActualHT)

df_train <- training(split)
df_test <- testing(split)



# Full model, no feature engineering
modelA <- lm(ActualHT ~  Building_Location + Type + isSectionA + isSectionB + isSectionC + isSectionD + isSectionE + isSectionF + isSectionG + isSectionH + isSectionI + isSectionJ + isSectionK + Copy + Installed_Power + Length + Width + Depth + isSectionL + isSectionM + isSectionN + isSectionO + isSectionP + isSectionQ + isSectionR + isSectionS + isSectionT + isSectionU + isSectionV + isSectionW + isSectionX + isSectionY + isSectionZ + isSectionAA, data = df_train)

stargazer(modelA, type="text")

sqrt(mean(modelA$residuals^2))

vif(modelA)

# Length, width and depth all correlated, as well as installed power, type and building location

# Combine all dimensions to volume
df_train$Volume <- df_train$Length * df_train$Width * df_train$Depth
df_test$Volume <- df_test$Length * df_test$Width * df_test$Depth

# New model with Volume
modelB <- lm(ActualHT ~  Building_Location + Type + isSectionA + isSectionB + isSectionC + isSectionD + isSectionE + isSectionF + isSectionG + isSectionH + isSectionI + isSectionJ + isSectionK + Copy + Installed_Power + Volume + isSectionL + isSectionM + isSectionN + isSectionO + isSectionP + isSectionQ + isSectionR + isSectionS + isSectionT + isSectionU + isSectionV + isSectionW + isSectionX + isSectionY + isSectionZ + isSectionAA, data = df_train)

stargazer(modelB, type="text")

sqrt(mean(modelB$residuals^2))

vif(modelB)

# Volume, Type, Buliding Location and Installed power are still correlated, let's explore these correlations

dfdummy <- fastDummies::dummy_cols(df_train)

dfdummy[-c(5:7, 9:35)] |> 
  select_if(is.numeric) |> 
  cor() |> 
  corrplot::corrplot()

# Installed Power is correlated with Volume. These can be combined to Power_per_m3 or one can be removed
# Type and Building Location are correlated, since these are categorical variables, we can only pick one
df_train$Type_BL <- paste(df_train$Type, "_", df_train$Building_Location)
df_test$Type_BL <- paste(df_test$Type, "_", df_test$Building_Location)

df_train$Power_per_m3 <- df_train$Installed_Power / df_train$Volume
df_test$Power_per_m3 <- df_test$Installed_Power / df_test$Volume

# Test model without Type
modelC <- lm(ActualHT ~  Building_Location + Installed_Power + isSectionA + isSectionB + isSectionC + isSectionD + isSectionE + isSectionF + isSectionG + isSectionH + isSectionI + isSectionJ + isSectionK + Copy + Volume + isSectionL + isSectionM + isSectionN + isSectionO + isSectionP + isSectionQ + isSectionR + isSectionS + isSectionT + isSectionU + isSectionV + isSectionW + isSectionX + isSectionY + isSectionZ + isSectionAA, data = df_train)
stargazer(modelC, type ="text")

sqrt(mean(modelC$residuals^2))

vif(modelC)

# Test model without Building_Location
modelD <- lm(ActualHT ~  Type + Installed_Power + isSectionA + isSectionB + isSectionC + isSectionD + isSectionE + isSectionF + isSectionG + isSectionH + isSectionI + isSectionJ + isSectionK + Copy + Volume + isSectionL + isSectionM + isSectionN + isSectionO + isSectionP + isSectionQ + isSectionR + isSectionS + isSectionT + isSectionU + isSectionV + isSectionW + isSectionX + isSectionY + isSectionZ + isSectionAA, data = df_train)
stargazer(modelD, type ="text")

sqrt(mean(modelD$residuals^2))

vif(modelD)

# Test model with combined type and building location and power / m3
modelE <- lm(ActualHT ~  Power_per_m3 + Type_BL + isSectionA + isSectionB + isSectionC + isSectionD + isSectionE + isSectionF + isSectionG + isSectionH + isSectionI + isSectionJ + isSectionK + Copy + isSectionL + isSectionM + isSectionN + isSectionO + isSectionP + isSectionQ + isSectionR + isSectionS + isSectionT + isSectionU + isSectionV + isSectionW + isSectionX + isSectionY + isSectionZ + isSectionAA, data = df_train)
stargazer(modelE, type ="text")

sqrt(mean(modelE$residuals^2))

vif(modelE)




# The RMSE without Building Location is lower, but remains correlated, the model without Type solves all multicollinearity issues so we pick that one
# It also allows for keeping both Volume and Installed_Power as raw variables

stargazer(modelC, type="text")


# Make standard beta's for interpretation
standardbeta <- lm.beta(modelC)
standardbeta$standardized.coefficients
standardbeta <- standardbeta$standardized.coefficients

standardbeta <- data.frame(as.list(standardbeta)) 

standardbeta2 <- data.frame(t(standardbeta[-1]))
colnames(standardbeta2) <- standardbeta[, 1]

d <- standardbeta2
names <- rownames(d)
rownames(d) <- NULL
standardbeta2 <- cbind(names,d)
colnames(standardbeta2) <- c("Feature", "StandardBeta")

standardbeta2$ABSstdb <- abs(standardbeta2$StandardBeta)

standardbeta2 <- standardbeta2[order(standardbeta2$ABSstdb, decreasing = TRUE),]


# Plot standard beta's
standardbeta2 |>
  ggplot(aes(x=reorder(Feature,-ABSstdb), y = StandardBeta)) +
  geom_col() +
  xlab("Feature") +
  ylab("Standard Beta") +
  theme_bw(base_size = 15) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))




# BP test to test for heteroskedasticity
bptest(modelC)


# Plot residuals, BP test shows heteroskedasticity
df_train |>
  ggplot(aes(x = modelC$residuals)) +
  geom_histogram(binwidth = 2) +
  xlab("Residuals")  

# Residuals seem not evenly spread, let's check the qqplot
ggplot(df_train, aes(sample=modelC$residuals)) +
  stat_qq() + 
  stat_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_bw(base_size = 22)


# Try model with robust STD
seWhite <- sqrt( diag ( vcovHC ( modelC, tpye ="HC3")))




modelCW <- lm(ActualHT ~  Building_Location + isSectionA + isSectionB + isSectionC + isSectionD + isSectionE + isSectionF + isSectionG + isSectionH + isSectionI + isSectionJ + isSectionK + Copy + Volume + isSectionL + isSectionM + isSectionN + isSectionO + isSectionP + isSectionQ + isSectionR + isSectionS + isSectionT + isSectionU + isSectionV + isSectionW + isSectionX + isSectionY + isSectionZ + isSectionAA, data = df_train, weights = abs(1/resid(modelC)))
sqrt(mean((df_train$ActualHT - predict(modelCW, df_train))^2))

ggplot(df_train, aes(sample=modelCW$residuals)) +
  stat_qq() + 
  stat_qq_line()

ggplot(df_train, aes(sample=modelC$residuals)) +
  stat_qq() + 
  stat_qq_line()


# Does not help

# Try GLS

modelCGLS <- gls(ActualHT ~  Building_Location + Installed_Power + isSectionA + isSectionB + isSectionC + isSectionD + isSectionE + isSectionF + isSectionG + isSectionH + isSectionI + isSectionJ + isSectionK + Copy + Volume + isSectionL + isSectionM + isSectionN + isSectionO + isSectionP + isSectionQ + isSectionR + isSectionS + isSectionT + isSectionU + isSectionV + isSectionW + isSectionX + isSectionY + isSectionZ + isSectionAA, data = df_train, weights = varPower())
stargazer(modelCGLS, type="text")
sqrt(mean((df_train$ActualHT - predict(modelCGLS, df_train))^2))

plot(modelCGLS)

ggplot(df_train, aes(sample=modelCGLS$residuals)) +
  stat_qq() + 
  stat_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_bw(base_size = 22)


# Try log-linear and log-log
modelF <- lm(log(ActualHT) ~  Building_Location + isSectionA + isSectionB + isSectionC + isSectionD + isSectionE + isSectionF + isSectionG + isSectionH + isSectionI + isSectionJ + isSectionK + Copy + Installed_Power + Volume + isSectionL + isSectionM + isSectionN + isSectionO + isSectionP + isSectionQ + isSectionR + isSectionS + isSectionT + isSectionU + isSectionV + isSectionW + isSectionX + isSectionY + isSectionZ + isSectionAA, data = df_train)
sqrt(mean((df_train$ActualHT - exp(predict(modelF, df_train)))^2))


modelG <- lm(log(ActualHT) ~  Building_Location + isSectionA + isSectionB + isSectionC + isSectionD + isSectionE + isSectionF + isSectionG + isSectionH + isSectionI + isSectionJ + isSectionK + Copy + log(Installed_Power) + log(Volume) + isSectionL + isSectionM + isSectionN + isSectionO + isSectionP + isSectionQ + isSectionR + isSectionS + isSectionT + isSectionU + isSectionV + isSectionW + isSectionX + isSectionY + isSectionZ + isSectionAA, data = df_train)
sqrt(mean((df_train$ActualHT - exp(predict(modelG, df_train)))^2))


ggplot(df_train, aes(sample=modelF$residuals)) +
  stat_qq() + 
  stat_qq_line()

ggplot(df_train, aes(sample=modelG$residuals)) +
  stat_qq() + 
  stat_qq_line()

# Help with heteroskedasticity, but lower RMSE. For a baseline, the standard model is preferred.


# Make predictions for test set
df_test$predictedActualHT <- predict(modelC, df_test)


# RMSE on Train set
trainrmse <- sqrt(mean((df_train$ActualHT - predict(modelC, df_train))^2))

# RMSE on test set
testrmse <- sqrt(mean((df_test$ActualHT - df_test$predictedActualHT)^2))

# R squared
rsq_trad(df_test, ActualHT, predictedActualHT)

# Make pred/actual plot
ggplot(aes(x = ActualHT, y = predictedActualHT), data = df_test) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  xlab("Actuals") +
  ylab("Predictions") +
  theme_bw(base_size = 22)
