# Elastic-Net Regression

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
library(themis)
library(glmnet)

# Split data
set.seed(912341)
split <- initial_split(data = df, prop = 0.8, strata = ActualHT)

df_train <- training(split)
df_test <- testing(split)

# Create cross validation folds
set.seed(82002)
cv_folds <- df_train |> 
  vfold_cv(v = 5, strata = "ActualHT")

# Create recipes and workflow
linear_reg_recipe <-
  recipe(ActualHT ~ ., data = df_train) |>
  step_dummy(all_nominal_predictors()) |>
  step_interact(~ all_predictors():all_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_predictors())

en_linear_reg <-
  linear_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet")

en_wf <-
  workflow() |>
  add_recipe(linear_reg_recipe) |>
  add_model(en_linear_reg)

# Set tuning grid
en_grid <- grid_regular(penalty(c(-2, 2),trans = log10_trans()),
                        mixture(c(0, 1)),
                        levels = c(51,21))

print(en_grid, n = 500)

# Tune model
en_tune <-
  en_wf |>
  tune_grid(
    resamples = cv_folds,
    grid = en_grid,
    metrics = metric_set(rmse, rsq)
  )

# Tune results
en_tune_metrics <-
  en_tune |>
  collect_metrics()
en_tune_metrics |>
  filter(.metric == "rmse") |>
  ggplot(aes(
    x = penalty, y = mean,
    ymin = mean - std_err, ymax = mean + std_err, colour = mixture
  )) +
  geom_pointrange(alpha = 0.5) +
  scale_x_log10() +
  labs(y = "RMSE", x = expression(lambda)) +
  theme_bw() +
  theme_bw(base_size = 22)

print(en_tune_metrics |> 
  select(penalty, mixture, .metric, mean, .config) |>
  pivot_wider(names_from = ".metric", 
              values_from = "mean") |> 
  arrange(rmse), n = 50)

# Pick model
best_en <-
  en_tune |>
  select_best(metric = "rmse")

best_en
en_final_wf <- finalize_workflow(en_wf, best_en)
en_final_wf

en_last_fit <-
  en_final_wf |>
  last_fit(split, metrics = metric_set(rmse, rsq))

en_test_metrics <-
  en_last_fit |>
  collect_metrics()

en_test_metrics

# Final plot
en_last_fit |> 
  collect_predictions() |> 
  ggplot(aes(x = ActualHT, y = .pred)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  xlab("Actuals") +
  ylab("Predictions") +
  theme_bw(base_size = 22)


