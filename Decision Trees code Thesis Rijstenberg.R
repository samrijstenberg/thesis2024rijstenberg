# Decision Trees

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
library(rpart.plot)



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
tree_model_tune <- 
  decision_tree(cost_complexity = tune(), tree_depth = tune()) |> 
  set_mode("regression") |> 
  set_engine("rpart")

tree_recipe <- 
  recipe(ActualHT ~ ., data = df_train) |> 
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors())

tree_wf <- 
  workflow() |> 
  add_recipe(tree_recipe) |> 
  add_model(tree_model_tune)

# Set tuning grid
tree_grid <- grid_regular(cost_complexity(range = c(-4, -1)), 
                          tree_depth(range = c(1, 30)),
                          levels = c(20, 30))

print(tree_grid, n = 100)

# Set metrics
reg_metrics <- metric_set(rmse, rsq)

# Tune model
tree_tune_res <- 
  tree_wf |> 
  tune_grid(resamples = cv_folds,
            grid = tree_grid,
            metrics = reg_metrics)

# Print results
tree_metrics <- 
  tree_tune_res |> 
  collect_metrics()
tree_metrics

# Plot tuning results
tree_metrics |> 
  filter(.metric %in% c("rmse")) |> 
  ggplot(aes(x = cost_complexity, y = mean, 
             ymin = mean - std_err, ymax = mean + std_err, 
             colour = factor(tree_depth))) +
  geom_line(size = 1.5) +
  scale_x_log10() +
  facet_wrap(~.metric) +
  theme_bw(base_size = 22) +
  ylab("RMSE") +
  labs(colour="Max Tree Depth") +
  xlab("Cost Complexity") 

tree_metrics |> 
  select(cost_complexity, tree_depth, .metric, mean, .config) |>
  pivot_wider(names_from = ".metric", 
              values_from = "mean") |> 
  arrange(rmse) 

# Model 208 performs best, select that configuration
tree_selected <- 
  tree_metrics |> 
  filter(.config == "Preprocessor1_Model208" & .metric == "rmse") |> 
  distinct(cost_complexity, tree_depth, .metric, mean, .config)

# Finalize workflow
tree_wf_finalized <- 
  tree_wf |> 
  finalize_workflow(tree_selected) 

# Retrain on entire training set and evaluate on test set
tree_last_fit <-
  tree_wf_finalized |>
  last_fit(split, metrics = reg_metrics)

# Plot tree
tree_last_fit |> 
  extract_fit_engine() |> 
  rpart.plot(roundint = FALSE, cex = 0.5)



# Get test RMSE
tree_last_fit |>
  collect_metrics()

# Get test predictions
tree_test_pred <- tree_last_fit |>
  collect_predictions()

tree_last_fit |> 
  collect_predictions() |> 
  ggplot(aes(x = ActualHT, y = .pred)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  xlab("Actuals") +
  ylab("Predictions") +
  theme_bw(base_size = 22)
