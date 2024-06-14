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
library(neuralnet)
library(brulee)
library(torch)

# Split data
set.seed(912341)
split <- initial_split(data = df, prop = 0.8, strata = ActualHT)

df_train <- training(split)
df_test <- testing(split)

# Create CV folds
set.seed(82002)
cv_folds <- df_train |> 
  vfold_cv(v = 5, strata = "ActualHT")

# Recipe relu
ann_recipe <- recipe(ActualHT ~ ., data = df_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


ann_nnet_mlp <- mlp(hidden_units = tune(), learn_rate = tune(), activation = 'relu', epochs = 100) |> 
  set_engine("brulee", optimizer = 'SGD', validation = 0, batch_size = tune()) |> 
  set_mode("regression")
ann_nnet_mlp |> translate()

nnet_mlp_wf <- 
  workflow() |>
  add_recipe(ann_recipe) |>
  add_model(ann_nnet_mlp)

regr_metrics <- metric_set(rmse, rsq)

nn_grid <- grid_regular(hidden_units(range = c(2, 20)),
                        learn_rate(range = c(-5, 0)),
                        batch_size(range = c(5, 7)),
       levels = c(19, 6, 3))
print(nn_grid, n = 990)


# Tune model relu
cl <- parallel::makePSOCKcluster(5)
doParallel::registerDoParallel(cl)


set.seed(99154345)
nnet_tune_res <- tune_grid(
  nnet_mlp_wf,
  resamples = cv_folds,
  grid = nn_grid,
  metrics = regr_metrics)

show_notes(.Last.tune.result)
parallel::stopCluster(cl)

nnet_tune_res |> 
  collect_metrics() |> 
  ggplot(aes(x = hidden_units, y = mean, ymin = mean - std_err, 
             ymax = mean + std_err, colour = learn_rate)) +
  geom_pointrange() + 
  geom_line()  +
  facet_wrap(~.metric) +
  theme_bw() +
  ylab("RMSE") +
  xlab("Number of Hidden Units") 

# Get results relu
ann_metrics <- 
  nnet_tune_res |> 
  collect_metrics()

ann_metrics |> 
  select(hidden_units, batch_size, learn_rate, .metric, mean, .config) |>
  pivot_wider(names_from = ".metric", 
              values_from = "mean") |> 
  arrange(rmse)

# Pick best performing model relu
nnet_selected <- 
  nnet_tune_res |> 
  select_best(metric = "rmse")

nnet_mlp_final_wf <- 
  nnet_mlp_wf |> 
  finalize_workflow(nnet_selected)

set.seed(9523)
nnet_mlp_final_fit <- 
  nnet_mlp_final_wf |> 
  last_fit(split, metrics = regr_metrics)





# Recipe tanh
ann_recipe_tanh <- recipe(ActualHT ~ ., data = df_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


ann_nnet_mlp_tanh <- mlp(hidden_units = tune(), learn_rate = tune(), activation = 'tanh', epochs = 100) |> 
  set_engine("brulee", optimizer = 'SGD', validation = 0, batch_size = tune()) |> 
  set_mode("regression")
ann_nnet_mlp_tanh |> translate()

nnet_mlp_wf_tanh <- 
  workflow() |>
  add_recipe(ann_recipe_tanh) |>
  add_model(ann_nnet_mlp_tanh)

# Tune model tanh
cl <- parallel::makePSOCKcluster(5)
doParallel::registerDoParallel(cl)


set.seed(99154345)
nnet_tune_res_tanh <- tune_grid(
  nnet_mlp_wf_tanh,
  resamples = cv_folds,
  grid = nn_grid,
  metrics = regr_metrics)

show_notes(.Last.tune.result)
parallel::stopCluster(cl)

nnet_tune_res_tanh |> 
  collect_metrics() |> 
  ggplot(aes(x = hidden_units, y = mean, ymin = mean - std_err, 
             ymax = mean + std_err, colour = learn_rate)) +
  geom_pointrange() + 
  geom_line()  +
  facet_wrap(~.metric) +
  theme_bw() +
  ylab("RMSE") +
  xlab("Number of Hidden Units") 

# Get results tanh
ann_metrics_tanh <- 
  nnet_tune_res_tanh |> 
  collect_metrics()

ann_metrics_tanh |> 
  select(hidden_units, batch_size, learn_rate, .metric, mean, .config) |>
  pivot_wider(names_from = ".metric", 
              values_from = "mean") |> 
  arrange(rmse)

# Pick best performing model tanh
nnet_selected_tanh <- 
  nnet_tune_res_tanh |> 
  select_best(metric = "rmse")

nnet_mlp_final_wf_tanh <- 
  nnet_mlp_wf_tanh |> 
  finalize_workflow(nnet_selected_tanh)

set.seed(9523)
nnet_mlp_final_fit_tanh <- 
  nnet_mlp_final_wf_tanh |> 
  last_fit(split, metrics = regr_metrics)


# Make pred/actual plot
nnet_mlp_final_fit |> 
  collect_predictions() |> 
  ggplot(aes(x = ActualHT, y = .pred)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  xlab("Actuals") +
  ylab("Predictions") +
  theme_bw(base_size = 22)

nnet_mlp_final_fit |>
  collect_metrics()

# Save predictions
predictions_ann <- nnet_mlp_final_fit |>
  collect_predictions()

write.csv(predictions_ann, "your directory", row.names=FALSE)


# Get SHAP values
predict_function_ann <- function(model, newdata) {
  predict(model, newdata) %>% pluck(.,1)}

mean_preds <- mean(predict_function_ann(extract_workflow(nnet_mlp_final_fit), df_test %>% select(-ActualHT)))


fastshap::explain( 
  extract_workflow(nnet_mlp_final_fit), 
  X = as.data.frame(df_test %>% select(-ActualHT)),
  nsim = 1000,
  pred_wrapper = predict_function_ann) -> explanations_ann

shapviz_ann <- shapviz(explanations_ann, X = df_test %>% select(-ActualHT), baseline = mean_preds)


sv_importance(shapviz_ann, kind = "beeswarm", show_numbers = TRUE, max_display = 15, number_size = 5)
sv_dependence(shapviz_ann, "Length", color_var = "auto")
sv_dependence(shapviz_ann, "Width", color_var = "auto")
sv_force(shapviz_ann, row_id = 300) # one row
sv_waterfall(shapviz_ann, row_id = 140, max_display = 12, annotation_size = 5) #one row


explanations_ann <- as.data.frame(explanations_ann)
Shapann <- colMeans(abs(explanations_ann))
write.csv(Shapann, "Shapann.csv")

rsq_trad(predictions_ann, ActualHT, .pred)


