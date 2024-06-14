# Clean environment
rm(list=ls())

# Load data
df <- read.csv("your directory")

# Remove Parts per ton
df <- df[,-c(9)]

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

# Plot histogram DV
df |>
  ggplot(aes(x = ActualHT)) +
  geom_histogram(bins = 60) +
  ylab("Frequency") +
  xlab("Man hours / ton") +
  theme_bw(base_size = 22)

# Split data
set.seed(912341)
split <- initial_split(data = df, prop = 0.8, strata = ActualHT)

df_train <- training(split)
df_test <- testing(split)

# Create cross folds
set.seed(82002)
cv_folds <- df_train |> 
  vfold_cv(v = 5, strata = "ActualHT")

# Make correlation plots
df_train[c(1:8)] |> 
  select_if(is.numeric) |> 
  cor() |> 
  corrplot::corrplot()

df_train |> 
  select_if(is.numeric) |> 
  cor() |> 
  corrplot::corrplot()


# Make recipe and tune model
rf_recipe <- recipe(ActualHT ~ ., data = df_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

  
rf_model_tune <- 
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation")

rf_tune_wf <-
  workflow() |>
  add_recipe(rf_recipe) |>
  add_model(rf_model_tune)

class_metrics <- metric_set(rmse, rsq)

rf_tune_grid <- grid_regular(mtry(range = c(1, 21)), 
                             trees(range = c(100, 1000)),
                             min_n(range = c(2, 8)),
                             levels = c(21, 4, 4))


print(rf_tune_grid, n = 544)


num_cores <- parallel::detectCores()
num_cores
doParallel::registerDoParallel(cores = num_cores - 1L)

set.seed(99154344)
rf_tune_res <- tune_grid(
  rf_tune_wf,
  resamples = cv_folds,
  grid = rf_tune_grid,
  metrics = class_metrics
)

..show_notes(.Last.tune.result)

# Collect metrics
rf_metrics <- 
  rf_tune_res |> 
  collect_metrics()

rf_tune_res |> collect_predictions()

rf_metrics |> 
  select(mtry, trees, min_n, .metric, mean, .config) |>
  pivot_wider(names_from = ".metric", 
              values_from = "mean") |> 
  arrange(rmse) 


rf_metrics <- as.data.frame(rf_metrics)

# Filter metrics for plotting
rf_metrics_filtered <- rf_metrics[rf_metrics$trees == 100 ,]
rf_metrics_filtered <- rf_metrics_filtered[rf_metrics_filtered$min_n == 4 ,]

# Plot mtry
rf_metrics_filtered |>
  ggplot(aes(x = mtry, y = mean, ymin = mean - std_err,
             ymax = mean + std_err, 
             colour = .metric)) +
  geom_errorbar(size = 1) + 
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("#D55E00", "#0072B2")) +
  facet_wrap(~.metric, ncol = 1, scales = "free_y") +
  guides(colour = 'none') +
  theme_bw(base_size = 22) +
  xlab("Max features") +
  ylab("RMSE")

# Select best model
best_rf <- select_best(rf_tune_res)
rf_final_wf <- finalize_workflow(rf_tune_wf, best_rf)
rf_final_wf


set.seed(9923)
rf_final_fit <- 
  rf_final_wf |>
  last_fit(split, metrics = class_metrics)

final_model <- rf_final_fit$.workflow[[1]]

final_model %>% 
  augment(new_data = df_test) %>%
  ggplot(aes(ActualHT, .pred)) +
  geom_point() +
  geom_abline()

train_df_with_pred <- final_model %>% 
  augment(new_data = df_train)

test_df_with_pred <- final_model %>% 
  augment(new_data = df_test)

# Save predictions for train and test sets
df_with_pred <- rbind(train_df_with_pred, test_df_with_pred)

df_with_pred <- df_with_pred[order(df_with_pred$ActualHT),]

write.csv(df_with_pred, "df_with_pred_ppt.csv", row.names = FALSE)


rf_final_fit |>
  collect_metrics()

rf_final_fit |>
  collect_predictions()

predictionsrf <- rf_final_fit |>
  collect_predictions()

# Make pred/actual plot
rf_final_fit |> 
  collect_predictions() |> 
  ggplot(aes(x = ActualHT, y = .pred)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) +
  xlab("Actuals") +
  ylab("Predictions") +
  theme_bw(base_size = 22)

# Create shap values
set.seed(401) # for reproducibility
rf_final_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 35,
      metric = "rmse",
      smaller_is_better = TRUE)


predict_function_rf <- function(model, newdata) {
  predict(model, newdata) %>% pluck(.,1)}

mean_preds <- mean(predict_function_rf(extract_workflow(rf_final_fit), df_test %>% select(-ActualHT)))

fastshap::explain( 
  extract_workflow(rf_final_fit), 
  X = as.data.frame(df_test %>% select(-ActualHT)),
  nsim = 1000,
  pred_wrapper = predict_function_rf) -> explanations_rf


# Make shap plots
shapviz_rf <- shapviz(explanations_rf, X = df_test %>% select(-ActualHT), baseline = mean_preds)

sv_importance(shapviz_rf, kind = "beeswarm", show_numbers = TRUE, max_display = 15, number_size = 5)
sv_dependence(shapviz_rf, "Length", color_var = "auto")
sv_dependence(shapviz_rf, "Width", color_var = "auto")
sv_force(shapviz_rf, row_id = 300) # one row
sv_waterfall(shapviz_rf, row_id = 140, max_display = 12, annotation_size = 5) #one row


# Save shap values
explanations_rf <- as.data.frame(explanations_rf)
Shaprf <- colMeans(abs(explanations_rf))
write.csv(Shaprf, "Shaprf_ppt.csv")

write.csv(predictionsrf, "rf_with_ppt_predictions.csv")
