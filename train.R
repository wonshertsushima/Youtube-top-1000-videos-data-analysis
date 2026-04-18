lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")


lm_fit <- lm_spec %>%
  fit(like_ratio ~ is_short + has_hashtags, data = videos_analysis)


tidy(lm_fit)


model_results <- tidy(lm_fit, conf.int = TRUE)


videos_analysis <- videos_analysis %>%
  mutate(
    is_viral = as.factor(as.character(is_viral)),
    content_type = as.factor(content_type),
    is_short = as.factor(as.character(is_short))
  ) %>%
  drop_na(is_viral, is_short, content_type)


set.seed(123)
video_split <- initial_split(videos_analysis, prop = 0.75, strata = is_viral)
video_train <- training(video_split)
video_test  <- testing(video_split)


video_folds <- vfold_cv(video_train, v = 5, strata = is_viral)


knn_recipe <- recipe(is_viral ~ is_short + content_type, data = video_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

knn_spec_tune <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")


knn_workflow <- workflow() %>%
  add_recipe(knn_recipe) %>%
  add_model(knn_spec_tune)


k_grid <- grid_regular(neighbors(range = c(1, 50)), levels = 25)

knn_tune_results <- knn_workflow %>%
  tune_grid(resamples = video_folds, grid = k_grid)


best_k <- knn_tune_results %>% select_best(metric = "accuracy")

final_fit <- knn_workflow %>%
  finalize_workflow(best_k) %>%
  fit(data = video_train)


predictions <- final_fit %>%
  predict(video_test) %>%
  bind_cols(video_test)


print("--- THE OPTIMAL K ---")
print(best_k)

print("--- CONFUSION MATRIX ---")
predictions %>% conf_mat(truth = is_viral, estimate = .pred_class)

print("--- FINAL ACCURACY ---")
predictions %>% accuracy(truth = is_viral, estimate = .pred_class)


simulation_results <- final_fit %>%
  predict(random_videos) %>%
  bind_cols(random_videos) %>%

  bind_cols(predict(final_fit, random_videos, type = "prob"))


print("--- STRATEGIC CONTENT PREDICTIONS ---")
print(simulation_results)
