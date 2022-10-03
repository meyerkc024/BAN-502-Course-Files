library(tidyverse)
library(tidymodels)
library(mice) #package for imputation
library(VIM) #visualizing missingness
library(rpart) #for classification trees
library(rpart.plot) #for plotting trees
library(RColorBrewer) #better visualization of classification trees
library(rattle) #better visualization of classification trees
library(caret)


heart = read_csv("heart_disease-1.csv")

heart = heart %>% mutate(Sex = as_factor(Sex)) %>%
  mutate(ChestPainType = as_factor(ChestPainType)) %>%
  mutate(RestingECG = as_factor(RestingECG)) %>%
  mutate(ExerciseAngina = as_factor(ExerciseAngina)) %>%
  mutate(ST_Slope = as_factor(ST_Slope)) %>%
  mutate(HeartDisease = as_factor(HeartDisease)) %>%
  mutate(HeartDisease = fct_recode(HeartDisease, "No" = "0", "Yes" = "1"))

set.seed(12345)
heart_split = initial_split(heart, prop=.70, strata = HeartDisease)
train = training(heart_split)
test = testing(heart_split)

#Create Tree
heart_recipe = recipe(HeartDisease ~., train) %>%
  step_dummy(all_nominal(),-all_outcomes())

tree_model = decision_tree() %>% 
  set_engine("rpart", model = TRUE) %>% #don't forget the model = TRUE flag
  set_mode("classification") #notice different mode here for a regression tree

heart_wflow = 
  workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(heart_recipe)

heart_fit = fit(heart_wflow, train)

#Plot Tree
tree = heart_fit %>% 
  pull_workflow_fit() %>% 
  pluck("fit")

fancyRpartPlot(tree, tweak=1.25)

heart_fit$fit$fit$fit$cptable


#Fold
set.seed(123)
folds = vfold_cv(train, v = 5)


heart_recipe = recipe(HeartDisease ~., train) %>%
  step_dummy(all_nominal(),-all_outcomes())

tree_model = decision_tree(cost_complexity = tune()) %>% 
  set_engine("rpart", model = TRUE) %>% 
  set_mode("classification")

tree_grid = grid_regular(cost_complexity(),
                         levels = 25) #try 25 sensible values for cp

heart_wflow = 
  workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(heart_recipe)

tree_res = 
  heart_wflow %>% 
  tune_grid(
    resamples = folds,
    grid = tree_grid
  )

tree_res

#Collect Metrics
tree_res %>%
  collect_metrics() %>%
  ggplot(aes(cost_complexity, mean)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) 

best_tree = tree_res %>%
  select_best("accuracy")

best_tree


final_wf = 
  heart_wflow %>% 
  finalize_workflow(best_tree)

#Fit
final_fit = fit(final_wf, train)

tree = final_fit %>% 
  pull_workflow_fit() %>% 
  pluck("fit")

fancyRpartPlot(tree, tweak = 1.5) 

#Predictions & Confusion Matrix
treepred = predict(final_fit, train, type = "class")
confusionMatrix(treepred$.pred_class,train$HeartDisease,positive="Yes")

#Test Set Predictions & Confusion Matrix
treepred_test = predict(final_fit, test, type = "class")
confusionMatrix(treepred_test$.pred_class,test$HeartDisease,positive="Yes")































