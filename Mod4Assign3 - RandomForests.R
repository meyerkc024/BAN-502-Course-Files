library(tidyverse)
library(tidymodels)
library(mice) #package for imputation
library(VIM) #visualizing missingness
library(ranger) #for random forests
library(randomForest) #also for random forests
library(caret)
library(skimr)
library(GGally)
library(gridExtra)
library(vip) #variable importance
library(readr)


drug = read_csv("drug_data-2.csv")

names(drug) = c("ID", "Age", "Gender", "Education", "Country", "Ethnicity",
                "Nscore", "Escore", "Oscore", "Ascore", "Cscore", "Impulsive",
                "SS", "Alcohol", "Amphet", "Amyl", "Benzos", "Caff", "Cannabis",
                "Choc", "Coke", "Crack", "Ecstasy", "Heroin", "Ketamine", "Legalh",
                "LSD", "Meth", "Mushrooms", "Nicotine", "Semer", "VSA")

drug[drug == "CL0"] = "No"
drug[drug == "CL1"] = "No"
drug[drug == "CL2"] = "Yes"
drug[drug == "CL3"] = "Yes"
drug[drug == "CL4"] = "Yes"
drug[drug == "CL5"] = "Yes"
drug[drug == "CL6"] = "Yes"

drug_clean = drug %>% mutate_at(vars(Age:Ethnicity), funs(as_factor)) %>%
  mutate(Age = factor(Age, labels = c("18_24", "25_34", "35_44", "45_54",
                                      "55_64", "65_"))) %>%
  mutate(Gender = factor(Gender, labels = c("Male", "Female"))) %>%
  mutate(Education = factor(Education, labels = c("Under16", "At16", "At17", "At18",
                                                  "SomeCollege","ProfessionalCert",
                                                  "Bachelors", "Masters",
                                                  "Doctorate"))) %>%
  mutate(Country = factor(Country, labels = c("USA", "NewZealand", "Other", "Australia",
                                              "Ireland","Canada","UK"))) %>%
  mutate(Ethnicity = factor(Ethnicity, labels = c("Black", "Asian", "White",
                                                  "White/Black", "Other",
                                                  "White/Asian", "Black/Asian"))) %>%
  mutate_at(vars(Alcohol:VSA), funs(as_factor)) %>%
  select(-ID)

str(drug_clean)
head(drug_clean,10)

drug_clean = drug_clean %>% select(!(Alcohol:Mushrooms)) %>% select(!(Semer:VSA))

head(drug_clean,10)

skim(drug_clean) #Check for missing data

set.seed(1234)
drug_split = initial_split(drug_clean, prop=.70, strata=Nicotine)
train = training(drug_split)
test = testing(drug_split)
head(train,10)

p1 = ggplot(train, aes(x = Age, fill = Nicotine)) + geom_bar(position = "fill")
p2 = ggplot(train, aes(x = Gender, fill = Nicotine)) + geom_bar(position = "fill")
p3 = ggplot(train, aes(x = Education, fill = Nicotine)) + geom_bar(position = "fill")
p4 = ggplot(train, aes(x = Country, fill = Nicotine)) + geom_bar(position = "fill")
grid.arrange(p1,p2,p3,p4)

p5 = ggplot(train, aes(x = Ethnicity, fill = Nicotine)) + geom_bar(position = "fill")
p6 = ggplot(train, aes(x = Nscore, fill = Nicotine)) + geom_bar(position = "fill")
p7 = ggplot(train, aes(x = Escore, fill = Nicotine)) + geom_bar(position = "fill")
p8 = ggplot(train, aes(x = Oscore, fill = Nicotine)) + geom_bar(position = "fill")
grid.arrange(p5,p6,p7,p8)

p9 = ggplot(train, aes(x = Ascore, fill = Nicotine)) + geom_bar(position = "fill")
p10 = ggplot(train, aes(x = Cscore, fill = Nicotine)) + geom_bar(position = "fill")
p11 = ggplot(train, aes(x = Impulsive, fill = Nicotine)) + geom_bar(position = "fill")
p12 = ggplot(train, aes(x = SS, fill = Nicotine)) + geom_bar(position = "fill")
grid.arrange(p9,p10,p11,p12)


#Create Fold
set.seed(123)
rf_folds = vfold_cv(train, v = 5)

#Random Forest
drug_recipe = recipe(Nicotine ~., train) %>%
  step_dummy(all_nominal(), -all_outcomes())

rf_model = rand_forest(mtry = tune(), min_n = tune(), trees = 100) %>% #add tuning of mtry and min_n parameters
  #setting trees to 100 here should also speed things up a bit, but more trees might be better
  set_engine("ranger", importance = "permutation") %>% #added importance metric
  set_mode("classification")

drug_wflow = 
  workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(drug_recipe)

rf_grid = grid_regular(
  mtry(range = c(2, 8)), #these values determined through significant trial and error
  min_n(range = c(5, 20)), #these values determined through significant trial and error
  levels = 10
)

set.seed(123)
rf_res_tuned = tune_grid(
  drug_wflow,
  resamples = rf_folds,
  grid = rf_grid #use the tuning grid
)


#Check for accuracy
rf_res_tuned %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Accuracy")



best_rf = select_best(rf_res_tuned, "accuracy")

final_rf = finalize_workflow(
  drug_wflow,
  best_rf)

final_rf

final_rf_fit = fit(final_rf, train)

final_rf_fit %>% pull_workflow_fit() %>% vip(geom = "point")


trainpredrf = predict(final_rf_fit, train)
confusionMatrix(trainpredrf$.pred_class, train$Nicotine, 
                positive = "Yes")

testpredrf = predict(final_rf_fit, test)
confusionMatrix(testpredrf$.pred_class, test$Nicotine, 
                positive = "Yes")









