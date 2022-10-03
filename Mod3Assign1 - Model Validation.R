library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidymodels)
library(GGally)
library(lmtest)
library(lubridate)


bike = read_csv("bike_cleaned-4.csv")
bike = bike %>% mutate(dteday = mdy(dteday)) 
bike = bike %>% mutate_if(is.character,as_factor)
bike = bike %>% mutate(hr = as_factor(hr))
head(bike,10)

set.seed(1234)
bike_split = initial_split(bike, prop=.70, strata=count)
train = training(bike_split)
test = testing(bike_split)

ggpairs(train)
ggcorr(train,label=TRUE)


train_recipe = recipe(count ~ season+mnth+hr+holiday+weekday+temp+weathersit, train)
lm_model = linear_reg() %>% set_engine("lm")
lm_wflow = workflow() %>% add_model(lm_model) %>% add_recipe(train_recipe)
lm_fit = fit(lm_wflow, train)
summary(lm_fit$fit$fit$fit)


testdata = data.frame(train)
predict_train = predict(lm_fit, new_data = testdata)
ggplot(predict_train,aes(x=.pred)) + geom_histogram()

lm_fit %>% predict(test) %>% bind_cols(test) %>% metrics(truth = count, estimate = .pred)


lm_fit %>% predict(test) %>% bind_cols(test) %>% metrics(truth = AnnualCharges, estimate = .pred)



