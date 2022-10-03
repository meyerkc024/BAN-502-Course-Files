library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidymodels)
library(GGally)
library(lmtest)
library(lubridate)
library(splines)

install.packages(glmnet)
install.packages(ggcorrplot)
install.packages(MASS)


bike = read_csv("bike_cleaned-3.csv")
bike = bike %>% mutate(dteday = mdy(dteday)) 
bike = bike %>% mutate_if(is.character,as_factor)
bike = bike %>% mutate(hr = as_factor(hr))
view(bike)
head(bike,10)

ggcorr(bike,label=TRUE)

ggplot(bike,aes(x=season,y=count)) + geom_boxplot()


bike_simple = recipe(count ~ hr, bike)
lm_model = linear_reg() %>% set_engine("lm")
lm_wflow = workflow() %>% add_model(lm_model) %>% add_recipe(bike_simple)
lm_fit = fit(lm_wflow, bike)
summary(lm_fit$fit$fit$fit)


bike_simple = recipe(count ~ hr+temp, bike)
lm_model = linear_reg() %>% set_engine("lm")
lm_wflow = workflow() %>% add_model(lm_model) %>% add_recipe(bike_simple)
lm_fit2 = fit(lm_wflow, bike)

summary(lm_fit2$fit$fit$fit)


bike_simple = recipe(count ~ temp+atemp, bike)
lm_model = linear_reg() %>% set_engine("lm")
lm_wflow = workflow() %>% add_model(lm_model) %>% add_recipe(bike_simple)
lm_fit3 = fit(lm_wflow, bike)

summary(lm_fit3$fit$fit$fit)


