install.packages(tidymodels)
install.packages(GGally)
install.packages(lmtest)

library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidymodels)
library(GGally)
library(lmtest)


air = airquality
view(air)

air2 = air %>% drop_na()
ggpairs(air2, columns = 1:4)
ggcorr(air2,label=TRUE)

ggplot(air2,aes(x=Ozone,y=Temp)) + geom_point()

#Linear Regression Model
ozone_simple = recipe(Ozone ~ Temp, air2)

lm_model = linear_reg() %>% set_engine("lm")
lm_wflow = workflow() %>% add_model(lm_model) %>% add_recipe(ozone_simple)
lm_fit = fit(lm_wflow, air2)

summary(lm_fit$fit$fit$fit)


#Using Predict function
testdata = data.frame(Temp = c(80))
predict(lm_fit, new_data = testdata)
