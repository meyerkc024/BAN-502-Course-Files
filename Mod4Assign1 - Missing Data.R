options(tidyverse.quiet = TRUE)
library(titanic)
library(tidyverse)
library(tidymodels)
library(mice) #package for imputation
library(VIM) #visualizing missingness
library(naniar) #visualizing missingness
library(skimr) #alternative way to view dataset summaries
library(UpSetR) #visualizing missingness
library(readr)

grades = read_csv("class-grades.csv")
head(grades,10)


str(grades)
summary(grades)
skim(grades)


gg_miss_var(grades)

grades_rowdel = grades %>% drop_na()
skim(grades_rowdel)

grades_coldel = grades %>% select(-Tutorial,-Midterm,-TakeHome,-Final)
vim_plot = aggr(grades_coldel, numbers = TRUE, prop = c(TRUE,FALSE),cex.axis=.7)
skim(grades_coldel)


set.seed(123)
imp_Final = mice(grades,m=5,method='pmm', printFlag = FALSE)
summary(imp_Final)
#m is the number of imputations, 5 is a reasonable value as a default
#pmm is "predictive mean matching" = imputation method for numeric data
#printFlag reduces amount of output

densityplot(imp_Final, ~Final)
grades_complete = complete(imp_Final)
summary(grades_complete)







