install.packages(e1071)
install.packages(ROCR)
library(ROCR)
library(e1071)

parole = read_csv("parole.csv") 
parole = parole %>% mutate(male = as_factor(male)) %>% mutate(male = fct_recode(male, "Female" = "0", "Male" = "1" )) 
parole = parole %>% mutate(race = as_factor(race)) %>% mutate(race = fct_recode(race, "Other" = "2", "White" = "1" ))
parole = parole %>% mutate(state = as_factor(state)) %>% mutate(state = fct_recode(state, "Kentucky" = "2", "Louisiana" = "3","Virginia" = "4","Other" = "1")) 
parole = parole %>% mutate(crime = as_factor(crime)) %>% mutate(crime = fct_recode(crime, "Larceny" = "2", "Drug" = "3","Driving" = "4","Other" = "1")) 
parole = parole %>% mutate(multiple.offenses = as_factor(multiple.offenses)) %>% mutate(multiple.offenses = fct_recode(multiple.offenses, "Yes" = "1", "No" = "0")) 
parole = parole %>% mutate(violator = as_factor(violator)) %>% mutate(violator = fct_recode(violator, "No" = "0", "Yes" = "1" ))

parole.violator = parole %>% filter(violator=="Yes")
head(parole.violator,10)


set.seed(12345)
parole_split = initial_split(parole, prop=.70, strata=violator)
train = training(parole_split)
test = testing(parole_split)

levels(train$violator)
train = train %>% mutate(violator = fct_relevel(violator, c("No","Yes"))) ##If needed to reorder

ggplot(train,aes(x=violator)) + geom_bar()

violator_model = logistic_reg() %>% set_engine("glm")
violator_recipe = recipe(violator ~ state + multiple.offenses + race, train) %>% step_dummy(all_nominal(), -all_outcomes())
logref_wf = workflow() %>% add_recipe(violator_recipe) %>% add_model(violator_model)
violator_fit = fit(logref_wf, train)

summary(violator_fit$fit$fit$fit)

newdata = data.frame(state="Louisiana", multiple.offenses="Yes",race="White")
predictions = predict(violator_fit, newdata, type="prob")[2]
head(predictions)

ROCRPred = prediction(predictions, newdata$violator)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))



#Change this next line to the names of your predictions and the response variable in the training data frame
ROCRpred = prediction(predictions, credit$SeriousDlqin2yrs) 




















