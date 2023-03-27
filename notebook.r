# Read datasets Cleveland_hd.csv into hd_data
hd_data <- read.csv("datasets/Cleveland_hd.csv")

# take a look at the first 5 rows of hd_data
head(hd_data, 5)

last_value <- .Last.value

# These packages need to be loaded in the first `@tests` cell. 
library(testthat) 
library(IRkernel.testthat)

# Then follows one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.

hd_temp <- read.csv("datasets/Cleveland_hd.csv")


run_tests({

    test_that("Read in data correctly.", { 
        expect_equivalent(hd_data, hd_temp, 
            info = 'hd_data should contain the data in "datasets/Cleveland_hd.csv".')
    })
    
    test_that("Show the top 5 rows of data correctly", {
    expect_equal(dim(last_value)[1], 5, 
        info = "There should be 5 rows of data being printed out")
    })
    
})

# load the tidyverse package
library(tidyverse)

# Use the 'mutate' function from dplyr to recode our data
hd_data %>% mutate(hd = ifelse(class > 0, 1, 0))-> hd_data

# recode sex using mutate function and save as hd_data
hd_data %>% mutate(sex = factor(sex, levels = 0:1, labels = c("Female", "Male")))-> hd_data

# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
last_value <- .Last.value

correct_data <- hd_temp %>% mutate(hd = ifelse(class > 0, 1, 0),sex = factor(sex, levels = 0:1, labels = c("Female","Male"))) -> hd_data

run_tests({
    
    test_that("Test that tidyverse is loaded", {
    expect_true( "package:tidyverse" %in% search(), 
        info = "The tidyverse package should be loaded using library().")
    })
    
    test_that("hd is created correctly", {
    expect_equivalent(last_value$hd, correct_data$hd ,
        info = "hd should have value 1 if class>0.")
    })
    
    test_that("sex is recoded as factor correct", {
    expect_true(is.factor(last_value$sex),
        info = "Sex should be recoded into factor using factor().")
    expect_equivalent(last_value$sex, correct_data$sex ,
        info = "0=Female; 1=Male ")
    })
    
})

# Does sex have an effect? Sex is a binary variable in this dataset,
# so the appropriate test is chi-squared test
hd_sex <- chisq.test(hd_data$sex, hd_data$hd)

# Does age have an effect? Age is continuous, so we use a t-test
hd_age <- t.test(hd_data$age~hd_data$hd)

# What about thalach? Thalach is continuous, so we use a t-test
hd_heartrate <- t.test(hd_data$thalach~hd_data$hd)

# Print the results to see if p<0.05.
print(hd_sex)
print(hd_age)
print(hd_heartrate)

hd_sex_correct <- chisq.test(hd_data$sex, hd_data$hd)
hd_age_correct <- t.test(hd_data$age ~ hd_data$hd)
hd_heartrate_correct <- t.test(hd_data$thalach ~hd_data$hd)

run_tests({
    test_that("chi2 test is calculated correctly", {
    expect_equivalent(hd_sex$p, hd_sex_correct$p, 
        info = "Chi-squared test should be used to test the association between hd and sex.")
    })
    
    test_that("t.test is calculated correctly", {
    expect_equivalent(hd_age$statistic, hd_age_correct$statistic, 
        info = "t.test(y~groupvar) is the correct structure (not t.test(y,x)).")
    })
    
    test_that("t.test is calculated correctly", {
    expect_equivalent(hd_heartrate$statistic, hd_heartrate_correct$statistic, 
        info = "t.test(y~groupvar) should be used to test the association between hd and thalach")
    })
})

# Recode hd to be labelled
hd_data%>%mutate(hd_labelled = ifelse(hd == 0, "No Disease", "Disease")) -> hd_data

# age vs hd
ggplot(data = hd_data, aes(x = hd_labelled,y = age)) + geom_boxplot()

p <- last_plot()
hd_data_correct <- mutate(hd_data, hd_labelled = ifelse(hd==0, "No disease", "Disease"))
#p_correct <- ggplot(data = hd_data_correct, aes(x = hd_labelled,y = age)) + geom_boxplot()

run_tests({
    test_that("correct columns are plotted", {
        mappings <- str_replace(as.character(p$mapping), "~", "")
        expect_true(all(c("hd_labelled", "age") %in% mappings), 
            info = "You should plot hd_labelled on the x-axis and age on the y-axis.")
    })
})

# sex vs hd
ggplot(data = hd_data,aes(x=hd_labelled, fill=sex)) + geom_bar(position = "fill") + ylab("Sex %")

p <- last_plot()

#p_correct <- ggplot(data=hd_data, aes(x=hd_labelled, fill=sex)) + geom_bar(position="fill") + ylab("Sex %")

run_tests({
    test_that("correct columns are plotted", {
        mappings <- str_replace(as.character(p$mapping), "~", "")
        expect_true(all(c("hd_labelled", "sex") %in% mappings), 
            info = "You should plot hd_labelled on the x-axis and color fill the bar by sex.")
    })
    
    test_that("Y-axis is labelled correctly", {
    expect_equivalent(p$labels$y,"Sex %", 
            info = "You should plot hd_labelled on the x-axis, color fill the bar by sex and label the y-axis as Sex %")
    })
    
    test_that("Position is fill", {
    expect_true(p$layers[[1]]$position$fill, 
            info = "The position parameter should be 'fill'.")
    })
})

# max heart rate vs hd
ggplot(data = hd_data,aes(x=hd_labelled, y=thalach)) + geom_boxplot()

p <- last_plot()
# p_correct <- ggplot(data=hd_data,aes(x=hd_labelled,y=thalach))+geom_boxplot()

run_tests({
    test_that("correct columns are plotted", {
        mappings <- str_replace(as.character(p$mapping), "~", "")
        expect_true(all(c("hd_labelled", "thalach") %in% mappings), 
            info = "You should plot hd_labelled on the x-axis and thalach on the y-axis.")
    })
})

# use glm function from base R and specify the family argument as binomial
model <- glm(data = hd_data, hd ~ age + sex + thalach, family = "binomial")

# extract the model summary
summary(model)

lastvalue <-  .Last.value
model_correct <- glm(data = hd_data, hd ~ age + sex + thalach, family = "binomial" )

run_tests({
    test_that("the model is a glm object", {
    expect_is(model, "glm", 
        info = "The model should be a glm object.")
    })

    test_that("the model family is binomial", {
    expect_equivalent(model$family$family, "binomial", 
        info = "The model family should be binomial.")
    })
    
    
    test_that("the model summary is printed correctly", {
    expect_equivalent(lastvalue, summary(model_correct), 
        info = "The model summary should be printed using the summary() function.")
    })
})

# load the broom package
library(broom)

# tidy up the coefficient table
tidy_m <- tidy(model)
tidy_m

# calculate OR
tidy_m$OR <- exp(tidy_m$estimate)

# calculate 95% CI and save as lower CI and upper CI
tidy_m$lower_CI <- exp(tidy_m$estimate - 1.96 * tidy_m$std.error)
tidy_m$upper_CI <- exp(tidy_m$estimate + 1.96 * tidy_m$std.error)

# display the updated coefficient table
tidy_m

tidy_m_correct <- tidy(model)
tidy_m_correct$OR <- exp(tidy_m_correct$estimate)
tidy_m_correct$upper_CI <- exp(tidy_m_correct$estimate + 1.96 * tidy_m_correct$std.error)

run_tests({
test_that("Test that broom is loaded", {
    expect_true( "package:broom" %in% search(), 
        info = "The broom package should be loaded using library().")
    })
    
    test_that("OR is calculated correctly.", {
        expect_equivalent(tidy_m$OR, tidy_m_correct$OR, 
            info = 'OR=exp(model_table$estimate).')
    })

    test_that("upper CI is calculated correctly.", {
        expect_equivalent(tidy_m$upper_CI, tidy_m_correct$upper_CI, 
            info = 'upper_CI=exp(model_table$estimate+1.96*model_table$std.error).')
    })
})

# get the predicted probability in our dataset using the predict() function
pred_prob <- predict(model,hd_data, type = "response")

# create a decision rule using probability 0.5 as cutoff and save the predicted decision into the main data frame
hd_data$pred_hd <- ifelse(pred_prob >= 0.5, 1, 0)

# create a newdata data frame to save a new case information
newdata <- data.frame(age = 45, sex = "Female", thalach = 150)

# predict probability for this new case and print out the predicted value
p_new <- predict(model,newdata, type = "response")
p_new

pred_prob_correct <- predict(model, hd_data, type="response")
hd_data$pred_hd_correct <- ifelse(pred_prob_correct >= 0.5, 1, 0)
p_new_correct <- predict(model, newdata, type="response")

run_tests({
test_that("Test that pred_prob is calculated correctly", {
    expect_equivalent(pred_prob, pred_prob_correct, 
        info = "The predict() function should be used on the model object.")
    })
    
    test_that("Predicted hd status is calculated correctly.", {
        expect_equivalent(hd_data$pred_hd, hd_data$pred_hd_correct, 
            info = 'pred_prob>0.5 should be coded as 1, and 0 otherwise.')
    })

    test_that("The new person's HD probability is calculated correctly.", {
        expect_equivalent(p_new, p_new_correct, 
            info = 'Apply the predict() function on the newdata.')
    })
})

# load Metrics package
library(Metrics)

# calculate auc, accuracy, clasification error
auc <- auc(hd_data$hd, hd_data$pred_hd)
accuracy <- accuracy(hd_data$hd, hd_data$pred_hd)
classification_error <- ce(hd_data$hd, hd_data$pred_hd) 

# print out the metrics on to screen
print(paste("AUC=", auc))
print(paste("Accuracy=", accuracy))
print(paste("Classification Error=", classification_error))

# confusion matrix
table(hd_data$hd,hd_data$pred_hd, dnn=c("True Status", "Predicted Status")) # confusion matrix

lastvalue <- .Last.value #table output
auc_correct <- auc(hd_data$hd, hd_data$pred_hd) 
accuracy_correct <- accuracy(hd_data$hd, hd_data$pred_hd)
classification_error_correct <- ce(hd_data$hd, hd_data$pred_hd) 
confusionmatrix <- table(hd_data$hd, hd_data$pred_hd, dnn=c("True Status","Predicted Status"))

run_tests({
    test_that("AUC is calculated correctly", {
    expect_equal(auc,auc_correct, 
        info = "The auc() function should be used.")
    })
    
    test_that("Accuracy is calculated correctly.", {
        expect_equal(accuracy, accuracy_correct, 
            info = 'The accuracy() function should be used.')
    })

    test_that("Classification error is calculated correctly", {
    expect_equal(classification_error,classification_error_correct, 
        info = "The ce() function should be used.")
    })
    
    test_that("Confusion matrix is calculated correctly.", {
        expect_equivalent(lastvalue, confusionmatrix, 
            info = 'The first argument should be the true status and the second should be your predicted status.')
    })
})
