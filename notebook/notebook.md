## 1. Heart disease and potential risk factors
<p>Millions of people develop some sort of heart disease every year and heart disease is the biggest killer of both men and women in the United States and around the world. Statistical analysis has identified many risk factors associated with heart disease such as age, blood pressure, total cholesterol, diabetes, hypertension, family history of heart disease, obesity, lack of physical exercise, etc. In this notebook, we're going to run statistical tests and regression models using the Cleveland heart disease dataset to assess one particular factor -- maximum heart rate one can achieve during exercise and how it is associated with a higher likelihood of getting heart disease.</p>
<p><img src="https://assets.datacamp.com/production/project_445/img/run31.png" height="300" width="300"></p>


```R
# Read datasets Cleveland_hd.csv into hd_data
hd_data <- read.csv("datasets/Cleveland_hd.csv")

head(hd_data, 5)
```


<table class="dataframe">
<caption>A data.frame: 5 × 14</caption>
<thead>
	<tr><th></th><th scope=col>age</th><th scope=col>sex</th><th scope=col>cp</th><th scope=col>trestbps</th><th scope=col>chol</th><th scope=col>fbs</th><th scope=col>restecg</th><th scope=col>thalach</th><th scope=col>exang</th><th scope=col>oldpeak</th><th scope=col>slope</th><th scope=col>ca</th><th scope=col>thal</th><th scope=col>class</th></tr>
	<tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>1</th><td>63</td><td>1</td><td>1</td><td>145</td><td>233</td><td>1</td><td>2</td><td>150</td><td>0</td><td>2.3</td><td>3</td><td>0</td><td>6</td><td>0</td></tr>
	<tr><th scope=row>2</th><td>67</td><td>1</td><td>4</td><td>160</td><td>286</td><td>0</td><td>2</td><td>108</td><td>1</td><td>1.5</td><td>2</td><td>3</td><td>3</td><td>2</td></tr>
	<tr><th scope=row>3</th><td>67</td><td>1</td><td>4</td><td>120</td><td>229</td><td>0</td><td>2</td><td>129</td><td>1</td><td>2.6</td><td>2</td><td>2</td><td>7</td><td>1</td></tr>
	<tr><th scope=row>4</th><td>37</td><td>1</td><td>3</td><td>130</td><td>250</td><td>0</td><td>0</td><td>187</td><td>0</td><td>3.5</td><td>3</td><td>0</td><td>3</td><td>0</td></tr>
	<tr><th scope=row>5</th><td>41</td><td>0</td><td>2</td><td>130</td><td>204</td><td>0</td><td>2</td><td>172</td><td>0</td><td>1.4</td><td>1</td><td>0</td><td>3</td><td>0</td></tr>
</tbody>
</table>




## 2. Converting diagnosis class into outcome variable
<p>We noticed that the outcome variable <code>class</code> has more than two levels. According to the codebook, any non-zero values can be coded as an "event." Creating a new variable called <code>hd</code> to represent a binary 1/0 outcome.</p>
<p>There are a few other categorical/discrete variables in the dataset. Converting sex into a 'factor' for next step analysis. Otherwise, R will treat this as continuous by default.</p>
<p>The full data dictionary is also displayed here.</p>
<p><img src="https://assets.datacamp.com/production/project_445/img/datadict.png" height="500" width="500"></p>


```R
# load the tidyverse package
library(tidyverse)

hd_data %>% mutate(hd = ifelse(class > 0, 1, 0))-> hd_data

hd_data %>% mutate(sex = factor(sex, levels = 0:1, labels = c("Female", "Male")))-> hd_data
```



## 3. Identifying important clinical variables
<p>Now, using statistical tests to see which predictors are related to heart disease. We can explore the associations for each variable in the dataset. Depending on the type of the data (i.e., continuous or categorical), we use t-test or chi-squared test to calculate the p-values.</p>


```R
# Does sex have effect? Sex is categorical so chi-square test
hd_sex <- chisq.test(hd_data$sex, hd_data$hd)

# Does age have an effect? Age is continuous, so we use a t-test
hd_age <- t.test(hd_data$age~hd_data$hd)

# Does thalach have an effect? Thalach is continuous, so we use a t-test
hd_heartrate <- t.test(hd_data$thalach~hd_data$hd)

# Print the results to see if p<0.05.
print(hd_sex)
print(hd_age)
print(hd_heartrate)
```

    
    	Pearson's Chi-squared test with Yates' continuity correction
    
    data:  hd_data$sex and hd_data$hd
    X-squared = 22.043, df = 1, p-value = 2.667e-06
    
    
    	Welch Two Sample t-test
    
    data:  hd_data$age by hd_data$hd
    t = -4.0303, df = 300.93, p-value = 7.061e-05
    alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    95 percent confidence interval:
     -6.013385 -2.067682
    sample estimates:
    mean in group 0 mean in group 1 
           52.58537        56.62590 
    
    
    	Welch Two Sample t-test
    
    data:  hd_data$thalach by hd_data$hd
    t = 7.8579, df = 272.27, p-value = 9.106e-14
    alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    95 percent confidence interval:
     14.32900 23.90912
    sample estimates:
    mean in group 0 mean in group 1 
            158.378         139.259 
    



## 4. Explore the associations graphically (i)
<p>A good picture is worth a thousand words. In addition to p-values from statistical tests, we can plot the age, sex, and maximum heart rate distributions with respect to our outcome variable. This will give us a sense of both the direction and magnitude of the relationship.</p>
<p>First, let's plot age using a boxplot since it is a continuous variable.</p>


```R
# Recode hd to be labelled
hd_data%>%mutate(hd_labelled = ifelse(hd == 0, "No Disease", "Disease")) -> hd_data

# age vs hd
ggplot(data = hd_data, aes(x = hd_labelled,y = age)) + geom_boxplot()
```


    
![png](output_10_0.png)
    


## 5. Explore the associations graphically (ii)
<p>Next, let's plot sex using a barplot since it is a binary variable in this dataset.</p>


```R
# sex vs hd
ggplot(data = hd_data,aes(x=hd_labelled, fill=sex)) + geom_bar(position = "fill") + ylab("Sex %")
```


    
![png](output_13_0.png)
    


## 6. Explore the associations graphically (iii)
<p>And finally, let's plot thalach using a boxplot since it is a continuous variable.</p>


```R
# max heart rate vs hd
ggplot(data = hd_data,aes(x=hd_labelled, y=thalach)) + geom_boxplot()
```


    
![png](output_16_0.png)
    



## 7. Putting all three variables in one model
<p>The plots and the statistical tests both confirmed that all the three variables are highly significantly associated with our outcome (p&lt;0.001 for all tests). </p>
<p>In general, we want to use multiple logistic regression when we have one binary outcome variable and two or more predicting variables. The binary variable is the dependent (Y) variable; we are studying the effect that the independent (X) variables have on the probability of obtaining a particular value of the dependent variable. For example, we might want to know the effect that maximum heart rate, age, and sex have on the probability that a person will have a heart disease in the next year. The model will also tell us what the remaining effect of maximum heart rate is after we control or adjust for the effects of the other two effectors. </p>
<p>The <code>glm()</code> command is designed to perform generalized linear models (regressions) on binary outcome data, count data, probability data, proportion data, and many other data types.</p>


```R
# use glm function from base R and specify the family argument as binomial
model <- glm(data = hd_data, hd ~ age + sex + thalach, family = "binomial")

# extract the model summary
summary(model)
```


    
    Call:
    glm(formula = hd ~ age + sex + thalach, family = "binomial", 
        data = hd_data)
    
    Deviance Residuals: 
        Min       1Q   Median       3Q      Max  
    -2.2250  -0.8486  -0.4570   0.9043   2.1156  
    
    Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
    (Intercept)  3.111610   1.607466   1.936   0.0529 .  
    age          0.031886   0.016440   1.940   0.0524 .  
    sexMale      1.491902   0.307193   4.857 1.19e-06 ***
    thalach     -0.040541   0.007073  -5.732 9.93e-09 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    (Dispersion parameter for binomial family taken to be 1)
    
        Null deviance: 417.98  on 302  degrees of freedom
    Residual deviance: 332.85  on 299  degrees of freedom
    AIC: 340.85
    
    Number of Fisher Scoring iterations: 4




## 8. Extracting useful information from the model output
<p>It's common practice in medical research to report Odds Ratio (OR) to quantify how strongly the presence or absence of property A is associated with the presence or absence of the outcome. When the OR is greater than 1, we say A is positively associated with outcome B (increases the Odds of having B). Otherwise, we say A is negatively associated with B (decreases the Odds of having B).</p>
<p>The raw glm coefficient table (the 'estimate' column in the printed output) in R represents the log(Odds Ratios) of the outcome. Therefore, we need to convert the values to the original OR scale and calculate the corresponding 95% Confidence Interval (CI) of the estimated Odds Ratios when reporting results from a logistic regression. </p>


```R
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
```


<table class="dataframe">
<caption>A tibble: 4 × 5</caption>
<thead>
	<tr><th scope=col>term</th><th scope=col>estimate</th><th scope=col>std.error</th><th scope=col>statistic</th><th scope=col>p.value</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>(Intercept)</td><td> 3.11161046</td><td>1.607466382</td><td> 1.935724</td><td>5.290157e-02</td></tr>
	<tr><td>age        </td><td> 0.03188572</td><td>0.016439824</td><td> 1.939541</td><td>5.243548e-02</td></tr>
	<tr><td>sexMale    </td><td> 1.49190218</td><td>0.307192627</td><td> 4.856569</td><td>1.194372e-06</td></tr>
	<tr><td>thalach    </td><td>-0.04054143</td><td>0.007072952</td><td>-5.731897</td><td>9.931367e-09</td></tr>
</tbody>
</table>




<table class="dataframe">
<caption>A tibble: 4 × 8</caption>
<thead>
	<tr><th scope=col>term</th><th scope=col>estimate</th><th scope=col>std.error</th><th scope=col>statistic</th><th scope=col>p.value</th><th scope=col>OR</th><th scope=col>lower_CI</th><th scope=col>upper_CI</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>(Intercept)</td><td> 3.11161046</td><td>1.607466382</td><td> 1.935724</td><td>5.290157e-02</td><td>22.4571817</td><td>0.9617280</td><td>524.3946593</td></tr>
	<tr><td>age        </td><td> 0.03188572</td><td>0.016439824</td><td> 1.939541</td><td>5.243548e-02</td><td> 1.0323995</td><td>0.9996637</td><td>  1.0662073</td></tr>
	<tr><td>sexMale    </td><td> 1.49190218</td><td>0.307192627</td><td> 4.856569</td><td>1.194372e-06</td><td> 4.4455437</td><td>2.4346539</td><td>  8.1173174</td></tr>
	<tr><td>thalach    </td><td>-0.04054143</td><td>0.007072952</td><td>-5.731897</td><td>9.931367e-09</td><td> 0.9602694</td><td>0.9470490</td><td>  0.9736743</td></tr>
</tbody>
</table>




## 9. Predicted probabilities from our model
<p>So far, we have built a logistic regression model and examined the model coefficients/ORs. We may wonder how can we use this model we developed to predict a person's likelihood of having heart disease given his/her age, sex, and maximum heart rate. Furthermore, we'd like to translate the predicted probability into a decision rule for clinical use by defining a cutoff value on the probability scale. In practice, when an individual comes in for a health check-up, the doctor would like to know the predicted probability of heart disease, for specific values of the predictors: a 45-year-old female with a max heart rate of 150. To do that, we create a data frame called newdata, in which we include the desired values for our prediction.</p>


```R
# get the predicted probability in our dataset using the predict() function
pred_prob <- predict(model,hd_data, type = "response")

# create a decision rule using probability 0.5 as cutoff and save the predicted decision into the main data frame
hd_data$pred_hd <- ifelse(pred_prob >= 0.5, 1, 0)

# create a newdata data frame to save a new case information
newdata <- data.frame(age = 45, sex = "Female", thalach = 150)

# predict probability for this new case and print out the predicted value
p_new <- predict(model,newdata, type = "response")
p_new
```


<strong>1:</strong> 0.177300249223782



## 10. Model performance metrics
<p>Are the predictions accurate? How well does the model fit our data? We are going to use some common metrics to evaluate the model performance.</p>


```R
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
```

    [1] "AUC= 0.706483593612915"
    [1] "Accuracy= 0.70957095709571"
    [1] "Classification Error= 0.29042904290429"



               Predicted Status
    True Status   0   1
              0 122  42
              1  46  93

