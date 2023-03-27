# What-your-Heart-Rate-is-Telling-you

This project uses the Cleveland heart disease dataset to assess how maximum heart rate is associated with a higher likelihood of getting heart disease. Heart disease is the biggest killer of both men and women in the United States and around the world. Statistical analysis has identified many risk factors associated with heart disease such as age, blood pressure, total cholesterol, diabetes, hypertension, family history of heart disease, obesity, lack of physical exercise, etc.

## Data
The dataset used in this project is available in the datasets directory. The dataset contains 303 rows and 14 columns, including information on the patient's age, sex, chest pain type, resting blood pressure, serum cholesterol levels, fasting blood sugar levels, electrocardiogram results, maximum heart rate achieved during exercise, exercise-induced angina, ST depression induced by exercise relative to rest, slope of the peak exercise ST segment, number of major vessels colored by fluoroscopy, thalassemia, and the diagnosis of heart disease (target variable).

## Exploratory Data Analysis
Converting diagnosis class into outcome variable
The outcome variable class has more than two levels, according to the codebook, any non-zero values can be coded as an "event." Therefore, a new variable called hd is created to represent a binary 1/0 outcome. Additionally, sex is converted into a 'factor' for next step analysis.

## Identifying important clinical variables
The associations of each variable in the dataset are explored by statistical tests to see which predictors are related to heart disease. Chi-squared and t-tests are used to calculate p-values for categorical and continuous variables, respectively. The results indicate that all three variables, age, sex, and maximum heart rate achieved during exercise (thalach), are significantly associated with heart disease (p<0.001 for all tests).

## Exploring associations graphically
The age distribution is plotted using a boxplot, and the sex distribution is plotted using a barplot. The maximum heart rate distribution is plotted using a boxplot, each with respect to the outcome variable. The plots provide a visual sense of the direction and magnitude of the relationship.

## Putting all three variables in one model
Multiple logistic regression is used to study the effect that the independent variables have on the probability of obtaining a particular value of the dependent variable. The model tells us the remaining effect of maximum heart rate after we control or adjust for the effects of the other two effectors. The results indicate that age, sex, and maximum heart rate achieved during exercise (thalach) are significantly associated with heart disease (p<0.001 for all variables).

## Extracting useful information from the model output
Odds ratios (OR) are used to quantify how strongly the presence or absence of a variable is associated with the presence or absence of the outcome. The raw glm coefficient table is converted to the original OR scale and the corresponding 95% confidence interval (CI) of the estimated Odds Ratios are calculated. The results suggest that male patients are almost 4.5 times more likely to have heart disease than female patients. An increase of 1 bpm in maximum heart rate achieved during exercise decreases the odds of heart disease by about 4%. Finally, an increase in age of 1 year increases the odds of having heart disease by about 3%.

## Conclusion
In conclusion, our logistic regression model shows that age, male sex, and maximum heart rate are important predictors of heart disease. These three predictors are significantly associated with the odds of having heart disease, controlling for the effect of the other two predictors. The strongest predictor of heart disease is sex, with males being 4.44 times more likely to have heart disease than females, holding age and maximum heart rate constant. Maximum heart rate is also an important predictor, with every one unit increase in maximum heart rate decreasing the odds of having heart disease by 4%, holding age and sex constant. However, the evidence for the effect of age is weak.
