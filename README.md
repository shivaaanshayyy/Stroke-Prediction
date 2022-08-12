# Stroke-Prediction
Heart Stroke Prediction Using R
### Introduction
In today’s world, Stroke is a critical health problem. It severely affects human health and lives. It is the second most deadly disease since 20th century. Stroke is caused as a result of blockage or bleeding of blood vessels which reduces the flow of blood to the brain. Due to this brain does not receives sufficient oxygen or nutrients and brain cells start to die.

### Objectives
As a data analytic students we want to identify the risk factors for stroke. Main objective of our project is to predict whether people will have a stroke and reasons for stroke based on historical data. We are also interested in finding the stroke outcome for potential patients.
1. We are trying to find out whether input variables or features affect the stroke out- come.
2. We are predicting stroke outcome.

### Dataset
We got the dataset from kaggle: https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset? select=healthcare-dataset-stroke-data.csv

Our dataset showcases person’s body features and their stroke status. It contains 5110 rows with 12 columns. Each row gives the required information about the person. Over- all we are finding whether a person is likely to get stroke based on the input parameters such as BMI, hypertension, work type, etc.

Below is the attribute information.

1. id: unique
2. gender: “Male”, “Female” or “Other”
3. age: age of the person
4. hypertension: 0 if the person does not have hypertension, 1 if the person has hyper- tension
5. heart disease: 0 if the patient does not have any heart diseases, 1 if the patient has a heart disease
6. ever married: “No” or “Yes”
7. work type: “Children”, “Govt job”, “Never worked”, “Private” or “Self-employed”
8. Residence type: “Rural” or “Urban”
9. avg glucose level: average glucose level in blood
10. bmi: body mass index
11. smoking status: “formerly smoked”, “never smoked”, “smokes” or “Unknown”*
12. stroke: 1 if the patient had a stroke or 0 if not

### Summary of the Dataset
1. There are 11 input variables and 1 outcome(stroke) in the dataset. For our analysis we don’t need ID.
2. We can see that some of the columns values are in character, therefore we must change it into factor or number.
3. The interesting fact that we must observe is then mean of stroke is 0.04, which means only 4% of the patients have stroke.

### Conclusion

1. In this project, we used logistic regression to discover the relationship between stroke and other input features.
2. We get the conclusion that age, hypertension and work type self-employed would affect the possibility of getting stroke.
3. We also use logistic regression and random forest to build a prediction model for stroke. Both model reach the accuracy of 95%, but the imbalance of dataset has limited the accuracy level.
4. Using the Under Sampling method, we saw that the accuracy reduced to 75%.
5. Using the Over Sampling method, we saw that the accuracy increase to 99.08% using Random Forest Classifier.
6. It appears that for this particular Dataset, Over Sampling and Random Forest are among the best of the options that we have tried here.

### Referrnces
1. https://towardsdatascience.com/methods-for-dealing-with-imbalanced-data-5b761be45a18
2. https://towardsdatascience.com/understanding-random-forest-58381e0602d2 
3. https://www.statology.org/logistic-regression-in-r/
