# -*- coding: utf-8 -*-
"""
Created on Sat Nov 18 15:04:32 2023

@author: chels
"""

import pandas as pd
from matplotlib import pyplot as plt
import seaborn as sns
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from sklearn.model_selection import KFold, cross_val_score
from sklearn.model_selection import RandomizedSearchCV
from scipy.stats import randint


#Load the dataset
course_data = pd.read_csv(r"C:\Users\chels\OneDrive\Desktop\Products_Information.csv")
course_data.info()

#Converting date to datetime for ease of use
course_data['date'] = pd.to_datetime(course_data['date'])

# Check data types of all columns
print("Data types after conversion:")
print(course_data.dtypes)

# Check the first few rows of the DataFrame
print("\nFirst few rows of the DataFrame:")
print(course_data.head())

#Pulling the mean and standard deviation from the original data
groupeddata = course_data.groupby(['store_nbr','product_type'])

mean_sales = groupeddata['sales'].mean()
std_sales = groupeddata['sales'].std()
print(f'Standard Deviation of {groupeddata} is {std_sales}')
print(f'Mean Sales of {groupeddata} is {mean_sales}')

#Encoding product type since it's categorical
label_encoder = LabelEncoder()
course_data['product_type'] = label_encoder.fit_transform(course_data['product_type'])

#CORRELATION heatmap
correlation_matrix = course_data.corr()
sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm')
plt.show()

#Adding year, month and dow as features
course_data['year'] = course_data['date'].dt.year
course_data['month'] = course_data['date'].dt.month
course_data['day_of_week'] = course_data['date'].dt.dayofweek
    
#Checking for any null or missing values
print(course_data.isnull().sum())

#Creating a Standardization Object
scaler = StandardScaler()

#Splitting the data into test and train data
train_data = course_data[(course_data['date'] >= '2016-01-01') & (course_data['date'] < '2017-07-31')]
test_data = course_data[(course_data['date'] >= '2017-07-31') & (course_data['date'] <= '2017-08-15')]

#Identifying the feature X and outcome variable Y in Dataframe for building the model
features = ['store_nbr', 'product_type', 'special_offer', 'year', 'month', 'day_of_week']
X_train = train_data[features]
y_train = train_data['sales']
X_test = test_data[features]
y_test = test_data['sales']

#Scaling and Transforming Train & Test data to fit our model
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)

#Model Training for Random Forest
model = RandomForestRegressor()

# Specify the number of folds, we have gone with 10 
k_fold = KFold(n_splits=10, shuffle = True, random_state=42)
#Performing a cross-validation check with 10 Folds
cv_scores = cross_val_score(model, X_train_scaled, y_train, cv=k_fold)
print(f"{len(cv_scores)}-fold CV Score: {cv_scores.mean():.2f}")

#Creating and Defining a Hyperparameter Grid
param_dist = {
    'n_estimators': randint(1,200),
    'max_depth': randint(1,25)
}

#Creating a RandomizedSearchCV object r2
random_search = RandomizedSearchCV(
    model,
    param_distributions=param_dist,
    n_iter=10,  # Number of parameter settings that are sampled
    cv=10,  # Number of cross-validation folds
    random_state=42
)

#Refit using all training data
random_search.fit(X_train_scaled, y_train)

#Getting the best set of parameters and estimator
best_params = random_search.best_params_
best_estimator = random_search.best_estimator_

#Prediction
#This formula will vary basis each model training
y_pred = random_search.predict(X_test_scaled)

df = pd.DataFrame(y_pred, columns=['Predicted Sales'])
output_file_path = "C:\\Users\\chels\\OneDrive\\Desktop\\Predicted_Data.csv"

df.to_csv(output_file_path, index = False)
print(f'Data has been saved to {output_file_path}')

#Calculating the Mean Squared Error
mse = mean_squared_error(y_test, y_pred)
print(f"Mean Squared Error: {mse}")

#Calculation of Mean Absolute Error
mae = mean_absolute_error(y_test, y_pred)
print(f'Mean Absolute Error: {mae}')

#Checking R Squared score for the accuracy of the model
r2 = r2_score(y_test, y_pred)
print(f"R-squared score: {r2}")

# Print the best parameters
print("\nBest Parameters:")
print(best_params)

#Find out the CV Score of the Best parameters set in place
print("\nMean CV Score of Best parameters:", random_search.best_score_)

# Print the best estimator
print("\nBest Estimator:")
print(best_estimator)