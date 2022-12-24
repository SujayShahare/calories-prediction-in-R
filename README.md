# Calories burnt prediction using regression algorithms in R

This project aims to predict the number of calories burned by individuals based on various variables such as gender, age, weight, height, duration of exercise, heart rate, and body temperature.

## Dataset
The dataset used in this project is called [fmendes-DAT263x-demos](https://www.kaggle.com/datasets/fmendes/fmendesdat263xdemos?select=exercise.csv) and is available on Kaggle. It contains information on 15,000 individuals.

## Data Preprocessing
Before using the dataset for prediction, some preprocessing was done to ensure that there were no missing values and to assess the significance of the predictor variables. Specifically, the p-values of the predictor variables were calculated and compared to determine their significance in the prediction.

## Data Split
To evaluate the effectiveness of the prediction models, the dataset was split into a training set (70%) and a testing set (30%). The training set was used to train the prediction model, while the testing set was used to evaluate the model.

## Data Visualization
To better understand the distribution of the data and the correlations between the target and predictor variables, data visualization was performed using inbuilt plotting methods in R. This included visualizations such as the distribution of calories burned versus frequency, the distribution of input variables (e.g. duration of exercise, heart rate) versus frequency, and the distribution of input variables with respect to the predictor (calories burned).

## Algorithms
1. Simple Linear Regression
2. Multiple Linear Regression 
3. Decision Tree Regression
4. Random Forest Regression
5. Support Vector Regression
6. K-Nearest Neighbor (KNN) regression
7. XGBoost Regression

