# Multiple-linear-regression-with-Ames-housing-data-set
This repository contains the full R code, analysis workflow, and final model used to build a Multiple Linear Regression (MLR)–based predictive model for the Ames Housing Dataset.
The project demonstrates end‑to‑end regression modeling, including feature selection, assumption diagnostics, remedial measures, and model validation.
# Project Overview
This project develops a predictive model for Sale Price of residential properties in Ames, Iowa.
The workflow includes:

Exploratory Data Analysis (EDA)

Feature selection using domain knowledge

Dummy encoding for categorical variables

Outlier detection using Cook’s Distance

Log transformation of the response

Interaction term modeling

Weighted Least Squares (GLS) to correct heteroscedasticity

Model comparison using AIC/BIC

Final model validation on a held‑out test set
The final model is a Generalized Least Squares (GLS) regression with interaction terms and variance‑structure correction.

# Research Questions
1.Which property characteristics significantly influence residential sale prices?

2.How effectively can a Multiple Linear Regression model predict sale prices using these features?

# Key Techniques Used
## Feature Engineering
Selected 17 predictors from the original 79 variables

Created dummy variables for Building Type

Scaled continuous predictors

Removed near‑zero variance features
## Outlier Treatment using statistical measures
Identified high‑leverage points using Cook’s Distance

Removed influential outliers to stabilize model fit
## Interaction Modeling
Included meaningful interactions such as:

1.Lot Area × Gr Liv Area

2.Overall Quality × 1st Floor SF

3.Year Remodeled × Gr Liv Area

4.Bedroom Count × Lot Area
## Addressing Heteroscedasticity
Detected via Breusch–Pagan test and residual plots

Corrected using Weighted Least Squares (GLS) with variance structure modeling
## Model Comparison
AIC / BIC

Adjusted R²

Residual diagnostics

Predictive performance on test data

The GLS model achieved the lowest AIC and best residual behavior.
## Final Model Summary 
The final GLS model includes:

11 main predictors

7 interaction terms

Weighted variance structure to correct heteroscedasticity
It demonstrated: Strong predictive accuracy, Stable residual variance, Minimal overfitting, Good generalization on test data
## Model Validation 
Train/test split (70/30)

Prediction error comparison (MSE, RMSE)

Actual vs Predicted plots

Residual diagnostics on test data

The GLS model performed consistently across both datasets.
## Key findings and results 
Overall Quality and Gr Liv Area are the strongest predictors of price

Lot Area is less influential than expected

Building Type significantly affects price (townhouses and duplexes sell for less)

Interaction effects reveal nuanced relationships between space, quality, and layout

Weighted Least Squares greatly improved model reliability
