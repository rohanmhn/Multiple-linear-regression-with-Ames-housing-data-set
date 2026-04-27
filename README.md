# Multiple-linear-regression-with-Ames-housing-data-set
MLR for Clark research project. This R file incorporates the linear model we fitted for the MS housing data set based on selected variables to demonstrate: - model fit - remedial measures used - outlier treatment - statistical validation of the model
This project applies Multiple Linear Regression (MLR) to predict residential property sale prices using the Ames Housing Dataset which represents a rich, multi-dimensional dataset from Ames, Iowa, USA. The dataset captures a wide array of property characteristics including structural attributes, quality ratings, amenity measures, and locational variables.
The predictor variables broadly cover four  (4) domains:
•	Property Area: lot size, living area, basement, and garage square footage
•	Construction Quality : overall quality rating, exterior and kitchen finish
•	Amenities : number of bathrooms, bedrooms, fireplaces, and garage capacity
•	Location / Building Type : neighborhood classification and dwelling type
The analytical workflow encompasses exploratory data analysis (EDA), feature selection, model building, statistical hypothesis testing, assumption diagnostics, remedial measures, and final model validation. Remedial techniques applied include:
•	Scaling and correlation transformation
•	Log transformation of the response variable
•	Outlier detection and removal via Cook's Distance
•	Interaction term detection and modelling
•	Robust regression to reduce sensitivity to remaining outliers
2. Research Questions
This analysis is structured around two (2) primary research questions:
a. Which property variables significantly affect the sale price of a residential property in Ames, Iowa?
b.How effectively can the identified feature set explain and predict property sale price using a Multiple Linear Regression model?
