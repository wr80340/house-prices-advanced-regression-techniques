# house-prices-advanced-regression-techniques
kaggle competition : house-prices-advanced-regression-techniques 
# link
https://www.kaggle.com/c/house-prices-advanced-regression-techniques


# Goal
It is your job to predict the sales price for each house. For each Id in the test set, you must predict the value of the SalePrice variable. 

# Metric
Submissions are evaluated on Root-Mean-Squared-Error (RMSE) between the logarithm of the predicted value and the logarithm of the observed sales price. (Taking logs means that errors in predicting expensive houses and cheap houses will affect the result equally.)

# issues
When I was traianing my model, I found some variables like "BsmtFinType1" use NA to present No Basement, but still it may be a real NA.
