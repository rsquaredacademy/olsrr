
# NumericEnsembles

<!-- badges: start -->
<!-- badges: end -->

The goal of NumericEnsembles is to automatically conduct a thorough analysis of numeric data. The user only needs to provide the data and answer a few questions (such as which column to analyze). NumericEnsembles fits 18 individual models to the training data, and also makes predictions and checks accuracy for each of the individual models. It also builds 14 ensembles from the ensembles of data, fits each ensemble model to the training data then makes predictions and tracks accuracy for each ensemble. The package also automatically returns 26 plots (such as train vs holdout for the best model), 6 tables (such as head of the data), and a grand summary table sorted by accuracy with the best model at the top of the report.

## Installation

You can install the development version of NumericEnsembles like so:

``` r
devtools::install_github("InfiniteCuriosity/NumericEnsembles")
```

## Example

NumericEnsembles will automatically build 32 models to predict the sale price of houses in Boston, from the Boston housing data set.

``` r
library(NumericEnsembles)
Numeric(data = MASS::Boston,
        colnum = 14,
        numresamples = 2,
        remove_VIF_above = 5.00,
        remove_ensemble_correlations_greater_than = 1.00,
        scale_all_predictors_in_data = "N",
        data_reduction_method = 0,
        ensemble_reduction_method = 0,
        how_to_handle_strings = 0,
        predict_on_new_data = "N",
        stratified_random_column = 0,
        save_all_trained_models = "N",
        set_seed = "N",
        save_all_plots = "N",
        use_parallel = "Y",
        train_amount = 0.60,
        test_amount = 0.20,
        validation_amount = 0.20)
```

The 32 models which are all built automatically and without error are:

1. Bagging
2. BayesGLM
3. BayesRNN
4. Cubist
5. Earth
6. Elastic (optimized by cross-validation)
7. Ensemble Bagging
8. Ensemble BayesGLM
9. Ensemble BayesRNN
10. Ensemble Cubist
11. Ensemble Earth
12. Ensemble Elastic (optimized by cross-validation)
13. Ensemble Gradient Boosted
14. Ensemble Lasso (optimized by cross-validation)
15. Ensemble Linear (tuned)
16. Ensemble Neuralnet (optimized)
17. Ensemble Ridge (optimized by cross-validation)
18. Ensemble RPart
10. EnsembleSVM (tuned)
20. Ensemble Trees
21. GAM (Generalized Additive Models, with smoothing splines)
22. Gradient Boosted (optimized)
23. Lasso
24. Linear (tuned)
25. Neuralnet
26. PCR (Principal Components Regression)
27. PLS (Partial Least Squares)
28. Ridge (optimized by cross-validation)
29. RPart
30. SVM (Support Vector Machines, tuned)
31. Tree
32. XGBoost

The 30 plots created automatically:

01. Correlation plot of the numeric data (as numbers and colors)
02. Correlation plot of the numeric data (as circles with colors)
03. Cook's D Bar Plot
04. Four plots in one for the most accurate model: Predicted vs actual, Residuals, Histogram of residuals, Q-Q plot
05. Most accurate model: Predicted vs actual
06. Most accurate model: Residuals
07. Most accurate model: Histogram of residuals
08. Most accurate model: Q-Q plot
09. Accuracy by resample and model, fixed scales
10. Accuracy by resample and model, free scales
11. Train vs holdout by model and resample, fixed scales
12. Train vs holdout by model and resample, free scales
13. Histograms of each numeric column
14. Boxplots of each numeric column
15. Predictor vs target variable
16. Model accuracy bar chart (RMSE)
17. t-test p-value bar chart
18. Overfitting by resample and model, free scales
19. Overfitting by resampleand model, fixed scales
20. Duration bar chart
21. Overfitting bar chart
22. Overfitting histograms
22. Mean bias bar chart
23. Kolmogorov-Smirnof test bar chart
24. Bias plot by model and resample

The tables created automatically (which are both searchable and sortable) are:

01. Variance Inflation Factor
02. Correlation of the ensemble
03. Head of the ensemble
04. Data summary
05. Correlation of the data
06. Grand summary table includes:
  1. Mean holdout RMSE
  2. Standard deviation of mean holdout RMSE
  3. t-test value
  4. t-test p-value
  5. t-test p-value standard deviation
  6. Kolmogorov-Smirnov stat mean
  7. Kolmogorov-Smirnov stat p-value
  8. Kolmogorov-Smirnov stat standard deviation
  9. Mean bias
  10. Mean bias standard deviation
  11. Mean data (this is the mean of the target column in the original data set)
  12. Standard deviation of mean data (this is the standard deviation of the data in the target column in the original data set)
  13. Mean train RMSE
  14. Mean test RMSE
  15. Mean validation RMSE
  16. Holdout vs train mean
  17. Holdout vs train standard deviation
  19. Duration
  19. Duration standard deviation

## Example using pre-trained models on totally new data in the NumericEnsembles package

The NumericEnsembles package also has a way to create trained models and test those pre-trained models on totally unseen data *using the same pre-trained models as on the initial analysis.*

The package contains two example data sets to demonstrate this result. Boston_Housing is the Boston Housing data set, but the first five rows have been removed. We will build our models on that data set. NewBoston is totally new data, and actually the first five rows from the original Boston Housing data set.

``` r
library(NumericEnsembles)
Numeric(data = Boston_housing,
        colnum = 14,
        numresamples = 25,
        remove_VIF_above = 5.00,
        remove_ensemble_correlations_greater_than = 1.00,
        scale_all_predictors_in_data = "N",
        data_reduction_method = 0,
        ensemble_reduction_method = 0,
        how_to_handle_strings = 0,
        stratified_random_column = 0,
        predict_on_new_data = "Y",
        set_seed = "N",
        save_all_trained_models = "N",
        save_all_plots = "N",
        use_parallel = "Y",
        train_amount = 0.60,
        test_amount = 0.20,
        validation_amount = 0.20)
```

Use the data set New_Boston when asked for "What is the URL of the new data?". The URL for the new data is: https://raw.githubusercontent.com/InfiniteCuriosity/EnsemblesData/refs/heads/main/NewBoston.csv

External data may be used to accomplish the same result.
