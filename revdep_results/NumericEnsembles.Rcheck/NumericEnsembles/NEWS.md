# NumericEnsembles 1.0.5

# 1.0.4
## Fixed errors that the variable importance barchart was not saving when the user requested it, and set_seed was crashing. Both are fixed, passed tests, running fine.

# 1.0.3
## Fixed errors on predicting on new data using XGBoost

# 1.0.2
## I received word that the package reactablefmtr will be archived on 2026-03-17. I replaced those functions in NumericEnsembles using the htmlwidges and htmltools libraries.

# 1.0.1
## Corrected mistakes in the vignette.

# 1.0.0
## All desired features are included and working great, ready for version 1.0.0!

# 0.10.10
## Added stratified random sampling
## Added extensive documentation, set up sections so the code is easy for the user to navigate.

# 0.10.9
## Added several functions to separate best performing from worst performing data, using the vip package

# 0.10.8
## Removed ensemble_XGBoost due to problems with overfitting, replaced with ensemble_neuralnet, which does not have any issues with overfitting.

# 0.10.7
## Added automatic stratified sampling for train, test and validation sets. Also added automatic histograms of the overfitting data.

# 0.10.6
## Add back remove_data_correlations_greater_than. This addresses issues when data has columns that are highly correlated.

# 0.10.5
## Fix name: Change holdout / RMSE to overfitting (both more accurate and easier to understand)

# 0.10.4
## Remove all references to MAE, MSE, SSE

# 0.10.3
## Update version number to reflect all changes.

# NumericEnsembles 0.10.2
## Updated ReadMe to reflect recent changes (32 models instead of 40), added a report (in the Vignettes) in Quarto format

# NumericEnsembles 0.10.1

# 0.9.2
## Corrected errors in the vignette, which now runs correctly
## Corrected errors in set.seed, so the results are consistent if the same seed is used.

# NumericEnsembles 0.9.0
## Corrected 160 plots (40 models x 4 plots each, pred_vs_actual, pred_vs_residuals, hist_residuals and qq plots. They were not printing the correct plot, that is now fixed.)

## Added set_seed, so NumericEnsembles runs correctly when a specific seed is set.

# NumericEnsembles 0.8.0
## Added Variance Inflation Factor. The user is able to set the VIF value, and models are built with VIF values at or below the user's choice of VIF value

## Added "free" and "fixed" scales to all appropriate plots. Each result has two plots, one with free scales, the other with fixed scales.

## Added Kolomogrov-Smirnovv test to help the user see which models test similar to the actual holdout data

## Added several "Holdout vs train" charts to show how each model performs across multiple resamples, and the range of values of holdout RMSE / train RMSE

# NumericEnsembles 0.7.0

# NumericEnsembles 0.6.0

# NumericEnsembles 0.5.0

# NumericEnsembles 0.4.0

# NumericEnsembles 0.3.0

# NumericEnsembles 0.2.0

# NumericEnsembles 0.1.0

* Initial CRAN submission.

# NumericEnsembles 1.0.0

## Added example of New_Boston data set as a new data set to use in NumericEnsembles ('do_you_have_new_data?')

* To see how this works, use the data as Boston_housing, and the new data as New_Boston.

## Removed neuralnet models since I could not get the RMSE down, added cppls, which creates a much more accurate ensemble compared to the previous version.

## Removed cppls, not reliable on several data sets.

## Re-added neuralnet models (individual and ensemble) since I was able to get those to work without error

## Added best subsets: Forward, backward, exhaustive and seqrep

## Added save_all_plots to automatically save all plots in the user's choice of one of six graphics formats: eps, jpeg, pdf, png, svg or tiff

