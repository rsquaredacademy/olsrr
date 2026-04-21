#' Numeric—function to automatically build 18 individual models and 14 ensembles then return the results to the user
#'
#' @param data data can be a CSV file or within an R package, such as MASS::Boston
#' @param colnum a column number in your data
#' @param numresamples the number of resamples
#' @param how_to_handle_strings 0: No strings, 1: Factor values, 2: One-hot encoding, 3: One-hot encoding AND jitter
#' @param predict_on_new_data "Y" or "N". If "Y", then you will be asked for the new data
#' @param save_all_trained_models "Y" or "N". If "Y", then places all the trained models in the temporary directory, tempdir(), and the trained models may be retreived from there
#' @param set_seed "Y" or "N" to set the seed to make the results fully reproducible
#' @param save_all_plots Saves all plots to the tempdir() directory, and the plots may be retreived from there
#' @param scale_all_predictors_in_data "Y" or "N" to scale numeric data
#' @param remove_data_correlations_greater_than maximum value for correlations of the original data (such as the Boston Housing data set)
#' @param remove_ensemble_correlations_greater_than maximum value for correlations of the ensemble
#' @param remove_VIF_above remove columns with Variable Inflation Factor above value chosen by the user
#' @param data_reduction_method 0(none), BIC (1, 2, 3, 4) or Mallow's_cp (5, 6, 7, 8) for Forward, Backward, Exhaustive and SeqRep
#' @param ensemble_reduction_method 0(none), BIC (1, 2, 3, 4) or Mallow's_cp (5, 6, 7, 8) for Forward, Backward, Exhaustive and SeqRep
#' @param stratified_random_column 0 if no stratified random sampling, or column number for stratified random sampling
#' @param use_parallel "Y" or "N" for parallel processing
#' @param train_amount set the amount for the training data
#' @param test_amount set the amount for the testing data
#' @param validation_amount Set the amount for the validation data
#'
#' @return a real number
#' @export Numeric

#' @importFrom arm bayesglm
#' @importFrom brnn brnn
#' @importFrom broom tidy
#' @importFrom car vif
#' @importFrom caret dummyVars
#' @importFrom corrplot corrplot
#' @importFrom Cubist cubist
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr all_of arrange bind_rows desc relocate rename last_col n_distinct filter %>% mutate_if
#' @importFrom e1071 tune.svm tune.gknn tune.randomForest
#' @importFrom earth earth
#' @importFrom gam gam gam.s s
#' @importFrom gbm gbm
#' @importFrom glmnet glmnet
#' @importFrom ggplot2 aes geom_boxplot facet_wrap labs geom_histogram coord_cartesian
#' @importFrom graphics mtext par hist rect panel.smooth
#' @importFrom grDevices dev.off recordPlot
#' @importFrom gridExtra arrangeGrob
#' @importFrom htmltools h2
#' @importFrom htmlwidgets prependContent
#' @importFrom ipred bagging
#' @importFrom leaps regsubsets
#' @importFrom Metrics rmse
#' @importFrom nnet nnet
#' @importFrom olsrr ols_plot_cooksd_bar
#' @importFrom parallel makeCluster
#' @importFrom pls pcr
#' @importFrom purrr keep map_dbl
#' @importFrom randomForest randomForest
#' @importFrom reactable reactable
#' @importFrom readr read_lines
#' @importFrom rpart rpart
#' @importFrom scales label_percent
#' @importFrom stats aggregate as.formula BIC cor lm sd predict residuals reorder quantile gaussian t.test var
#' @importFrom tidyr gather pivot_longer
#' @importFrom tree tree cv.tree misclass.tree
#' @importFrom utils tail str head read.csv
#' @importFrom vip vip
#' @importFrom xgboost xgb.DMatrix xgb.train

Numeric <- function(data, colnum, numresamples,
                    remove_VIF_above = 5.00, remove_data_correlations_greater_than = 0.99, remove_ensemble_correlations_greater_than = 0.98, scale_all_predictors_in_data = c("Y", "N"),
                    data_reduction_method = c(0("none"), 1("BIC exhaustive"), 2("BIC forward"), 3("BIC backward"), 4("BIC seqrep"),
                                              5("Mallows_cp exhaustive"), 6("Mallows_cp forward"), 7("Mallows_cp backward"), 8("Mallows_cp, seqrep")),
                    ensemble_reduction_method = c(0("none"), 1("BIC exhaustive"), 2("BIC forward"), 3("BIC backward"), 4("BIC seqrep"),
                                                  5("Mallows_cp exhaustive"), 6("Mallows_cp forward"), 7("Mallows_cp backward"), 8("Mallows_cp, seqrep")),
                    how_to_handle_strings = c(0("none"), 1("factor levels"), 2("One-hot encoding"), 3("One-hot encoding with jitter")),
                    predict_on_new_data = c("Y", "N"), set_seed = c("Y", "N"), save_all_trained_models = c("Y", "N"), save_all_plots = c("Y", "N"),
                    use_parallel = c("Y", "N"), stratified_random_column,
                    train_amount, test_amount, validation_amount) {

use_parallel <- 0
no_cores <- 0

if(set_seed == "Y"){
  seed = as.integer(readline("Which integer would you like to use for the seed? "))
}

if (use_parallel == "Y") {
  cl <- parallel::makeCluster(no_cores, type = "FORK")
  doParallel::registerDoParallel(cl)
}
old_data <- data
y <- 0
colnames(data)[colnum] <- "y"

df <- data %>% dplyr::relocate(y, .after = last_col()) # Moves the target column to the last column on the right

if(data_reduction_method == 1){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "exhaustive")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(BIC == min(BIC)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, y = df$y)
}

if(data_reduction_method == 2){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "forward")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(BIC == min(BIC)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, y = df$y)
}

if(data_reduction_method == 3){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "backward")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(BIC == min(BIC)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, df$y)
}

if(data_reduction_method == 4){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "seqrep")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(BIC == min(BIC)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, df$y)
}

if(data_reduction_method == 5){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "exhaustive")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(mallows_cp == min(mallows_cp)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, df$y)
}

if(data_reduction_method == 6){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "forward")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(mallows_cp == min(mallows_cp)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, df$y)
}

if(data_reduction_method == 7){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "backward")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(mallows_cp == min(mallows_cp)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, y)
}

if(data_reduction_method == 8){
  data_regsubsets <- leaps::regsubsets(y ~ ., data = df, method = "seqrep")

  cols_sel <- data_regsubsets |>
    broom::tidy() |>
    filter(mallows_cp == min(mallows_cp)) |>
    dplyr::select(dplyr::where(~.x == TRUE)) |>
    colnames()

  data <- dplyr::select(data, dplyr::any_of(cols_sel))
  df <- cbind(data, y)
}

if (scale_all_predictors_in_data == "Y"){
  df <- as.data.frame(scale(df[, 1:ncol(df) -1]) %>% cbind(y = df$y))
}

if (predict_on_new_data == "Y") {
  new_data <- readline("What is the URL of the new data? ")
  new_data <- read.csv(new_data)

  y <- 0
  colnames(new_data)[colnum] <- "y"

  new_data <- new_data %>% dplyr::select(colnames(df))

  new_data <- new_data %>% dplyr::relocate(y, .after = last_col()) # Moves the target column in the new data to the last column on the right
}

if(stratified_random_column >0) {
  levels <- levels(as.factor((df[, stratified_random_column-1]))) # gets the levels for stratified data
}

if (how_to_handle_strings == 1) {
  df <- dplyr::mutate_if(df, is.character, as.factor)
  df <- dplyr::mutate_if(df, is.factor, as.numeric)
}

if (how_to_handle_strings == 1 && predict_on_new_data == "Y") {
  newdata <- dplyr::mutate_if(newdata, is.character, as.factor)
  newdata <- dplyr::mutate_if(newdata, is.factor, as.numeric)
}

if (how_to_handle_strings == 2) {
  dummy <- caret::dummyVars(" ~ .", data=df)
  df <- data.frame(predict(dummy, newdata=df))
}

if (how_to_handle_strings == 2 && predict_on_new_data == "Y") {
  dummy <- caret::dummyVars(" ~ .", data=newdata)
  newdata <- data.frame(predict(dummy, newdata=newdata))
}

if (how_to_handle_strings == 3) {
  dummy <- caret::dummyVars(" ~ .", data=df)
  df <- data.frame(predict(dummy, newdata=df))
  df <- data.frame(lapply(df, jitter))
}

if (how_to_handle_strings == 3 && predict_on_new_data == "Y") {
  dummy <- caret::dummyVars(" ~ .", data=newdata)
  newdata <- data.frame(predict(dummy, newdata=newdata))
  newdata <- data.frame(lapply(newdata, jitter))
}

if(remove_data_correlations_greater_than <1.00){
  tmp <- stats::cor(df) # This section removes strongly correlated the user chooses (for example >0.995) rows and columns from the data
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  data_new <- df[, !apply(tmp, 2, function(x) any(abs(x) > remove_data_correlations_greater_than, na.rm = TRUE))]
  df <- data_new # new data without strongly correlated predictors
}


if(predict_on_new_data == "Y"){
vif <- car::vif(lm(y ~ ., data = df[, 1:ncol(df)]))
for (i in 1:ncol(df)) {
  if(max(vif) > remove_VIF_above){
    df <- df %>% dplyr::select(-which.max(vif))
    new_data <- new_data %>% dplyr::select(-which.max(vif))
    vif <- car::vif(lm(y ~ ., data = df[, 1:ncol(df)]))
  }
}

VIF <- reactable::reactable(as.data.frame(vif),
                            searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                            striped = TRUE, highlight = TRUE, resizable = TRUE
)
htmltools::div(class = "table",
               htmltools::div(class = "title", "VIF")
)

VIF_report <- htmlwidgets::prependContent(VIF, htmltools::h2(class = "title", "VIF"))
}

if(predict_on_new_data == "N"){
  vif <- car::vif(lm(y ~ ., data = df[, 1:ncol(df)]))
  for (i in 1:ncol(df)) {
    if(max(vif) > remove_VIF_above){
      df <- df %>% dplyr::select(-which.max(vif))
      vif <- car::vif(lm(y ~ ., data = df[, 1:ncol(df)]))
    }
  }

  VIF <- reactable::reactable(as.data.frame(vif),
                              searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                              striped = TRUE, highlight = TRUE, resizable = TRUE
  )
  htmltools::div(class = "table",
                 htmltools::div(class = "title", "VIF")
  )

  VIF_report <- htmlwidgets::prependContent(VIF, htmltools::h2(class = "title", "VIF"))
}


if(save_all_plots == "Y"){
  width = as.numeric(readline("Width of the graphics: "))
  height = as.numeric(readline("Height of the graphics: "))
  units = readline("Which units? You may use in, cm, mm or px. ")
  scale = as.numeric(readline("What multiplicative scaling factor? "))
  device = readline("Which device to use? You may enter eps, jpeg, pdf, png, svg or tiff: ")
  dpi <- as.numeric(readline("Plot resolution. Applies only to raster output types (jpeg, png, tiff): "))
}

head_df <- reactable::reactable(head(df, n = 10),
                                searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "head_df")
)

head_df <- htmlwidgets::prependContent(head_df, htmltools::h2(class = "title", "Head of the data frame"))

## Set baseline RMSE and Standard Deviation (SD) based on the full data set
actual_RMSE <- Metrics::rmse(actual = df$y, predicted = df$y)
actual_mean <- round(mean(df$y), 4)
actual_sd <- round(sd(df$y), 4)

# Data summary
data_summary <- summary(df)
data_summary <- reactable::reactable(round(as.data.frame(do.call(cbind, lapply(df, summary))), 4),
                                     searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                     striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "data_summary")
)

data_summary <- htmlwidgets::prependContent(data_summary, htmltools::h2(class = "title", "Data summary"))

tempdir1 <- tempdir()

## Correlation data and plots ##
df1 <- df %>% purrr::keep(is.numeric)
M1 <- stats::cor(df1)
data_correlation <- reactable::reactable(round(cor(df), 4),
                                         searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                         striped = TRUE, highlight = TRUE, resizable = TRUE
)



htmltools::div(class = "table",
               htmltools::div(class = "title", "data_correlation")
)

data_correlation <- htmlwidgets::prependContent(data_correlation, htmltools::h2(class = "title", "Data correlation"))

title <- "Correlation plot of the numerical data"
corrplot_number <- corrplot::corrplot(stats::cor(df1), method = "number", title = title, mar = c(0, 0, 1, 0)) # http://stackoverflow.com/a/14754408/54964)
corrplot_number <- grDevices::recordPlot(attach = corrplot_number)

corrplot_circle <- corrplot::corrplot(stats::cor(df1), method = "circle", title = title, mar = c(0, 0, 1, 0)) # http://stackoverflow.com/a/14754408/54964)
corrplot_circle <- grDevices::recordPlot(corrplot_circle)

corrplot_full <- corrplot::corrplot(stats::cor(df1), method = "circle", title = title, mar = c(0, 0, 1, 0), addCoef.col = "white", type = "upper", bg = "gray")
corrplot_full <- grDevices::recordPlot(corrplot_full)


## Boxplots of the numeric data ##
boxplots <- df %>%
  tidyr::gather(key = "var", value = "value") %>%
  ggplot2::ggplot(aes(x = "", y = value)) +
  ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  ggplot2::facet_wrap(~var, scales = "free") +
  ggplot2::labs(title = "Boxplots of the numeric data")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("boxplots.eps", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("boxplots.jpeg", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("boxplots.pdf", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("boxplots.png", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("boxplots.svg", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("boxplots.tiff", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
# Thanks to https://rstudio-pubs-static.s3.amazonaws.com/388596_e21196f1adf04e0ea7cd68edd9eba966.html

#### Histograms here ####

histograms <- ggplot2::ggplot(tidyr::gather(df1, cols, value), aes(x = value)) +
  ggplot2::geom_histogram(bins = round(nrow(df1))) +
  ggplot2::facet_wrap(. ~ cols, scales = "free") +
  ggplot2::labs(title = "Histograms of each numeric column.")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("histograms.eps", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("histograms.jpeg", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("histograms.pdf", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("histograms.png", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("histograms.svg", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("histograms.tiff", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Predictor vs target here ####

predictor_vs_target <- df %>%
  tidyr::gather(-y, key = "var", value = "value") %>%
  ggplot2::ggplot(aes(x = value, y = y)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ var, scales = "free") +
  ggplot2::theme_bw()+
  ggplot2::labs(title = "y (predictor variable) vs target variables")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("predictor_vs_target.eps", plot = predictor_vs_target, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("predictor_vs_target.jpeg", plot = predictor_vs_target, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("predictor_vs_target.pdf", plot = predictor_vs_target, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("predictor_vs_target.png", plot = predictor_vs_target, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("predictor_vs_target.svg", plot = predictor_vs_target, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("predictor_vs_target.tiff", plot = predictor_vs_target, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Calculate outliers here ####

model <- lm(y ~ ., data = df)
cooks_distance_plot <- olsrr::ols_plot_cooksd_bar(model) # Thanks to https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html
k <- olsrr::ols_prep_cdplot_data(model)
outliers <- old_data[olsrr::ols_prep_cdplot_outliers(k)[, 1], ]
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("cooks_distance_plot.eps", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("cooks_distance_plot.jpeg", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("cooks_distance_plot.pdf", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("cooks_distance_plot.png", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("cooks_distance_plot.svg", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("cooks_distance_plot.tiff", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
outlier_list <- reactable::reactable(outliers,
                                     searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                     striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "outlier_list")
)

outlier_list <- htmlwidgets::prependContent(outlier_list, htmltools::h2(class = "title", "Outlier list"))


#### Full analysis starts here ####

#### Initialize values to 0 ####

bagging_train_RMSE <- 0
bagging_test_RMSE <- 0
bagging_validation_RMSE <- 0
bagging_sd <- 0
bagging_overfitting <- 0
bagging_duration <- 0
bagging_holdout_RMSE <- 0
bagging_holdout_RMSE_mean <- 0
bagging_bias <- 0
bagging_ks_stat <- 0
bagging_ks_p_value <- 0
bagging_t_test_t <- 0
bagging_t_test_p_value <- 0
y_hat_bagging_total <- 0
bagging_actual <- 0
bagging_actual_total <- 0

bayesglm_train_RMSE <- 0
bayesglm_test_RMSE <- 0
bayesglm_validation_RMSE <- 0
bayesglm_sd <- 0
bayesglm_overfitting <- 0
bayesglm_duration <- 0
bayesglm_duration_mean <- 0
bayesglm_holdout_mean <- 0
bayesglm_holdout_RMSE <- 0
bayesglm_holdout_RMSE_mean <- 0
bayesglm_bias <- 0
bayesglm_ks_stat <- 0
bayesglm_ks_p_value <- 0
bayesglm_t_test_t <- 0
bayesglm_t_test_p_value <- 0
y_hat_bayesglm_total <- 0
bayesglm_actual <- 0
bayesglm_actual_total <- 0
Bayesglm_summary <- 0

bayesrnn_train_RMSE <- 0
bayesrnn_test_RMSE <- 0
bayesrnn_validation_RMSE <- 0
bayesrnn_sd <- 0
bayesrnn_overfitting <- 0
bayesrnn_duration <- 0
bayesrnn_duration_mean <- 0
bayesrnn_holdout_mean <- 0
bayesrnn_holdout_RMSE <- 0
bayesrnn_holdout_RMSE_mean <- 0
bayesrnn_bias <- 0
bayesrnn_ks_stat <- 0
bayesrnn_ks_p_value <- 0
bayesrnn_t_test_t <- 0
bayesrnn_t_test_p_value <- 0
y_hat_bayesrnn_total <- 0
bayesrnn_actual <- 0
bayesrnn_actual_total <- 0

cubist_train_RMSE <- 0
cubist_test_RMSE <- 0
cubist_validation_RMSE <- 0
cubist_sd <- 0
cubist_overfitting <- 0
cubist_duration <- 0
cubist_duration_mean <- 0
cubist_holdout_mean <- 0
cubist_holdout_RMSE <- 0
cubist_holdout_RMSE_mean <- 0
cubist_bias <- 0
cubist_ks_stat <- 0
cubist_ks_p_value <- 0
cubist_t_test_t <- 0
cubist_t_test_p_value <- 0
y_hat_cubist_total <- 0
cubist_actual <- 0
cubist_actual_total <- 0

earth_train_RMSE <- 0
earth_test_RMSE <- 0
earth_validation_RMSE <- 0
earth_sd <- 0
earth_overfitting <- 0
earth_duration <- 0
earth_duration_mean <- 0
earth_holdout_mean <- 0
earth_holdout_RMSE <- 0
earth_holdout_RMSE_mean <- 0
earth_bias <- 0
earth_ks_stat <- 0
earth_ks_p_value <- 0
earth_t_test_t <- 0
earth_t_test_p_value <- 0
y_hat_earth_total <- 0
earth_actual <- 0
earth_actual_total <- 0

elastic_train_RMSE <- 0
elastic_train_RMSE_df <- data.frame(elastic_train_RMSE)
elastic_test_RMSE <- 0
elastic_test_RMSE_df <- data.frame(elastic_test_RMSE)
elastic_test_pred <- 0
elastic_validation_RMSE <- 0
elastic_validation_RMSE_df <- data.frame(elastic_validation_RMSE)
elastic_validation_RMSE_mean <- 0
elastic_validation_predict_value <- 0
elastic_validation_predict_value_mean <- 0
elastic_test_predict_value <- 0
elastic_validation_predict_value <- 0
elastic_validation_predict_value_sd <- 0
elastic_test_predict_value_mean <- 0
elastic_test_predict_value_sd <- 0
elastic_duration <- 0
elastic_holdout_RMSE <- 0
elastic_holdout_RMSE_df <- data.frame(elastic_holdout_RMSE)
elastic_holdout_RMSE_sd <- 0
elastic_holdout_RMSE_sd_mean <- 0
elastic_holdout_RMSE_sd_df <- data.frame(elastic_holdout_RMSE_sd)
elastic_overfitting <- 0
elastic_overfitting_df <- data.frame(elastic_overfitting)
elastic_sd <- 0
elastic_sd_mean <- 0
y_hat_elastic <- 0
elastic_bias <- 0
elastic_ks_stat <- 0
elastic_ks_p_value <- 0
elastic_t_test_t <- 0
elastic_t_test_p_value <- 0
y_hat_elastic_total <- 0
elastic_actual <- 0
elastic_actual_total <- 0

gam_train_RMSE <- 0
gam_test_RMSE <- 0
gam_validation_RMSE <- 0
gam_sd <- 0
gam_overfitting <- 0
gam_duration <- 0
gam_duration_mean <- 0
gam_holdout_mean <- 0
gam_holdout_RMSE <- 0
gam_holdout_RMSE_mean <- 0
gam_bias <- 0
gam_ks_stat <- 0
gam_ks_p_value <- 0
gam_t_test_t <- 0
gam_t_test_p_value <- 0
y_hat_gam_total <- 0
gam_actual <- 0
gam_actual_total <- 0

gb_train_RMSE <- 0
gb_test_RMSE <- 0
gb_validation_RMSE <- 0
gb_sd <- 0
gb_overfitting <- 0
gb_duration <- 0
gb_duration_mean <- 0
gb_holdout_mean <- 0
gb_holdout_RMSE <- 0
gb_holdout_RMSE_mean <- 0
gb_bias <- 0
gb_ks_stat <- 0
gb_ks_p_value <- 0
gb_t_test_t <- 0
gb_t_test_p_value <- 0
y_hat_gb_total <- 0
gb_actual <- 0
gb_actual_total <- 0

lasso_train_RMSE <- 0
lasso_train_RMSE_df <- data.frame(lasso_train_RMSE)
lasso_test_RMSE <- 0
lasso_test_RMSE_df <- data.frame(lasso_test_RMSE)
lasso_test_pred <- 0
lasso_validation_RMSE <- 0
lasso_validation_RMSE_df <- data.frame(lasso_validation_RMSE)
lasso_validation_RMSE_mean <- 0
lasso_validation_predict_value <- 0
lasso_validation_predict_value_mean <- 0
lasso_test_predict_value <- 0
lasso_validation_predict_value <- 0
lasso_validation_predict_value_sd <- 0
lasso_test_predict_value_mean <- 0
lasso_test_predict_value_sd <- 0
lasso_duration <- 0
lasso_holdout_RMSE <- 0
lasso_holdout_RMSE_df <- data.frame(lasso_holdout_RMSE)
lasso_holdout_RMSE_sd <- 0
lasso_holdout_RMSE_sd_mean <- 0
lasso_holdout_RMSE_sd_df <- data.frame(lasso_holdout_RMSE_sd)
lasso_overfitting <- 0
lasso_overfitting_df <- data.frame(lasso_overfitting)
lasso_sd <- 0
lasso_sd_mean <- 0
y_hat_lasso <- 0
lasso_bias <- 0
lasso_ks_stat <- 0
lasso_ks_p_value <- 0
lasso_t_test_t <- 0
lasso_t_test_p_value <- 0
y_hat_lasso_total <- 0
lasso_actual <- 0
lasso_actual_total <- 0

linear_train_RMSE <- 0
linear_test_RMSE <- 0
linear_validation_RMSE <- 0
linear_sd <- 0
linear_overfitting <- 0
linear_duration <- 0
linear_holdout_RMSE <- 0
linear_holdout_RMSE_mean <- 0
linear_bias <- 0
linear_ks_stat <- 0
linear_ks_p_value <- 0
linear_t_test_t <- 0
linear_t_test_p_value <- 0
y_hat_linear_total <- 0
linear_actual <- 0
linear_actual_total <- 0

neuralnet_train_RMSE <- 0
neuralnet_test_RMSE <- 0
neuralnet_validation_RMSE <- 0
neuralnet_sd <- 0
neuralnet_overfitting <- 0
neuralnet_duration <- 0
neuralnet_holdout_RMSE <- 0
neuralnet_holdout_RMSE_mean <- 0
neuralnet_bias <- 0
neuralnet_ks_stat <- 0
neuralnet_ks_p_value <- 0
neuralnet_test_predict_value_mean <- 0
neuralnet_t_test_t <- 0
neuralnet_t_test_p_value <- 0
y_hat_neuralnet_total <- 0
neuralnet_actual <- 0
neuralnet_actual_total <- 0

pls_train_RMSE <- 0
pls_test_RMSE <- 0
pls_validation_RMSE <- 0
pls_sd <- 0
pls_overfitting <- 0
pls_duration <- 0
pls_duration_mean <- 0
pls_holdout_mean <- 0
pls_holdout_RMSE <- 0
pls_holdout_RMSE_mean <- 0
pls_bias <- 0
pls_ks_stat <- 0
pls_ks_p_value <- 0
pls_t_test_t <- 0
pls_t_test_p_value <- 0
y_hat_pls_total <- 0
pls_actual <- 0
pls_actual_total <- 0

pcr_train_RMSE <- 0
pcr_test_RMSE <- 0
pcr_validation_RMSE <- 0
pcr_sd <- 0
pcr_overfitting <- 0
pcr_duration <- 0
pcr_duration_mean <- 0
pcr_holdout_mean <- 0
pcr_holdout_RMSE <- 0
pcr_holdout_RMSE_mean <- 0
pcr_bias <- 0
pcr_ks_stat <- 0
pcr_ks_p_value <- 0
pcr_t_test_t <- 0
pcr_t_test_p_value <- 0
y_hat_pcr_total <- 0
pcr_actual <- 0
pcr_actual_total <- 0

ridge_train_RMSE <- 0
ridge_train_RMSE_df <- data.frame(ridge_train_RMSE)
ridge_test_RMSE <- 0
ridge_test_RMSE_df <- data.frame(ridge_test_RMSE)
ridge_test_pred <- 0
ridge_validation_RMSE <- 0
ridge_validation_RMSE_df <- data.frame(ridge_validation_RMSE)
ridge_validation_RMSE_mean <- 0
ridge_validation_predict_value <- 0
ridge_validation_predict_value_mean <- 0
ridge_test_predict_value <- 0
ridge_validation_predict_value <- 0
ridge_validation_predict_value_sd <- 0
ridge_test_predict_value_mean <- 0
ridge_test_predict_value_sd <- 0
ridge_duration <- 0
ridge_holdout_RMSE <- 0
ridge_holdout_RMSE_df <- data.frame(ridge_holdout_RMSE)
ridge_holdout_RMSE_sd <- 0
ridge_holdout_RMSE_sd_df <- data.frame(ridge_holdout_RMSE_sd)
ridge_holdout_RMSE_sd_mean <- 0
ridge_overfitting <- 0
ridge_overfitting_df <- data.frame(ridge_overfitting)
ridge_sd <- 0
ridge_sd_mean <- 0
ridge_bias <- 0
y_hat_ridge <- 0
ridge_ks_stat <- 0
ridge_ks_p_value <- 0
ridge_t_test_t <- 0
ridge_t_test_p_value <- 0
y_hat_ridge_total <- 0
ridge_actual <- 0
ridge_actual_total <- 0

rpart_train_RMSE <- 0
rpart_test_RMSE <- 0
rpart_validation_RMSE <- 0
rpart_sd <- 0
rpart_overfitting <- 0
rpart_duration <- 0
rpart_duration_mean <- 0
rpart_holdout_mean <- 0
rpart_holdout_RMSE <- 0
rpart_holdout_RMSE_mean <- 0
rpart_bias <- 0
rpart_ks_stat <- 0
rpart_ks_p_value <- 0
rpart_t_test_t <- 0
rpart_t_test_p_value <- 0
y_hat_rpart_total <- 0
rpart_actual <- 0
rpart_actual_total <- 0

svm_train_RMSE <- 0
svm_test_RMSE <- 0
svm_validation_RMSE <- 0
svm_sd <- 0
svm_overfitting <- 0
svm_duration <- 0
svm_duration_mean <- 0
svm_holdout_mean <- 0
svm_holdout_RMSE <- 0
svm_holdout_RMSE_mean <- 0
svm_bias <- 0
svm_ks_stat <- 0
svm_ks_p_value <- 0
svm_t_test_t <- 0
svm_t_test_p_value <- 0
y_hat_svm_total <- 0
svm_actual <- 0
svm_actual_total <- 0

tree_train_RMSE <- 0
tree_test_RMSE <- 0
tree_validation_RMSE <- 0
tree_sd <- 0
tree_overfitting <- 0
tree_duration <- 0
tree_duration_mean <- 0
tree_holdout_mean <- 0
tree_holdout_RMSE <- 0
tree_holdout_RMSE_mean <- 0
tree_bias <- 0
tree_ks_stat <- 0
tree_ks_p_value <- 0
tree_t_test_t <- 0
tree_t_test_p_value <- 0
y_hat_tree_total <- 0
tree_actual <- 0
tree_actual_total <- 0

xgb_train_RMSE <- 0
xgb_test_RMSE <- 0
xgb_validation_RMSE <- 0
xgb_sd <- 0
xgb_overfitting <- 0
xgb_duration <- 0
xgb_duration_mean <- 0
xgb_holdout_mean <- 0
xgb_holdout_RMSE <- 0
xgb_holdout_RMSE_mean <- 0
xgb_bias <- 0
xgb_ks_stat <- 0
xgb_ks_p_value <- 0
xgb_t_test_t <- 0
xgb_t_test_p_value <- 0
y_hat_xgb_total <- 0
xgb_actual <- 0
xgb_actual_total <- 0

ensemble_bagging_train_RMSE <- 0
ensemble_bagging_test_RMSE <- 0
ensemble_bagging_validation_RMSE <- 0
ensemble_bagging_sd <- 0
ensemble_bagging_overfitting <- 0
ensemble_bagging_duration <- 0
ensemble_bagging_holdout_RMSE <- 0
ensemble_bagging_holdout_RMSE_mean <- 0
ensemble_bagging_predict_value_mean <- 0
ensemble_bagging_bias <- 0
ensemble_bagging_ks_stat <- 0
ensemble_bagging_ks_p_value <- 0
ensemble_bagging_t_test_t <- 0
ensemble_bagging_t_test_p_value <- 0
ensemble_y_hat_bagging_total <- 0
ensemble_bagging_actual <- 0
ensemble_bagging_actual_total <- 0

ensemble_bayesglm_train_RMSE <- 0
ensemble_bayesglm_test_RMSE <- 0
ensemble_bayesglm_validation_RMSE <- 0
ensemble_bayesglm_sd <- 0
ensemble_bayesglm_overfitting <- 0
ensemble_bayesglm_duration <- 0
ensemble_bayesglm_holdout_RMSE <- 0
ensemble_bayesglm_holdout_RMSE_mean <- 0
ensemble_bayesglm_predict_value_mean <- 0
ensemble_bayesglm_bias <- 0
ensemble_bayesglm_ks_stat <- 0
ensemble_bayesglm_ks_p_value <- 0
ensemble_bayesglm_t_test_t <- 0
ensemble_bayesglm_t_test_p_value <- 0
ensemble_y_hat_bayesglm_total <- 0
ensemble_bayesglm_actual <- 0
ensemble_bayesglm_actual_total <- 0

ensemble_bayesrnn_train_RMSE <- 0
ensemble_bayesrnn_test_RMSE <- 0
ensemble_bayesrnn_validation_RMSE <- 0
ensemble_bayesrnn_sd <- 0
ensemble_bayesrnn_overfitting <- 0
ensemble_bayesrnn_duration <- 0
ensemble_bayesrnn_holdout_RMSE <- 0
ensemble_bayesrnn_holdout_RMSE_mean <- 0
ensemble_bayesrnn_predict_value_mean <- 0
ensemble_bayesrnn_bias <- 0
ensemble_bayesrnn_ks_stat <- 0
ensemble_bayesrnn_ks_p_value <- 0
ensemble_bayesrnn_t_test_t <- 0
ensemble_bayesrnn_t_test_p_value <- 0
ensemble_y_hat_bayesrnn_total <- 0
ensemble_bayesrnn_actual <- 0
ensemble_bayesrnn_actual_total <- 0

ensemble_cubist_train_RMSE <- 0
ensemble_cubist_test_RMSE <- 0
ensemble_cubist_validation_RMSE <- 0
ensemble_cubist_sd <- 0
ensemble_cubist_overfitting <- 0
ensemble_cubist_duration <- 0
ensemble_cubist_holdout_RMSE <- 0
ensemble_cubist_holdout_RMSE_mean <- 0
ensemble_cubist_predict_value_mean <- 0
ensemble_cubist_bias <- 0
ensemble_cubist_ks_stat <- 0
ensemble_cubist_ks_p_value <- 0
ensemble_cubist_t_test_t <- 0
ensemble_cubist_t_test_p_value <- 0
ensemble_y_hat_cubist_total <- 0
ensemble_cubist_actual <- 0
ensemble_cubist_actual_total <- 0

ensemble_earth_train_RMSE <- 0
ensemble_earth_test_RMSE <- 0
ensemble_earth_validation_RMSE <- 0
ensemble_earth_sd <- 0
ensemble_earth_overfitting <- 0
ensemble_earth_duration <- 0
ensemble_earth_holdout_RMSE <- 0
ensemble_earth_holdout_RMSE_mean <- 0
ensemble_earth_predict_value_mean <- 0
ensemble_earth_bias <- 0
ensemble_earth_ks_stat <- 0
ensemble_earth_ks_p_value <- 0
ensemble_earth_t_test_t <- 0
ensemble_earth_t_test_p_value <- 0
ensemble_y_hat_earth_total <- 0
ensemble_earth_actual <- 0
ensemble_earth_actual_total <- 0

ensemble_elastic_test_RMSE <- 0
ensemble_elastic_test_RMSE_df <- data.frame(ensemble_elastic_test_RMSE)
ensemble_elastic_validation_RMSE <- 0
ensemble_elastic_validation_RMSE_df <- data.frame(ensemble_elastic_validation_RMSE)
ensemble_elastic_validation_RMSE_mean <- 0
ensemble_elastic_validation_predict_value <- 0
ensemble_elastic_validation_predict_value_mean <- 0
ensemble_elastic_validation_predict_value_df <- data.frame(ensemble_elastic_validation_predict_value)
ensemble_elastic_test_predict_value <- 0
ensemble_elastic_test_predict_value_df <- data.frame(ensemble_elastic_test_predict_value)
ensemble_elastic_validation_predict_value <- 0
ensemble_elastic_test_predict_value_mean <- 0
ensemble_elastic_test_predict_value_df <- data.frame(ensemble_elastic_test_predict_value)
ensemble_elastic_test_predict_value_sd <- 0
ensemble_y_hat_elastic <- 0
ensemble_elastic_sd <- 0
ensemble_elastic_sd_df <- data.frame(ensemble_elastic_sd)
ensemble_elastic_sd_mean <- 0
ensemble_elastic_validation_predict_value_sd <- 0
ensemble_lasso_validation_predict_value_sd <- 0
ensemble_elastic_validation_sd <- 0
ensemble_elastic_validation_sd_df <- data.frame(ensemble_elastic_validation_sd)
ensemble_elastic_validation_predict_value_sd_df <- data.frame(ensemble_elastic_validation_predict_value_sd)
ensemble_elastic_test_sd <- 0
ensemble_elastic_test_sd_df <- data.frame(ensemble_elastic_test_sd)
ensemble_elastic_validation_sd <- 0
ensemble_y_hat_elastic <- 0
ensemble_elastic_train_RMSE <- 0
ensemble_elastic_train_RMSE_df <- data.frame(ensemble_elastic_train_RMSE)
ensemble_elastic_train_RMSE_mean <- 0
ensemble_elastic_holdout_RMSE <- 0
ensemble_elastic_holdout_RMSE_mean <- 0
ensemble_elastic_holdout_RMSE_sd_mean <- 0
ensemble_elastic_overfitting <- 0
ensemble_elastic_overfitting_df <- data.frame(ensemble_elastic_overfitting)
ensemble_elastic_overfitting_mean <- 0
ensemble_elastic_predict_value_mean <- 0
ensemble_elastic_duration <- 0
ensemble_elastic_duration_mean <- 0
ensemble_elastic_bias <- 0
ensemble_elastic_ks_stat <- 0
ensemble_elastic_ks_p_value <- 0
ensemble_elastic_t_test_t <- 0
ensemble_elastic_t_test_p_value <- 0
ensemble_y_hat_elastic_total <- 0
ensemble_elastic_actual <- 0
ensemble_elastic_actual_total <- 0

ensemble_gb_train_RMSE <- 0
ensemble_gb_test_RMSE <- 0
ensemble_gb_validation_RMSE <- 0
ensemble_gb_sd <- 0
ensemble_gb_overfitting <- 0
ensemble_gb_duration <- 0
ensemble_gb_holdout_RMSE <- 0
ensemble_gb_holdout_RMSE_mean <- 0
ensemble_gb_predict_value_mean <- 0
ensemble_gb_bias <- 0
ensemble_gb_ks_stat <- 0
ensemble_gb_ks_p_value <- 0
ensemble_gb_t_test_t <- 0
ensemble_gb_t_test_p_value <- 0
ensemble_y_hat_gb_total <- 0
ensemble_gb_actual <- 0
ensemble_gb_actual_total <- 0

ensemble_lasso_test_RMSE <- 0
ensemble_lasso_test_RMSE_df <- data.frame(ensemble_lasso_test_RMSE)
ensemble_lasso_validation_RMSE <- 0
ensemble_lasso_validation_RMSE_df <- data.frame(ensemble_lasso_validation_RMSE)
ensemble_lasso_validation_RMSE_mean <- 0
ensemble_lasso_validation_predict_value <- 0
ensemble_lasso_validation_predict_value_mean <- 0
ensemble_lasso_validation_predict_value_df <- data.frame(ensemble_lasso_validation_predict_value)
ensemble_lasso_test_predict_value <- 0
ensemble_lasso_test_predict_value_df <- data.frame(ensemble_lasso_test_predict_value)
ensemble_lasso_validation_predict_value <- 0
ensemble_lasso_test_predict_value_mean <- 0
ensemble_lasso_test_predict_value_df <- data.frame(ensemble_lasso_test_predict_value)
ensemble_lasso_test_predict_value_sd <- 0
ensemble_y_hat_lasso <- 0
ensemble_lasso_sd <- 0
ensemble_lasso_sd_df <- data.frame(ensemble_lasso_sd)
ensemble_lasso_sd_mean <- 0
ensemble_lasso_validation_predict_value_sd <- 0
ensemble_lasso_validation_predict_value_sd <- 0
ensemble_lasso_validation_predict_value_sd_df <- data.frame(ensemble_lasso_validation_predict_value_sd)
ensemble_lasso_test_sd <- 0
ensemble_lasso_test_sd_df <- data.frame(ensemble_lasso_test_sd)
ensemble_lasso_validation_sd <- 0
ensemble_lasso_validation_sd_df <- data.frame(ensemble_lasso_validation_sd)
ensemble_y_hat_lasso <- 0
ensemble_lasso_train_RMSE <- 0
ensemble_lasso_train_RMSE_df <- data.frame(ensemble_lasso_train_RMSE)
ensemble_lasso_train_RMSE_mean <- 0
ensemble_lasso_holdout_RMSE <- 0
ensemble_lasso_holdout_RMSE_mean <- 0
ensemble_lasso_holdout_RMSE_sd_mean <- 0
ensemble_lasso_overfitting <- 0
ensemble_lasso_overfitting_df <- data.frame(ensemble_lasso_overfitting)
ensemble_lasso_overfitting_mean <- 0
ensemble_lasso_predict_value_mean <- 0
ensemble_lasso_duration <- 0
ensemble_lasso_duration_mean <- 0
ensemble_lasso_bias <- 0
ensemble_lasso_ks_stat <- 0
ensemble_lasso_ks_p_value <- 0
ensemble_lasso_t_test_t <- 0
ensemble_lasso_t_test_p_value <- 0
ensemble_y_hat_lasso_total <- 0
ensemble_lasso_actual <- 0
ensemble_lasso_actual_total <- 0

ensemble_linear_train_RMSE <- 0
ensemble_linear_test_RMSE <- 0
ensemble_linear_validation_RMSE <- 0
ensemble_linear_sd <- 0
ensemble_linear_overfitting <- 0
ensemble_linear_duration <- 0
ensemble_linear_holdout_RMSE <- 0
ensemble_linear_holdout_RMSE_mean <- 0
ensemble_linear_predict_value_mean <- 0
ensemble_linear_bias <- 0
ensemble_linear_ks_stat <- 0
ensemble_linear_ks_p_value <- 0
ensemble_linear_t_test_t <- 0
ensemble_linear_t_test_p_value <- 0
ensemble_y_hat_linear_total <- 0
ensemble_linear_actual <- 0
ensemble_linear_actual_total <- 0

ensemble_neuralnet_train_RMSE <- 0
ensemble_neuralnet_test_RMSE <- 0
ensemble_neuralnet_validation_RMSE <- 0
ensemble_neuralnet_sd <- 0
ensemble_neuralnet_overfitting <- 0
ensemble_neuralnet_duration <- 0
ensemble_neuralnet_holdout_RMSE <- 0
ensemble_neuralnet_holdout_RMSE_mean <- 0
ensemble_neuralnet_bias <- 0
ensemble_neuralnet_ks_stat <- 0
ensemble_neuralnet_ks_p_value <- 0
ensemble_neuralnet_test_predict_value_mean <- 0
ensemble_neuralnet_t_test_t <- 0
ensemble_neuralnet_t_test_p_value <- 0
y_hat_ensemble_neuralnet_total <- 0
ensemble_neuralnet_actual <- 0
ensemble_neuralnet_actual_total <- 0

ensemble_ridge_test_RMSE <- 0
ensemble_ridge_test_RMSE_df <- data.frame(ensemble_ridge_test_RMSE)
ensemble_ridge_validation_RMSE <- 0
ensemble_ridge_validation_RMSE_df <- data.frame(ensemble_ridge_validation_RMSE)
ensemble_ridge_validation_RMSE_mean <- 0
ensemble_ridge_validation_predict_value <- 0
ensemble_ridge_validation_predict_value_mean <- 0
ensemble_ridge_validation_predict_value_df <- data.frame(ensemble_ridge_validation_predict_value)
ensemble_ridge_test_predict_value <- 0
ensemble_ridge_test_predict_value_df <- data.frame(ensemble_ridge_test_predict_value)
ensemble_ridge_validation_predict_value <- 0
ensemble_ridge_test_predict_value_mean <- 0
ensemble_ridge_test_predict_value_df <- data.frame(ensemble_ridge_test_predict_value)
ensemble_ridge_test_predict_value_sd <- 0
ensemble_y_hat_ridge <- 0
ensemble_ridge_sd <- 0
ensemble_ridge_sd_df <- data.frame(ensemble_ridge_sd)
ensemble_ridge_sd_mean <- 0
ensemble_ridge_validation_predict_value_sd <- 0
ensemble_lasso_validation_predict_value_sd <- 0
ensemble_ridge_validation_predict_value_sd_df <- data.frame(ensemble_ridge_validation_predict_value_sd)
ensemble_ridge_test_sd <- 0
ensemble_ridge_test_sd_df <- data.frame(ensemble_ridge_test_sd)
ensemble_ridge_validation_sd <- 0
ensemble_ridge_validation_sd_mean <- 0
ensemble_ridge_validation_sd_df <- data.frame(ensemble_ridge_validation_sd)
ensemble_y_hat_ridge <- 0
ensemble_ridge_train_RMSE <- 0
ensemble_ridge_train_RMSE_df <- data.frame(ensemble_ridge_train_RMSE)
ensemble_ridge_train_RMSE_mean <- 0
ensemble_ridge_holdout_RMSE <- 0
ensemble_ridge_holdout_RMSE_mean <- 0
ensemble_ridge_holdout_RMSE_sd_mean <- 0
ensemble_ridge_overfitting <- 0
ensemble_ridge_overfitting_df <- data.frame(ensemble_ridge_overfitting)
ensemble_ridge_overfitting_mean <- 0
ensemble_ridge_predict_value_mean <- 0
ensemble_ridge_duration <- 0
ensemble_ridge_duration_mean <- 0
ensemble_ridge_bias <- 0
ensemble_ridge_ks_stat <- 0
ensemble_ridge_ks_p_value <- 0
ensemble_ridge_t_test_t <- 0
ensemble_ridge_t_test_p_value <- 0
ensemble_y_hat_ridge_total <- 0
ensemble_ridge_actual <- 0
ensemble_ridge_actual_total <- 0

ensemble_rpart_train_RMSE <- 0
ensemble_rpart_test_RMSE <- 0
ensemble_rpart_validation_RMSE <- 0
ensemble_rpart_sd <- 0
ensemble_rpart_overfitting <- 0
ensemble_rpart_duration <- 0
ensemble_rpart_holdout_RMSE <- 0
ensemble_rpart_holdout_RMSE_mean <- 0
ensemble_rpart_predict_value_mean <- 0
ensemble_rpart_train_RMSE <- 0
ensemble_rpart_test_RMSE <- 0
ensemble_rpart_validation_RMSE <- 0
ensemble_rpart_sd <- 0
ensemble_rpart_overfitting <- 0
ensemble_rpart_duration <- 0
ensemble_rpart_holdout_RMSE <- 0
ensemble_rpart_holdout_RMSE_mean <- 0
ensemble_rpart_predict_value_mean <- 0
ensemble_rpart_bias <- 0
ensemble_rpart_ks_stat <- 0
ensemble_rpart_ks_p_value <- 0
ensemble_rpart_t_test_t <- 0
ensemble_rpart_t_test_p_value <- 0
ensemble_y_hat_rpart_total <- 0
ensemble_rpart_actual <- 0
ensemble_rpart_actual_total <- 0

ensemble_svm_train_RMSE <- 0
ensemble_svm_test_RMSE <- 0
ensemble_svm_validation_RMSE <- 0
ensemble_svm_sd <- 0
ensemble_svm_overfitting <- 0
ensemble_svm_duration <- 0
ensemble_svm_holdout_RMSE <- 0
ensemble_svm_holdout_RMSE_mean <- 0
ensemble_svm_predict_value_mean <- 0
ensemble_svm_bias <- 0
ensemble_svm_bias <- 0
ensemble_svm_ks_stat <- 0
ensemble_svm_ks_p_value <- 0
ensemble_svm_t_test_t <- 0
ensemble_svm_t_test_p_value <- 0
ensemble_y_hat_svm_total <- 0
ensemble_svm_actual <- 0
ensemble_svm_actual_total <- 0

ensemble_tree_train_RMSE <- 0
ensemble_tree_test_RMSE <- 0
ensemble_tree_validation_RMSE <- 0
ensemble_tree_sd <- 0
ensemble_tree_overfitting <- 0
ensemble_tree_duration <- 0
ensemble_tree_holdout_RMSE <- 0
ensemble_tree_holdout_RMSE_mean <- 0
ensemble_tree_predict_value_mean <- 0
ensemble_tree_bias <- 0
ensemble_tree_ks_stat <- 0
ensemble_tree_ks_p_value <- 0
ensemble_tree_t_test_t <- 0
ensemble_tree_t_test_p_value <- 0
ensemble_y_hat_tree_total <- 0
ensemble_tree_actual <- 0
ensemble_tree_actual_total <- 0


actual <- 0
cols <- 0
actual <- 0
predicted <- 0
value <- 0
grid.arrange <- 0
holdout <- 0
Mean_holdout_RMSE <- 0
count <- 0
model <- 0
mallows_cp <- 0
overfitting <- 0
Duration <- 0
Model <- 0
overfitting_mean <- 0
overfitting_sd <- 0
Mean_Bias <- 0

outliers_df <- data.frame()
Std_Deviation_of_holdout_RMSE <- 0
overfitting_sd <- 0
Bias <- 0

Duration_sd <- 0
KS_Test_P_Value_mean <- 0
KS_Test_P_Value_std_dev <- 0
t.test <- 0
t_test_p.value_mean <- 0
t_test_p_value_sd <- 0
plot <- 0
total_plot <- 0
Overfitting_mean <- 0
Overfitting_sd <- 0
Percentage <- 0
Variable <- 0
RMSE_Std_Dev <- 0
Group.1 <- 0
.pt <- 0
which_column_number <- 0
train_ratio_df <- data.frame()
test_ratio_df <- data.frame()
validation_ratio_df <- data.frame()
stratified_sampling_report <- 0
section <- 0

#### Resampling start here ####

for (i in 1:numresamples) {
  message(noquote(""))
  message(paste0("Resampling number ", i, " of ", numresamples, sep = ','))
  message(noquote(""))

  if(set_seed == "Y"){
    train <- df[1:round(train_amount*nrow(df)), ]
    test <- df[round(train_amount*nrow(df)) +1:round(test_amount*nrow(df)), ]
    validation <- df[(nrow(test) + nrow(train) +1) : nrow(df), ]
  }

  if(set_seed == "N"){
    idx <- sample(seq(1, 3), size = nrow(df), replace = TRUE, prob = c(train_amount, test_amount, validation_amount))
    train <- df[idx == 1, ]
    test <- df[idx == 2, ]
    validation <- df[idx == 3, ]
  }

  if(stratified_random_column > 0){
    df <- df[sample(nrow(df)),]
    train <- as.data.frame(df %>% dplyr::group_by(colnames(df[, stratified_random_column-1])) %>% dplyr::sample_frac(train_amount))
    train_ratio <- table(train[, stratified_random_column-1])/nrow(train)
    train_ratio_df <- dplyr::bind_rows(train_ratio_df, train_ratio)
    train_ratio_mean <- colMeans(train_ratio_df)

    test <- as.data.frame(df %>% dplyr::group_by(colnames(df[, stratified_random_column-1])) %>% dplyr::sample_frac(test_amount))
    test_ratio <- table(test[, stratified_random_column-1])/nrow(test)
    test_ratio_df <- dplyr::bind_rows(test_ratio_df, test_ratio)
    test_ratio_mean <- colMeans(test_ratio_df)

    validation <- as.data.frame(df %>% dplyr::group_by(colnames(df[, stratified_random_column-1])) %>% dplyr::sample_frac(validation_amount))
    validation_ratio <- table(validation[, stratified_random_column-1])/nrow(validation)
    validation_ratio_df <- dplyr::bind_rows(validation_ratio_df, validation_ratio)
    validation_ratio_mean <- colMeans(validation_ratio_df)

    df1 <- as.data.frame(rbind(train_ratio_mean, test_ratio_mean, validation_ratio_mean))
    colnames(df1) <- levels
    row.names(df1) <- c('train ratios', 'test ratios', 'validation ratios')
    df1 <- df1 %>% mutate_if(is.numeric, round, 4)

    stratified_sampling_report <- reactable::reactable(df1, searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                                       striped = TRUE, highlight = TRUE, resizable = TRUE
    )

    htmltools::div(class = "table",
                   htmltools::div(class = "title", "stratified_sampling_report")
    )

    stratified_sampling_report <- htmlwidgets::prependContent(stratified_sampling_report, htmltools::h2(class = "title", "Stratified sampling report"))

  }

  ####  Model #1 Bagging ####
  bagging_start <- Sys.time()
  message("Working on Bagging")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    bagging_train_fit <- ipred::bagging(formula = y ~ ., data = train)
  }
  if(set_seed == "N"){
    bagging_train_fit <- ipred::bagging(formula = y ~ ., data = train)
  }
  bagging_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = bagging_train_fit, newdata = train))
  bagging_train_RMSE_mean <- mean(bagging_train_RMSE)
  bagging_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = bagging_train_fit, newdata = test))
  bagging_test_RMSE_mean <- mean(bagging_test_RMSE)
  bagging_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = bagging_train_fit, newdata = validation))
  bagging_validation_RMSE_mean <- mean(bagging_validation_RMSE)
  bagging_holdout_RMSE[i] <- mean(bagging_test_RMSE_mean, bagging_validation_RMSE_mean)
  bagging_holdout_RMSE_mean <- mean(bagging_holdout_RMSE)
  bagging_holdout_RMSE_sd_mean <- sd(c(bagging_test_RMSE_mean, bagging_validation_RMSE_mean))
  bagging_train_predict_value <- as.numeric(predict(object = bagging_train_fit, newdata = train))
  bagging_test_predict_value <- as.numeric(predict(object = bagging_train_fit, newdata = test))
  bagging_validation_predict_value <- as.numeric(predict(object = bagging_train_fit, newdata = validation))
  bagging_predict_value_mean <- mean(c(bagging_test_predict_value, bagging_validation_predict_value))
  bagging_sd[i] <- sd(c(bagging_test_predict_value, bagging_validation_predict_value))
  bagging_sd_mean <- mean(bagging_sd)
  bagging_overfitting[i] <- bagging_holdout_RMSE_mean / bagging_train_RMSE_mean
  bagging_overfitting_mean <- mean(bagging_overfitting)
  bagging_overfitting_range <- range(bagging_overfitting)
  bagging_overfitting_sd <- sd(bagging_overfitting)
  y_hat_bagging <- c(bagging_test_predict_value, bagging_validation_predict_value)
  y_hat_bagging_total <- c(y_hat_bagging, y_hat_bagging_total)
  bagging_actual <- c(test$y, validation$y)
  bagging_actual_total <- c(bagging_actual, bagging_actual_total)
  bagging_t_test_t[i] <- as.numeric(t.test(x = y_hat_bagging, y = c(test$y, validation$y), var.equal = TRUE)[1])
  bagging_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_bagging, y = c(test$y, validation$y), var.equal = TRUE)[3])
  bagging_t_test_t_mean <- mean(as.numeric(bagging_t_test_t))
  bagging_t_test_p_value_mean <- mean(as.numeric(bagging_t_test_p_value))
  bagging_t_test_p_value_sd <- sd(as.numeric(bagging_t_test_p_value))
  bagging_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(bagging_test_predict_value, bagging_validation_predict_value))
  bagging_bias_mean <- mean(bagging_bias)
  bagging_bias_sd <- sd(bagging_bias)
  bagging_ks_p_value[i] <- stats::ks.test(x = y_hat_bagging, y = c(test$y, validation$y), exact = TRUE)$p.value
  bagging_ks_p_value_mean <- mean(bagging_ks_p_value)
  bagging_ks_p_value_sd <- sd(bagging_ks_p_value)
  bagging_ks_stat[i] <- stats::ks.test(x = y_hat_bagging, y = c(test$y, validation$y), exact = TRUE)$statistic
  bagging_ks_stat_mean <- mean(bagging_ks_stat)
  bagging_ks_test <- c(bagging_ks_stat_mean, bagging_ks_p_value_mean)
  bagging_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*bagging_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  bagging_lower_95 <- bagging_holdout_RMSE_mean - bagging_margin
  bagging_upper_95 <- bagging_holdout_RMSE_mean + bagging_margin
  bagging_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*bagging_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  bagging_overfitting_lower_95 <- bagging_overfitting_mean - bagging_overfitting_margin
  bagging_overfitting_upper_95 <- bagging_overfitting_mean + bagging_overfitting_margin
  bagging_end <- Sys.time()
  bagging_duration[i] <- bagging_end - bagging_start
  bagging_duration_mean <- mean(bagging_duration)
  bagging_duration_sd <- sd(bagging_duration)

  ####  Model #2 Bayes Generalized Linear Model (GLM) ####
  bayesglm_start <- Sys.time()
  message("Working on BayesGLM")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    bayesglm_train_fit <- arm::bayesglm(y ~ ., data = train, family = gaussian(link = "identity"))
  }
  if(set_seed == "N"){
    bayesglm_train_fit <- arm::bayesglm(y ~ ., data = train, family = gaussian(link = "identity"))
  }
  bayesglm_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = bayesglm_train_fit, newdata = train))
  bayesglm_train_RMSE_mean <- mean(bayesglm_train_RMSE)
  bayesglm_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = bayesglm_train_fit, newdata = test))
  bayesglm_test_RMSE_mean <- mean(bayesglm_test_RMSE)
  bayesglm_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = bayesglm_train_fit, newdata = validation))
  bayesglm_validation_RMSE_mean <- mean(bayesglm_validation_RMSE)
  bayesglm_holdout_RMSE[i] <- mean(bayesglm_test_RMSE_mean, bayesglm_validation_RMSE_mean)
  bayesglm_holdout_RMSE_mean <- mean(bayesglm_holdout_RMSE)
  bayesglm_holdout_RMSE_sd_mean <- sd(c(bayesglm_test_RMSE_mean, bayesglm_validation_RMSE_mean))
  bayesglm_train_predict_value <- as.numeric(predict(object = bayesglm_train_fit, newdata = train))
  bayesglm_test_predict_value <- as.numeric(predict(object = bayesglm_train_fit, newdata = test))
  bayesglm_validation_predict_value <- as.numeric(predict(object = bayesglm_train_fit, newdata = validation))
  bayesglm_predict_value_mean <- mean(c(bayesglm_test_predict_value, bayesglm_validation_predict_value))
  bayesglm_sd[i] <- sd(c(bayesglm_test_predict_value, bayesglm_validation_predict_value))
  bayesglm_sd_mean <- mean(bayesglm_sd)
  bayesglm_overfitting[i] <- bayesglm_holdout_RMSE_mean / bayesglm_train_RMSE_mean
  bayesglm_overfitting_mean <- mean(bayesglm_overfitting)
  bayesglm_overfitting_range <- range(bayesglm_overfitting)
  bayesglm_overfitting_sd <- sd(bayesglm_overfitting)
  y_hat_bayesglm <- c(bayesglm_test_predict_value, bayesglm_validation_predict_value)
  y_hat_bayesglm_total <- c(y_hat_bayesglm, y_hat_bayesglm_total)
  bayesglm_actual <- c(test$y, validation$y)
  bayesglm_actual_total <- c(bayesglm_actual, bayesglm_actual_total)
  bayesglm_t_test_t[i] <- as.numeric(t.test(x = y_hat_bayesglm, y = c(test$y, validation$y), var.equal = TRUE)[1])
  bayesglm_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_bayesglm, y = c(test$y, validation$y), var.equal = TRUE)[3])
  bayesglm_t_test_t_mean <- mean(as.numeric(bayesglm_t_test_t))
  bayesglm_t_test_p_value_mean <- mean(as.numeric(bayesglm_t_test_p_value))
  bayesglm_t_test_p_value_sd <- sd(as.numeric(bayesglm_t_test_p_value))
  bayesglm_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(bayesglm_test_predict_value, bayesglm_validation_predict_value))
  bayesglm_bias_mean <- mean(bayesglm_bias)
  bayesglm_bias_sd <- sd(bayesglm_bias)
  bayesglm_ks_p_value[i] <- stats::ks.test(x = y_hat_bayesglm, y = c(test$y, validation$y), exact = TRUE)$p.value
  bayesglm_ks_p_value_mean <- mean(bayesglm_ks_p_value)
  bayesglm_ks_p_value_sd <- sd(bayesglm_ks_p_value)
  bayesglm_ks_stat[i] <- stats::ks.test(x = y_hat_bayesglm, y = c(test$y, validation$y), exact = TRUE)$statistic
  bayesglm_ks_stat_mean <- mean(bayesglm_ks_stat)
  bayesglm_ks_test <- c(bayesglm_ks_stat_mean, bayesglm_ks_p_value_mean)
  bayesglm_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*bayesglm_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  bayesglm_lower_95 <- bayesglm_holdout_RMSE_mean - bayesglm_margin
  bayesglm_upper_95 <- bayesglm_holdout_RMSE_mean + bayesglm_margin
  bayesglm_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*bayesglm_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  bayesglm_overfitting_lower_95 <- bayesglm_overfitting_mean - bayesglm_overfitting_margin
  bayesglm_overfitting_upper_95 <- bayesglm_overfitting_mean + bayesglm_overfitting_margin

  bayesglm_end <- Sys.time()
  bayesglm_duration[i] <- bayesglm_end - bayesglm_start
  bayesglm_duration_mean <- mean(bayesglm_duration)
  bayesglm_duration_sd <- sd(bayesglm_duration)

  ####  Model #3 Bayes RNN: Bayes Regularization for feed forward neural networks ####
  bayesrnn_start <- Sys.time()
  message("Working on BayesRNN")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    bayesrnn_train_fit <- brnn::brnn(x = as.matrix(train), y = train$y)
  }
  if(set_seed == "N"){
    bayesrnn_train_fit <- brnn::brnn(x = as.matrix(train), y = train$y)
  }
  bayesrnn_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = bayesrnn_train_fit, newdata = train))
  bayesrnn_train_RMSE_mean <- mean(bayesrnn_train_RMSE)
  bayesrnn_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = bayesrnn_train_fit, newdata = test))
  bayesrnn_test_RMSE_mean <- mean(bayesrnn_test_RMSE)
  bayesrnn_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = bayesrnn_train_fit, newdata = validation))
  bayesrnn_validation_RMSE_mean <- mean(bayesrnn_validation_RMSE)
  bayesrnn_holdout_RMSE[i] <- mean(c(bayesrnn_test_RMSE_mean, bayesrnn_validation_RMSE_mean))
  bayesrnn_holdout_RMSE_mean <- mean(bayesrnn_holdout_RMSE)
  bayesrnn_holdout_RMSE_sd_mean <- sd(c(bayesrnn_test_RMSE_mean, bayesrnn_validation_RMSE_mean))
  bayesrnn_train_predict_value <- as.numeric(predict(object = bayesrnn_train_fit, newdata = train))
  bayesrnn_test_predict_value <- as.numeric(predict(object = bayesrnn_train_fit, newdata = test))
  bayesrnn_validation_predict_value <- as.numeric(predict(object = bayesrnn_train_fit, newdata = validation))
  bayesrnn_predict_value_mean <- mean(c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value))
  bayesrnn_sd_mean <- sd(c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value))
  bayesrnn_overfitting[i] <- bayesrnn_holdout_RMSE_mean / bayesrnn_train_RMSE_mean
  bayesrnn_overfitting_mean <- mean(bayesrnn_overfitting)
  bayesrnn_overfitting_range <- range(bayesrnn_overfitting)
  bayesrnn_overfitting_sd <- sd(bayesrnn_overfitting)
  y_hat_bayesrnn <- c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value)
  y_hat_bayesrnn_total <- c(y_hat_bayesrnn, y_hat_bayesrnn_total)
  bayesrnn_actual <- c(test$y, validation$y)
  bayesrnn_actual_total <- c(bayesrnn_actual, bayesrnn_actual_total)
  bayesrnn_t_test_t[i] <- as.numeric(t.test(x = y_hat_bayesrnn, y = c(test$y, validation$y), var.equal = TRUE)[1])
  bayesrnn_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_bayesrnn, y = c(test$y, validation$y), var.equal = TRUE)[3])
  bayesrnn_t_test_t_mean <- mean(as.numeric(bayesrnn_t_test_t))
  bayesrnn_t_test_p_value_mean <- mean(as.numeric(bayesrnn_t_test_p_value))
  bayesrnn_t_test_p_value_sd <- sd(as.numeric(bayesrnn_t_test_p_value))
  bayesrnn_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(bayesrnn_test_predict_value, bayesrnn_validation_predict_value))
  bayesrnn_bias_mean <- mean(bayesrnn_bias)
  bayesrnn_bias_sd <- sd(bayesrnn_bias)
  bayesrnn_ks_p_value[i] <- stats::ks.test(x = y_hat_bayesrnn, y = c(test$y, validation$y), exact = TRUE)$p.value
  bayesrnn_ks_p_value_mean <- mean(bayesrnn_ks_p_value)
  bayesrnn_ks_p_value_sd <- sd(bayesrnn_ks_p_value)
  bayesrnn_ks_stat[i] <- stats::ks.test(x = y_hat_bayesrnn, y = c(test$y, validation$y), exact = TRUE)$statistic
  bayesrnn_ks_stat_mean <- mean(bayesrnn_ks_stat)
  bayesrnn_ks_test <- c(bayesrnn_ks_stat_mean, bayesrnn_ks_p_value_mean)
  bayesrnn_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*bayesrnn_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  bayesrnn_lower_95 <- bayesrnn_holdout_RMSE_mean - bayesrnn_margin
  bayesrnn_upper_95 <- bayesrnn_holdout_RMSE_mean + bayesrnn_margin
  bayesrnn_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*bayesrnn_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  bayesrnn_overfitting_lower_95 <- bayesrnn_overfitting_mean - bayesrnn_overfitting_margin
  bayesrnn_overfitting_upper_95 <- bayesrnn_overfitting_mean + bayesrnn_overfitting_margin

  bayesrnn_end <- Sys.time()
  bayesrnn_duration[i] <- bayesrnn_end - bayesrnn_start
  bayesrnn_duration_mean <- mean(bayesrnn_duration)
  bayesrnn_duration_sd <- sd(bayesrnn_duration)

  ####  Model #4 Cubist ####
  cubist_start <- Sys.time()
  message("Working on Cubist")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    cubist_train_fit <- Cubist::cubist(x = train[, 1:ncol(train) - 1], y = train$y)
  }
  if(set_seed == "N"){
    cubist_train_fit <- Cubist::cubist(x = train[, 1:ncol(train) - 1], y = train$y)
  }
  cubist_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = cubist_train_fit, newdata = train))
  cubist_train_RMSE_mean <- mean(cubist_train_RMSE)
  cubist_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = cubist_train_fit, newdata = test))
  cubist_test_RMSE_mean <- mean(cubist_test_RMSE)
  cubist_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = cubist_train_fit, newdata = validation))
  cubist_validation_RMSE_mean <- mean(cubist_validation_RMSE)
  cubist_holdout_RMSE[i] <- mean(cubist_test_RMSE_mean, cubist_validation_RMSE_mean)
  cubist_holdout_RMSE_mean <- mean(cubist_holdout_RMSE)
  cubist_holdout_RMSE_sd_mean <- sd(c(cubist_test_RMSE_mean, cubist_validation_RMSE_mean))
  cubist_train_predict_value <- as.numeric(predict(object = cubist_train_fit, newdata = train))
  cubist_test_predict_value <- as.numeric(predict(object = cubist_train_fit, newdata = test))
  cubist_validation_predict_value <- as.numeric(predict(object = cubist_train_fit, newdata = validation))
  cubist_predict_value_mean <- mean(c(cubist_test_predict_value, cubist_validation_predict_value))
  cubist_sd[i] <- sd(c(cubist_test_predict_value, cubist_validation_predict_value))
  cubist_sd_mean <- mean(cubist_sd)
  cubist_overfitting[i] <- cubist_holdout_RMSE_mean / cubist_train_RMSE_mean
  cubist_overfitting_mean <- mean(cubist_overfitting)
  cubist_overfitting_range <- range(cubist_overfitting)
  cubist_overfitting_sd <- sd(cubist_overfitting)
  y_hat_cubist <- c(cubist_test_predict_value, cubist_validation_predict_value)
  y_hat_cubist_total <- c(y_hat_cubist, y_hat_cubist_total)
  cubist_actual <- c(test$y, validation$y)
  cubist_actual_total <- c(cubist_actual, cubist_actual_total)
  cubist_t_test_t[i] <- as.numeric(t.test(x = y_hat_cubist, y = c(test$y, validation$y), var.equal = TRUE)[1])
  cubist_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_cubist, y = c(test$y, validation$y), var.equal = TRUE)[3])
  cubist_t_test_t_mean <- mean(as.numeric(cubist_t_test_t))
  cubist_t_test_p_value_mean <- mean(as.numeric(cubist_t_test_p_value))
  cubist_t_test_p_value_sd <- sd(as.numeric(cubist_t_test_p_value))
  cubist_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(cubist_test_predict_value, cubist_validation_predict_value))
  cubist_bias_mean <- mean(cubist_bias)
  cubist_bias_sd <- sd(cubist_bias)
  cubist_ks_p_value[i] <- stats::ks.test(x = y_hat_cubist, y = c(test$y, validation$y), exact = TRUE)$p.value
  cubist_ks_p_value_mean <- mean(cubist_ks_p_value)
  cubist_ks_p_value_sd <- sd(cubist_ks_p_value)
  cubist_ks_stat[i] <- stats::ks.test(x = y_hat_cubist, y = c(test$y, validation$y), exact = TRUE)$statistic
  cubist_ks_stat_mean <- mean(cubist_ks_stat)
  cubist_ks_test <- c(cubist_ks_stat_mean, cubist_ks_p_value_mean)
  cubist_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*cubist_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  cubist_lower_95 <- cubist_holdout_RMSE_mean - cubist_margin
  cubist_upper_95 <- cubist_holdout_RMSE_mean + cubist_margin
  cubist_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*cubist_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  cubist_overfitting_lower_95 <- cubist_overfitting_mean - cubist_overfitting_margin
  cubist_overfitting_upper_95 <- cubist_overfitting_mean + cubist_overfitting_margin

  cubist_end <- Sys.time()
  cubist_duration[i] <- cubist_end - cubist_start
  cubist_duration_mean <- mean(cubist_duration)
  cubist_duration_sd <- sd(cubist_duration)

  ####  Model #5 earth ####
  earth_start <- Sys.time()
  message("Working on Earth")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    earth_train_fit <- earth::earth(x = train[, 1:ncol(train) - 1], y = train$y)
  }
  if(set_seed == "N"){
    earth_train_fit <- earth::earth(x = train[, 1:ncol(train) - 1], y = train$y)
  }
  earth_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = earth_train_fit, newdata = train))
  earth_train_RMSE_mean <- mean(earth_train_RMSE)
  earth_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = earth_train_fit, newdata = test))
  earth_test_RMSE_mean <- mean(earth_test_RMSE)
  earth_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = earth_train_fit, newdata = validation))
  earth_validation_RMSE_mean <- mean(earth_validation_RMSE)
  earth_holdout_RMSE[i] <- mean(earth_test_RMSE_mean, earth_validation_RMSE_mean)
  earth_holdout_RMSE_mean <- mean(earth_holdout_RMSE)
  earth_holdout_RMSE_sd_mean <- sd(c(earth_test_RMSE_mean, earth_validation_RMSE_mean))
  earth_train_predict_value <- as.numeric(predict(object = earth_train_fit, newdata = train))
  earth_test_predict_value <- as.numeric(predict(object = earth_train_fit, newdata = test))
  earth_validation_predict_value <- as.numeric(predict(object = earth_train_fit, newdata = validation))
  earth_predict_value_mean <- mean(c(earth_test_predict_value, earth_validation_predict_value))
  earth_sd[i] <- sd(c(earth_test_predict_value, earth_validation_predict_value))
  earth_sd_mean <- mean(earth_sd)
  earth_overfitting[i] <- earth_holdout_RMSE_mean / earth_train_RMSE_mean
  earth_overfitting_mean <- mean(earth_overfitting)
  earth_overfitting_range <- range(earth_overfitting)
  earth_overfitting_sd <- sd(earth_overfitting)
  y_hat_earth <- c(earth_test_predict_value, earth_validation_predict_value)
  y_hat_earth_total <- c(y_hat_earth, y_hat_earth_total)
  earth_actual <- c(test$y, validation$y)
  earth_actual_total <- c(earth_actual, earth_actual_total)
  earth_t_test_t[i] <- as.numeric(t.test(x = y_hat_earth, y = c(test$y, validation$y), var.equal = TRUE)[1])
  earth_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_earth, y = c(test$y, validation$y), var.equal = TRUE)[3])
  earth_t_test_t_mean <- mean(as.numeric(earth_t_test_t))
  earth_t_test_p_value_mean <- mean(as.numeric(earth_t_test_p_value))
  earth_t_test_p_value_sd <- sd(as.numeric(earth_t_test_p_value))
  earth_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(earth_test_predict_value, earth_validation_predict_value))
  earth_bias_mean <- mean(earth_bias)
  earth_bias_sd <- sd(earth_bias)
  earth_ks_p_value[i] <- stats::ks.test(x = y_hat_earth, y = c(test$y, validation$y), exact = TRUE)$p.value
  earth_ks_p_value_mean <- mean(earth_ks_p_value)
  earth_ks_p_value_sd <- sd(earth_ks_p_value)
  earth_ks_stat[i] <- stats::ks.test(x = y_hat_earth, y = c(test$y, validation$y), exact = TRUE)$statistic
  earth_ks_stat_mean <- mean(earth_ks_stat)
  earth_ks_test <- c(earth_ks_stat_mean, earth_ks_p_value_mean)
  earth_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*earth_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  earth_lower_95 <- earth_holdout_RMSE_mean - earth_margin
  earth_upper_95 <- earth_holdout_RMSE_mean + earth_margin
  earth_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*earth_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  earth_overfitting_lower_95 <- earth_overfitting_mean - earth_overfitting_margin
  earth_overfitting_upper_95 <- earth_overfitting_mean + earth_overfitting_margin

  earth_end <- Sys.time()
  earth_duration[i] <- earth_end - earth_start
  earth_duration_mean <- mean(earth_duration)
  earth_duration_sd <- sd(earth_duration)

  #### Model #6  Elastic Net ####
  elastic_start <- Sys.time()
  message("Working on Elastic")
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  elastic_model <- glmnet::glmnet(x, y, alpha = 0.5)
  elastic_cv <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  best_elastic_lambda <- elastic_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    best_elastic_model <- glmnet::glmnet(x, y, alpha = 0, lambda = best_elastic_lambda)
  }
  if(set_seed == "N"){
    best_elastic_model <- glmnet::glmnet(x, y, alpha = 0, lambda = best_elastic_lambda)
  }
  elastic_train_pred <- predict(best_elastic_model, s = best_elastic_lambda, newx = data.matrix(train %>% dplyr::select(-y)))
  elastic_train_RMSE <- Metrics::rmse(actual = y, predicted = elastic_train_pred)
  elastic_train_RMSE_df <- rbind(elastic_train_RMSE_df, elastic_train_RMSE)
  elastic_train_RMSE_mean <- mean(elastic_train_RMSE_df$elastic_train_RMSE[2:nrow(elastic_train_RMSE_df)])
  ## Elastic using the test data set ##

  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  elastic_model <- glmnet::glmnet(x, y, alpha = 0.5)
  elastic_cv <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  best_elastic_lambda <- elastic_cv$lambda.min
  best_elastic_model <- glmnet::glmnet(x, y, alpha = 0, lambda = best_elastic_lambda)
  elastic_test_pred <- predict(best_elastic_model, s = best_elastic_lambda, newx = data.matrix(test %>% dplyr::select(-y)))

  elastic_test_RMSE <- Metrics::rmse(actual = test$y, predicted = elastic_test_pred)
  elastic_test_RMSE_df <- rbind(elastic_test_RMSE_df, elastic_test_RMSE)
  elastic_test_RMSE_mean <- mean(elastic_test_RMSE_df$elastic_test_RMSE[2:nrow(elastic_test_RMSE_df)])
  elastic_test_predict_value[i] <- round(mean(elastic_test_pred), 4)
  elastic_test_predict_value_mean <- mean(elastic_test_predict_value)
  elastic_test_predict_value_sd[i] <- round(sd(elastic_test_pred), 4)
  elastic_test_predict_value_sd_mean <- mean(elastic_test_predict_value_sd)
  ## Elastic using the validation data set
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  elastic_model <- glmnet::glmnet(x, y, alpha = 0.5)
  elastic_cv <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  best_elastic_lambda <- elastic_cv$lambda.min
  best_elastic_model <- glmnet::glmnet(x, y, alpha = 0, lambda = best_elastic_lambda)
  elastic_validation_pred <- predict(best_elastic_model, s = best_elastic_lambda, newx = data.matrix(validation %>% dplyr::select(-y)))
  elastic_validation_RMSE <- Metrics::rmse(actual = validation$y, predicted = elastic_validation_pred)
  elastic_validation_RMSE_df <- rbind(elastic_validation_RMSE_df, elastic_validation_RMSE)
  elastic_validation_RMSE_mean <- mean(elastic_validation_RMSE_df$elastic_validation_RMSE[2:nrow(elastic_validation_RMSE_df)])
  elastic_validation_predict_value[i] <- round(mean(elastic_validation_pred), 4)
  elastic_validation_predict_value_mean <- mean(elastic_validation_predict_value)
  elastic_validation_predict_value_sd[i] <- round(sd(elastic_validation_pred), 4)
  elastic_validation_predict_value_sd_mean <- mean(elastic_validation_predict_value_sd)

  elastic_holdout_RMSE <- mean(elastic_test_RMSE_mean, elastic_validation_RMSE_mean)
  elastic_holdout_RMSE_df <- rbind(elastic_holdout_RMSE_df, elastic_holdout_RMSE)
  elastic_holdout_RMSE_mean <- mean(elastic_holdout_RMSE_df$elastic_holdout_RMSE[2:nrow(elastic_holdout_RMSE_df)])

  elastic_holdout_RMSE_sd <- sd(c(elastic_test_RMSE_mean, elastic_validation_RMSE_mean))
  elastic_holdout_RMSE_sd_df <- rbind(elastic_holdout_RMSE_sd, elastic_holdout_RMSE_sd_df)
  elastic_holdout_RMSE_sd_mean <- mean(elastic_holdout_RMSE_sd_df$elastic_holdout_RMSE_sd[2:nrow(elastic_holdout_RMSE_sd_df)])

  elastic_overfitting <- c(elastic_holdout_RMSE / elastic_train_RMSE)
  elastic_overfitting_df <- rbind(elastic_overfitting_df, elastic_overfitting)
  elastic_overfitting_mean <- mean(elastic_overfitting_df$elastic_overfitting[2:nrow(elastic_overfitting_df)])
  elastic_overfitting_sd <- sd(elastic_overfitting_df$elastic_overfitting)

  elastic_test_predict_value_mean <- mean(c(elastic_test_predict_value_mean, elastic_validation_predict_value_mean))

  elastic_sd[i] <- mean(elastic_test_predict_value_sd_mean, elastic_validation_predict_value_sd_mean)
  elastic_sd_mean <- mean(elastic_sd)

  y_hat_elastic <- as.numeric(c(rowMeans(elastic_test_pred), rowMeans(elastic_validation_pred)))
  y_hat_elastic_total <- c(y_hat_elastic, y_hat_elastic_total)
  elastic_actual <- c(test$y, validation$y)
  elastic_actual_total <- c(elastic_actual, elastic_actual_total)
  elastic_t_test_t[i] <- as.numeric(t.test(x = y_hat_elastic, y = c(test$y, validation$y), var.equal = TRUE)[1])
  elastic_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_elastic, y = c(test$y, validation$y), var.equal = TRUE)[3])
  elastic_t_test_t_mean <- mean(as.numeric(elastic_t_test_t))
  elastic_t_test_p_value_mean <- mean(as.numeric(elastic_t_test_p_value))
  elastic_t_test_p_value_sd <- sd(as.numeric(elastic_t_test_p_value))

  elastic_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = as.numeric(c(elastic_test_pred, elastic_validation_pred)))
  elastic_bias_mean <- mean(elastic_bias)
  elastic_bias_sd <- sd(elastic_bias)
  elastic_ks_p_value[i] <- stats::ks.test(x = y_hat_elastic, y = c(test$y, validation$y), exact = TRUE)$p.value
  elastic_ks_p_value_mean <- mean(elastic_ks_p_value)
  elastic_ks_p_value_sd <- sd(elastic_ks_p_value)
  elastic_ks_stat[i] <- stats::ks.test(x = y_hat_elastic, y = c(test$y, validation$y), exact = TRUE)$statistic
  elastic_ks_stat_mean <- mean(elastic_ks_stat)
  elastic_ks_test <- c(elastic_ks_stat_mean, elastic_ks_p_value_mean)
  elastic_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*elastic_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  elastic_lower_95 <- elastic_holdout_RMSE_mean - elastic_margin
  elastic_upper_95 <- elastic_holdout_RMSE_mean + elastic_margin
  elastic_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*elastic_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  elastic_overfitting_lower_95 <- elastic_overfitting_mean - elastic_overfitting_margin
  elastic_overfitting_upper_95 <- elastic_overfitting_mean + elastic_overfitting_margin

  elastic_end <- Sys.time()
  elastic_duration[i] <- elastic_end - elastic_start
  elastic_duration_mean <- mean(elastic_duration)
  elastic_duration_sd <- sd(elastic_duration)


  #### Model #7 GAM (Generalized Additive Models) with Smoothing Splines ####
  gam_start <- Sys.time()
  message("Working on Generalized Additive Models with Smoothing Splines")
  n_unique_vals <- purrr::map_dbl(df, dplyr::n_distinct)

  # Names of columns with >= 4 unique vals
  keep <- names(n_unique_vals)[n_unique_vals >= 4]

  gam_data <- df %>%
    dplyr::select(dplyr::all_of(keep))

  # Model data
  train1 <- train %>%
    dplyr::select(dplyr::all_of(keep))

  test1 <- test %>%
    dplyr::select(dplyr::all_of(keep))

  validation1 <- validation %>%
    dplyr::select(dplyr::all_of(keep))

  names_df <- names(gam_data[, 1:ncol(gam_data) - 1])
  f2 <- stats::as.formula(paste0("y ~", paste0("gam::s(", names_df, ")", collapse = "+")))
  if(set_seed == "Y"){
    set.seed(seed = seed)
    gam_train_fit <- gam(f2, data = train1)
  }
  if(set_seed == "N"){
    gam_train_fit <- gam(f2, data = train1)
  }
  gam_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = gam_train_fit, newdata = train))
  gam_train_RMSE_mean <- mean(gam_train_RMSE)
  gam_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = gam_train_fit, newdata = test))
  gam_test_RMSE_mean <- mean(gam_test_RMSE)
  gam_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = gam_train_fit, newdata = validation))
  gam_validation_RMSE_mean <- mean(gam_validation_RMSE)
  gam_holdout_RMSE[i] <- mean(gam_test_RMSE_mean, gam_validation_RMSE_mean)
  gam_holdout_RMSE_mean <- mean(gam_holdout_RMSE)
  gam_holdout_RMSE_sd_mean <- sd(c(gam_test_RMSE_mean, gam_validation_RMSE_mean))
  gam_train_predict_value <- as.numeric(predict(object = gam_train_fit, newdata = train))
  gam_test_predict_value <- as.numeric(predict(object = gam_train_fit, newdata = test))
  gam_validation_predict_value <- as.numeric(predict(object = gam_train_fit, newdata = validation))
  gam_predict_value_mean <- mean(c(gam_test_predict_value, gam_validation_predict_value))
  gam_sd[i] <- sd(c(gam_test_predict_value, gam_validation_predict_value))
  gam_sd_mean <- mean(gam_sd)
  gam_overfitting[i] <- gam_holdout_RMSE_mean / gam_train_RMSE_mean
  gam_overfitting_mean <- mean(gam_overfitting)
  gam_overfitting_range <- range(gam_overfitting)
  gam_overfitting_sd <- sd(gam_overfitting)
  gam_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(gam_test_predict_value, gam_validation_predict_value))
  gam_bias_mean <- mean(gam_bias)
  gam_bias_sd <- sd(gam_bias)
  y_hat_gam <- c(gam_test_predict_value, gam_validation_predict_value)
  y_hat_gam_total <- c(y_hat_gam, y_hat_gam_total)
  gam_actual <- c(test$y, validation$y)
  gam_actual_total <- c(gam_actual, gam_actual_total)
  gam_t_test_t[i] <- as.numeric(t.test(x = y_hat_gam, y = c(test$y, validation$y), var.equal = TRUE)[1])
  gam_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_gam, y = c(test$y, validation$y), var.equal = TRUE)[3])
  gam_t_test_t_mean <- mean(as.numeric(gam_t_test_t))
  gam_t_test_p_value_mean <- mean(as.numeric(gam_t_test_p_value))
  gam_t_test_p_value_sd <- sd(as.numeric(gam_t_test_p_value))
  gam_ks_p_value[i] <- stats::ks.test(x = y_hat_gam, y = c(test$y, validation$y), exact = TRUE)$p.value
  gam_ks_p_value_mean <- mean(gam_ks_p_value)
  gam_ks_p_value_sd <- sd(gam_ks_p_value)
  gam_ks_stat[i] <- stats::ks.test(x = y_hat_gam, y = c(test$y, validation$y), exact = TRUE)$statistic
  gam_ks_stat_mean <- mean(gam_ks_stat)
  gam_ks_test <- c(gam_ks_stat_mean, gam_ks_p_value_mean)
  gam_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*gam_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  gam_lower_95 <- gam_holdout_RMSE_mean - gam_margin
  gam_upper_95 <- gam_holdout_RMSE_mean + gam_margin
  gam_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*gam_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  gam_overfitting_lower_95 <- gam_overfitting_mean - gam_overfitting_margin
  gam_overfitting_upper_95 <- gam_overfitting_mean + gam_overfitting_margin

  gam_end <- Sys.time()
  gam_duration[i] <- gam_end - gam_start
  gam_duration_mean <- mean(gam_duration)
  gam_duration_sd <- sd(gam_duration)

  ####  Model #8 Gradient Boosted ####
  gb_start <- Sys.time()
  message("Working on Gradient Boosted")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    gb_train_fit <- gbm::gbm(train$y ~ ., data = train, distribution = "gaussian", n.trees = 100, shrinkage = 0.1, interaction.depth = 10)
  }
  if(set_seed == "N"){
    gb_train_fit <- gbm::gbm(train$y ~ ., data = train, distribution = "gaussian", n.trees = 100, shrinkage = 0.1, interaction.depth = 10)
  }
  gb_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = gb_train_fit, newdata = train))
  gb_train_RMSE_mean <- mean(gb_train_RMSE)
  gb_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = gb_train_fit, newdata = test))
  gb_test_RMSE_mean <- mean(gb_test_RMSE)
  gb_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = gb_train_fit, newdata = validation))
  gb_validation_RMSE_mean <- mean(gb_validation_RMSE)
  gb_holdout_RMSE[i] <- mean(c(gb_test_RMSE_mean, gb_validation_RMSE_mean))
  gb_holdout_RMSE_mean <- mean(gb_holdout_RMSE)
  gb_holdout_RMSE_sd_mean <- sd(c(gb_test_RMSE_mean, gb_validation_RMSE_mean))
  gb_train_predict_value <- as.numeric(predict(object = gb_train_fit, newdata = train))
  gb_test_predict_value <- as.numeric(predict(object = gb_train_fit, newdata = test))
  gb_validation_predict_value <- as.numeric(predict(object = gb_train_fit, newdata = validation))
  gb_predict_value_mean <- mean(c(gb_test_predict_value, gb_validation_predict_value))
  gb_sd[i] <- sd(c(gb_test_predict_value, gb_validation_predict_value))
  gb_sd_mean <- mean(gb_sd)
  gb_overfitting[i] <- gb_holdout_RMSE_mean / gb_train_RMSE_mean
  gb_overfitting_mean <- mean(gb_overfitting)
  gb_overfitting_range <- range(gb_overfitting)
  gb_overfitting_sd <- sd(gb_overfitting)
  y_hat_gb <- c(gb_test_predict_value, gb_validation_predict_value)
  y_hat_gb_total <- c(y_hat_gb, y_hat_gb_total)
  gb_actual <- c(test$y, validation$y)
  gb_actual_total <- c(gb_actual, gb_actual_total)
  gb_t_test_t[i] <- as.numeric(t.test(x = y_hat_gb, y = c(test$y, validation$y), var.equal = TRUE)[1])
  gb_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_gb, y = c(test$y, validation$y), var.equal = TRUE)[3])
  gb_t_test_t_mean <- mean(as.numeric(gb_t_test_t))
  gb_t_test_p_value_mean <- mean(as.numeric(gb_t_test_p_value))
  gb_t_test_p_value_sd <- sd(as.numeric(gb_t_test_p_value))
  gb_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(gb_test_predict_value, gb_validation_predict_value))
  gb_bias_mean <- mean(gb_bias)
  gb_bias_sd <- sd(gb_bias)
  gb_ks_p_value[i] <- stats::ks.test(x = y_hat_gb, y = c(test$y, validation$y), exact = TRUE)$p.value
  gb_ks_p_value_mean <- mean(gb_ks_p_value)
  gb_ks_p_value_sd <- sd(gb_ks_p_value)
  gb_ks_stat[i] <- stats::ks.test(x = y_hat_gb, y = c(test$y, validation$y), exact = TRUE)$statistic
  gb_ks_stat_mean <- mean(gb_ks_stat)
  gb_ks_test <- c(gb_ks_stat_mean, gb_ks_p_value_mean)
  gb_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*gb_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  gb_lower_95 <- gb_holdout_RMSE_mean - gb_margin
  gb_upper_95 <- gb_holdout_RMSE_mean + gb_margin
  gb_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*gb_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  gb_overfitting_lower_95 <- gb_overfitting_mean - gb_overfitting_margin
  gb_overfitting_upper_95 <- gb_overfitting_mean + gb_overfitting_margin

  gb_end <- Sys.time()
  gb_duration[i] <- gb_end - gb_start
  gb_duration_mean <- mean(gb_duration)
  gb_duration_sd <- sd(gb_duration)

  ####  9. Lasso Net ####
  lasso_start <- Sys.time()
  message("Working on Lasso")
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  lasso_model <- glmnet(x, y, alpha = 1)
  lasso_cv <- glmnet::cv.glmnet(x, y, alpha = 1)
  best_lasso_lambda <- lasso_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    best_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lasso_lambda)
  }
  if(set_seed == "N"){
    best_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lasso_lambda)
  }
  lasso_train_pred <- predict(best_lasso_model, s = best_lasso_lambda, newx = data.matrix(train %>% dplyr::select(-y)))
  lasso_train_RMSE <- Metrics::rmse(actual = y, predicted = lasso_train_pred)
  lasso_train_RMSE_df <- rbind(lasso_train_RMSE_df, lasso_train_RMSE)
  lasso_train_RMSE_mean <- mean(lasso_train_RMSE_df$lasso_train_RMSE[2:nrow(lasso_train_RMSE_df)])
  ## lasso using the test data set ##

  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  lasso_model <- glmnet(x, y, alpha = 1)
  lasso_cv <- glmnet::cv.glmnet(x, y, alpha = 1)
  best_lasso_lambda <- lasso_cv$lambda.min
  best_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lasso_lambda)
  lasso_test_pred <- predict(best_lasso_model, s = best_lasso_lambda, newx = data.matrix(test %>% dplyr::select(-y)))
  lasso_test_RMSE <- Metrics::rmse(actual = test$y, predicted = lasso_test_pred)
  lasso_test_RMSE_df <- rbind(lasso_test_RMSE_df, lasso_test_RMSE)
  lasso_test_RMSE_mean <- mean(lasso_test_RMSE_df$lasso_test_RMSE[2:nrow(lasso_test_RMSE_df)])
  lasso_test_predict_value[i] <- round(mean(lasso_test_pred), 4)
  lasso_test_predict_value_mean <- mean(lasso_test_predict_value)
  lasso_test_predict_value_sd[i] <- round(sd(lasso_test_pred), 4)
  lasso_test_predict_value_sd_mean <- mean(lasso_test_predict_value_sd)
  ## lasso using the validation data set
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  lasso_model <- glmnet(x, y, alpha = 1)
  lasso_cv <- glmnet::cv.glmnet(x, y, alpha = 1)
  best_lasso_lambda <- lasso_cv$lambda.min
  best_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lasso_lambda)
  lasso_validation_pred <- predict(best_lasso_model, s = best_lasso_lambda, newx = data.matrix(validation %>% dplyr::select(-y)))
  lasso_validation_RMSE <- Metrics::rmse(actual = validation$y, predicted = lasso_validation_pred)
  lasso_validation_RMSE_df <- rbind(lasso_validation_RMSE_df, lasso_validation_RMSE)
  lasso_validation_RMSE_mean <- mean(lasso_validation_RMSE_df$lasso_validation_RMSE[2:nrow(lasso_validation_RMSE_df)])
  lasso_validation_predict_value[i] <- round(mean(lasso_validation_pred), 4)
  lasso_validation_predict_value_mean <- mean(lasso_validation_predict_value)
  lasso_validation_predict_value_sd[i] <- round(sd(lasso_validation_pred), 4)
  lasso_validation_predict_value_sd_mean <- mean(lasso_validation_predict_value_sd)

  lasso_holdout_RMSE <- mean(lasso_test_RMSE_mean, lasso_validation_RMSE_mean)
  lasso_holdout_RMSE_df <- rbind(lasso_holdout_RMSE_df, lasso_holdout_RMSE)
  lasso_holdout_RMSE_mean <- mean(lasso_holdout_RMSE_df$lasso_holdout_RMSE[2:nrow(lasso_holdout_RMSE_df)])

  lasso_holdout_RMSE_sd <- sd(c(lasso_test_RMSE_mean, lasso_validation_RMSE_mean))
  lasso_holdout_RMSE_sd_df <- rbind(lasso_holdout_RMSE_sd_df, lasso_holdout_RMSE_sd)
  lasso_holdout_RMSE_sd_mean <- mean(lasso_holdout_RMSE_sd_df$lasso_holdout_RMSE_sd[2:nrow(lasso_holdout_RMSE_sd_df)])

  lasso_overfitting <- c(lasso_holdout_RMSE / lasso_train_RMSE)
  lasso_overfitting_df <- rbind(lasso_overfitting_df, lasso_overfitting)
  lasso_overfitting_mean <- mean(lasso_overfitting_df$lasso_overfitting[2:nrow(lasso_overfitting_df)])
  lasso_overfitting_sd <- sd(lasso_overfitting_df$lasso_overfitting)

  lasso_predict_value_mean <- mean(c(lasso_test_predict_value_mean, lasso_validation_predict_value_mean))

  lasso_sd[i] <- mean(lasso_test_predict_value_sd_mean, lasso_validation_predict_value_sd_mean)
  lasso_sd_mean <- mean(lasso_sd)

  lasso_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = as.numeric(c(lasso_test_pred, lasso_validation_pred)))
  lasso_bias_mean <- mean(lasso_bias)
  lasso_bias_sd <- sd(lasso_bias)

  lasso_ks_p_value[i] <- stats::ks.test(x = y_hat_lasso, y = c(test$y, validation$y), exact = TRUE)$p.value
  lasso_ks_p_value_mean <- mean(lasso_ks_p_value)
  lasso_ks_p_value_sd <- sd(lasso_ks_p_value)
  lasso_ks_stat[i] <- stats::ks.test(x = y_hat_lasso, y = c(test$y, validation$y), exact = TRUE)$statistic
  lasso_ks_stat_mean <- mean(lasso_ks_stat)
  lasso_ks_test <- c(lasso_ks_stat_mean, lasso_ks_p_value_mean)

  y_hat_lasso <- as.numeric(c(rowMeans(lasso_test_pred), rowMeans(lasso_validation_pred)))
  y_hat_lasso_total <- c(y_hat_lasso, y_hat_lasso_total)
  lasso_actual <- c(test$y, validation$y)
  lasso_actual_total <- c(lasso_actual, lasso_actual_total)
  lasso_t_test_t[i] <- as.numeric(t.test(x = y_hat_lasso, y = c(test$y, validation$y), var.equal = TRUE)[1])
  lasso_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_lasso, y = c(test$y, validation$y), var.equal = TRUE)[3])
  lasso_t_test_t_mean <- mean(as.numeric(lasso_t_test_t))
  lasso_t_test_p_value_mean <- mean(as.numeric(lasso_t_test_p_value))
  lasso_t_test_p_value_sd <- sd(as.numeric(lasso_t_test_p_value))
  lasso_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*lasso_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  lasso_lower_95 <- lasso_holdout_RMSE_mean - lasso_margin
  lasso_upper_95 <- lasso_holdout_RMSE_mean + lasso_margin
  lasso_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*lasso_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  lasso_overfitting_lower_95 <- lasso_overfitting_mean - lasso_overfitting_margin
  lasso_overfitting_upper_95 <- lasso_overfitting_mean + lasso_overfitting_margin

  lasso_end <- Sys.time()
  lasso_duration[i] <- lasso_end - lasso_start
  lasso_duration_mean <- mean(lasso_duration)
  lasso_duration_sd <- sd(lasso_duration)


  ####  Model 10 Linear ####
  linear_start <- Sys.time()
  message("Working on Linear")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    linear_train_fit <- e1071::tune.rpart(formula = y ~ ., data = train)
  }
  if(set_seed == "N"){
    linear_train_fit <- e1071::tune.rpart(formula = y ~ ., data = train)
  }
  linear_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = linear_train_fit$best.model, newdata = train))
  linear_train_RMSE_mean <- mean(linear_train_RMSE)
  linear_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = linear_train_fit$best.model, newdata = test))
  linear_test_RMSE_mean <- mean(linear_test_RMSE)
  linear_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = linear_train_fit$best.model, newdata = validation))
  linear_validation_RMSE_mean <- mean(linear_validation_RMSE)
  linear_holdout_RMSE[i] <- mean(c(linear_test_RMSE_mean, linear_validation_RMSE_mean))
  linear_holdout_RMSE_mean <- mean(linear_holdout_RMSE)
  linear_holdout_RMSE_sd_mean <- sd(c(linear_test_RMSE_mean, linear_validation_RMSE_mean))
  linear_train_predict_value <- as.numeric(predict(object = linear_train_fit$best.model, newdata = train))
  linear_test_predict_value <- as.numeric(predict(object = linear_train_fit$best.model, newdata = test))
  linear_validation_predict_value <- as.numeric(predict(object = linear_train_fit$best.model, newdata = validation))
  linear_predict_value_mean <- mean(c(linear_test_predict_value, linear_validation_predict_value))
  linear_sd[i] <- sd(c(linear_test_predict_value, linear_validation_predict_value))
  linear_sd_mean <- mean(linear_sd)
  linear_overfitting[i] <- linear_holdout_RMSE_mean / linear_train_RMSE_mean
  linear_overfitting_mean <- mean(linear_overfitting)
  linear_overfitting_range <- range(linear_overfitting)
  linear_overfitting_sd <- sd(linear_overfitting)
  linear_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(linear_test_predict_value, linear_validation_predict_value))
  linear_bias_mean <- mean(linear_bias)
  linear_bias_sd <- sd(linear_bias)
  y_hat_linear <- c(linear_test_predict_value, linear_validation_predict_value)
  y_hat_linear_total <- c(y_hat_linear, y_hat_linear_total)
  linear_actual <- c(test$y, validation$y)
  linear_actual_total <- c(linear_actual, linear_actual_total)
  linear_t_test_t[i] <- as.numeric(t.test(x = y_hat_linear, y = c(test$y, validation$y), var.equal = TRUE)[1])
  linear_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_linear, y = c(test$y, validation$y), var.equal = TRUE)[3])
  linear_t_test_t_mean <- mean(as.numeric(linear_t_test_t))
  linear_t_test_p_value_mean <- mean(as.numeric(linear_t_test_p_value))
  linear_t_test_p_value_sd <- sd(as.numeric(linear_t_test_p_value))
  linear_ks_p_value[i] <- stats::ks.test(x = y_hat_linear, y = c(test$y, validation$y), exact = TRUE)$p.value
  linear_ks_p_value_mean <- mean(linear_ks_p_value)
  linear_ks_p_value_sd <- sd(linear_ks_p_value)
  linear_ks_stat[i] <- stats::ks.test(x = y_hat_linear, y = c(test$y, validation$y), exact = TRUE)$statistic
  linear_ks_stat_mean <- mean(linear_ks_stat)
  linear_ks_test <- c(linear_ks_stat_mean, linear_ks_p_value_mean)
  linear_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*linear_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  linear_lower_95 <- linear_holdout_RMSE_mean - linear_margin
  linear_upper_95 <- linear_holdout_RMSE_mean + linear_margin
  linear_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*linear_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  linear_overfitting_lower_95 <- linear_overfitting_mean - linear_overfitting_margin
  linear_overfitting_upper_95 <- linear_overfitting_mean + linear_overfitting_margin

  linear_end <- Sys.time()
  linear_duration[i] <- linear_end - linear_start
  linear_duration_mean <- mean(linear_duration)
  linear_duration_sd <- sd(linear_duration)

  ####  Model 11 Neuralnet ####
  neuralnet_start <- Sys.time()
  message("Working on Neuralnet")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    neuralnet_train_fit <- nnet::nnet(train$y ~ ., data = train, size = 0, linout = TRUE, skip = TRUE)
  }
  if(set_seed == "N"){
    neuralnet_train_fit <- nnet::nnet(train$y ~ ., data = train, size = 0, linout = TRUE, skip = TRUE)
  }
  neuralnet_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = neuralnet_train_fit, newdata = train))
  neuralnet_train_RMSE_mean <- mean(neuralnet_train_RMSE)
  neuralnet_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = neuralnet_train_fit, newdata = test))
  neuralnet_test_RMSE_mean <- mean(neuralnet_test_RMSE)
  neuralnet_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = neuralnet_train_fit, newdata = validation))
  neuralnet_validation_RMSE_mean <- mean(neuralnet_validation_RMSE)
  neuralnet_holdout_RMSE[i] <- mean(c(neuralnet_test_RMSE_mean, neuralnet_validation_RMSE_mean))
  neuralnet_holdout_RMSE_mean <- mean(neuralnet_holdout_RMSE)
  neuralnet_holdout_RMSE_sd_mean <- sd(c(neuralnet_test_RMSE_mean, neuralnet_validation_RMSE_mean))
  neuralnet_train_predict_value <- predict(object = neuralnet_train_fit, newdata = train)
  neuralnet_test_predict_value <- predict(object = neuralnet_train_fit, newdata = test)
  neuralnet_validation_predict_value <- predict(object = neuralnet_train_fit, newdata = validation)
  neuralnet_predict_value_mean <- mean(c(neuralnet_test_predict_value, neuralnet_validation_predict_value))
  neuralnet_sd[i] <- sd(c(neuralnet_test_predict_value, neuralnet_validation_predict_value))
  neuralnet_sd_mean <- mean(neuralnet_sd)
  neuralnet_overfitting[i] <- neuralnet_holdout_RMSE_mean / neuralnet_train_RMSE_mean
  neuralnet_overfitting_mean <- mean(neuralnet_overfitting)
  neuralnet_overfitting_range <- range(neuralnet_overfitting)
  neuralnet_overfitting_sd <- sd(neuralnet_overfitting)
  y_hat_neuralnet <- c(neuralnet_test_predict_value, neuralnet_validation_predict_value)
  y_hat_neuralnet_total <- c(y_hat_neuralnet, y_hat_neuralnet_total)
  neuralnet_actual <- c(test$y, validation$y)
  neuralnet_actual_total <- c(neuralnet_actual, neuralnet_actual_total)
  neuralnet_t_test_t[i] <- as.numeric(t.test(x = y_hat_neuralnet, y = c(test$y, validation$y), var.equal = TRUE)[1])
  neuralnet_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_neuralnet, y = c(test$y, validation$y), var.equal = TRUE)[3])
  neuralnet_t_test_t_mean <- mean(as.numeric(neuralnet_t_test_t))
  neuralnet_t_test_p_value_mean <- mean(as.numeric(neuralnet_t_test_p_value))
  neuralnet_t_test_p_value_sd <- sd(as.numeric(neuralnet_t_test_p_value))
  neuralnet_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(neuralnet_test_predict_value, neuralnet_validation_predict_value))
  neuralnet_bias_mean <- mean(neuralnet_bias)
  neuralnet_bias_sd <- sd(neuralnet_bias)
  neuralnet_ks_p_value[i] <- stats::ks.test(x = y_hat_neuralnet, y = c(test$y, validation$y), exact = TRUE)$p.value
  neuralnet_ks_p_value_mean <- mean(neuralnet_ks_p_value)
  neuralnet_ks_p_value_sd <- sd(neuralnet_ks_p_value)
  neuralnet_ks_stat[i] <- stats::ks.test(x = y_hat_neuralnet, y = c(test$y, validation$y), exact = TRUE)$statistic
  neuralnet_ks_stat_mean <- mean(neuralnet_ks_stat)
  neuralnet_ks_test <- c(neuralnet_ks_stat_mean, neuralnet_ks_p_value_mean)
  neuralnet_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*neuralnet_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  neuralnet_lower_95 <- neuralnet_holdout_RMSE_mean - neuralnet_margin
  neuralnet_upper_95 <- neuralnet_holdout_RMSE_mean + neuralnet_margin
  neuralnet_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*neuralnet_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  neuralnet_overfitting_lower_95 <- neuralnet_overfitting_mean - neuralnet_overfitting_margin
  neuralnet_overfitting_upper_95 <- neuralnet_overfitting_mean + neuralnet_overfitting_margin

  neuralnet_end <- Sys.time()
  neuralnet_duration[i] <- neuralnet_end - neuralnet_start
  neuralnet_duration_mean <- mean(neuralnet_duration)
  neuralnet_duration_sd <- sd(neuralnet_duration)

  #### Model 12 Partial Least Squares ####
  pls_start <- Sys.time()
  message("Working on Partial Least Squares")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    pls_train_fit <- pls::plsr(train$y ~ ., data = train)
  }
  if(set_seed == "N"){
    pls_train_fit <- pls::plsr(train$y ~ ., data = train)
  }
  pls_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = pls_train_fit, newdata = train))
  pls_train_RMSE_mean <- mean(pls_train_RMSE)
  pls_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = pls_train_fit, newdata = test))
  pls_test_RMSE_mean <- mean(pls_test_RMSE)
  pls_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = pls_train_fit, newdata = validation))
  pls_validation_RMSE_mean <- mean(pls_validation_RMSE)
  pls_holdout_RMSE[i] <- mean(c(pls_test_RMSE_mean, pls_validation_RMSE_mean))
  pls_holdout_RMSE_mean <- mean(pls_holdout_RMSE)
  pls_holdout_RMSE_sd_mean <- sd(c(pls_test_RMSE_mean, pls_validation_RMSE_mean))
  pls_train_predict_value <- predict(object = pls_train_fit, newdata = train)
  pls_test_predict_value <- predict(object = pls_train_fit, newdata = test)
  pls_validation_predict_value <- predict(object = pls_train_fit, newdata = validation)
  pls_predict_value_mean <- mean(c(pls_test_predict_value, pls_validation_predict_value))
  pls_sd[i] <- sd(c(pls_test_predict_value, pls_validation_predict_value))
  pls_sd_mean <- mean(pls_sd)
  pls_overfitting[i] <- pls_holdout_RMSE_mean / pls_train_RMSE_mean
  pls_overfitting_mean <- mean(pls_overfitting)
  pls_overfitting_range <- range(pls_overfitting)
  pls_overfitting_sd <- sd(pls_overfitting)
  y_hat_pls <- c(pls_test_predict_value[, , 1], pls_validation_predict_value[, , 1])
  y_hat_pls_total <- c(y_hat_pls, y_hat_pls_total)
  pls_actual <- c(test$y, validation$y)
  pls_actual_total <- c(pls_actual, pls_actual_total)
  pls_t_test_t[i] <- as.numeric(t.test(x = y_hat_pls, y = c(test$y, validation$y), var.equal = TRUE)[1])
  pls_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_pls, y = c(test$y, validation$y), var.equal = TRUE)[3])
  pls_t_test_t_mean <- mean(as.numeric(pls_t_test_t))
  pls_t_test_p_value_mean <- mean(as.numeric(pls_t_test_p_value))
  pls_t_test_p_value_sd <- sd(as.numeric(pls_t_test_p_value))
  pls_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(pls_test_predict_value, pls_validation_predict_value))
  pls_bias_mean <- mean(pls_bias)
  pls_bias_sd <- sd(pls_bias)
  pls_ks_p_value[i] <- stats::ks.test(x = y_hat_pls, y = c(test$y, validation$y), exact = TRUE)$p.value
  pls_ks_p_value_mean <- mean(pls_ks_p_value)
  pls_ks_p_value_sd <- sd(pls_ks_p_value)
  pls_ks_stat[i] <- stats::ks.test(x = y_hat_pls, y = c(test$y, validation$y), exact = TRUE)$statistic
  pls_ks_stat_mean <- mean(pls_ks_stat)
  pls_ks_test <- c(pls_ks_stat_mean, pls_ks_p_value_mean)
  pls_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*pls_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  pls_lower_95 <- pls_holdout_RMSE_mean - pls_margin
  pls_upper_95 <- pls_holdout_RMSE_mean + pls_margin
  pls_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*pls_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  pls_overfitting_lower_95 <- pls_overfitting_mean - pls_overfitting_margin
  pls_overfitting_upper_95 <- pls_overfitting_mean + pls_overfitting_margin

  pls_end <- Sys.time()
  pls_duration[i] <- pls_end - pls_start
  pls_duration_mean <- mean(pls_duration)
  pls_duration_sd <- sd(pls_duration)

  ####  Model 13 Principal Components Analysis ####
  pcr_start <- Sys.time()
  message("Working on Principal Components Regression")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    pcr_train_fit <- pls::pcr(train$y ~ ., data = train)
  }
  if(set_seed == "N"){
    pcr_train_fit <- pls::pcr(train$y ~ ., data = train)
  }
  pcr_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = pcr_train_fit, newdata = train))
  pcr_train_RMSE_mean <- mean(pcr_train_RMSE)
  pcr_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = pcr_train_fit, newdata = test))
  pcr_test_RMSE_mean <- mean(pcr_test_RMSE)
  pcr_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = pcr_train_fit, newdata = validation))
  pcr_validation_RMSE_mean <- mean(pcr_validation_RMSE)
  pcr_holdout_RMSE[i] <- mean(pcr_test_RMSE_mean, pcr_validation_RMSE_mean)
  pcr_holdout_RMSE_mean <- mean(pcr_holdout_RMSE)
  pcr_holdout_RMSE_sd_mean <- sd(c(pcr_test_RMSE_mean, pcr_validation_RMSE_mean))
  pcr_train_predict_value <- predict(object = pcr_train_fit, newdata = train)
  pcr_test_predict_value <- predict(object = pcr_train_fit, newdata = test)
  pcr_validation_predict_value <- predict(object = pcr_train_fit, newdata = validation)
  pcr_predict_value_mean <- mean(c(pcr_test_predict_value, pcr_validation_predict_value))
  pcr_sd[i] <- sd(c(pcr_test_predict_value, pcr_validation_predict_value))
  pcr_sd_mean <- mean(pcr_sd)
  pcr_overfitting[i] <- pcr_holdout_RMSE_mean / pcr_train_RMSE_mean
  pcr_overfitting_mean <- mean(pcr_overfitting)
  pcr_overfitting_range <- range(pcr_overfitting)
  pcr_overfitting_sd <- sd(pcr_overfitting)
  y_hat_pcr <- c(pcr_test_predict_value[, , 1], pcr_validation_predict_value[, , 1])
  y_hat_pcr_total <- c(y_hat_pcr, y_hat_pcr_total)
  pcr_actual <- c(test$y, validation$y)
  pcr_actual_total <- c(pcr_actual, pcr_actual_total)
  pcr_t_test_t[i] <- as.numeric(t.test(x = y_hat_pcr, y = c(test$y, validation$y), var.equal = TRUE)[1])
  pcr_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_pcr, y = c(test$y, validation$y), var.equal = TRUE)[3])
  pcr_t_test_t_mean <- mean(as.numeric(pcr_t_test_t))
  pcr_t_test_p_value_mean <- mean(as.numeric(pcr_t_test_p_value))
  pcr_t_test_p_value_sd <- sd(as.numeric(pcr_t_test_p_value))
  pcr_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(pcr_test_predict_value, pcr_validation_predict_value))
  pcr_bias_mean <- mean(pcr_bias)
  pcr_bias_sd <- sd(pcr_bias)
  pcr_ks_p_value[i] <- stats::ks.test(x = y_hat_pcr, y = c(test$y, validation$y), exact = TRUE)$p.value
  pcr_ks_p_value_mean <- mean(pcr_ks_p_value)
  pcr_ks_p_value_sd <- sd(pcr_ks_p_value)
  pcr_ks_stat[i] <- stats::ks.test(x = y_hat_pcr, y = c(test$y, validation$y), exact = TRUE)$statistic
  pcr_ks_stat_mean <- mean(pcr_ks_stat)
  pcr_ks_test <- c(pcr_ks_stat_mean, pcr_ks_p_value_mean)
  pcr_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*pcr_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  pcr_lower_95 <- pcr_holdout_RMSE_mean - pcr_margin
  pcr_upper_95 <- pcr_holdout_RMSE_mean + pcr_margin
  pcr_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*pcr_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  pcr_overfitting_lower_95 <- pcr_overfitting_mean - pcr_overfitting_margin
  pcr_overfitting_upper_95 <- pcr_overfitting_mean + pcr_overfitting_margin

  pcr_end <- Sys.time()
  pcr_duration[i] <- pcr_end - pcr_start
  pcr_duration_mean <- mean(pcr_duration)
  pcr_duration_sd <- sd(pcr_duration)

  ####  Model 14 Ridge Net ####
  ridge_start <- Sys.time()
  message("Working on Ridge")
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  ridge_model <- glmnet(x, y, alpha = 0)
  ridge_cv <- glmnet::cv.glmnet(x, y, alpha = 0)
  best_ridge_lambda <- ridge_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    best_ridge_model <- glmnet(x, y, alpha = 0, lambda = best_ridge_lambda)
  }
  if(set_seed == "N"){
    best_ridge_model <- glmnet(x, y, alpha = 0, lambda = best_ridge_lambda)
  }
  ridge_train_pred <- predict(best_ridge_model, s = best_ridge_lambda, newx = data.matrix(train %>% dplyr::select(-y)))
  ridge_train_RMSE <- Metrics::rmse(actual = y, predicted = ridge_train_pred)
  ridge_train_RMSE_df <- rbind(ridge_train_RMSE_df, ridge_train_RMSE)
  ridge_train_RMSE_mean <- mean(ridge_train_RMSE_df$ridge_train_RMSE[2:nrow(ridge_train_RMSE_df)])
  ## Ridge using the test data set ##

  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  ridge_model <- glmnet(x, y, alpha = 0)
  ridge_cv <- glmnet::cv.glmnet(x, y, alpha = 0)
  best_ridge_lambda <- ridge_cv$lambda.min

  best_ridge_model <- glmnet(x, y, alpha = 0, lambda = best_ridge_lambda)
  ridge_test_pred <- predict(best_ridge_model, s = best_ridge_lambda, newx = data.matrix(test %>% dplyr::select(-y)))
  ridge_test_RMSE <- Metrics::rmse(actual = test$y, predicted = ridge_test_pred)
  ridge_test_RMSE_df <- rbind(ridge_test_RMSE_df, ridge_test_RMSE)
  ridge_test_RMSE_mean <- mean(ridge_test_RMSE_df$ridge_test_RMSE[2:nrow(ridge_test_RMSE_df)])
  ridge_test_predict_value[i] <- round(mean(ridge_test_pred), 4)
  ridge_test_predict_value_mean <- mean(ridge_test_predict_value)
  ridge_test_predict_value_sd[i] <- round(sd(ridge_test_pred), 4)
  ridge_test_predict_value_sd_mean <- mean(ridge_test_predict_value_sd)
  ## Ridge using the validation data set
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  ridge_model <- glmnet(x, y, alpha = 0)
  ridge_cv <- glmnet::cv.glmnet(x, y, alpha = 0)
  best_ridge_lambda <- ridge_cv$lambda.min
  best_ridge_model <- glmnet(x, y, alpha = 0, lambda = best_ridge_lambda)
  ridge_validation_pred <- predict(best_ridge_model, s = best_ridge_lambda, newx = data.matrix(validation %>% dplyr::select(-y)))
  ridge_validation_RMSE <- Metrics::rmse(actual = validation$y, predicted = ridge_validation_pred)
  ridge_validation_RMSE_df <- rbind(ridge_validation_RMSE_df, ridge_validation_RMSE)
  ridge_validation_RMSE_mean <- mean(ridge_validation_RMSE_df$ridge_validation_RMSE[2:nrow(ridge_validation_RMSE_df)])
  ridge_validation_predict_value[i] <- round(mean(ridge_validation_pred), 4)
  ridge_validation_predict_value_mean <- mean(ridge_validation_predict_value)
  ridge_validation_predict_value_sd[i] <- round(sd(ridge_validation_pred), 4)
  ridge_validation_predict_value_sd_mean <- mean(ridge_validation_predict_value_sd)

  ridge_holdout_RMSE <- mean(ridge_test_RMSE_mean, ridge_validation_RMSE_mean)
  ridge_holdout_RMSE_df <- rbind(ridge_holdout_RMSE_df, ridge_holdout_RMSE)
  ridge_holdout_RMSE_mean <- mean(ridge_holdout_RMSE_df$ridge_holdout_RMSE[2:nrow(ridge_holdout_RMSE_df)])

  ridge_holdout_RMSE_sd <- sd(c(ridge_test_RMSE_mean, ridge_validation_RMSE_mean))
  ridge_holdout_RMSE_sd_df <- rbind(ridge_holdout_RMSE_sd, ridge_holdout_RMSE_sd_df)
  ridge_holdout_RMSE_sd_mean <- mean(ridge_holdout_RMSE_sd_df$ridge_holdout_RMSE_sd[2:nrow(ridge_holdout_RMSE_sd_df)])

  ridge_overfitting <- c(ridge_holdout_RMSE / ridge_train_RMSE)
  ridge_overfitting_df <- rbind(ridge_overfitting_df, ridge_overfitting)
  ridge_overfitting_mean <- mean(ridge_overfitting_df$ridge_overfitting[2:nrow(ridge_overfitting_df)])
  ridge_overfitting_sd <- sd(ridge_overfitting_df$ridge_overfitting)

  ridge_test_predict_value_mean <- mean(c(ridge_test_predict_value_mean, ridge_validation_predict_value_mean))

  ridge_sd[i] <- mean(c(ridge_test_predict_value_sd_mean, ridge_validation_predict_value_sd_mean))
  ridge_sd_mean <- mean(ridge_sd)

  y_hat_ridge <- as.numeric(c(rowMeans(ridge_test_pred), rowMeans(ridge_validation_pred)))
  y_hat_ridge_total <- c(y_hat_ridge, y_hat_ridge_total)
  ridge_actual <- c(test$y, validation$y)
  ridge_actual_total <- c(ridge_actual, ridge_actual_total)
  ridge_t_test_t[i] <- as.numeric(t.test(x = y_hat_ridge, y = c(test$y, validation$y), var.equal = TRUE)[1])
  ridge_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_ridge, y = c(test$y, validation$y), var.equal = TRUE)[3])
  ridge_t_test_t_mean <- mean(as.numeric(ridge_t_test_t))
  ridge_t_test_p_value_mean <- mean(as.numeric(ridge_t_test_p_value))
  ridge_t_test_p_value_sd <- sd(as.numeric(ridge_t_test_p_value))

  ridge_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = as.numeric(c(ridge_test_pred, ridge_validation_pred)))
  ridge_bias_mean <- mean(ridge_bias)
  ridge_bias_sd <- sd(ridge_bias)
  ridge_ks_p_value[i] <- stats::ks.test(x = y_hat_ridge, y = c(test$y, validation$y), exact = TRUE)$p.value
  ridge_ks_p_value_mean <- mean(ridge_ks_p_value)
  ridge_ks_p_value_sd <- sd(ridge_ks_p_value)
  ridge_ks_stat[i] <- stats::ks.test(x = y_hat_ridge, y = c(test$y, validation$y), exact = TRUE)$statistic
  ridge_ks_stat_mean <- mean(ridge_ks_stat)
  ridge_ks_test <- c(ridge_ks_stat_mean, ridge_ks_p_value_mean)
  ridge_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ridge_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ridge_lower_95 <- ridge_holdout_RMSE_mean - ridge_margin
  ridge_upper_95 <- ridge_holdout_RMSE_mean + ridge_margin
  ridge_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ridge_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ridge_overfitting_lower_95 <- ridge_overfitting_mean - ridge_overfitting_margin
  ridge_overfitting_upper_95 <- ridge_overfitting_mean + ridge_overfitting_margin

  ridge_end <- Sys.time()
  ridge_duration[i] <- ridge_end - ridge_start
  ridge_duration_mean <- mean(ridge_duration)
  ridge_duration_sd <- sd(ridge_duration)

  ####  Model 15 Rpart (also known as cart) ####
  rpart_start <- Sys.time()
  message("Working on RPart")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    rpart_train_fit <- rpart::rpart(train$y ~ ., data = train)
  }
  if(set_seed == "N"){
    rpart_train_fit <- rpart::rpart(train$y ~ ., data = train)
  }
  rpart_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = rpart_train_fit, newdata = train))
  rpart_train_RMSE_mean <- mean(rpart_train_RMSE)
  rpart_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = rpart_train_fit, newdata = test))
  rpart_test_RMSE_mean <- mean(rpart_test_RMSE)
  rpart_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = rpart_train_fit, newdata = validation))
  rpart_validation_RMSE_mean <- mean(rpart_validation_RMSE)
  rpart_holdout_RMSE[i] <- mean(c(rpart_test_RMSE_mean, rpart_validation_RMSE_mean))
  rpart_holdout_RMSE_mean <- mean(rpart_holdout_RMSE)
  rpart_holdout_RMSE_sd_mean <- sd(c(rpart_test_RMSE_mean, rpart_validation_RMSE_mean))
  rpart_train_predict_value <- as.numeric(predict(object = rpart::rpart(y ~ ., data = train), newdata = train))
  rpart_test_predict_value <- as.numeric(predict(object = rpart::rpart(y ~ ., data = train), newdata = test))
  rpart_validation_predict_value <- as.numeric(predict(object = rpart::rpart(y ~ ., data = train), newdata = validation))
  rpart_predict_value_mean <- mean(c(rpart_test_predict_value, rpart_validation_predict_value))
  rpart_sd[i] <- sd(rpart_test_predict_value)
  rpart_sd_mean <- mean(rpart_sd)
  rpart_overfitting[i] <- rpart_holdout_RMSE_mean / rpart_train_RMSE_mean
  rpart_overfitting_mean <- mean(rpart_overfitting)
  rpart_overfitting_range <- range(rpart_overfitting)
  rpart_overfitting_sd <- sd(rpart_overfitting)
  y_hat_rpart <- c(rpart_test_predict_value, rpart_validation_predict_value)
  y_hat_rpart_total <- c(y_hat_rpart, y_hat_rpart_total)
  rpart_actual <- c(test$y, validation$y)
  rpart_actual_total <- c(rpart_actual, rpart_actual_total)
  rpart_t_test_t[i] <- as.numeric(t.test(x = y_hat_rpart, y = c(test$y, validation$y), var.equal = TRUE)[1])
  rpart_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_rpart, y = c(test$y, validation$y), var.equal = TRUE)[3])
  rpart_t_test_t_mean <- mean(as.numeric(rpart_t_test_t))
  rpart_t_test_p_value_mean <- mean(as.numeric(rpart_t_test_p_value))
  rpart_t_test_p_value_sd <- sd(as.numeric(rpart_t_test_p_value))
  rpart_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(rpart_test_predict_value, rpart_validation_predict_value))
  rpart_bias_mean <- mean(rpart_bias)
  rpart_bias_sd <- sd(rpart_bias)
  rpart_ks_p_value[i] <- stats::ks.test(x = y_hat_rpart, y = c(test$y, validation$y), exact = TRUE)$p.value
  rpart_ks_p_value_mean <- mean(rpart_ks_p_value)
  rpart_ks_p_value_sd <- sd(rpart_ks_p_value)
  rpart_ks_stat[i] <- stats::ks.test(x = y_hat_rpart, y = c(test$y, validation$y), exact = TRUE)$statistic
  rpart_ks_stat_mean <- mean(rpart_ks_stat)
  rpart_ks_test <- c(rpart_ks_stat_mean, rpart_ks_p_value_mean)
  rpart_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*rpart_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  rpart_lower_95 <- rpart_holdout_RMSE_mean - rpart_margin
  rpart_upper_95 <- rpart_holdout_RMSE_mean + rpart_margin
  rpart_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*rpart_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  rpart_overfitting_lower_95 <- rpart_overfitting_mean - rpart_overfitting_margin
  rpart_overfitting_upper_95 <- rpart_overfitting_mean + rpart_overfitting_margin

  rpart_end <- Sys.time()
  rpart_duration[i] <- rpart_end - rpart_start
  rpart_duration_mean <- mean(rpart_duration)
  rpart_duration_sd <- sd(rpart_duration)


  ####  Model 16 Support Vector Machines ####
  svm_start <- Sys.time()
  message("Working on Support Vector Machines")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    svm_train_fit <- e1071::tune.svm(x = train, y = train$y, data = train)
  }
  if(set_seed == "N"){
    svm_train_fit <- e1071::tune.svm(x = train, y = train$y, data = train)
  }
  svm_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = svm_train_fit$best.model, newdata = train))
  svm_train_RMSE_mean <- mean(svm_train_RMSE)
  svm_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = svm_train_fit$best.model, newdata = test))
  svm_test_RMSE_mean <- mean(svm_test_RMSE)
  svm_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = svm_train_fit$best.model, newdata = validation))
  svm_validation_RMSE_mean <- mean(svm_validation_RMSE)
  svm_holdout_RMSE[i] <- mean(c(svm_test_RMSE_mean, svm_validation_RMSE_mean))
  svm_holdout_RMSE_mean <- mean(svm_holdout_RMSE)
  svm_holdout_RMSE_sd_mean <- sd(svm_validation_RMSE)
  svm_train_predict_value <- as.numeric(predict(object = svm_train_fit$best.model, newdata = train))
  svm_test_predict_value <- as.numeric(predict(object = svm_train_fit$best.model, newdata = test))
  svm_validation_predict_value <- as.numeric(predict(object = svm_train_fit$best.model, newdata = validation))
  svm_predict_value_mean <- mean(c(svm_test_predict_value, svm_validation_predict_value))
  svm_sd[i] <- sd(c(svm_test_predict_value, svm_validation_predict_value))
  svm_sd_mean <- mean(svm_sd)
  svm_overfitting[i] <- svm_holdout_RMSE_mean / svm_train_RMSE_mean
  svm_overfitting_mean <- mean(svm_overfitting)
  svm_overfitting_range <- range(svm_overfitting)
  svm_overfitting_sd <- sd(svm_overfitting)
  y_hat_svm <- c(svm_test_predict_value, svm_validation_predict_value)
  y_hat_svm_total <- c(y_hat_svm, y_hat_svm_total)
  svm_actual <- c(test$y, validation$y)
  svm_actual_total <- c(svm_actual, svm_actual_total)
  svm_t_test_t[i] <- as.numeric(t.test(x = y_hat_svm, y = c(test$y, validation$y), var.equal = TRUE)[1])
  svm_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_svm, y = c(test$y, validation$y), var.equal = TRUE)[3])
  svm_t_test_t_mean <- mean(as.numeric(svm_t_test_t))
  svm_t_test_p_value_mean <- mean(as.numeric(svm_t_test_p_value))
  svm_t_test_p_value_sd <- sd(as.numeric(svm_t_test_p_value))
  svm_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(svm_test_predict_value, svm_validation_predict_value))
  svm_bias_mean <- mean(svm_bias)
  svm_bias_sd <- sd(svm_bias)
  svm_ks_p_value[i] <- stats::ks.test(x = y_hat_svm, y = c(test$y, validation$y), exact = TRUE)$p.value
  svm_ks_p_value_mean <- mean(svm_ks_p_value)
  svm_ks_p_value_sd <- sd(svm_ks_p_value)
  svm_ks_stat[i] <- stats::ks.test(x = y_hat_svm, y = c(test$y, validation$y), exact = TRUE)$statistic
  svm_ks_stat_mean <- mean(svm_ks_stat)
  svm_ks_test <- c(svm_ks_stat_mean, svm_ks_p_value_mean)
  svm_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*svm_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  svm_lower_95 <- svm_holdout_RMSE_mean - svm_margin
  svm_upper_95 <- svm_holdout_RMSE_mean + svm_margin
  svm_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*svm_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  svm_overfitting_lower_95 <- svm_overfitting_mean - svm_overfitting_margin
  svm_overfitting_upper_95 <- svm_overfitting_mean + svm_overfitting_margin

  svm_end <- Sys.time()
  svm_duration[i] <- svm_end - svm_start
  svm_duration_mean <- mean(svm_duration)
  svm_duration_sd <- sd(svm_duration)


  ####  Model 17 Trees ####
  tree_start <- Sys.time()
  message("Working on Trees")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    tree_train_fit <- tree::tree(train$y ~ ., data = train)
  }
  if(set_seed == "N"){
    tree_train_fit <- tree::tree(train$y ~ ., data = train)
  }
  tree_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = tree_train_fit, newdata = train))
  tree_train_RMSE_mean <- mean(tree_train_RMSE)
  tree_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = tree_train_fit, newdata = test))
  tree_test_RMSE_mean <- mean(tree_test_RMSE)
  tree_validation_RMSE[i] <- Metrics::rmse(actual = validation$y, predicted = predict(object = tree_train_fit, newdata = validation))
  tree_validation_RMSE_mean <- mean(tree_validation_RMSE)
  tree_holdout_RMSE[i] <- mean(c(tree_test_RMSE_mean, tree_validation_RMSE_mean))
  tree_holdout_RMSE_mean <- mean(tree_holdout_RMSE)
  tree_holdout_RMSE_sd_mean <- sd(c(tree_test_RMSE_mean, tree_validation_RMSE_mean))
  tree_train_predict_value <- as.numeric(predict(object = tree::tree(y ~ ., data = train), newdata = train))
  tree_test_predict_value <- as.numeric(predict(object = tree::tree(y ~ ., data = train), newdata = test))
  tree_validation_predict_value <- as.numeric(predict(object = tree::tree(y ~ ., data = train), newdata = validation))
  tree_predict_value_mean <- mean(c(tree_test_predict_value, tree_validation_predict_value))
  tree_sd[i] <- sd(tree_test_predict_value)
  tree_sd_mean <- mean(tree_sd)
  tree_overfitting[i] <- tree_holdout_RMSE_mean / tree_train_RMSE_mean
  tree_overfitting_mean <- mean(tree_overfitting)
  tree_overfitting_range <- range(tree_overfitting)
  tree_overfitting_sd <- sd(tree_overfitting)
  y_hat_tree <- c(tree_test_predict_value, tree_validation_predict_value)
  y_hat_tree_total <- c(y_hat_tree, y_hat_tree_total)
  tree_actual <- c(test$y, validation$y)
  tree_actual_total <- c(tree_actual, tree_actual_total)
  tree_t_test_t[i] <- as.numeric(t.test(x = y_hat_tree, y = c(test$y, validation$y), var.equal = TRUE)[1])
  tree_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_tree, y = c(test$y, validation$y), var.equal = TRUE)[3])
  tree_t_test_t_mean <- mean(as.numeric(tree_t_test_t))
  tree_t_test_p_value_mean <- mean(as.numeric(tree_t_test_p_value))
  tree_t_test_p_value_sd <- sd(as.numeric(tree_t_test_p_value))
  tree_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = c(tree_test_predict_value, tree_validation_predict_value))
  tree_bias_mean <- mean(tree_bias)
  tree_bias_sd <- sd(tree_bias)
  tree_ks_p_value[i] <- stats::ks.test(x = y_hat_tree, y = c(test$y, validation$y), exact = TRUE)$p.value
  tree_ks_p_value_mean <- mean(tree_ks_p_value)
  tree_ks_p_value_sd <- sd(tree_ks_p_value)
  tree_ks_stat[i] <- stats::ks.test(x = y_hat_tree, y = c(test$y, validation$y), exact = TRUE)$statistic
  tree_ks_stat_mean <- mean(tree_ks_stat)
  tree_ks_test <- c(tree_ks_stat_mean, tree_ks_p_value_mean)
  tree_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*tree_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  tree_lower_95 <- tree_holdout_RMSE_mean - tree_margin
  tree_upper_95 <- tree_holdout_RMSE_mean + tree_margin
  tree_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*tree_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  tree_overfitting_lower_95 <- tree_overfitting_mean - tree_overfitting_margin
  tree_overfitting_upper_95 <- tree_overfitting_mean + tree_overfitting_margin

  tree_end <- Sys.time()
  tree_duration[i] <- tree_end - tree_start
  tree_duration_mean <- mean(tree_duration)
  tree_duration_sd <- sd(tree_duration)

  ####  Model 18 XGBoost ####
  xgb_start <- Sys.time()
  message("Working on XGBoost")
  train_x <- data.matrix(train[, -ncol(train)])
  train_y <- train[, ncol(train)]

  # define predictor and response variables in test set
  test_x <- data.matrix(test[, -ncol(test)])
  test_y <- test[, ncol(test)]

  # define predictor and response variables in validation set
  validation_x <- data.matrix(validation[, -ncol(validation)])
  validation_y <- validation[, ncol(validation)]

  # define final train, test and validation sets
  xgb_train <- xgboost::xgb.DMatrix(data = train_x, label = train_y)
  xgb_test <- xgboost::xgb.DMatrix(data = test_x, label = test_y)
  xgb_validation <- xgboost::xgb.DMatrix(data = validation_x, label = validation_y)

  # define watchlist
  watchlist <- list(train = xgb_train, validation = xgb_validation)
  watchlist_test <- list(train = xgb_train, test = xgb_test)
  watchlist_validation <- list(train = xgb_train, validation = xgb_validation)

  # fit XGBoost model and display training and validation data at each round

  if(set_seed == "Y"){
    set.seed(seed = seed)
    xgb_model <- xgboost::xgb.train(data = xgb_train, params = xgboost::xgb.params(max_depth = 3), nrounds = 70)
    xgb_model_validation <- xgboost::xgb.train(data = xgb_train, params = xgboost::xgb.params(max_depth = 3), nrounds = 70)
  }
  if(set_seed == "N"){
    xgb_model <- xgboost::xgb.train(data = xgb_train, params = xgboost::xgb.params(max_depth = 3), nrounds = 70)
    xgb_model_validation <- xgboost::xgb.train(data = xgb_train, params = xgboost::xgb.params(max_depth = 3), nrounds = 70)
  }
  xgboost_min <- which.min(xgb_model$evaluation_log$validation_rmse)
  xgboost_validation.min <- which.min(xgb_model$evaluation_log$validation_rmse)

  xgb_train_RMSE[i] <- Metrics::rmse(actual = train$y, predicted = predict(object = xgb_model, newdata = train_x))
  xgb_train_RMSE_mean <- mean(xgb_train_RMSE)
  xgb_test_RMSE[i] <- Metrics::rmse(actual = test$y, predicted = predict(object = xgb_model, newdata = test_x))
  xgb_test_RMSE_mean <- mean(xgb_test_RMSE)
  xgb_validation_RMSE[i] <- round(Metrics::rmse(actual = validation$y, predicted = predict(object = xgb_model, newdata = validation_x)), 4)
  xgb_validation_RMSE_mean <- mean(xgb_validation_RMSE)

  xgb_holdout_RMSE[i] <- mean(xgb_test_RMSE_mean, xgb_validation_RMSE_mean)
  xgb_holdout_RMSE_mean <- mean(xgb_holdout_RMSE)
  xgb_holdout_RMSE_sd_mean <- sd(c(xgb_test_RMSE_mean, xgb_validation_RMSE_mean))

  y_hat_xgb <- c(predict(object = xgb_model, newdata = test_x), predict(object = xgb_model, newdata = validation_x))
  y_hat_xgb_total <- c(y_hat_xgb, y_hat_xgb_total)
  xgb_actual <- c(test$y, validation$y)
  xgb_actual_total <- c(xgb_actual, xgb_actual_total)
  xgb_t_test_t[i] <- as.numeric(t.test(x = y_hat_xgb, y = c(test$y, validation$y), var.equal = TRUE)[1])
  xgb_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_xgb, y = c(test$y, validation$y), var.equal = TRUE)[3])
  xgb_t_test_t_mean <- mean(as.numeric(xgb_t_test_t))
  xgb_t_test_p_value_mean <- mean(as.numeric(xgb_t_test_p_value))
  xgb_t_test_p_value_sd <- sd(as.numeric(xgb_t_test_p_value))
  xgb_predict_value_mean <- mean(y_hat_xgb)
  xgb_sd_mean <- sd(y_hat_xgb)
  xgb_overfitting[i] <- xgb_holdout_RMSE_mean / xgb_train_RMSE_mean
  xgb_overfitting_mean <- mean(xgb_overfitting)
  xgb_overfitting_range <- range(xgb_overfitting)
  xgb_overfitting_sd <- sd(xgb_overfitting)

  xgb_bias[i] <- Metrics::bias(actual = c(test$y, validation$y), predicted = y_hat_xgb)
  xgb_bias_mean <- mean(xgb_bias)
  xgb_bias_sd <- sd(xgb_bias)
  xgb_ks_p_value[i] <- stats::ks.test(x = y_hat_xgb, y = c(test$y, validation$y), exact = TRUE)$p.value
  xgb_ks_p_value_mean <- mean(xgb_ks_p_value)
  xgb_ks_p_value_sd <- sd(xgb_ks_p_value)
  xgb_ks_stat[i] <- stats::ks.test(x = y_hat_xgb, y = c(test$y, validation$y), exact = TRUE)$statistic
  xgb_ks_stat_mean <- mean(xgb_ks_stat)
  xgb_ks_test <- c(xgb_ks_stat_mean, xgb_ks_p_value_mean)
  xgb_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*xgb_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  xgb_lower_95 <- xgb_holdout_RMSE_mean - xgb_margin
  xgb_upper_95 <- xgb_holdout_RMSE_mean + xgb_margin
  xgb_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*xgb_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  xgb_overfitting_lower_95 <- xgb_overfitting_mean - xgb_overfitting_margin
  xgb_overfitting_upper_95 <- xgb_overfitting_mean + xgb_overfitting_margin

  xgb_end <- Sys.time()
  xgb_duration[i] <- xgb_end - xgb_start
  xgb_duration_mean <- mean(xgb_duration)
  xgb_duration_sd <- sd(xgb_duration)


  #### Begin weighted ensembles here ####

  ensemble <- data.frame(
    "Bagging" = y_hat_bagging * 1 / bagging_holdout_RMSE_mean,
    "BayesGLM" = y_hat_bayesglm * 1 / bayesglm_holdout_RMSE_mean,
    "BayesRNN" = y_hat_bayesrnn * 1 / bayesrnn_holdout_RMSE_mean,
    "Cubist" = y_hat_cubist * 1 / cubist_holdout_RMSE_mean,
    "Earth" = y_hat_earth * 1 / earth_holdout_RMSE_mean,
    "Elastic" = y_hat_elastic *1 / elastic_holdout_RMSE,
    "GAM" = y_hat_gam * 1 / gam_holdout_RMSE_mean,
    "GBM" = y_hat_gb * 1 / gb_holdout_RMSE_mean,
    "Lasso" = y_hat_lasso *1 / lasso_holdout_RMSE_mean,
    "Linear" = y_hat_linear * 1 / linear_holdout_RMSE_mean,
    "Neuralnet" = y_hat_neuralnet *1 / neuralnet_holdout_RMSE_mean,
    "PCR" = y_hat_pcr * 1 / pcr_holdout_RMSE_mean,
    "PLS" = y_hat_pls * 1 / pls_holdout_RMSE_mean,
    "Ridge" = y_hat_ridge *1 / ridge_holdout_RMSE_mean,
    "Rpart" = y_hat_rpart * 1 / rpart_holdout_RMSE_mean,
    "SVM" = y_hat_svm * 1 / svm_holdout_RMSE_mean,
    "Tree" = y_hat_tree * 1 / tree_holdout_RMSE_mean,
    "XGBoost" = y_hat_xgb * 1 / xgb_holdout_RMSE_mean
  )

  ensemble$y_ensemble <- c(test$y, validation$y)
  y_ensemble <- c(test$y, validation$y)

  if(sum(is.na(ensemble > 0))){
    ensemble <- ensemble[stats::complete.cases(ensemble), ] # Removes rows with NAs
  }

  ensemble <- Filter(function(x) stats::var(x) != 0, ensemble) # Removes columns with no variation

  tmp <- stats::cor(ensemble) # This section removes strongly correlated (>0.995) rows and columns from the ensemble
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  data_new <- ensemble[, !apply(tmp, 2, function(x) any(abs(x) > remove_ensemble_correlations_greater_than, na.rm = TRUE))]
  ensemble <- data_new # new ensemble without strongly correlated predictors

  if(ensemble_reduction_method == 1){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "exhaustive")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(BIC == min(BIC)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 2){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "forward")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(BIC == min(BIC)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 3){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "backward")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(BIC == min(BIC)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 4){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "seqrep")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(BIC == min(BIC)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 5){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "exhaustive")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(mallows_cp == min(mallows_cp)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }


  if(ensemble_reduction_method == 6){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "forward")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(mallows_cp == min(mallows_cp)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 7){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "backward")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(mallows_cp == min(mallows_cp)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  if(ensemble_reduction_method == 8){
    ensemble_regsubsets <- leaps::regsubsets(y_ensemble ~ ., data = ensemble, method = "seqrep")

    cols_sel <- ensemble_regsubsets |>
      broom::tidy() |>
      filter(mallows_cp == min(mallows_cp)) |>
      dplyr::select(dplyr::where(~.x == TRUE)) |>
      colnames()

    ensemble <- dplyr::select(ensemble, dplyr::any_of(cols_sel))
    ensemble <- cbind(ensemble, y_ensemble)
  }

  message(noquote(""))
  message("Working on the Ensembles section")
  message(noquote(""))

  head_ensemble <- head(ensemble)
  head_ensemble <- # Head of the ensemble
    reactable::reactable(round(head_ensemble, 4),
                         searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                         striped = TRUE, highlight = TRUE, resizable = TRUE
    )

  htmltools::div(class = "table",
                 htmltools::div(class = "title", "head_ensemble")
  )

  head_ensemble <- htmlwidgets::prependContent(head_ensemble, htmltools::h2(class = "title", "Head of the ensemble"))

  ensemble_correlation <- cor(ensemble)
  ensemble_correlation <- reactable::reactable(round(cor(ensemble), 4),
                                               searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                               striped = TRUE, highlight = TRUE, resizable = TRUE
  )

  htmltools::div(class = "table",
                 htmltools::div(class = "title", "ensemble_correlation")
  )

  ensemble_correlation <- htmlwidgets::prependContent(ensemble_correlation, htmltools::h2(class = "title", "Ensemble correlation"))


  #### Split the ensemble data into train (60%), test (20%) and validation (20%) ####
  if(set_seed == "N"){
    ensemble_idx <- sample(seq(1, 3), size = nrow(ensemble), replace = TRUE, prob = c(train_amount, test_amount, validation_amount))
    ensemble_train <- ensemble[ensemble_idx == 1, ]
    ensemble_test <- ensemble[ensemble_idx == 2, ]
    ensemble_validation <- ensemble[ensemble_idx == 3, ]
  }

  if(set_seed == "Y"){
    ensemble_train <- ensemble[1:round(train_amount*nrow(ensemble)), ]
    ensemble_test <- ensemble[round(train_amount*nrow(ensemble)) +1:round(test_amount*nrow(ensemble)), ]
    ensemble_validation <- ensemble[(nrow(ensemble_train) + nrow(ensemble_test) +1) : nrow(ensemble), ]
  }

  #### Model 19: Ensemble Using Bagging tuned ####
  ensemble_bagging_start <- Sys.time()
  message("Working on Ensemble Bagging")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_bagging_train_fit <- ipred::bagging(formula = y_ensemble ~ ., data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_bagging_train_fit <- ipred::bagging(formula = y_ensemble ~ ., data = ensemble_train)
  }
  ensemble_bagging_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_bagging_train_fit,
    newdata = ensemble_train
  ))
  ensemble_bagging_train_RMSE_mean <- mean(ensemble_bagging_train_RMSE)
  ensemble_bagging_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_bagging_train_fit,
    newdata = ensemble_test
  ))
  ensemble_bagging_test_RMSE_mean <- mean(ensemble_bagging_test_RMSE)
  ensemble_bagging_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_bagging_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_bagging_validation_RMSE_mean <- mean(ensemble_bagging_validation_RMSE)
  ensemble_bagging_holdout_RMSE[i] <- mean(c(ensemble_bagging_test_RMSE_mean, ensemble_bagging_validation_RMSE_mean))
  ensemble_bagging_holdout_RMSE_mean <- mean(ensemble_bagging_holdout_RMSE)
  ensemble_bagging_holdout_RMSE_sd_mean <- sd(c(ensemble_bagging_test_RMSE_mean, ensemble_bagging_validation_RMSE_mean))
  ensemble_bagging_train_predict_value <- as.numeric(predict(object = ensemble_bagging_train_fit, newdata = ensemble_train))
  ensemble_bagging_test_predict_value <- as.numeric(predict(object = ensemble_bagging_train_fit, newdata = ensemble_test))
  ensemble_bagging_validation_predict_value <- as.numeric(predict(object = ensemble_bagging_train_fit, newdata = ensemble_validation))
  ensemble_bagging_predict_value_mean <- mean(c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value))
  ensemble_bagging_sd[i] <- sd(c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value))
  ensemble_bagging_sd_mean <- mean(ensemble_bagging_sd)
  ensemble_bagging_overfitting[i] <- ensemble_bagging_holdout_RMSE_mean / ensemble_bagging_train_RMSE_mean
  ensemble_bagging_overfitting_mean <- mean(ensemble_bagging_overfitting)
  ensemble_bagging_overfitting_range <- range(ensemble_bagging_overfitting)
  ensemble_bagging_overfitting_sd <- sd(ensemble_bagging_overfitting)
  ensemble_y_hat_bagging <- c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value)
  ensemble_y_hat_bagging_total <- c(ensemble_y_hat_bagging, ensemble_y_hat_bagging_total)
  ensemble_bagging_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_bagging_actual_total <- c(ensemble_bagging_actual, ensemble_bagging_actual_total)
  ensemble_bagging_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_bagging, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_bagging_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_bagging, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_bagging_t_test_t_mean <- mean(as.numeric(ensemble_bagging_t_test_t))
  ensemble_bagging_t_test_p_value_mean <- mean(as.numeric(ensemble_bagging_t_test_p_value))
  ensemble_bagging_t_test_p_value_sd <- sd(as.numeric(ensemble_bagging_t_test_p_value))
  ensemble_bagging_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bagging_test_predict_value, ensemble_bagging_validation_predict_value))
  ensemble_bagging_bias_mean <- mean(ensemble_bagging_bias)
  ensemble_bagging_bias_sd <- sd(ensemble_bagging_bias)
  ensemble_bagging_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_bagging, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_bagging_ks_p_value_mean <- mean(ensemble_bagging_ks_p_value)
  ensemble_bagging_ks_p_value_sd <- sd(ensemble_bagging_ks_p_value)
  ensemble_bagging_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_bagging, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_bagging_ks_stat_mean <- mean(ensemble_bagging_ks_stat)
  ensemble_bagging_ks_test <- c(ensemble_bagging_ks_stat_mean, ensemble_bagging_ks_p_value_mean)
  ensemble_bagging_margin <- stats::qt(0.975, df = (nrow(ensemble_test) + nrow(ensemble_validation)) -1)*ensemble_bagging_holdout_RMSE_mean/sqrt(nrow(ensemble_test) + nrow(ensemble_validation))
  ensemble_bagging_lower_95 <- ensemble_bagging_holdout_RMSE_mean - ensemble_bagging_margin
  ensemble_bagging_upper_95 <- ensemble_bagging_holdout_RMSE_mean + ensemble_bagging_margin
  ensemble_bagging_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_bagging_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_bagging_overfitting_lower_95 <- ensemble_bagging_overfitting_mean - ensemble_bagging_overfitting_margin
  ensemble_bagging_overfitting_upper_95 <- ensemble_bagging_overfitting_mean + ensemble_bagging_overfitting_margin

  ensemble_bagging_end <- Sys.time()
  ensemble_bagging_duration[i] <- ensemble_bagging_end - ensemble_bagging_start
  ensemble_bagging_duration_mean <- mean(ensemble_bagging_duration)
  ensemble_bagging_duration_sd <- sd(ensemble_bagging_duration)

  #### Model 20: Ensemble Using BayesGLM ####
  ensemble_bayesglm_start <- Sys.time()
  message("Working on Ensemble BayesGLM")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_bayesglm_train_fit <- arm::bayesglm(y_ensemble ~ ., data = ensemble_train, family = gaussian(link = "identity"))
  }
  if(set_seed == "N"){
    ensemble_bayesglm_train_fit <- arm::bayesglm(y_ensemble ~ ., data = ensemble_train, family = gaussian(link = "identity"))
  }
  ensemble_bayesglm_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_bayesglm_train_fit,
    newdata = ensemble_train
  ))
  ensemble_bayesglm_train_RMSE_mean <- mean(ensemble_bayesglm_train_RMSE)
  ensemble_bayesglm_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_bayesglm_train_fit,
    newdata = ensemble_test
  ))
  ensemble_bayesglm_test_RMSE_mean <- mean(ensemble_bayesglm_test_RMSE)
  ensemble_bayesglm_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_bayesglm_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_bayesglm_validation_RMSE_mean <- mean(ensemble_bayesglm_validation_RMSE)
  ensemble_bayesglm_holdout_RMSE[i] <- mean(c(ensemble_bayesglm_test_RMSE_mean, ensemble_bayesglm_validation_RMSE_mean))
  ensemble_bayesglm_holdout_RMSE_mean <- mean(ensemble_bayesglm_holdout_RMSE)
  ensemble_bayesglm_holdout_RMSE_sd_mean <- sd(c(ensemble_bayesglm_test_RMSE_mean, ensemble_bayesglm_validation_RMSE_mean))
  ensemble_bayesglm_train_predict_value <- as.numeric(predict(object = ensemble_bayesglm_train_fit, newdata = ensemble_train))
  ensemble_bayesglm_test_predict_value <- as.numeric(predict(object = ensemble_bayesglm_train_fit, newdata = ensemble_test))
  ensemble_bayesglm_validation_predict_value <- as.numeric(predict(object = ensemble_bayesglm_train_fit, newdata = ensemble_validation))
  ensemble_bayesglm_predict_value_mean <- mean(c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value))
  ensemble_bayesglm_sd[i] <- sd(c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value))
  ensemble_bayesglm_sd_mean <- mean(ensemble_bayesglm_sd)
  ensemble_bayesglm_overfitting[i] <- ensemble_bayesglm_holdout_RMSE_mean / ensemble_bayesglm_train_RMSE_mean
  ensemble_bayesglm_overfitting_mean <- mean(ensemble_bayesglm_overfitting)
  ensemble_bayesglm_overfitting_range <- range(ensemble_bayesglm_overfitting)
  ensemble_bayesglm_overfitting_sd <- sd(ensemble_bayesglm_overfitting)
  ensemble_y_hat_bayesglm <- c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value)
  ensemble_y_hat_bayesglm_total <- c(ensemble_y_hat_bayesglm, ensemble_y_hat_bayesglm_total)
  ensemble_bayesglm_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_bayesglm_actual_total <- c(ensemble_bayesglm_actual, ensemble_bayesglm_actual_total)
  ensemble_bayesglm_t_test_t[i] <- as.numeric(t.test(x =  ensemble_y_hat_bayesglm, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_bayesglm_t_test_p_value[i] <- as.numeric(t.test(x =  ensemble_y_hat_bayesglm, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_bayesglm_t_test_t_mean <- mean(as.numeric(ensemble_bayesglm_t_test_t))
  ensemble_bayesglm_t_test_p_value_mean <- mean(as.numeric(ensemble_bayesglm_t_test_p_value))
  ensemble_bayesglm_t_test_p_value_sd <- sd(as.numeric(ensemble_bayesglm_t_test_p_value))
  ensemble_bayesglm_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bayesglm_test_predict_value, ensemble_bayesglm_validation_predict_value))
  ensemble_bayesglm_bias_mean <- mean(ensemble_bayesglm_bias)
  ensemble_bayesglm_bias_sd <- sd(ensemble_bayesglm_bias)
  ensemble_bayesglm_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_bayesglm , y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_bayesglm_ks_p_value_mean <- mean(ensemble_bayesglm_ks_p_value)
  ensemble_bayesglm_ks_p_value_sd <- sd(ensemble_bayesglm_ks_p_value)
  ensemble_bayesglm_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_bayesglm , y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_bayesglm_ks_stat_mean <- mean(ensemble_bayesglm_ks_stat)
  ensemble_bayesglm_ks_test <- c(ensemble_bayesglm_ks_stat_mean, ensemble_bayesglm_ks_p_value_mean)
  ensemble_bayesglm_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_bayesglm_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_bayesglm_lower_95 <- ensemble_bayesglm_holdout_RMSE_mean - ensemble_bayesglm_margin
  ensemble_bayesglm_upper_95 <- ensemble_bayesglm_holdout_RMSE_mean + ensemble_bayesglm_margin
  ensemble_bayesglm_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_bayesglm_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_bayesglm_overfitting_lower_95 <- ensemble_bayesglm_overfitting_mean - ensemble_bayesglm_overfitting_margin
  ensemble_bayesglm_overfitting_upper_95 <- ensemble_bayesglm_overfitting_mean + ensemble_bayesglm_overfitting_margin

  ensemble_bayesglm_end <- Sys.time()
  ensemble_bayesglm_duration[i] <- ensemble_bayesglm_end - ensemble_bayesglm_start
  ensemble_bayesglm_duration_mean <- mean(ensemble_bayesglm_duration)
  ensemble_bayesglm_duration_sd <- sd(ensemble_bayesglm_duration)

  #### Model 21: Ensemble Using Bayes RNN ####
  ensemble_bayesrnn_start <- Sys.time()
  message("Working on Ensemble BayesRNN")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_bayesrnn_train_fit <- brnn::brnn(x = as.matrix(ensemble_train), y = ensemble_train$y_ensemble)
  }
  if(set_seed == "N"){
    ensemble_bayesrnn_train_fit <- brnn::brnn(x = as.matrix(ensemble_train), y = ensemble_train$y_ensemble)
  }
  ensemble_bayesrnn_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_bayesrnn_train_fit,
    newdata = ensemble_train
  ))
  ensemble_bayesrnn_train_RMSE_mean <- mean(ensemble_bayesrnn_train_RMSE)
  ensemble_bayesrnn_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_bayesrnn_train_fit,
    newdata = ensemble_test
  ))
  ensemble_bayesrnn_test_RMSE_mean <- mean(ensemble_bayesrnn_test_RMSE)
  ensemble_bayesrnn_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_bayesrnn_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_bayesrnn_validation_RMSE_mean <- mean(ensemble_bayesrnn_validation_RMSE)
  ensemble_bayesrnn_holdout_RMSE[i] <- mean(c(ensemble_bayesrnn_test_RMSE_mean, ensemble_bayesrnn_validation_RMSE_mean))
  ensemble_bayesrnn_holdout_RMSE_mean <- mean(ensemble_bayesrnn_holdout_RMSE)
  ensemble_bayesrnn_holdout_RMSE_sd_mean <- sd(c(ensemble_bayesrnn_test_RMSE_mean, ensemble_bayesrnn_validation_RMSE_mean))
  ensemble_bayesrnn_train_predict_value <- as.numeric(predict(object = ensemble_bayesrnn_train_fit, newdata = ensemble_train))
  ensemble_bayesrnn_test_predict_value <- as.numeric(predict(object = ensemble_bayesrnn_train_fit, newdata = ensemble_test))
  ensemble_bayesrnn_validation_predict_value <- as.numeric(predict(object = ensemble_bayesrnn_train_fit, newdata = ensemble_validation))
  ensemble_bayesrnn_predict_value_mean <- mean(c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value))
  ensemble_bayesrnn_sd[i] <- sd(c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value))
  ensemble_bayesrnn_sd_mean <- mean(ensemble_bayesrnn_sd)
  ensemble_bayesrnn_overfitting[i] <- ensemble_bayesrnn_holdout_RMSE_mean / ensemble_bayesrnn_train_RMSE_mean
  ensemble_bayesrnn_overfitting_mean <- mean(ensemble_bayesrnn_overfitting)
  ensemble_bayesrnn_overfitting_range <- range(ensemble_bayesrnn_overfitting)
  ensemble_bayesrnn_overfitting_sd <- sd(ensemble_bayesrnn_overfitting)
  ensemble_y_hat_bayesrnn <- c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value)
  ensemble_y_hat_bayesrnn_total <- c(ensemble_y_hat_bayesrnn, ensemble_y_hat_bayesrnn_total)
  ensemble_bayesrnn_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_bayesrnn_actual_total <- c(ensemble_bayesrnn_actual, ensemble_bayesrnn_actual_total)
  ensemble_bayesrnn_t_test_t[i] <- as.numeric(t.test(x =  ensemble_y_hat_bayesrnn, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_bayesrnn_t_test_p_value[i] <- as.numeric(t.test(x =  ensemble_y_hat_bayesrnn, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_bayesrnn_t_test_t_mean <- mean(as.numeric(ensemble_bayesrnn_t_test_t))
  ensemble_bayesrnn_t_test_p_value_mean <- mean(as.numeric(ensemble_bayesrnn_t_test_p_value))
  ensemble_bayesrnn_t_test_p_value_sd <- sd(as.numeric(ensemble_bayesrnn_t_test_p_value))
  ensemble_bayesrnn_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_bayesrnn_test_predict_value, ensemble_bayesrnn_validation_predict_value))
  ensemble_bayesrnn_bias_mean <- mean(ensemble_bayesrnn_bias)
  ensemble_bayesrnn_bias_sd <- sd(ensemble_bayesrnn_bias)
  ensemble_bayesrnn_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_bayesrnn, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_bayesrnn_ks_p_value_mean <- mean(ensemble_bayesrnn_ks_p_value)
  ensemble_bayesrnn_ks_p_value_sd <- sd(ensemble_bayesrnn_ks_p_value)
  ensemble_bayesrnn_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_bayesrnn, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_bayesrnn_ks_stat_mean <- mean(ensemble_bayesrnn_ks_stat)
  ensemble_bayesrnn_ks_test <- c(ensemble_bayesrnn_ks_stat_mean, ensemble_bayesrnn_ks_p_value_mean)
  ensemble_bayesrnn_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_bayesrnn_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_bayesrnn_lower_95 <- ensemble_bayesrnn_holdout_RMSE_mean - ensemble_bayesrnn_margin
  ensemble_bayesrnn_upper_95 <- ensemble_bayesrnn_holdout_RMSE_mean + ensemble_bayesrnn_margin
  ensemble_bayesrnn_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_bayesrnn_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_bayesrnn_overfitting_lower_95 <- ensemble_bayesrnn_overfitting_mean - ensemble_bayesrnn_overfitting_margin
  ensemble_bayesrnn_overfitting_upper_95 <- ensemble_bayesrnn_overfitting_mean + ensemble_bayesrnn_overfitting_margin

  ensemble_bayesrnn_end <- Sys.time()
  ensemble_bayesrnn_duration[i] <- ensemble_bayesrnn_end - ensemble_bayesrnn_start
  ensemble_bayesrnn_duration_mean <- mean(ensemble_bayesrnn_duration)
  ensemble_bayesrnn_duration_sd <- sd(ensemble_bayesrnn_duration)

  #### Model 22: Ensemble Using Cubist ####
  ensemble_cubist_start <- Sys.time()
  message("Working on Ensemble Cubist")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_cubist_train_fit <- Cubist::cubist(x = ensemble_train[, 1:ncol(ensemble_train) - 1], y = ensemble_train$y_ensemble)
  }
  if(set_seed == "N"){
    ensemble_cubist_train_fit <- Cubist::cubist(x = ensemble_train[, 1:ncol(ensemble_train) - 1], y = ensemble_train$y_ensemble)
  }
  ensemble_cubist_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_cubist_train_fit,
    newdata = ensemble_train
  ))
  ensemble_cubist_train_RMSE_mean <- mean(ensemble_cubist_train_RMSE)
  ensemble_cubist_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_cubist_train_fit,
    newdata = ensemble_test
  ))
  ensemble_cubist_test_RMSE_mean <- mean(ensemble_cubist_test_RMSE)
  ensemble_cubist_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_cubist_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_cubist_validation_RMSE_mean <- mean(ensemble_cubist_validation_RMSE)
  ensemble_cubist_holdout_RMSE[i] <- mean(c(ensemble_cubist_test_RMSE_mean, ensemble_cubist_validation_RMSE_mean))
  ensemble_cubist_holdout_RMSE_mean <- mean(ensemble_cubist_holdout_RMSE)
  ensemble_cubist_holdout_RMSE_sd_mean <- sd(c(ensemble_cubist_test_RMSE_mean, ensemble_cubist_validation_RMSE_mean))
  ensemble_cubist_train_predict_value <- as.numeric(predict(object = ensemble_cubist_train_fit, newdata = ensemble_train))
  ensemble_cubist_test_predict_value <- as.numeric(predict(object = ensemble_cubist_train_fit, newdata = ensemble_test))
  ensemble_cubist_validation_predict_value <- as.numeric(predict(object = ensemble_cubist_train_fit, newdata = ensemble_validation))
  ensemble_cubist_predict_value_mean <- mean(c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value))
  ensemble_cubist_sd[i] <- sd(c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value))
  ensemble_cubist_sd_mean <- mean(ensemble_cubist_sd)
  ensemble_cubist_overfitting[i] <- ensemble_cubist_holdout_RMSE_mean / ensemble_cubist_train_RMSE_mean
  ensemble_cubist_overfitting_mean <- mean(ensemble_cubist_overfitting)
  ensemble_cubist_overfitting_range <- range(ensemble_cubist_overfitting)
  ensemble_cubist_overfitting_sd <- sd(ensemble_cubist_overfitting)
  ensemble_y_hat_cubist <- c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value)
  ensemble_y_hat_cubist_total <- c(ensemble_y_hat_cubist, ensemble_y_hat_cubist_total)
  ensemble_cubist_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_cubist_actual_total <- c(ensemble_cubist_actual, ensemble_cubist_actual_total)
  ensemble_cubist_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_cubist, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_cubist_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_cubist, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_cubist_t_test_t_mean <- mean(as.numeric(ensemble_cubist_t_test_t))
  ensemble_cubist_t_test_p_value_mean <- mean(as.numeric(ensemble_cubist_t_test_p_value))
  ensemble_cubist_t_test_p_value_sd <- sd(as.numeric(ensemble_cubist_t_test_p_value))
  ensemble_cubist_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_cubist_test_predict_value, ensemble_cubist_validation_predict_value))
  ensemble_cubist_bias_mean <- mean(ensemble_cubist_bias)
  ensemble_cubist_bias_sd <- sd(ensemble_cubist_bias)
  ensemble_cubist_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_cubist, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_cubist_ks_p_value_mean <- mean(ensemble_cubist_ks_p_value)
  ensemble_cubist_ks_p_value_sd <- sd(ensemble_cubist_ks_p_value)
  ensemble_cubist_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_cubist, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_cubist_ks_stat_mean <- mean(ensemble_cubist_ks_stat)
  ensemble_cubist_ks_test <- c(ensemble_cubist_ks_stat_mean, ensemble_cubist_ks_p_value_mean)
  ensemble_cubist_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_cubist_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_cubist_lower_95 <- ensemble_cubist_holdout_RMSE_mean - ensemble_cubist_margin
  ensemble_cubist_upper_95 <- ensemble_cubist_holdout_RMSE_mean + ensemble_cubist_margin
  ensemble_cubist_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_cubist_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_cubist_overfitting_lower_95 <- ensemble_cubist_overfitting_mean - ensemble_cubist_overfitting_margin
  ensemble_cubist_overfitting_upper_95 <- ensemble_cubist_overfitting_mean + ensemble_cubist_overfitting_margin

  ensemble_cubist_end <- Sys.time()
  ensemble_cubist_duration[i] <- ensemble_cubist_end - ensemble_cubist_start
  ensemble_cubist_duration_mean <- mean(ensemble_cubist_duration)
  ensemble_cubist_duration_sd <- sd(ensemble_cubist_duration)

  #### Model 23: Ensemble Using Earth ####
  ensemble_earth_start <- Sys.time()
  message("Working on Ensemble Earth")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_earth_train_fit <- earth::earth(x = ensemble_train[, 1:ncol(ensemble_train) - 1], y = ensemble_train$y_ensemble)
  }
  if(set_seed == "N"){
    ensemble_earth_train_fit <- earth::earth(x = ensemble_train[, 1:ncol(ensemble_train) - 1], y = ensemble_train$y_ensemble)
  }
  ensemble_earth_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_earth_train_fit,
    newdata = ensemble_train
  ))
  ensemble_earth_train_RMSE_mean <- mean(ensemble_earth_train_RMSE)
  ensemble_earth_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_earth_train_fit,
    newdata = ensemble_test
  ))
  ensemble_earth_test_RMSE_mean <- mean(ensemble_earth_test_RMSE)
  ensemble_earth_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_earth_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_earth_validation_RMSE_mean <- mean(ensemble_earth_validation_RMSE)
  ensemble_earth_holdout_RMSE[i] <- mean(c(ensemble_earth_test_RMSE_mean, ensemble_earth_validation_RMSE_mean))
  ensemble_earth_holdout_RMSE_mean <- mean(ensemble_earth_holdout_RMSE)
  ensemble_earth_holdout_RMSE_sd_mean <- sd(c(ensemble_earth_test_RMSE_mean, ensemble_earth_validation_RMSE_mean))
  ensemble_earth_train_predict_value <- as.numeric(predict(object = ensemble_earth_train_fit, newdata = ensemble_train))
  ensemble_earth_test_predict_value <- as.numeric(predict(object = ensemble_earth_train_fit, newdata = ensemble_test))
  ensemble_earth_validation_predict_value <- as.numeric(predict(object = ensemble_earth_train_fit, newdata = ensemble_validation))
  ensemble_earth_predict_value_mean <- mean(c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value))
  ensemble_earth_sd[i] <- sd(c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value))
  ensemble_earth_sd_mean <- mean(ensemble_earth_sd)
  ensemble_earth_overfitting[i] <- ensemble_earth_holdout_RMSE_mean / ensemble_earth_train_RMSE_mean
  ensemble_earth_overfitting_mean <- mean(ensemble_earth_overfitting)
  ensemble_earth_overfitting_range <- range(ensemble_earth_overfitting)
  ensemble_earth_overfitting_sd <- sd(ensemble_earth_overfitting)
  ensemble_y_hat_earth <- c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value)
  ensemble_y_hat_earth_total <- c(ensemble_y_hat_earth, ensemble_y_hat_earth_total)
  ensemble_earth_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_earth_actual_total <- c(ensemble_earth_actual, ensemble_earth_actual_total)
  ensemble_earth_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_earth, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_earth_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_earth, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_earth_t_test_t_mean <- mean(as.numeric(ensemble_earth_t_test_t))
  ensemble_earth_t_test_p_value_mean <- mean(as.numeric(ensemble_earth_t_test_p_value))
  ensemble_earth_t_test_p_value_sd <- sd(as.numeric(ensemble_earth_t_test_p_value))
  ensemble_earth_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_earth_test_predict_value, ensemble_earth_validation_predict_value))
  ensemble_earth_bias_mean <- mean(ensemble_earth_bias)
  ensemble_earth_bias_sd <- sd(ensemble_earth_bias)
  ensemble_earth_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_earth, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_earth_ks_p_value_mean <- mean(ensemble_earth_ks_p_value)
  ensemble_earth_ks_p_value_sd <- sd(ensemble_earth_ks_p_value)
  ensemble_earth_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_earth, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_earth_ks_stat_mean <- mean(ensemble_earth_ks_stat)
  ensemble_earth_ks_test <- c(ensemble_earth_ks_stat_mean, ensemble_earth_ks_p_value_mean)
  ensemble_earth_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_earth_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_earth_lower_95 <- ensemble_earth_holdout_RMSE_mean - ensemble_earth_margin
  ensemble_earth_upper_95 <- ensemble_earth_holdout_RMSE_mean + ensemble_earth_margin
  ensemble_earth_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_earth_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_earth_overfitting_lower_95 <- ensemble_earth_overfitting_mean - ensemble_earth_overfitting_margin
  ensemble_earth_overfitting_upper_95 <- ensemble_earth_overfitting_mean + ensemble_earth_overfitting_margin

  ensemble_earth_end <- Sys.time()
  ensemble_earth_duration[i] <- ensemble_earth_end - ensemble_earth_start
  ensemble_earth_duration_mean <- mean(ensemble_earth_duration)
  ensemble_earth_duration_sd <- sd(ensemble_earth_duration)

  #### Model # 24 Ensembles Using Elastic ####
  ensemble_elastic_start <- Sys.time()
  message("Working on Ensemble Elastic")

  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_elastic_model <- glmnet(ensemble_x, ensemble_y, alpha = 0.5)
  ensemble_elastic_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 0.5)
  ensemble_best_elastic_lambda <- ensemble_elastic_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_best_elastic_model <- glmnet(ensemble_x, ensemble_y, alpha = 0, lambda = ensemble_best_elastic_lambda)
  }
  if(set_seed == "N"){
    ensemble_best_elastic_model <- glmnet(ensemble_x, ensemble_y, alpha = 0, lambda = ensemble_best_elastic_lambda)
  }
  ensemble_elastic_train_RMSE <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))))
  ensemble_elastic_train_RMSE_df <- rbind(ensemble_elastic_train_RMSE_df, ensemble_elastic_train_RMSE)
  ensemble_elastic_train_RMSE_mean <- mean(ensemble_elastic_train_RMSE_df$ensemble_elastic_train_RMSE[2:nrow(ensemble_elastic_train_RMSE_df)])
  ensemble_elastic_test_RMSE <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_elastic_test_RMSE_df <- rbind(ensemble_elastic_test_RMSE_df, ensemble_elastic_test_RMSE)
  ensemble_elastic_test_RMSE_mean <- mean(ensemble_elastic_test_RMSE_df$ensemble_elastic_test_RMSE[2:nrow(ensemble_elastic_test_RMSE_df)])
  ensemble_elastic_test_predict_value <- rowMeans(predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_elastic_test_predict_value_mean <- mean(ensemble_elastic_test_predict_value)
  ensemble_elastic_test_predict_value_sd <- sd(ensemble_elastic_test_predict_value)
  ensemble_elastic_test_predict_value_sd_mean <- mean(ensemble_elastic_test_predict_value_sd, na.rm = TRUE)
  ## Elastic using the validation data set
  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_elastic_model <- glmnet(ensemble_x, ensemble_y, alpha = 0.5)
  ensemble_elastic_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 0.5)
  ensemble_best_elastic_lambda <- ensemble_elastic_cv$lambda.min
  ensemble_best_elastic_model <- glmnet(ensemble_x, ensemble_y, alpha = 0, lambda = ensemble_best_elastic_lambda)
  ensemble_elastic_validation_pred <- predict(ensemble_best_elastic_model, s = ensemble_best_elastic_lambda, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble)))
  ensemble_elastic_validation_RMSE <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_elastic_validation_RMSE_df <- rbind(ensemble_elastic_validation_RMSE_df, ensemble_elastic_validation_RMSE)
  ensemble_elastic_validation_RMSE_mean <- mean(ensemble_elastic_validation_RMSE_df$ensemble_elastic_validation_RMSE[2:nrow(ensemble_elastic_validation_RMSE_df)])
  ensemble_elastic_validation_predict_value <- rowMeans(predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_elastic_validation_predict_value_mean <- mean(ensemble_elastic_validation_predict_value)
  ensemble_elastic_validation_predict_value_sd <- round(sd(ensemble_elastic_validation_pred), 4)
  ensemble_elastic_validation_predict_value_sd_df <- rbind(ensemble_elastic_validation_predict_value_sd_df, ensemble_elastic_validation_predict_value_sd)
  ensemble_elastic_validation_predict_value_sd_mean <- mean(ensemble_elastic_validation_predict_value_sd_df$ensemble_elastic_validation_predict_value_sd[2:nrow(ensemble_elastic_validation_predict_value_sd_df)])
  ensemble_elastic_holdout_RMSE[i] <- mean(c(ensemble_elastic_test_RMSE, ensemble_elastic_validation_RMSE))
  ensemble_elastic_holdout_RMSE_mean <- mean(ensemble_elastic_holdout_RMSE)
  ensemble_elastic_holdout_RMSE_sd_mean <- mean(sd(c(ensemble_elastic_holdout_RMSE)))
  ensemble_elastic_test_sd <- sd(ensemble_elastic_test_predict_value, na.rm = TRUE)
  ensemble_elastic_test_sd_df <- rbind(ensemble_elastic_test_sd_df, ensemble_elastic_test_sd)
  ensemble_elastic_test_sd_mean <- mean(ensemble_elastic_test_sd_df$ensemble_elastic_test_sd[2:nrow(ensemble_elastic_test_sd_df)])
  ensemble_elastic_validation_sd <- sd(ensemble_elastic_validation_predict_value, na.rm = TRUE)
  ensemble_elastic_validation_sd_df <- rbind(ensemble_elastic_validation_sd_df, ensemble_elastic_validation_sd)
  ensemble_elastic_validation_sd_mean <- mean(ensemble_elastic_validation_sd_df$ensemble_elastic_validation_sd[2:nrow(ensemble_elastic_validation_sd_df)])
  ensemble_elastic_overfitting <- ensemble_elastic_holdout_RMSE_mean / ensemble_elastic_train_RMSE_mean
  ensemble_elastic_overfitting_df <- rbind(ensemble_elastic_overfitting_df, ensemble_elastic_overfitting)
  ensemble_elastic_overfitting_mean <- mean(ensemble_elastic_overfitting_df$ensemble_elastic_overfitting[2:nrow(ensemble_elastic_overfitting_df)])
  ensemble_elastic_overfitting_range <- range(ensemble_elastic_overfitting_df$ensemble_elastic_overfitting[2:nrow(ensemble_elastic_overfitting_df)])
  ensemble_elastic_overfitting_sd <- sd(ensemble_elastic_overfitting_df$ensemble_elastic_overfitting)
  ensemble_elastic_predict_value_mean <- mean(c(ensemble_elastic_test_predict_value_mean, ensemble_elastic_validation_predict_value_mean))
  ensemble_elastic_sd_mean <- mean(c(ensemble_elastic_test_sd_mean, ensemble_elastic_validation_sd_mean))
  ensemble_y_hat_elastic <- predict(object = ensemble_best_elastic_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble)))
  ensemble_y_hat_elastic <- c(ensemble_elastic_test_predict_value, ensemble_elastic_validation_predict_value)
  ensemble_y_hat_elastic_total <- c(ensemble_y_hat_elastic, ensemble_y_hat_elastic_total)
  ensemble_elastic_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_elastic_actual_total <- c(ensemble_elastic_actual, ensemble_elastic_actual_total)
  ensemble_elastic_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_elastic, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_elastic_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_elastic, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_elastic_t_test_t_mean <- mean(as.numeric(ensemble_elastic_t_test_t))
  ensemble_elastic_t_test_p_value_mean <- mean(as.numeric(ensemble_elastic_t_test_p_value))
  ensemble_elastic_t_test_p_value_sd <- sd(as.numeric(ensemble_elastic_t_test_p_value))
  ensemble_elastic_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_elastic_test_predict_value, ensemble_elastic_validation_predict_value))
  ensemble_elastic_bias_mean <- mean(ensemble_elastic_bias)
  ensemble_elastic_bias_sd <- sd(ensemble_elastic_bias)
  ensemble_elastic_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_elastic, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_elastic_ks_p_value_mean <- mean(ensemble_elastic_ks_p_value)
  ensemble_elastic_ks_p_value_sd <- sd(ensemble_elastic_ks_p_value)
  ensemble_elastic_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_elastic, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_elastic_ks_stat_mean <- mean(ensemble_elastic_ks_stat)
  ensemble_elastic_ks_test <- c(ensemble_elastic_ks_stat_mean, ensemble_elastic_ks_p_value_mean)
  ensemble_elastic_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_elastic_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_elastic_lower_95 <- ensemble_elastic_holdout_RMSE_mean - ensemble_elastic_margin
  ensemble_elastic_upper_95 <- ensemble_elastic_holdout_RMSE_mean + ensemble_elastic_margin
  ensemble_elastic_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_elastic_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_elastic_overfitting_lower_95 <- ensemble_elastic_overfitting_mean - ensemble_elastic_overfitting_margin
  ensemble_elastic_overfitting_upper_95 <- ensemble_elastic_overfitting_mean + ensemble_elastic_overfitting_margin

  ensemble_elastic_end <- Sys.time()
  ensemble_elastic_duration[i] <- ensemble_elastic_end - ensemble_elastic_start
  ensemble_elastic_duration_mean <- mean(ensemble_elastic_duration)
  ensemble_elastic_duration_sd <- sd(ensemble_elastic_duration)


  #### Model 25: Ensemble Gradient Boosted ####
  ensemble_gb_start <- Sys.time()
  message("Working on Ensemble Gradient Boosted")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_gb_train_fit <- gbm::gbm(ensemble_train$y_ensemble ~ .,
                                      data = ensemble_train, distribution = "gaussian", n.trees = 100,
                                      shrinkage = 0.1, interaction.depth = 10
    )
  }
  if(set_seed == "N"){
    ensemble_gb_train_fit <- gbm::gbm(ensemble_train$y_ensemble ~ .,
                                      data = ensemble_train, distribution = "gaussian", n.trees = 100,
                                      shrinkage = 0.1, interaction.depth = 10
    )
  }

  ensemble_gb_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_gb_train_fit,
    newdata = ensemble_train
  ))
  ensemble_gb_train_RMSE_mean <- mean(ensemble_gb_train_RMSE)
  ensemble_gb_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_gb_train_fit,
    newdata = ensemble_test
  ))
  ensemble_gb_test_RMSE_mean <- mean(ensemble_gb_test_RMSE)
  ensemble_gb_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_gb_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_gb_validation_RMSE_mean <- mean(ensemble_gb_validation_RMSE)
  ensemble_gb_holdout_RMSE[i] <- mean(c(ensemble_gb_test_RMSE_mean, ensemble_gb_validation_RMSE_mean))
  ensemble_gb_holdout_RMSE_mean <- mean(ensemble_gb_holdout_RMSE)
  ensemble_gb_holdout_RMSE_sd_mean <- sd(c(ensemble_gb_test_RMSE_mean, ensemble_gb_validation_RMSE_mean))
  ensemble_gb_train_predict_value <- as.numeric(predict(object = ensemble_gb_train_fit, newdata = ensemble_train))
  ensemble_gb_test_predict_value <- as.numeric(predict(object = ensemble_gb_train_fit, newdata = ensemble_test))
  ensemble_gb_validation_predict_value <- as.numeric(predict(object = ensemble_gb_train_fit, newdata = ensemble_validation))
  ensemble_gb_predict_value_mean <- mean(c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value))
  ensemble_gb_sd[i] <- sd(c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value))
  ensemble_gb_sd_mean <- mean(ensemble_gb_sd)
  ensemble_gb_overfitting[i] <- ensemble_gb_holdout_RMSE_mean / ensemble_gb_train_RMSE_mean
  ensemble_gb_overfitting_mean <- mean(ensemble_gb_overfitting)
  ensemble_gb_overfitting_range <- range(ensemble_gb_overfitting)
  ensemble_gb_overfitting_sd <- sd(ensemble_gb_overfitting)
  ensemble_y_hat_gb <- c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value)
  ensemble_y_hat_gb_total <- c(ensemble_y_hat_gb, ensemble_y_hat_gb_total)
  ensemble_gb_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_gb_actual_total <- c(ensemble_gb_actual, ensemble_gb_actual_total)
  ensemble_gb_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_gb, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_gb_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_gb, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_gb_t_test_t_mean <- mean(as.numeric(ensemble_gb_t_test_t))
  ensemble_gb_t_test_p_value_mean <- mean(as.numeric(ensemble_gb_t_test_p_value))
  ensemble_gb_t_test_p_value_sd <- sd(as.numeric(ensemble_gb_t_test_p_value))
  ensemble_gb_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_gb_test_predict_value, ensemble_gb_validation_predict_value))
  ensemble_gb_bias_mean <- mean(ensemble_gb_bias)
  ensemble_gb_bias_sd <- sd(ensemble_gb_bias)
  ensemble_gb_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_gb, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_gb_ks_p_value_mean <- mean(ensemble_gb_ks_p_value)
  ensemble_gb_ks_p_value_sd <- sd(ensemble_gb_ks_p_value)
  ensemble_gb_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_gb, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_gb_ks_stat_mean <- mean(ensemble_gb_ks_stat)
  ensemble_gb_ks_test <- c(ensemble_gb_ks_stat_mean, ensemble_gb_ks_p_value_mean)
  ensemble_gb_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_gb_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_gb_lower_95 <- ensemble_gb_holdout_RMSE_mean - ensemble_gb_margin
  ensemble_gb_upper_95 <- ensemble_gb_holdout_RMSE_mean + ensemble_gb_margin
  ensemble_gb_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_gb_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_gb_overfitting_lower_95 <- ensemble_gb_overfitting_mean - ensemble_gb_overfitting_margin
  ensemble_gb_overfitting_upper_95 <- ensemble_gb_overfitting_mean + ensemble_gb_overfitting_margin

  ensemble_gb_end <- Sys.time()
  ensemble_gb_duration[i] <- ensemble_gb_end - ensemble_gb_start
  ensemble_gb_duration_mean <- mean(ensemble_gb_duration)
  ensemble_gb_duration_sd <- sd(ensemble_gb_duration)

  #### Model # 26 Ensembles Using lasso ####
  ensemble_lasso_start <- Sys.time()
  message("Working on Ensemble Lasso")

  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_lasso_model <- glmnet(ensemble_x, ensemble_y, alpha = 1)
  ensemble_lasso_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 1)
  ensemble_best_lasso_lambda <- ensemble_lasso_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_best_lasso_model <- glmnet(ensemble_x, ensemble_y, alpha = 1, lambda = ensemble_best_lasso_lambda)
  }
  if(set_seed == "N"){
    ensemble_best_lasso_model <- glmnet(ensemble_x, ensemble_y, alpha = 1, lambda = ensemble_best_lasso_lambda)
  }
  ensemble_lasso_train_RMSE <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))))
  ensemble_lasso_train_RMSE_df <- rbind(ensemble_lasso_train_RMSE_df, ensemble_lasso_train_RMSE)
  ensemble_lasso_train_RMSE_mean <- mean(ensemble_lasso_train_RMSE_df$ensemble_lasso_train_RMSE[2:nrow(ensemble_lasso_train_RMSE_df)])
  ensemble_lasso_test_RMSE <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_lasso_test_RMSE_df <- rbind(ensemble_lasso_test_RMSE_df, ensemble_lasso_test_RMSE)
  ensemble_lasso_test_RMSE_mean <- mean(ensemble_lasso_test_RMSE_df$ensemble_lasso_test_RMSE[2:nrow(ensemble_lasso_test_RMSE_df)])
  ensemble_lasso_test_predict_value <- rowMeans(predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_lasso_test_predict_value_mean <- mean(ensemble_lasso_test_predict_value)
  ensemble_lasso_test_predict_value_sd <- sd(ensemble_lasso_test_predict_value)
  ensemble_lasso_test_predict_value_sd_mean <- mean(ensemble_lasso_test_predict_value_sd, na.rm = TRUE)
  ## lasso using the validation data set
  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_lasso_model <- glmnet(ensemble_x, ensemble_y, alpha = 1)
  ensemble_lasso_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 1)
  ensemble_best_lasso_lambda <- ensemble_lasso_cv$lambda.min
  ensemble_best_lasso_model <- glmnet(ensemble_x, ensemble_y, alpha = 1, lambda = ensemble_best_lasso_lambda)
  ensemble_lasso_validation_pred <- predict(ensemble_best_lasso_model, s = ensemble_best_lasso_lambda, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble)))
  ensemble_lasso_validation_RMSE <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_lasso_validation_RMSE_df <- rbind(ensemble_lasso_validation_RMSE_df, ensemble_lasso_validation_RMSE)
  ensemble_lasso_validation_RMSE_mean <- mean(ensemble_lasso_validation_RMSE_df$ensemble_lasso_validation_RMSE[2:nrow(ensemble_lasso_validation_RMSE_df)])
  ensemble_lasso_validation_predict_value <- rowMeans(predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_lasso_validation_predict_value_mean <- mean(ensemble_lasso_validation_predict_value)
  ensemble_lasso_validation_predict_value_sd <- round(sd(ensemble_lasso_validation_pred), 4)
  ensemble_lasso_validation_predict_value_sd_df <- rbind(ensemble_lasso_validation_predict_value_sd_df, ensemble_lasso_validation_predict_value_sd)
  ensemble_lasso_validation_predict_value_sd_mean <- mean(ensemble_lasso_validation_predict_value_sd_df$ensemble_lasso_validation_predict_value_sd[2:nrow(ensemble_lasso_validation_predict_value_sd_df)])
  ensemble_lasso_holdout_RMSE[i] <- mean(c(ensemble_lasso_test_RMSE, ensemble_lasso_validation_RMSE))
  ensemble_lasso_holdout_RMSE_mean <- mean(ensemble_lasso_holdout_RMSE)
  ensemble_lasso_holdout_RMSE_sd_mean <- mean(sd(c(ensemble_lasso_test_RMSE, ensemble_lasso_validation_RMSE)))
  ensemble_lasso_test_sd <- sd(ensemble_lasso_test_predict_value, na.rm = TRUE)
  ensemble_lasso_test_sd_df <- rbind(ensemble_lasso_test_sd_df, ensemble_lasso_test_sd)
  ensemble_lasso_test_sd_mean <- mean(ensemble_lasso_test_sd_df$ensemble_lasso_test_sd[2:nrow(ensemble_lasso_test_sd_df)])
  ensemble_lasso_validation_sd <- sd(ensemble_lasso_validation_predict_value, na.rm = TRUE)
  ensemble_lasso_validation_sd_df <- rbind(ensemble_lasso_validation_sd_df, ensemble_lasso_validation_sd)
  ensemble_lasso_validation_sd_mean <- mean(ensemble_lasso_validation_sd_df$ensemble_lasso_validation_sd[2:nrow(ensemble_lasso_validation_sd_df)])
  ensemble_lasso_overfitting <- ensemble_lasso_holdout_RMSE_mean / ensemble_lasso_train_RMSE_mean
  ensemble_lasso_overfitting_df <- rbind(ensemble_lasso_overfitting_df, ensemble_lasso_overfitting)
  ensemble_lasso_overfitting_mean <- mean(ensemble_lasso_overfitting_df$ensemble_lasso_overfitting[2:nrow(ensemble_lasso_overfitting_df)])
  ensemble_lasso_overfitting_range <- range(ensemble_lasso_overfitting_df$ensemble_lasso_overfitting[2:nrow(ensemble_lasso_overfitting_df)])
  ensemble_lasso_overfitting_sd <- sd(ensemble_lasso_overfitting_df$ensemble_lasso_overfitting)
  ensemble_lasso_predict_value_mean <- mean(c(ensemble_lasso_test_predict_value_mean, ensemble_lasso_validation_predict_value_mean))
  ensemble_lasso_sd[i] <- sd(c(ensemble_lasso_test_predict_value_mean, ensemble_lasso_validation_predict_value_mean))
  ensemble_lasso_sd_mean <- mean(c(ensemble_lasso_test_sd, ensemble_lasso_validation_sd))
  ensemble_y_hat_lasso <- predict(object = ensemble_best_lasso_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble)))
  ensemble_y_hat_lasso <- c(ensemble_lasso_test_predict_value, ensemble_lasso_validation_predict_value)
  ensemble_y_hat_lasso_total <- c(ensemble_y_hat_lasso, ensemble_y_hat_lasso_total)
  ensemble_lasso_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_lasso_actual_total <- c(ensemble_lasso_actual, ensemble_lasso_actual_total)
  ensemble_lasso_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_lasso, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_lasso_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_lasso, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_lasso_t_test_t_mean <- mean(as.numeric(ensemble_lasso_t_test_t))
  ensemble_lasso_t_test_p_value_mean <- mean(as.numeric(ensemble_lasso_t_test_p_value))
  ensemble_lasso_t_test_p_value_sd <- sd(as.numeric(ensemble_lasso_t_test_p_value))
  ensemble_lasso_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_lasso_test_predict_value, ensemble_lasso_validation_predict_value))
  ensemble_lasso_bias_mean <- mean(ensemble_lasso_bias)
  ensemble_lasso_bias_sd <- sd(ensemble_lasso_bias)
  ensemble_lasso_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_lasso, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_lasso_ks_p_value_mean <- mean(ensemble_lasso_ks_p_value)
  ensemble_lasso_ks_p_value_sd <- sd(ensemble_lasso_ks_p_value)
  ensemble_lasso_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_lasso, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_lasso_ks_stat_mean <- mean(ensemble_lasso_ks_stat)
  ensemble_lasso_ks_test <- c(ensemble_lasso_ks_stat_mean, ensemble_lasso_ks_p_value_mean)
  ensemble_lasso_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_lasso_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_lasso_lower_95 <- ensemble_lasso_holdout_RMSE_mean - ensemble_lasso_margin
  ensemble_lasso_upper_95 <- ensemble_lasso_holdout_RMSE_mean + ensemble_lasso_margin
  ensemble_lasso_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_lasso_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_lasso_overfitting_lower_95 <- ensemble_lasso_overfitting_mean - ensemble_lasso_overfitting_margin
  ensemble_lasso_overfitting_upper_95 <- ensemble_lasso_overfitting_mean + ensemble_lasso_overfitting_margin

  ensemble_lasso_end <- Sys.time()
  ensemble_lasso_duration[i] <- ensemble_lasso_end - ensemble_lasso_start
  ensemble_lasso_duration_mean <- mean(ensemble_lasso_duration)
  ensemble_lasso_duration_sd <- sd(ensemble_lasso_duration)


  #### Model 27: Ensembles Using Linear tuned ####
  ensemble_linear_start <- Sys.time()
  message("Working on Ensemble Linear")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_linear_train_fit <- e1071::tune.rpart(formula = y_ensemble ~ ., data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_linear_train_fit <- e1071::tune.rpart(formula = y_ensemble ~ ., data = ensemble_train)
  }
  ensemble_linear_train_RMSE[i] <- Metrics::rmse(
    actual = ensemble_train$y_ensemble,
    predicted = predict(
      object = ensemble_linear_train_fit$best.model,
      newdata = ensemble_train
    )
  )
  ensemble_linear_train_RMSE_mean <- mean(ensemble_linear_train_RMSE)
  ensemble_linear_test_RMSE[i] <- Metrics::rmse(
    actual = ensemble_test$y_ensemble,
    predicted = predict(
      object = ensemble_linear_train_fit$best.model,
      newdata = ensemble_test
    )
  )
  ensemble_linear_test_RMSE_mean <- mean(ensemble_linear_test_RMSE)
  ensemble_linear_validation_RMSE[i] <- Metrics::rmse(
    actual = ensemble_validation$y_ensemble,
    predicted = predict(
      object = ensemble_linear_train_fit$best.model,
      newdata = ensemble_validation
    )
  )
  ensemble_linear_validation_RMSE_mean <- mean(ensemble_linear_validation_RMSE)
  ensemble_linear_holdout_RMSE[i] <- mean(c(ensemble_linear_test_RMSE_mean, ensemble_linear_validation_RMSE_mean))
  ensemble_linear_holdout_RMSE_mean <- mean(ensemble_linear_holdout_RMSE)
  ensemble_linear_holdout_RMSE_sd_mean <- sd(c(ensemble_linear_test_RMSE_mean, ensemble_linear_validation_RMSE_mean))
  ensemble_linear_train_predict_value <- as.numeric(predict(object = ensemble_linear_train_fit$best.model, newdata = ensemble_train))
  ensemble_linear_test_predict_value <- as.numeric(predict(object = ensemble_linear_train_fit$best.model, newdata = ensemble_test))
  ensemble_linear_validation_predict_value <- as.numeric(predict(object = ensemble_linear_train_fit$best.model, newdata = ensemble_validation))
  ensemble_linear_predict_value_mean <- mean(c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value))
  ensemble_linear_sd[i] <- sd(c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value))
  ensemble_linear_sd_mean <- mean(ensemble_linear_sd)
  ensemble_linear_overfitting[i] <- ensemble_linear_holdout_RMSE_mean / ensemble_linear_train_RMSE_mean
  ensemble_linear_overfitting_mean <- mean(ensemble_linear_overfitting)
  ensemble_linear_overfitting_range <- range(ensemble_linear_overfitting)
  ensemble_linear_overfitting_sd <- sd(ensemble_linear_overfitting)
  ensemble_y_hat_linear <- c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value)
  ensemble_y_hat_linear_total <- c(ensemble_y_hat_linear, ensemble_y_hat_linear_total)
  ensemble_linear_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_linear_actual_total <- c(ensemble_linear_actual, ensemble_linear_actual_total)
  ensemble_linear_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_linear, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_linear_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_linear, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_linear_t_test_t_mean <- mean(as.numeric(ensemble_linear_t_test_t))
  ensemble_linear_t_test_p_value_mean <- mean(as.numeric(ensemble_linear_t_test_p_value))
  ensemble_linear_t_test_p_value_sd <- sd(as.numeric(ensemble_linear_t_test_p_value))
  ensemble_linear_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_linear_test_predict_value, ensemble_linear_validation_predict_value))
  ensemble_linear_bias_mean <- mean(ensemble_linear_bias)
  ensemble_linear_bias_sd <- sd(ensemble_linear_bias)
  ensemble_linear_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_linear, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_linear_ks_p_value_mean <- mean(ensemble_linear_ks_p_value)
  ensemble_linear_ks_p_value_sd <- sd(ensemble_linear_ks_p_value)
  ensemble_linear_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_linear, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_linear_ks_stat_mean <- mean(ensemble_linear_ks_stat)
  ensemble_linear_ks_test <- c(ensemble_linear_ks_stat_mean, ensemble_linear_ks_p_value_mean)
  ensemble_linear_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_linear_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_linear_lower_95 <- ensemble_linear_holdout_RMSE_mean - ensemble_linear_margin
  ensemble_linear_upper_95 <- ensemble_linear_holdout_RMSE_mean + ensemble_linear_margin
  ensemble_linear_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_linear_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_linear_overfitting_lower_95 <- ensemble_linear_overfitting_mean - ensemble_linear_overfitting_margin
  ensemble_linear_overfitting_upper_95 <- ensemble_linear_overfitting_mean + ensemble_linear_overfitting_margin

  ensemble_linear_end <- Sys.time()
  ensemble_linear_duration[i] <- ensemble_linear_end - ensemble_linear_start
  ensemble_linear_duration_mean <- mean(ensemble_linear_duration)
  ensemble_linear_duration_sd <- sd(ensemble_linear_duration)

  #### Model 28 Ensemble Neuralnet ####

  ensemble_neuralnet_start <- Sys.time()
  message("Working on Ensemble Neuralnet")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_neuralnet_train_fit <- nnet::nnet(ensemble_train$y_ensemble ~ ., data = ensemble_train, size = 0, linout = TRUE, skip = TRUE)
  }
  if(set_seed == "N"){
    ensemble_neuralnet_train_fit <- nnet::nnet(ensemble_train$y_ensemble ~ ., data = ensemble_train, size = 0, linout = TRUE, skip = TRUE)
  }
  ensemble_neuralnet_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_neuralnet_train_fit, newdata = ensemble_train))
  ensemble_neuralnet_train_RMSE_mean <- mean(ensemble_neuralnet_train_RMSE)
  ensemble_neuralnet_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_neuralnet_train_fit, newdata = ensemble_test))
  ensemble_neuralnet_test_RMSE_mean <- mean(ensemble_neuralnet_test_RMSE)
  ensemble_neuralnet_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_neuralnet_train_fit, newdata = ensemble_validation))
  ensemble_neuralnet_validation_RMSE_mean <- mean(ensemble_neuralnet_validation_RMSE)
  ensemble_neuralnet_holdout_RMSE[i] <- mean(c(ensemble_neuralnet_test_RMSE_mean, ensemble_neuralnet_validation_RMSE_mean))
  ensemble_neuralnet_holdout_RMSE_mean <- mean(ensemble_neuralnet_holdout_RMSE)
  ensemble_neuralnet_holdout_RMSE_sd_mean <- sd(c(ensemble_neuralnet_test_RMSE_mean, ensemble_neuralnet_validation_RMSE_mean))
  ensemble_neuralnet_train_predict_value <- predict(object = ensemble_neuralnet_train_fit, newdata = ensemble_train)
  ensemble_neuralnet_test_predict_value <- predict(object = ensemble_neuralnet_train_fit, newdata = ensemble_test)
  ensemble_neuralnet_validation_predict_value <- predict(object = ensemble_neuralnet_train_fit, newdata = ensemble_validation)
  ensemble_neuralnet_predict_value_mean <- mean(c(ensemble_neuralnet_test_predict_value, ensemble_neuralnet_validation_predict_value))
  ensemble_neuralnet_sd[i] <- sd(c(ensemble_neuralnet_test_predict_value, ensemble_neuralnet_validation_predict_value))
  ensemble_neuralnet_sd_mean <- mean(ensemble_neuralnet_sd)
  ensemble_neuralnet_overfitting[i] <- ensemble_neuralnet_holdout_RMSE_mean / ensemble_neuralnet_train_RMSE_mean
  ensemble_neuralnet_overfitting_mean <- mean(ensemble_neuralnet_overfitting)
  ensemble_neuralnet_overfitting_range <- range(ensemble_neuralnet_overfitting)
  ensemble_neuralnet_overfitting_sd <- sd(ensemble_neuralnet_overfitting)
  y_hat_ensemble_neuralnet <- c(ensemble_neuralnet_test_predict_value, ensemble_neuralnet_validation_predict_value)
  y_hat_ensemble_neuralnet_total <- c(y_hat_ensemble_neuralnet, y_hat_ensemble_neuralnet_total)
  ensemble_neuralnet_actual <- c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble)
  ensemble_neuralnet_actual_total <- c(ensemble_neuralnet_actual, ensemble_neuralnet_actual_total)
  ensemble_neuralnet_t_test_t[i] <- as.numeric(t.test(x = y_hat_ensemble_neuralnet, y = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), var.equal = TRUE)[1])
  ensemble_neuralnet_t_test_p_value[i] <- as.numeric(t.test(x = y_hat_ensemble_neuralnet, y = c(ensemble_test$y_ensemble, validation$y), var.equal = TRUE)[3])
  ensemble_neuralnet_t_test_t_mean <- mean(as.numeric(ensemble_neuralnet_t_test_t))
  ensemble_neuralnet_t_test_p_value_mean <- mean(as.numeric(ensemble_neuralnet_t_test_p_value))
  ensemble_neuralnet_t_test_p_value_sd <- sd(as.numeric(ensemble_neuralnet_t_test_p_value))
  ensemble_neuralnet_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_neuralnet_test_predict_value, ensemble_neuralnet_validation_predict_value))
  ensemble_neuralnet_bias_mean <- mean(ensemble_neuralnet_bias)
  ensemble_neuralnet_bias_sd <- sd(ensemble_neuralnet_bias)
  ensemble_neuralnet_ks_p_value[i] <- stats::ks.test(x = y_hat_ensemble_neuralnet, y = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), exact = TRUE)$p.value
  ensemble_neuralnet_ks_p_value_mean <- mean(ensemble_neuralnet_ks_p_value)
  ensemble_neuralnet_ks_p_value_sd <- sd(ensemble_neuralnet_ks_p_value)
  ensemble_neuralnet_ks_stat[i] <- stats::ks.test(x = y_hat_ensemble_neuralnet, y = c(test$y, validation$y), exact = TRUE)$statistic
  ensemble_neuralnet_ks_stat_mean <- mean(ensemble_neuralnet_ks_stat)
  ensemble_neuralnet_ks_test <- c(ensemble_neuralnet_ks_stat_mean, ensemble_neuralnet_ks_p_value_mean)
  ensemble_neuralnet_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_neuralnet_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_neuralnet_lower_95 <- ensemble_neuralnet_holdout_RMSE_mean - ensemble_neuralnet_margin
  ensemble_neuralnet_upper_95 <- ensemble_neuralnet_holdout_RMSE_mean + ensemble_neuralnet_margin
  ensemble_neuralnet_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_neuralnet_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_neuralnet_overfitting_lower_95 <- ensemble_neuralnet_overfitting_mean - ensemble_neuralnet_overfitting_margin
  ensemble_neuralnet_overfitting_upper_95 <- ensemble_neuralnet_overfitting_mean + ensemble_neuralnet_overfitting_margin

  ensemble_neuralnet_end <- Sys.time()
  ensemble_neuralnet_duration[i] <- ensemble_neuralnet_end - ensemble_neuralnet_start
  ensemble_neuralnet_duration_mean <- mean(ensemble_neuralnet_duration)
  ensemble_neuralnet_duration_sd <- sd(ensemble_neuralnet_duration)

  #### Model # 29 Ensembles Using Ridge ####
  ensemble_ridge_start <- Sys.time()
  message("Working on Ensemble Ridge")

  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_ridge_model <- glmnet(ensemble_x, ensemble_y, alpha = 0)
  ensemble_ridge_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 0)
  ensemble_best_ridge_lambda <- ensemble_ridge_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_best_ridge_model <- glmnet(ensemble_x, ensemble_y, alpha = 0, lambda = ensemble_best_ridge_lambda)
  }
  if(set_seed == "N"){
    ensemble_best_ridge_model <- glmnet(ensemble_x, ensemble_y, alpha = 0, lambda = ensemble_best_ridge_lambda)
  }
  ensemble_ridge_train_RMSE <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))))
  ensemble_ridge_train_RMSE_df <- rbind(ensemble_ridge_train_RMSE_df, ensemble_ridge_train_RMSE)
  ensemble_ridge_train_RMSE_mean <- mean(ensemble_ridge_train_RMSE_df$ensemble_ridge_train_RMSE[2:nrow(ensemble_ridge_train_RMSE_df)])
  ensemble_ridge_test_RMSE <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_ridge_test_RMSE_df <- rbind(ensemble_ridge_test_RMSE_df, ensemble_ridge_test_RMSE)
  ensemble_ridge_test_RMSE_mean <- mean(ensemble_ridge_test_RMSE_df$ensemble_ridge_test_RMSE[2:nrow(ensemble_ridge_test_RMSE_df)])
  ensemble_ridge_test_predict_value <- rowMeans(predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble))))
  ensemble_ridge_test_predict_value_mean <- mean(ensemble_ridge_test_predict_value)
  ensemble_ridge_test_predict_value_sd <- sd(ensemble_ridge_test_predict_value)
  ensemble_ridge_test_predict_value_sd_mean <- mean(ensemble_ridge_test_predict_value_sd, na.rm = TRUE)
  ## ridge using the validation data set
  ensemble_y <- ensemble_train$y_ensemble
  ensemble_x <- data.matrix(ensemble_train %>% dplyr::select(-y_ensemble))
  ensemble_ridge_model <- glmnet(ensemble_x, ensemble_y, alpha = 0)
  ensemble_ridge_cv <- glmnet::cv.glmnet(ensemble_x, ensemble_y, alpha = 0)
  ensemble_best_ridge_lambda <- ensemble_ridge_cv$lambda.min
  ensemble_best_ridge_model <- glmnet(ensemble_x, ensemble_y, alpha = 0, lambda = ensemble_best_ridge_lambda)
  ensemble_ridge_validation_pred <- predict(ensemble_best_ridge_model, s = ensemble_best_ridge_lambda, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble)))
  ensemble_ridge_validation_RMSE <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_ridge_validation_RMSE_df <- rbind(ensemble_ridge_validation_RMSE_df, ensemble_ridge_validation_RMSE)
  ensemble_ridge_validation_RMSE_mean <- mean(ensemble_ridge_validation_RMSE_df$ensemble_ridge_validation_RMSE[2:nrow(ensemble_ridge_validation_RMSE_df)])
  ensemble_ridge_validation_predict_value <- rowMeans(predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_validation %>% dplyr::select(-y_ensemble))))
  ensemble_ridge_validation_predict_value_mean <- mean(ensemble_ridge_validation_predict_value)
  ensemble_ridge_validation_predict_value_sd <- round(sd(ensemble_ridge_validation_pred), 4)
  ensemble_ridge_validation_predict_value_sd_df <- rbind(ensemble_ridge_validation_predict_value_sd_df, ensemble_ridge_validation_predict_value_sd)
  ensemble_ridge_validation_predict_value_sd_mean <- mean(ensemble_ridge_validation_predict_value_sd_df$ensemble_ridge_validation_predict_value_sd[2:nrow(ensemble_ridge_validation_predict_value_sd_df)])
  ensemble_ridge_holdout_RMSE[i] <- mean(c(ensemble_ridge_test_RMSE, ensemble_ridge_validation_RMSE))
  ensemble_ridge_holdout_RMSE_mean <- mean(ensemble_ridge_holdout_RMSE)
  ensemble_ridge_holdout_RMSE_sd_mean <- mean(sd(c(ensemble_ridge_test_RMSE, ensemble_ridge_validation_RMSE)))
  ensemble_ridge_test_sd <- sd(ensemble_ridge_test_predict_value, na.rm = TRUE)
  ensemble_ridge_test_sd_df <- rbind(ensemble_ridge_test_sd_df, ensemble_ridge_test_sd)
  ensemble_ridge_test_sd_mean <- mean(ensemble_ridge_test_sd_df$ensemble_ridge_test_sd[2:nrow(ensemble_ridge_test_sd_df)])
  ensemble_ridge_validation_sd <- sd(ensemble_ridge_validation_predict_value, na.rm = TRUE)
  ensemble_ridge_validation_sd_df <- rbind(ensemble_ridge_validation_sd_df, ensemble_ridge_validation_sd)
  ensemble_ridge_validation_sd_mean <- mean(ensemble_ridge_validation_sd_df$ensemble_ridge_validation_sd[2:nrow(ensemble_ridge_validation_sd_df)])
  ensemble_ridge_overfitting <- ensemble_ridge_holdout_RMSE_mean / ensemble_ridge_train_RMSE_mean
  ensemble_ridge_overfitting_df <- rbind(ensemble_ridge_overfitting_df, ensemble_ridge_overfitting)
  ensemble_ridge_overfitting_mean <- mean(ensemble_ridge_overfitting_df$ensemble_ridge_overfitting[2:nrow(ensemble_ridge_overfitting_df)])
  ensemble_ridge_overfitting_range <- range(ensemble_ridge_overfitting_df$ensemble_ridge_overfitting[2:nrow(ensemble_ridge_overfitting_df)])
  ensemble_ridge_overfitting_sd <- sd(ensemble_ridge_overfitting_df$ensemble_ridge_overfitting)
  ensemble_ridge_predict_value_mean <- mean(c(ensemble_ridge_test_predict_value_mean, ensemble_ridge_validation_predict_value_mean))
  ensemble_ridge_sd[i] <- mean(ensemble_ridge_test_sd_mean, ensemble_ridge_validation_sd_mean)
  ensemble_ridge_sd_mean <- mean(ensemble_ridge_sd)
  ensemble_y_hat_ridge <- predict(object = ensemble_best_ridge_model, newx = data.matrix(ensemble_test %>% dplyr::select(-y_ensemble)))
  ensemble_y_hat_ridge <- c(ensemble_ridge_test_predict_value, ensemble_ridge_validation_predict_value)
  ensemble_y_hat_ridge_total <- c(ensemble_y_hat_ridge, ensemble_y_hat_ridge_total)
  ensemble_ridge_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_ridge_actual_total <- c(ensemble_ridge_actual, ensemble_ridge_actual_total)
  ensemble_ridge_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_ridge, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_ridge_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_ridge, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_ridge_t_test_t_mean <- mean(as.numeric(ensemble_ridge_t_test_t))
  ensemble_ridge_t_test_p_value_mean <- mean(as.numeric(ensemble_ridge_t_test_p_value))
  ensemble_ridge_t_test_p_value_sd <- sd(as.numeric(ensemble_ridge_t_test_p_value))
  ensemble_ridge_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_ridge_test_predict_value, ensemble_ridge_validation_predict_value))
  ensemble_ridge_bias_mean <- mean(ensemble_ridge_bias)
  ensemble_ridge_bias_sd <- sd(ensemble_ridge_bias)
  ensemble_ridge_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_ridge, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_ridge_ks_p_value_mean <- mean(ensemble_ridge_ks_p_value)
  ensemble_ridge_ks_p_value_sd <- sd(ensemble_ridge_ks_p_value)
  ensemble_ridge_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_ridge, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_ridge_ks_stat_mean <- mean(ensemble_ridge_ks_stat)
  ensemble_ridge_ks_test <- c(ensemble_ridge_ks_stat_mean, ensemble_ridge_ks_p_value_mean)
  ensemble_ridge_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_ridge_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_ridge_lower_95 <- ensemble_ridge_holdout_RMSE_mean - ensemble_ridge_margin
  ensemble_ridge_upper_95 <- ensemble_ridge_holdout_RMSE_mean + ensemble_ridge_margin
  ensemble_ridge_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_ridge_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_ridge_overfitting_lower_95 <- ensemble_ridge_overfitting_mean - ensemble_ridge_overfitting_margin
  ensemble_ridge_overfitting_upper_95 <- ensemble_ridge_overfitting_mean + ensemble_ridge_overfitting_margin

  ensemble_ridge_end <- Sys.time()
  ensemble_ridge_duration[i] <- ensemble_ridge_end - ensemble_ridge_start
  ensemble_ridge_duration_mean <- mean(ensemble_ridge_duration)
  ensemble_ridge_duration_sd <- sd(ensemble_ridge_duration)


  #### Model #30: Ensembles Using Rpart ####
  ensemble_rpart_start <- Sys.time()
  message("Working on Ensemble RPart")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_rpart_train_fit <- rpart::rpart(ensemble_train$y_ensemble ~ ., data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_rpart_train_fit <- rpart::rpart(ensemble_train$y_ensemble ~ ., data = ensemble_train)
  }
  ensemble_rpart_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_rpart_train_fit,
    newdata = ensemble_train
  ))
  ensemble_rpart_train_RMSE_mean <- mean(ensemble_rpart_train_RMSE)
  ensemble_rpart_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_rpart_train_fit,
    newdata = ensemble_test
  ))
  ensemble_rpart_test_RMSE_mean <- mean(ensemble_rpart_test_RMSE)
  ensemble_rpart_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_rpart_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_rpart_validation_RMSE_mean <- mean(ensemble_rpart_validation_RMSE)
  ensemble_rpart_holdout_RMSE[i] <- mean(c(ensemble_rpart_test_RMSE_mean, ensemble_rpart_validation_RMSE_mean))
  ensemble_rpart_holdout_RMSE_mean <- mean(ensemble_rpart_holdout_RMSE)
  ensemble_rpart_holdout_RMSE_sd_mean <- sd(c(ensemble_rpart_test_RMSE_mean, ensemble_rpart_validation_RMSE_mean))
  ensemble_rpart_train_predict_value <- as.numeric(predict(
    object = rpart::rpart(y_ensemble ~ ., data = ensemble_train),
    newdata = ensemble_train
  ))
  ensemble_rpart_test_predict_value <- as.numeric(predict(
    object = rpart::rpart(y_ensemble ~ ., data = ensemble_train),
    newdata = ensemble_test
  ))
  ensemble_rpart_validation_predict_value <- as.numeric(predict(
    object = rpart::rpart(y_ensemble ~ ., data = ensemble_train),
    newdata = ensemble_validation
  ))
  ensemble_rpart_predict_value_mean <- mean(c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value))
  ensemble_rpart_sd[i] <- sd(c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value))
  ensemble_rpart_sd_mean <- mean(ensemble_rpart_sd)
  ensemble_rpart_overfitting[i] <- ensemble_rpart_holdout_RMSE_mean / ensemble_rpart_train_RMSE_mean
  ensemble_rpart_overfitting_mean <- mean(ensemble_rpart_overfitting)
  ensemble_rpart_overfitting_range <- range(ensemble_rpart_overfitting)
  ensemble_rpart_overfitting_sd <- sd(ensemble_rpart_overfitting)
  ensemble_y_hat_rpart <- c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value)
  ensemble_y_hat_rpart_total <- c(ensemble_y_hat_rpart, ensemble_y_hat_rpart_total)
  ensemble_rpart_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_rpart_actual_total <- c(ensemble_rpart_actual, ensemble_rpart_actual_total)
  ensemble_rpart_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_rpart, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_rpart_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_rpart, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_rpart_t_test_t_mean <- mean(as.numeric(ensemble_rpart_t_test_t))
  ensemble_rpart_t_test_p_value_mean <- mean(as.numeric(ensemble_rpart_t_test_p_value))
  ensemble_rpart_t_test_p_value_sd <- sd(as.numeric(ensemble_rpart_t_test_p_value))
  ensemble_rpart_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_rpart_test_predict_value, ensemble_rpart_validation_predict_value))
  ensemble_rpart_bias_mean <- mean(ensemble_rpart_bias)
  ensemble_rpart_bias_sd <- sd(ensemble_rpart_bias)
  ensemble_rpart_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_rpart, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_rpart_ks_p_value_mean <- mean(ensemble_rpart_ks_p_value)
  ensemble_rpart_ks_p_value_sd <- sd(ensemble_rpart_ks_p_value)
  ensemble_rpart_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_rpart, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_rpart_ks_stat_mean <- mean(ensemble_rpart_ks_stat)
  ensemble_rpart_ks_test <- c(ensemble_rpart_ks_stat_mean, ensemble_rpart_ks_p_value_mean)
  ensemble_rpart_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_rpart_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_rpart_lower_95 <- ensemble_rpart_holdout_RMSE_mean - ensemble_rpart_margin
  ensemble_rpart_upper_95 <- ensemble_rpart_holdout_RMSE_mean + ensemble_rpart_margin
  ensemble_rpart_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_rpart_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_rpart_overfitting_lower_95 <- ensemble_rpart_overfitting_mean - ensemble_rpart_overfitting_margin
  ensemble_rpart_overfitting_upper_95 <- ensemble_rpart_overfitting_mean + ensemble_rpart_overfitting_margin

  ensemble_rpart_end <- Sys.time()
  ensemble_rpart_duration[i] <- ensemble_rpart_end - ensemble_rpart_start
  ensemble_rpart_duration_mean <- mean(ensemble_rpart_duration)
  ensemble_rpart_duration_sd <- sd(ensemble_rpart_duration)

  #### Model # 31 Ensemble using Support Vector Machines ####
  ensemble_svm_start <- Sys.time()
  message("Working on Ensemble Support Vector Machines (SVM)")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_svm_train_fit <- e1071::tune.svm(x = ensemble_train, y = ensemble_train$y_ensemble, data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_svm_train_fit <- e1071::tune.svm(x = ensemble_train, y = ensemble_train$y_ensemble, data = ensemble_train)
  }
  ensemble_svm_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_train))
  ensemble_svm_train_RMSE_mean <- mean(ensemble_svm_train_RMSE)
  ensemble_svm_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_test))
  ensemble_svm_test_RMSE_mean <- mean(ensemble_svm_test_RMSE)
  ensemble_svm_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_validation))
  ensemble_svm_validation_RMSE_mean <- mean(ensemble_svm_validation_RMSE)
  ensemble_svm_holdout_RMSE[i] <- mean(c(ensemble_svm_test_RMSE_mean, ensemble_svm_validation_RMSE_mean))
  ensemble_svm_holdout_RMSE_mean <- mean(ensemble_svm_holdout_RMSE)
  ensemble_svm_holdout_RMSE_sd_mean <- sd(c(ensemble_svm_test_RMSE_mean, ensemble_svm_validation_RMSE_mean))
  ensemble_svm_train_predict_value <- as.numeric(predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_train))
  ensemble_svm_test_predict_value <- as.numeric(predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_test))
  ensemble_svm_validation_predict_value <- as.numeric(predict(object = ensemble_svm_train_fit$best.model, newdata = ensemble_validation))
  ensemble_svm_predict_value_mean <- mean(c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value))
  ensemble_svm_sd[i] <- sd(c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value))
  ensemble_svm_sd_mean <- mean(ensemble_svm_sd)
  ensemble_svm_overfitting[i] <- ensemble_svm_holdout_RMSE_mean / ensemble_svm_train_RMSE_mean
  ensemble_svm_overfitting_mean <- mean(ensemble_svm_overfitting)
  ensemble_svm_overfitting_range <- range(ensemble_svm_overfitting)
  ensemble_svm_overfitting_sd <- sd(ensemble_svm_overfitting)

  ensemble_y_hat_svm <- c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value)
  ensemble_y_hat_svm_total <- c(ensemble_y_hat_svm, ensemble_y_hat_svm_total)
  ensemble_svm_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_svm_actual_total <- c(ensemble_svm_actual, ensemble_svm_actual_total)
  ensemble_svm_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_svm, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_svm_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_svm, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_svm_t_test_t_mean <- mean(as.numeric(ensemble_svm_t_test_t))
  ensemble_svm_t_test_p_value_mean <- mean(as.numeric(ensemble_svm_t_test_p_value))
  ensemble_svm_t_test_p_value_sd <- sd(as.numeric(ensemble_svm_t_test_p_value))
  ensemble_svm_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_svm_test_predict_value, ensemble_svm_validation_predict_value))
  ensemble_svm_bias_mean <- mean(ensemble_svm_bias)
  ensemble_svm_bias_sd <- sd(ensemble_svm_bias)
  ensemble_svm_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_svm , y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_svm_ks_p_value_mean <- mean(ensemble_svm_ks_p_value)
  ensemble_svm_ks_p_value_sd <- sd(ensemble_svm_ks_p_value)
  ensemble_svm_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_svm , y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_svm_ks_stat_mean <- mean(ensemble_svm_ks_stat)
  ensemble_svm_ks_test <- c(ensemble_svm_ks_stat_mean, ensemble_svm_ks_p_value_mean)
  ensemble_svm_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_svm_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_svm_lower_95 <- ensemble_svm_holdout_RMSE_mean - ensemble_svm_margin
  ensemble_svm_upper_95 <- ensemble_svm_holdout_RMSE_mean + ensemble_svm_margin
  ensemble_svm_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_svm_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_svm_overfitting_lower_95 <- ensemble_svm_overfitting_mean - ensemble_svm_overfitting_margin
  ensemble_svm_overfitting_upper_95 <- ensemble_svm_overfitting_mean + ensemble_svm_overfitting_margin

  ensemble_svm_end <- Sys.time()
  ensemble_svm_duration[i] <- ensemble_svm_end - ensemble_svm_start
  ensemble_svm_duration_mean <- mean(ensemble_svm_duration)
  ensemble_svm_duration_sd <- sd(ensemble_svm_duration)

  #### Model 32: Ensemble Using Trees ####
  ensemble_tree_start <- Sys.time()
  message("Working on Ensemble Trees")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_tree_train_fit <- tree::tree(ensemble_train$y_ensemble ~ ., data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_tree_train_fit <- tree::tree(ensemble_train$y_ensemble ~ ., data = ensemble_train)
  }
  ensemble_tree_train_RMSE[i] <- Metrics::rmse(actual = ensemble_train$y_ensemble, predicted = predict(
    object = ensemble_tree_train_fit,
    newdata = ensemble_train
  ))
  ensemble_tree_train_RMSE_mean <- mean(ensemble_tree_train_RMSE)
  ensemble_tree_test_RMSE[i] <- Metrics::rmse(actual = ensemble_test$y_ensemble, predicted = predict(
    object = ensemble_tree_train_fit,
    newdata = ensemble_test
  ))
  ensemble_tree_test_RMSE_mean <- mean(ensemble_tree_test_RMSE)
  ensemble_tree_validation_RMSE[i] <- Metrics::rmse(actual = ensemble_validation$y_ensemble, predicted = predict(
    object = ensemble_tree_train_fit,
    newdata = ensemble_validation
  ))
  ensemble_tree_validation_RMSE_mean <- mean(ensemble_tree_validation_RMSE)
  ensemble_tree_holdout_RMSE[i] <- mean(c(ensemble_tree_test_RMSE_mean, ensemble_tree_validation_RMSE_mean))
  ensemble_tree_holdout_RMSE_mean <- mean(ensemble_tree_holdout_RMSE)
  ensemble_tree_holdout_RMSE_sd_mean <- sd(c(ensemble_tree_test_RMSE_mean, ensemble_tree_validation_RMSE_mean))
  ensemble_tree_train_predict_value <- predict(object = ensemble_tree_train_fit, newdata = ensemble_train)
  ensemble_tree_test_predict_value <- predict(object = ensemble_tree_train_fit, newdata = ensemble_test)
  ensemble_tree_validation_predict_value <- predict(object = ensemble_tree_train_fit, newdata = ensemble_validation)
  ensemble_tree_predict_value_mean <- mean(c(ensemble_tree_test_predict_value, ensemble_tree_validation_predict_value))
  ensemble_tree_sd_mean <- sd(ensemble_tree_test_predict_value)
  ensemble_tree_overfitting[i] <- ensemble_tree_holdout_RMSE_mean / ensemble_tree_train_RMSE_mean
  ensemble_tree_overfitting_mean <- mean(ensemble_tree_overfitting)
  ensemble_tree_overfitting_range <- range(ensemble_tree_overfitting)
  ensemble_tree_overfitting_sd <- sd(ensemble_tree_overfitting)
  ensemble_y_hat_tree <- c(ensemble_tree_test_predict_value, ensemble_tree_validation_predict_value)
  ensemble_y_hat_tree_total <- c(ensemble_y_hat_tree, ensemble_y_hat_tree_total)
  ensemble_tree_actual <- c(ensemble_test$y, ensemble_validation$y)
  ensemble_tree_actual_total <- c(ensemble_tree_actual, ensemble_tree_actual_total)
  ensemble_tree_t_test_t[i] <- as.numeric(t.test(x = ensemble_y_hat_tree, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[1])
  ensemble_tree_t_test_p_value[i] <- as.numeric(t.test(x = ensemble_y_hat_tree, y = c(ensemble_test$y, ensemble_validation$y), var.equal = TRUE)[3])
  ensemble_tree_t_test_t_mean <- mean(as.numeric(ensemble_tree_t_test_t))
  ensemble_tree_t_test_p_value_mean <- mean(as.numeric(ensemble_tree_t_test_p_value))
  ensemble_tree_t_test_p_value_sd <- sd(as.numeric(ensemble_tree_t_test_p_value))
  ensemble_tree_bias[i] <- Metrics::bias(actual = c(ensemble_test$y_ensemble, ensemble_validation$y_ensemble), predicted = c(ensemble_tree_test_predict_value, ensemble_tree_validation_predict_value))
  ensemble_tree_bias_mean <- mean(ensemble_tree_bias)
  ensemble_tree_bias_sd <- sd(ensemble_tree_bias)
  ensemble_tree_ks_p_value[i] <- stats::ks.test(x = ensemble_y_hat_tree, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$p.value
  ensemble_tree_ks_p_value_mean <- mean(ensemble_tree_ks_p_value)
  ensemble_tree_ks_p_value_sd <- sd(ensemble_tree_ks_p_value)
  ensemble_tree_ks_stat[i] <- stats::ks.test(x = ensemble_y_hat_tree, y = c(ensemble_test$y, ensemble_validation$y), exact = TRUE)$statistic
  ensemble_tree_ks_stat_mean <- mean(ensemble_tree_ks_stat)
  ensemble_tree_ks_test <- c(ensemble_tree_ks_stat_mean, ensemble_tree_ks_p_value_mean)
  ensemble_tree_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_tree_holdout_RMSE_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_tree_lower_95 <- ensemble_tree_holdout_RMSE_mean - ensemble_tree_margin
  ensemble_tree_upper_95 <- ensemble_tree_holdout_RMSE_mean + ensemble_tree_margin
  ensemble_tree_overfitting_margin <- stats::qt(0.975, df = (nrow(test) + nrow(validation)) -1)*ensemble_tree_overfitting_mean/sqrt(nrow(test) + nrow(validation))
  ensemble_tree_overfitting_lower_95 <- ensemble_tree_overfitting_mean - ensemble_tree_overfitting_margin
  ensemble_tree_overfitting_upper_95 <- ensemble_tree_overfitting_mean + ensemble_tree_overfitting_margin

  ensemble_tree_end <- Sys.time()
  ensemble_tree_duration[i] <- ensemble_tree_end - ensemble_tree_start
  ensemble_tree_duration_mean <- mean(ensemble_tree_duration)
  ensemble_tree_duration_sd <- sd(ensemble_tree_duration)

}

#### End of models here ####


#### All summary results start here ####

#### Summary results data frame ####

summary_results <- data.frame(
  "Model" = c(
    "Actual data", "Bagging", "BayesGLM", "BayesRNN",
    "Cubist", "Earth", "Elastic", "GAM", "Gradient Boosted", "Lasso", "Linear", "Neuralnet", "PLS",
    "PCR", "Ridge", "Rpart", "SVM", "Tree", "XGBoost",
    "Ensemble Bagging", "Ensemble BayesGLM", "Ensemble BayesRNN", "Ensemble Cubist",
    "Ensemble Earth", "Ensemble Elastic", "Ensemble Gradient Boosted",
    "Ensemble Lasso", "Ensemble Linear", "Ensemble Neuralnet",  "Ensemble Ridge", "Ensemble Rpart",
    "Ensemble SVM", "Ensemble Trees"
  ),
  "Mean_holdout_RMSE" = round(c(
    actual_RMSE, bagging_holdout_RMSE_mean, bayesglm_holdout_RMSE_mean,
    bayesrnn_holdout_RMSE_mean, cubist_holdout_RMSE_mean, earth_holdout_RMSE_mean, elastic_holdout_RMSE_mean,
    gam_holdout_RMSE_mean, gb_holdout_RMSE_mean, lasso_holdout_RMSE_mean, linear_holdout_RMSE_mean,
    neuralnet_holdout_RMSE_mean, pls_holdout_RMSE_mean, pcr_holdout_RMSE_mean,
    ridge_holdout_RMSE_mean, rpart_holdout_RMSE_mean, svm_holdout_RMSE_mean, tree_holdout_RMSE_mean, xgb_holdout_RMSE_mean,
    ensemble_bagging_holdout_RMSE_mean, ensemble_bayesglm_holdout_RMSE_mean,
    ensemble_bayesrnn_holdout_RMSE_mean, ensemble_cubist_holdout_RMSE_mean, ensemble_earth_holdout_RMSE_mean,
    ensemble_elastic_holdout_RMSE_mean, ensemble_gb_holdout_RMSE_mean,
    ensemble_lasso_holdout_RMSE_mean, ensemble_linear_holdout_RMSE_mean, ensemble_neuralnet_holdout_RMSE_mean,
    ensemble_ridge_holdout_RMSE_mean, ensemble_rpart_holdout_RMSE_mean,
    ensemble_svm_holdout_RMSE_mean, ensemble_tree_holdout_RMSE_mean
  ), 4),
  "RMSE_Lower_95_Conf_Int" = round(c(
    actual_RMSE, bagging_lower_95, bayesglm_lower_95,
    bayesrnn_lower_95, cubist_lower_95, earth_lower_95, elastic_lower_95,
    gam_lower_95, gb_lower_95, lasso_lower_95, linear_lower_95,
    neuralnet_lower_95, pls_lower_95, pcr_lower_95,
    ridge_lower_95, rpart_lower_95, svm_lower_95, tree_lower_95, xgb_lower_95,
    ensemble_bagging_lower_95, ensemble_bayesglm_lower_95,
    ensemble_bayesrnn_lower_95, ensemble_cubist_lower_95, ensemble_earth_lower_95,
    ensemble_elastic_lower_95, ensemble_gb_lower_95,
    ensemble_lasso_lower_95, ensemble_linear_lower_95, ensemble_neuralnet_lower_95,
    ensemble_ridge_lower_95, ensemble_rpart_lower_95,
    ensemble_svm_lower_95, ensemble_tree_lower_95
  ), 4),
  "RMSE_Upper_95_Conf_Int" = round(c(
    actual_RMSE, bagging_upper_95, bayesglm_upper_95,
    bayesrnn_upper_95, cubist_upper_95, earth_upper_95, elastic_upper_95,
    gam_upper_95, gb_upper_95, lasso_upper_95, linear_upper_95,
    neuralnet_upper_95, pls_upper_95, pcr_upper_95,
    ridge_upper_95, rpart_upper_95, svm_upper_95, tree_upper_95, xgb_upper_95,
    ensemble_bagging_upper_95, ensemble_bayesglm_upper_95,
    ensemble_bayesrnn_upper_95, ensemble_cubist_upper_95, ensemble_earth_upper_95,
    ensemble_elastic_upper_95, ensemble_gb_upper_95,
    ensemble_lasso_upper_95, ensemble_linear_upper_95, ensemble_neuralnet_upper_95,
    ensemble_ridge_upper_95, ensemble_rpart_upper_95,
    ensemble_svm_upper_95, ensemble_tree_upper_95
  ), 4),
  "RMSE_Std_Dev" = round(c(
    actual_RMSE, bagging_holdout_RMSE_sd_mean, bayesglm_holdout_RMSE_sd_mean,
    bayesrnn_holdout_RMSE_sd_mean, cubist_holdout_RMSE_sd_mean, earth_holdout_RMSE_sd_mean, elastic_holdout_RMSE_sd_mean,
    gam_holdout_RMSE_sd_mean, gb_holdout_RMSE_sd_mean, lasso_holdout_RMSE_sd_mean, linear_holdout_RMSE_sd_mean,
    neuralnet_holdout_RMSE_sd_mean, pls_holdout_RMSE_sd_mean, pcr_holdout_RMSE_sd_mean,
    ridge_holdout_RMSE_sd_mean, rpart_holdout_RMSE_sd_mean, svm_holdout_RMSE_sd_mean, tree_holdout_RMSE_sd_mean, xgb_holdout_RMSE_sd_mean,
    ensemble_bagging_holdout_RMSE_sd_mean, ensemble_bayesglm_holdout_RMSE_sd_mean,
    ensemble_bayesrnn_holdout_RMSE_sd_mean, ensemble_cubist_holdout_RMSE_sd_mean, ensemble_earth_holdout_RMSE_sd_mean,
    ensemble_elastic_holdout_RMSE_sd_mean, ensemble_gb_holdout_RMSE_sd_mean,
    ensemble_lasso_holdout_RMSE_sd_mean, ensemble_linear_holdout_RMSE_sd_mean, ensemble_neuralnet_holdout_RMSE_sd_mean,
    ensemble_ridge_holdout_RMSE_sd_mean, ensemble_rpart_holdout_RMSE_sd_mean,
    ensemble_svm_holdout_RMSE_sd_mean, ensemble_tree_holdout_RMSE_sd_mean
  ), 4),
  "Overfitting_mean" = round(c(
    0, bagging_overfitting_mean, bayesglm_overfitting_mean, bayesrnn_overfitting_mean,
    cubist_overfitting_mean, earth_overfitting_mean, elastic_overfitting_mean, gam_overfitting_mean, gb_overfitting_mean,
    lasso_overfitting_mean, linear_overfitting_mean, neuralnet_overfitting_mean,
    pls_overfitting_mean, pcr_overfitting_mean, ridge_overfitting_mean,
    rpart_overfitting_mean, svm_overfitting_mean, tree_overfitting_mean, xgb_overfitting_mean,
    ensemble_bagging_overfitting_mean,  ensemble_bayesglm_overfitting_mean,
    ensemble_bayesrnn_overfitting_mean, ensemble_cubist_overfitting_mean, ensemble_earth_overfitting_mean,
    ensemble_elastic_overfitting_mean, ensemble_gb_overfitting_mean,
    ensemble_lasso_overfitting_mean, ensemble_linear_overfitting_mean, ensemble_neuralnet_overfitting_mean, ensemble_ridge_overfitting_mean,
    ensemble_rpart_overfitting_mean, ensemble_svm_overfitting_mean, ensemble_tree_overfitting_mean
  ), 4),
  "Overfitting_lower_95_CI" = round(c(
    0, bagging_overfitting_lower_95, bayesglm_overfitting_lower_95, bayesrnn_overfitting_lower_95,
    cubist_overfitting_lower_95, earth_overfitting_lower_95, elastic_overfitting_lower_95, gam_overfitting_lower_95, gb_overfitting_lower_95,
    lasso_overfitting_lower_95, linear_overfitting_lower_95, neuralnet_overfitting_lower_95,
    pls_overfitting_lower_95, pcr_overfitting_lower_95, ridge_overfitting_lower_95,
    rpart_overfitting_lower_95, svm_overfitting_lower_95, tree_overfitting_lower_95, xgb_overfitting_lower_95,
    ensemble_bagging_overfitting_lower_95,  ensemble_bayesglm_overfitting_lower_95,
    ensemble_bayesrnn_overfitting_lower_95, ensemble_cubist_overfitting_lower_95, ensemble_earth_overfitting_lower_95,
    ensemble_elastic_overfitting_lower_95, ensemble_gb_overfitting_lower_95,
    ensemble_lasso_overfitting_lower_95, ensemble_linear_overfitting_lower_95, ensemble_neuralnet_overfitting_lower_95, ensemble_ridge_overfitting_lower_95,
    ensemble_rpart_overfitting_lower_95, ensemble_svm_overfitting_lower_95, ensemble_tree_overfitting_lower_95
  ), 4),
  "Overfitting_upper_95_CI" = round(c(
    0, bagging_overfitting_upper_95, bayesglm_overfitting_upper_95, bayesrnn_overfitting_upper_95,
    cubist_overfitting_upper_95, earth_overfitting_upper_95, elastic_overfitting_upper_95, gam_overfitting_upper_95, gb_overfitting_upper_95,
    lasso_overfitting_upper_95, linear_overfitting_upper_95, neuralnet_overfitting_upper_95,
    pls_overfitting_upper_95, pcr_overfitting_upper_95, ridge_overfitting_upper_95,
    rpart_overfitting_upper_95, svm_overfitting_upper_95, tree_overfitting_upper_95, xgb_overfitting_upper_95,
    ensemble_bagging_overfitting_upper_95,  ensemble_bayesglm_overfitting_upper_95,
    ensemble_bayesrnn_overfitting_upper_95, ensemble_cubist_overfitting_upper_95, ensemble_earth_overfitting_upper_95,
    ensemble_elastic_overfitting_upper_95, ensemble_gb_overfitting_upper_95,
    ensemble_lasso_overfitting_upper_95, ensemble_linear_overfitting_upper_95, ensemble_neuralnet_overfitting_upper_95, ensemble_ridge_overfitting_upper_95,
    ensemble_rpart_overfitting_upper_95, ensemble_svm_overfitting_upper_95, ensemble_tree_overfitting_upper_95
  ), 4),
  "Overfitting_sd" = round(c(
    0, bagging_overfitting_sd, bayesglm_overfitting_sd, bayesrnn_overfitting_sd,
    cubist_overfitting_sd, earth_overfitting_sd, elastic_overfitting_sd, gam_overfitting_sd, gb_overfitting_sd,
    lasso_overfitting_sd, linear_overfitting_sd, neuralnet_overfitting_sd,
    pls_overfitting_sd, pcr_overfitting_sd, ridge_overfitting_sd,
    rpart_overfitting_sd, svm_overfitting_sd, tree_overfitting_sd, xgb_overfitting_sd,
    ensemble_bagging_overfitting_sd,  ensemble_bayesglm_overfitting_sd,
    ensemble_bayesrnn_overfitting_sd, ensemble_cubist_overfitting_sd, ensemble_earth_overfitting_sd,
    ensemble_elastic_overfitting_sd, ensemble_gb_overfitting_sd,
    ensemble_lasso_overfitting_sd, ensemble_linear_overfitting_sd, ensemble_neuralnet_overfitting_sd, ensemble_ridge_overfitting_sd,
    ensemble_rpart_overfitting_sd, ensemble_svm_overfitting_sd, ensemble_tree_overfitting_sd
  ), 4),
  "t-test" = round(c(
    0, bagging_t_test_t_mean, bayesglm_t_test_t_mean,
    bayesrnn_t_test_t_mean, cubist_t_test_t_mean, earth_t_test_t_mean, elastic_t_test_t_mean,
    gam_t_test_t_mean, gb_t_test_t_mean, lasso_t_test_t_mean, linear_t_test_t_mean,
    neuralnet_t_test_t_mean, pls_t_test_t_mean, pcr_t_test_t_mean,
    ridge_t_test_t_mean, rpart_t_test_t_mean, svm_t_test_t_mean, tree_t_test_t_mean, xgb_t_test_t_mean,
    ensemble_bagging_t_test_t_mean,  ensemble_bayesglm_t_test_t_mean,
    ensemble_bayesrnn_t_test_t_mean, ensemble_cubist_t_test_t_mean, ensemble_earth_t_test_t_mean,
    ensemble_elastic_t_test_t_mean, ensemble_gb_t_test_t_mean,
    ensemble_lasso_t_test_t_mean, ensemble_linear_t_test_t_mean, ensemble_neuralnet_t_test_t_mean,
    ensemble_ridge_t_test_t_mean, ensemble_rpart_t_test_t_mean,
    ensemble_svm_t_test_t_mean, ensemble_tree_t_test_t_mean
  ), 4),
  "t_test_p-value_mean" = round(c(
    0, bagging_t_test_p_value_mean, bayesglm_t_test_p_value_mean,
    bayesrnn_t_test_p_value_mean, cubist_t_test_p_value_mean, earth_t_test_p_value_mean, elastic_t_test_p_value_mean,
    gam_t_test_p_value_mean, gb_t_test_p_value_mean, lasso_t_test_p_value_mean, linear_t_test_p_value_mean,
    neuralnet_t_test_p_value_mean, pls_t_test_p_value_mean, pcr_t_test_p_value_mean,
    ridge_t_test_p_value_mean, rpart_t_test_p_value_mean, svm_t_test_p_value_mean, tree_t_test_p_value_mean, xgb_t_test_p_value_mean,
    ensemble_bagging_t_test_p_value_mean,  ensemble_bayesglm_t_test_p_value_mean,
    ensemble_bayesrnn_t_test_p_value_mean, ensemble_cubist_t_test_p_value_mean, ensemble_earth_t_test_p_value_mean,
    ensemble_elastic_t_test_p_value_mean, ensemble_gb_t_test_p_value_mean,
    ensemble_lasso_t_test_p_value_mean, ensemble_linear_t_test_p_value_mean, ensemble_neuralnet_t_test_p_value_mean,
    ensemble_ridge_t_test_p_value_mean, ensemble_rpart_t_test_p_value_mean,
    ensemble_svm_t_test_p_value_mean, ensemble_tree_t_test_p_value_mean
  ), 4),
  "t_test_p_value_sd" = round(c(
    0, bagging_t_test_p_value_sd, bayesglm_t_test_p_value_sd,
    bayesrnn_t_test_p_value_sd, cubist_t_test_p_value_sd, earth_t_test_p_value_sd, elastic_t_test_p_value_sd,
    gam_t_test_p_value_sd, gb_t_test_p_value_sd, lasso_t_test_p_value_sd, linear_t_test_p_value_sd,
    neuralnet_t_test_p_value_sd, pls_t_test_p_value_sd, pcr_t_test_p_value_sd,
    ridge_t_test_p_value_sd, rpart_t_test_p_value_sd, svm_t_test_p_value_sd, tree_t_test_p_value_sd, xgb_t_test_p_value_sd,
    ensemble_bagging_t_test_p_value_sd,  ensemble_bayesglm_t_test_p_value_sd,
    ensemble_bayesrnn_t_test_p_value_sd, ensemble_cubist_t_test_p_value_sd, ensemble_earth_t_test_p_value_sd,
    ensemble_elastic_t_test_p_value_sd, ensemble_gb_t_test_p_value_sd,
    ensemble_lasso_t_test_p_value_sd, ensemble_linear_t_test_p_value_sd, ensemble_neuralnet_t_test_p_value_sd,
    ensemble_ridge_t_test_p_value_sd, ensemble_rpart_t_test_p_value_sd,
    ensemble_svm_t_test_p_value_sd, ensemble_tree_t_test_p_value_sd
  ), 4),
  "KS_Test_Stat_mean" = round(c(
    0, bagging_ks_stat_mean, bayesglm_ks_stat_mean,
    bayesrnn_ks_stat_mean, cubist_ks_stat_mean, earth_ks_stat_mean, elastic_ks_stat_mean,
    gam_ks_stat_mean, gb_ks_stat_mean, lasso_ks_stat_mean, linear_ks_stat_mean,
    neuralnet_ks_stat_mean, pls_ks_stat_mean, pcr_ks_stat_mean,
    ridge_ks_stat_mean, rpart_ks_stat_mean, svm_ks_stat_mean, tree_ks_stat_mean, xgb_ks_stat_mean,
    ensemble_bagging_ks_stat_mean,  ensemble_bayesglm_ks_stat_mean,
    ensemble_bayesrnn_ks_stat_mean, ensemble_cubist_ks_stat_mean, ensemble_earth_ks_stat_mean,
    ensemble_elastic_ks_stat_mean, ensemble_gb_ks_stat_mean,
    ensemble_lasso_ks_stat_mean, ensemble_linear_ks_stat_mean, ensemble_neuralnet_ks_stat_mean,
    ensemble_ridge_ks_stat_mean, ensemble_rpart_ks_stat_mean,
    ensemble_svm_ks_stat_mean, ensemble_tree_ks_stat_mean
  ), 4),
  "KS_Test_P_Value_mean" = round(c(
    0, bagging_ks_p_value_mean, bayesglm_ks_p_value_mean,
    bayesrnn_ks_p_value_mean, cubist_ks_p_value_mean, earth_ks_p_value_mean, elastic_ks_p_value_mean,
    gam_ks_p_value_mean, gb_ks_p_value_mean, lasso_ks_p_value_mean, linear_ks_p_value_mean,
    neuralnet_ks_p_value_mean, pls_ks_p_value_mean, pcr_ks_p_value_mean,
    ridge_ks_p_value_mean, rpart_ks_p_value_mean, svm_ks_p_value_mean, tree_ks_p_value_mean, xgb_ks_p_value_mean,
    ensemble_bagging_ks_p_value_mean,  ensemble_bayesglm_ks_p_value_mean,
    ensemble_bayesrnn_ks_p_value_mean, ensemble_cubist_ks_p_value_mean, ensemble_earth_ks_p_value_mean,
    ensemble_elastic_ks_p_value_mean, ensemble_gb_ks_p_value_mean,
    ensemble_lasso_ks_p_value_mean, ensemble_linear_ks_p_value_mean, ensemble_neuralnet_ks_p_value_mean,
    ensemble_ridge_ks_p_value_mean, ensemble_rpart_ks_p_value_mean,
    ensemble_svm_ks_p_value_mean, ensemble_tree_ks_p_value_mean
  ), 4),
  "KS_Test_P_Value_std_dev" = round(c(
    0, bagging_ks_p_value_sd, bayesglm_ks_p_value_sd,
    bayesrnn_ks_p_value_sd, cubist_ks_p_value_sd, earth_ks_p_value_sd, elastic_ks_p_value_sd,
    gam_ks_p_value_sd, gb_ks_p_value_sd, lasso_ks_p_value_sd, linear_ks_p_value_sd,
    neuralnet_ks_p_value_sd, pls_ks_p_value_sd, pcr_ks_p_value_sd,
    ridge_ks_p_value_sd, rpart_ks_p_value_sd, svm_ks_p_value_sd, tree_ks_p_value_sd, xgb_ks_p_value_sd,
    ensemble_bagging_ks_p_value_sd,  ensemble_bayesglm_ks_p_value_sd,
    ensemble_bayesrnn_ks_p_value_sd, ensemble_cubist_ks_p_value_sd, ensemble_earth_ks_p_value_sd,
    ensemble_elastic_ks_p_value_sd, ensemble_gb_ks_p_value_sd,
    ensemble_lasso_ks_p_value_sd, ensemble_linear_ks_p_value_sd, ensemble_neuralnet_ks_p_value_sd,
    ensemble_ridge_ks_p_value_sd, ensemble_rpart_ks_p_value_sd,
    ensemble_svm_ks_p_value_sd, ensemble_tree_ks_p_value_sd
  ), 4),
  "Bias" = round(c(
    0, bagging_bias_mean, bayesglm_bias_mean,
    bayesrnn_bias_mean, cubist_bias_mean, earth_bias_mean, elastic_bias_mean,
    gam_bias_mean, gb_bias_mean, lasso_bias_mean, linear_bias_mean,
    neuralnet_bias_mean, pls_bias_mean, pcr_bias_mean,
    ridge_bias_mean, rpart_bias_mean, svm_bias_mean, tree_bias_mean, xgb_bias_mean,
    ensemble_bagging_bias_mean, ensemble_bayesglm_bias_mean,
    ensemble_bayesrnn_bias_mean, ensemble_cubist_bias_mean, ensemble_earth_bias_mean,
    ensemble_elastic_bias_mean, ensemble_gb_bias_mean,
    ensemble_lasso_bias_mean, ensemble_linear_bias_mean, ensemble_neuralnet_bias_mean,
    ensemble_ridge_bias_mean, ensemble_rpart_bias_mean,
    ensemble_svm_bias_mean, ensemble_tree_bias_mean
  ), 4),
  "Bias_sd" = round(c(
    0, bagging_bias_sd, bayesglm_bias_sd,
    bayesrnn_bias_sd, cubist_bias_sd, earth_bias_sd, elastic_bias_sd,
    gam_bias_sd, gb_bias_sd, lasso_bias_sd, linear_bias_sd,
    neuralnet_bias_sd, pls_bias_sd, pcr_bias_sd,
    ridge_bias_sd, rpart_bias_sd, svm_bias_sd, tree_bias_sd, xgb_bias_sd,
    ensemble_bagging_bias_sd, ensemble_bayesglm_bias_sd,
    ensemble_bayesrnn_bias_sd, ensemble_cubist_bias_sd, ensemble_earth_bias_sd,
    ensemble_elastic_bias_sd, ensemble_gb_bias_sd,
    ensemble_lasso_bias_sd, ensemble_linear_bias_sd, ensemble_neuralnet_bias_sd,
    ensemble_ridge_bias_sd, ensemble_rpart_bias_sd,
    ensemble_svm_bias_sd, ensemble_tree_bias_sd
  ), 4),
  "Mean_data" = round(c(
    actual_mean, bagging_predict_value_mean, bayesglm_predict_value_mean,
    bayesrnn_predict_value_mean, cubist_predict_value_mean, earth_predict_value_mean, elastic_test_predict_value_mean,
    gam_predict_value_mean, gb_predict_value_mean, lasso_predict_value_mean, linear_predict_value_mean,
    neuralnet_test_predict_value_mean, pls_predict_value_mean, pcr_predict_value_mean,
    ridge_test_predict_value_mean, rpart_predict_value_mean, svm_predict_value_mean, tree_predict_value_mean, xgb_predict_value_mean,
    ensemble_bagging_predict_value_mean, ensemble_bayesglm_predict_value_mean,
    ensemble_bayesrnn_predict_value_mean, ensemble_cubist_predict_value_mean, ensemble_earth_predict_value_mean,
    ensemble_elastic_predict_value_mean, ensemble_gb_predict_value_mean,
    ensemble_lasso_predict_value_mean, ensemble_linear_predict_value_mean, ensemble_neuralnet_predict_value_mean, ensemble_ridge_predict_value_mean,
    ensemble_rpart_predict_value_mean, ensemble_svm_predict_value_mean, ensemble_tree_predict_value_mean
  ), 4),
  "Std_Dev_of_the_model" = round(c(
    actual_sd, bagging_sd_mean, bayesglm_sd_mean, bayesrnn_sd_mean,
    cubist_sd_mean, earth_sd_mean, elastic_sd_mean, gam_sd_mean, gb_sd_mean, lasso_sd_mean,
    linear_sd_mean, neuralnet_sd_mean, pls_sd_mean, pcr_sd_mean, ridge_sd_mean,
    rpart_sd_mean, svm_sd_mean, tree_sd_mean, xgb_sd_mean,
    ensemble_bagging_sd_mean, ensemble_bayesglm_sd_mean,
    ensemble_bayesrnn_sd_mean, ensemble_cubist_sd_mean, ensemble_earth_sd_mean,
    ensemble_elastic_sd_mean, ensemble_gb_sd_mean,
    ensemble_lasso_sd_mean, ensemble_linear_sd_mean, ensemble_neuralnet_sd_mean, ensemble_ridge_sd_mean,
    ensemble_rpart_sd_mean, ensemble_svm_sd_mean, ensemble_tree_sd_mean
  ), 4),
  "Mean_train_RMSE" = round(c(
    0, bagging_train_RMSE_mean, bayesglm_train_RMSE_mean, bayesrnn_train_RMSE_mean,
    cubist_train_RMSE_mean, earth_train_RMSE_mean, elastic_train_RMSE_mean, gam_train_RMSE_mean, gb_train_RMSE_mean,
    lasso_train_RMSE_mean, linear_train_RMSE_mean, neuralnet_train_RMSE_mean,
    pls_train_RMSE_mean, pcr_train_RMSE_mean, ridge_train_RMSE_mean,
    rpart_train_RMSE_mean, svm_train_RMSE_mean, tree_train_RMSE_mean, xgb_train_RMSE_mean,
    ensemble_bagging_train_RMSE_mean,ensemble_bayesglm_train_RMSE_mean,
    ensemble_bayesrnn_train_RMSE_mean, ensemble_cubist_train_RMSE_mean, ensemble_earth_train_RMSE_mean,
    ensemble_elastic_train_RMSE_mean, ensemble_gb_train_RMSE_mean,
    ensemble_lasso_train_RMSE_mean, ensemble_linear_train_RMSE_mean, ensemble_neuralnet_train_RMSE_mean, ensemble_ridge_train_RMSE_mean,
    ensemble_rpart_train_RMSE_mean, ensemble_svm_train_RMSE_mean, ensemble_tree_train_RMSE_mean
  ), 4),
  "Mean_test_RMSE" = round(c(
    0, bagging_test_RMSE_mean, bayesglm_test_RMSE_mean,
    bayesrnn_test_RMSE_mean, cubist_test_RMSE_mean, earth_test_RMSE_mean, elastic_test_RMSE_mean,
    gam_test_RMSE_mean, gb_test_RMSE_mean, lasso_test_RMSE_mean, linear_test_RMSE_mean,
    neuralnet_test_RMSE_mean, pls_test_RMSE_mean, pcr_test_RMSE_mean, ridge_test_RMSE_mean,
    rpart_test_RMSE_mean, svm_test_RMSE_mean, tree_test_RMSE_mean, xgb_test_RMSE_mean,
    ensemble_bagging_test_RMSE_mean, ensemble_bayesglm_test_RMSE_mean,
    ensemble_bayesrnn_test_RMSE_mean, ensemble_cubist_test_RMSE_mean, ensemble_earth_test_RMSE_mean,
    ensemble_elastic_test_RMSE_mean, ensemble_gb_test_RMSE_mean,
    ensemble_lasso_test_RMSE_mean, ensemble_linear_test_RMSE_mean, ensemble_neuralnet_test_RMSE_mean,  ensemble_ridge_test_RMSE,
    ensemble_rpart_test_RMSE_mean, ensemble_svm_test_RMSE_mean, ensemble_tree_test_RMSE_mean
  ), 4),
  "Mean_validation_RMSE" = round(c(
    0, bagging_validation_RMSE_mean, bayesglm_validation_RMSE_mean,
    bayesrnn_validation_RMSE_mean, cubist_validation_RMSE_mean, earth_validation_RMSE_mean, elastic_validation_RMSE_mean,
    gam_validation_RMSE_mean, gb_validation_RMSE_mean, lasso_validation_RMSE_mean,
    linear_validation_RMSE_mean, neuralnet_validation_RMSE_mean, pls_validation_RMSE_mean,
    pcr_validation_RMSE_mean, ridge_validation_RMSE_mean,
    rpart_validation_RMSE_mean, svm_validation_RMSE_mean, tree_validation_RMSE_mean, xgb_validation_RMSE_mean,
    ensemble_bagging_validation_RMSE_mean, ensemble_bayesglm_validation_RMSE_mean,
    ensemble_bayesrnn_validation_RMSE_mean, ensemble_cubist_validation_RMSE_mean, ensemble_earth_validation_RMSE_mean,
    ensemble_elastic_validation_RMSE_mean, ensemble_gb_validation_RMSE_mean,
    ensemble_lasso_validation_RMSE_mean, ensemble_linear_validation_RMSE_mean, ensemble_neuralnet_validation_RMSE_mean, ensemble_ridge_validation_RMSE_mean,
    ensemble_rpart_validation_RMSE_mean, ensemble_svm_validation_RMSE_mean, ensemble_tree_validation_RMSE_mean
  ), 4),
  "Duration" = round(c(
    0, bagging_duration_mean, bayesglm_duration_mean, bayesrnn_duration_mean,
    cubist_duration_mean, earth_duration_mean, elastic_duration_mean, gam_duration_mean, gb_duration_mean,
    lasso_duration_mean, linear_duration_mean, neuralnet_duration_mean,
    pls_duration_mean, pcr_duration_mean, ridge_duration_mean,
    rpart_duration_mean, svm_duration_mean, tree_duration_mean, xgb_duration_mean,
    ensemble_bagging_duration_mean, ensemble_bayesglm_duration_mean,
    ensemble_bayesrnn_duration_mean, ensemble_cubist_duration_mean, ensemble_earth_duration_mean,
    ensemble_elastic_duration_mean, ensemble_gb_duration_mean,
    ensemble_lasso_duration_mean, ensemble_linear_duration_mean, ensemble_neuralnet_duration_mean, ensemble_ridge_duration_mean,
    ensemble_rpart_duration_mean, ensemble_svm_duration_mean, ensemble_tree_duration_mean
  ), 4),
  "Duration_sd" = round(c(
    0, bagging_duration_sd, bayesglm_duration_sd, bayesrnn_duration_sd,
    cubist_duration_sd, earth_duration_sd, elastic_duration_sd, gam_duration_sd, gb_duration_sd,
    lasso_duration_sd, linear_duration_sd, neuralnet_duration_sd,
    pls_duration_sd, pcr_duration_sd, ridge_duration_sd,
    rpart_duration_sd, svm_duration_sd, tree_duration_sd, xgb_duration_sd,
    ensemble_bagging_duration_sd, ensemble_bayesglm_duration_sd,
    ensemble_bayesrnn_duration_sd, ensemble_cubist_duration_sd, ensemble_earth_duration_sd,
    ensemble_elastic_duration_sd, ensemble_gb_duration_sd,
    ensemble_lasso_duration_sd, ensemble_linear_duration_sd, ensemble_neuralnet_duration_sd, ensemble_ridge_duration_sd,
    ensemble_rpart_duration_sd, ensemble_svm_duration_sd, ensemble_tree_duration_sd
  ), 4)
)


#### Overfitting data frame and plots ####
overfitting_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" = c(
      c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
      c(rep("BayesRNN", numresamples)),
      c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
      c(rep("Gradient Boosted", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
      c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
      c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
      c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
      c(rep("Ensemble Bagging", numresamples)),
      c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
      c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
      c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble Lasso", numresamples)),
      c(rep("Ensemble Linear", numresamples)), rep("Ensemble Neuralnet", numresamples), c(rep("Ensemble Ridge", numresamples)),
      c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
      c(rep("Ensemble Trees", numresamples))
    ),
    "data" = c(
      bagging_overfitting, bayesglm_overfitting,
      bayesrnn_overfitting,
      cubist_overfitting, earth_overfitting, elastic_overfitting_df$elastic_overfitting[2:nrow(elastic_overfitting_df)], gam_overfitting,
      gb_overfitting, lasso_overfitting_df$lasso_overfitting[2:nrow(lasso_overfitting_df)], linear_overfitting,
      neuralnet_overfitting, pcr_overfitting, pls_overfitting,
      ridge_overfitting_df$ridge_overfitting[2:nrow(ridge_overfitting_df)], rpart_overfitting,
      svm_overfitting, tree_overfitting, xgb_overfitting,
      ensemble_bagging_overfitting,
      ensemble_bayesglm_overfitting, ensemble_bayesrnn_overfitting,
      ensemble_cubist_overfitting, ensemble_earth_overfitting,
      ensemble_elastic_overfitting_df$ensemble_elastic_overfitting[2:nrow(ensemble_elastic_overfitting_df)],
      ensemble_gb_overfitting, ensemble_lasso_overfitting_df$ensemble_lasso_overfitting[2:nrow(ensemble_lasso_overfitting_df)],
      ensemble_linear_overfitting, ensemble_neuralnet_overfitting, ensemble_ridge_overfitting_df$ensemble_ridge_overfitting[2:nrow(ensemble_ridge_overfitting_df)],
      ensemble_rpart_overfitting, ensemble_svm_overfitting,
      ensemble_tree_overfitting
    ),
    "mean" = rep(c(
      bagging_overfitting_mean, bayesglm_overfitting_mean,
      bayesrnn_overfitting_mean,
      cubist_overfitting_mean, earth_overfitting_mean, elastic_overfitting_mean, gam_overfitting_mean,
      gb_overfitting_mean, lasso_overfitting_mean, linear_overfitting_mean,
      neuralnet_overfitting_mean, pcr_overfitting_mean, pls_overfitting_mean,
      ridge_overfitting_mean, rpart_overfitting_mean,
      svm_overfitting_mean, tree_overfitting_mean, xgb_overfitting_mean,
      ensemble_bagging_overfitting_mean,
      ensemble_bayesglm_overfitting_mean, ensemble_bayesrnn_overfitting_mean,
      ensemble_cubist_overfitting_mean, ensemble_earth_overfitting_mean,
      ensemble_elastic_overfitting_mean,
      ensemble_gb_overfitting_mean, ensemble_lasso_overfitting_mean, ensemble_linear_overfitting_mean,
      ensemble_neuralnet_overfitting_mean, ensemble_ridge_overfitting_mean,
      ensemble_rpart_overfitting_mean, ensemble_svm_overfitting_mean,
      ensemble_tree_overfitting_mean
    ), each = numresamples)
  )

overfitting_plot_fixed_scales <- ggplot2::ggplot(data = overfitting_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean)) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("Overfitting fixed scales by model \n fixed scales, closer to one is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Overfitting fixed scales closer to one is better \n The black horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("overfitting_plot_fixed_scales.eps", plot = overfitting_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("overfitting_plot_fixed_scales.jpeg", plot = overfitting_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("overfitting_plot_fixed_scales.pdf", plot = overfitting_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("overfitting_plot_fixed_scales.png", plot = overfitting_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("overfitting_plot_fixed_scales.svg", plot = overfitting_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("overfitting_plot_fixed_scales.tiff", plot = overfitting_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

overfitting_plot_free_scales <- ggplot2::ggplot(data = overfitting_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean)) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("Overfitting, free scales\n by model, closer to one is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Overfitting, free scales closer to one is better \n The black horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("overfitting_plot_free_scales.eps", plot = overfitting_plot_free_scales,  width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("overfitting_plot_free_scales.jpeg", plot = overfitting_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("overfitting_plot_free_scales.pdf", plot = overfitting_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("overfitting_plot_free_scales.png", plot = overfitting_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("overfitting_plot_free_scales.svg", plot = overfitting_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("overfitting_plot_free_scales.tiff", plot = overfitting_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

overfitting_histograms <- ggplot2::ggplot(overfitting_data, aes(x=data, fill=model)) +
  geom_histogram(color='black', alpha=0.4, position='identity', bins = numresamples) +
  ggplot2::geom_vline(xintercept = 1, color = "red") +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::ggtitle("Overfitting histograms by model, closer to 1 and normally distributed are better. \nThe red vertical line = 1.00") +
  ggplot2::labs(y = "Overfitting by model, closer to 1 and normally distributed are better.")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("overfitting_histograms.eps", plot = overfitting_histograms,  width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("overfitting_histograms.jpeg", plot = overfitting_histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("overfitting_histograms.pdf", plot = overfitting_histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("overfitting_histograms.png", plot = overfitting_histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("overfitting_histograms.svg", plot = overfitting_histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("overfitting_histograms.tiff", plot = overfitting_histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### Bias data frame and plots ####
bias_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" = c(
      c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
      c(rep("BayesRNN", numresamples)),
      c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
      c(rep("Gradient Boosted", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
      c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
      c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
      c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
      c(rep("Ensemble Bagging", numresamples)),
      c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
      c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
      c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble Lasso", numresamples)),
      c(rep("Ensemble Linear", numresamples)), c(rep("Ensemble Neuralnet", numresamples)), c(rep("Ensemble Ridge", numresamples)),
      c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
      c(rep("Ensemble Trees", numresamples))
    ),
    "data" = c(
      bagging_bias_mean, bayesglm_bias_mean,
      bayesrnn_bias_mean,
      cubist_bias_mean, earth_bias_mean, elastic_bias_mean, gam_bias_mean,
      gb_bias_mean, lasso_bias_mean, linear_bias_mean,
      neuralnet_bias_mean, pcr_bias_mean, pls_bias_mean,
      ridge_bias_mean, rpart_bias_mean,
      svm_bias_mean, tree_bias_mean, xgb_bias_mean,
      ensemble_bagging_bias_mean,
      ensemble_bayesglm_bias_mean, ensemble_bayesrnn_bias_mean,
      ensemble_cubist_bias_mean, ensemble_earth_bias_mean,
      ensemble_elastic_bias_mean, ensemble_gb_bias_mean, ensemble_lasso_bias_mean,
      ensemble_linear_bias_mean, ensemble_neuralnet_bias_mean, ensemble_ridge_bias_mean,
      ensemble_rpart_bias_mean, ensemble_svm_bias_mean,
      ensemble_tree_bias_mean
    )
  )

bias_plot <- ggplot2::ggplot(data = bias_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4) +
  ggplot2::ggtitle("Bias plot\nBias value by model, closer to zero is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "Bias value, closer to zero is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("bias_plot.eps", plot = bias_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("bias_plot.jpeg", plot = bias_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("bias_plot.pdf", plot = bias_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("bias_plot.png", plot = bias_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("bias_plot.svg", plot = bias_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("bias_plot.tiff", plot = bias_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


summary_results <- summary_results %>% dplyr::arrange(Mean_holdout_RMSE)

final_results <- reactable::reactable(summary_results,
                                      searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                      striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "final_results")
)

final_results <- htmlwidgets::prependContent(final_results, htmltools::h2(class = "title", "Summary report"))


#### <-----------------------------------------  8. Summary model data visualizations ----------------------------------------------------> ####

#### bagging data visualizations ####
bagging_df <- data.frame(
  actual = bagging_actual_total,
  predicted = y_hat_bagging_total ,
  residuals = bagging_actual_total - y_hat_bagging_total
)

bagging_pred_vs_actual <- ggplot2::ggplot(bagging_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Bagging model: Predicted vs actual", x = "Actual", y = "Predicted")

bagging_resid_vs_actual <- ggplot2::ggplot(bagging_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Bagging model: Residuals", x = "Actual", y = "Predicted")

bagging_hist_residuals <- ggplot2::ggplot(bagging_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Bagging model: Histogram of residuals")

bagging_qq <- ggplot2::ggplot(bagging_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Bagging model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

bagging_summary <- grid.arrange(bagging_pred_vs_actual, bagging_resid_vs_actual, bagging_hist_residuals, bagging_qq, ncol = 2)

#### Bayes GLM visualizations ####
bayesglm_df <- data.frame(
  actual = bayesglm_actual_total,
  predicted = y_hat_bayesglm_total ,
  residuals = bayesglm_actual_total - y_hat_bayesglm_total
)

bayesglm_pred_vs_actual <- ggplot2::ggplot(bayesglm_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "BayesGLM model: Predicted vs actual", x = "Actual", y = "Predicted")

bayesglm_resid_vs_actual <- ggplot2::ggplot(bayesglm_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "BayesGLM model: Residuals", x = "Actual", y = "Predicted")

bayesglm_hist_residuals <- ggplot2::ggplot(bayesglm_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "BayesGLM model: Histogram of residuals")

bayesglm_qq <- ggplot2::ggplot(bayesglm_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "BayesGLM model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

bayesglm_summary <- grid.arrange(bayesglm_pred_vs_actual, bayesglm_resid_vs_actual, bayesglm_hist_residuals, bayesglm_qq, ncol = 2)

#### Bayes RNN visualizations ####
bayesrnn_df <- data.frame(
  actual = bayesrnn_actual_total,
  predicted = y_hat_bayesrnn_total ,
  residuals = bayesrnn_actual_total - y_hat_bayesrnn_total
)

bayesrnn_pred_vs_actual <- ggplot2::ggplot(bayesrnn_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "BayesRNN model: Predicted vs actual", x = "Actual", y = "Predicted")

bayesrnn_resid_vs_actual <- ggplot2::ggplot(bayesrnn_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "BayesRNN model: Residuals", x = "Actual", y = "Predicted")

bayesrnn_hist_residuals <- ggplot2::ggplot(bayesrnn_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "BayesRNN model: Histogram of residuals")

bayesrnn_qq <- ggplot2::ggplot(bayesrnn_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "BayesRNN model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

bayesrnn_summary <- grid.arrange(bayesrnn_pred_vs_actual, bayesrnn_resid_vs_actual, bayesrnn_hist_residuals, bayesrnn_qq, ncol = 2)

#### Cubist visualizations
cubist_df <- data.frame(
  actual = cubist_actual_total,
  predicted = y_hat_cubist_total ,
  residuals = cubist_actual_total - y_hat_cubist_total
)

cubist_pred_vs_actual <- ggplot2::ggplot(cubist_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Cubist model: Predicted vs actual", x = "Actual", y = "Predicted")

cubist_resid_vs_actual <- ggplot2::ggplot(cubist_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Cubist model: Residuals", x = "Actual", y = "Predicted")

cubist_hist_residuals <- ggplot2::ggplot(cubist_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Cubist model: Histogram of residuals")

cubist_qq <- ggplot2::ggplot(cubist_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Cubist model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

cubist_summary <- grid.arrange(cubist_pred_vs_actual, cubist_resid_vs_actual, cubist_hist_residuals, cubist_qq, ncol = 2)

#### Earth visualizations
earth_df <- data.frame(
  actual = earth_actual_total,
  predicted = y_hat_earth_total ,
  residuals = earth_actual_total - y_hat_earth_total
)

earth_pred_vs_actual <- ggplot2::ggplot(earth_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Earth model: Predicted vs actual", x = "Actual", y = "Predicted")

earth_resid_vs_actual <- ggplot2::ggplot(earth_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Earth model: Residuals", x = "Actual", y = "Predicted")

earth_hist_residuals <- ggplot2::ggplot(earth_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Earth model: Histogram of residuals")

earth_qq <- ggplot2::ggplot(earth_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Earth model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

earth_summary <- grid.arrange(earth_pred_vs_actual, earth_resid_vs_actual, earth_hist_residuals, earth_qq, ncol = 2)

#### Elastic Net visualizations ####
elastic_df <- data.frame(
  actual = elastic_actual_total,
  predicted = y_hat_elastic_total ,
  residuals = elastic_actual_total - y_hat_elastic_total
)

elastic_pred_vs_actual <- ggplot2::ggplot(elastic_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Elastic Net model: Predicted vs actual", x = "Actual", y = "Predicted")

elastic_resid_vs_actual <- ggplot2::ggplot(elastic_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Elastic Net model: Residuals", x = "Actual", y = "Predicted")

elastic_hist_residuals <- ggplot2::ggplot(elastic_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Elastic Net model: Histogram of residuals")

elastic_qq <- ggplot2::ggplot(elastic_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Elastic Net model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

elastic_summary <- grid.arrange(elastic_pred_vs_actual, elastic_resid_vs_actual, elastic_hist_residuals, elastic_qq, ncol = 2)


#### Generalized Additive Model with splines (GAM) ####
gam_df <- data.frame(
  actual = gam_actual_total,
  predicted = y_hat_gam_total ,
  residuals = gam_actual_total - y_hat_gam_total
)

gam_pred_vs_actual <- ggplot2::ggplot(gam_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Generalized Additive model: Predicted vs actual", x = "Actual", y = "Predicted")

gam_resid_vs_actual <- ggplot2::ggplot(gam_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Generalized Additive model: Residuals", x = "Actual", y = "Predicted")

gam_hist_residuals <- ggplot2::ggplot(gam_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Generalized Additive model: Histogram of residuals")

gam_qq <- ggplot2::ggplot(gam_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Generalized Additive model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

gam_summary <- grid.arrange(gam_pred_vs_actual, gam_resid_vs_actual, gam_hist_residuals, gam_qq, ncol = 2)

#### Gradient Boosted visualizations ####
gb_df <- data.frame(
  actual = gb_actual_total,
  predicted = y_hat_gb_total ,
  residuals = gb_actual_total - y_hat_gb_total
)

gb_pred_vs_actual <- ggplot2::ggplot(gb_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Gardient Boosted model: Predicted vs actual", x = "Actual", y = "Predicted")

gb_resid_vs_actual <- ggplot2::ggplot(gb_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Gradient Boosted model: Residuals", x = "Actual", y = "Predicted")

gb_hist_residuals <- ggplot2::ggplot(gb_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Gradient Boosted model: Histogram of residuals")

gb_qq <- ggplot2::ggplot(gb_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Gradient Boosted model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

gb_summary <- grid.arrange(gb_pred_vs_actual, gb_resid_vs_actual, gb_hist_residuals, gb_qq, ncol = 2)

#### lasso data visualizations ####
lasso_df <- data.frame(
  actual = lasso_actual_total,
  predicted = y_hat_lasso_total ,
  residuals = lasso_actual_total - y_hat_lasso_total
)

lasso_pred_vs_actual <- ggplot2::ggplot(lasso_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Lasso model: Predicted vs actual", x = "Actual", y = "Predicted")

lasso_resid_vs_actual <- ggplot2::ggplot(lasso_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Lasso model: Residuals", x = "Actual", y = "Predicted")

lasso_hist_residuals <- ggplot2::ggplot(lasso_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Lasso model: Histogram of residuals")

lasso_qq <- ggplot2::ggplot(lasso_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Lasso model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

lasso_summary <- grid.arrange(lasso_pred_vs_actual, lasso_resid_vs_actual, lasso_hist_residuals, lasso_qq, ncol = 2)


####  Linear visualizations ####
linear_df <- data.frame(
  actual = linear_actual_total,
  predicted = y_hat_linear_total ,
  residuals = linear_actual_total - y_hat_linear_total
)

linear_pred_vs_actual <- ggplot2::ggplot(linear_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Linear model: Predicted vs actual", x = "Actual", y = "Predicted")

linear_resid_vs_actual <- ggplot2::ggplot(linear_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Linear model: Residuals", x = "Actual", y = "Predicted")

linear_hist_residuals <- ggplot2::ggplot(linear_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Linear model: Histogram of residuals")

linear_qq <- ggplot2::ggplot(linear_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Linear model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

linear_summary <- grid.arrange(linear_pred_vs_actual, linear_resid_vs_actual, linear_hist_residuals, linear_qq, ncol = 2)

#### Neuralnet data visualizations ####
neuralnet_df <- data.frame(
  actual = neuralnet_actual_total,
  predicted = y_hat_neuralnet_total ,
  residuals = neuralnet_actual_total - y_hat_neuralnet_total
)

neuralnet_pred_vs_actual <- ggplot2::ggplot(neuralnet_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Neuralnet model: Predicted vs actual", x = "Actual", y = "Predicted")

neuralnet_resid_vs_actual <- ggplot2::ggplot(neuralnet_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Neuralnet model: Residuals", x = "Actual", y = "Predicted")

neuralnet_hist_residuals <- ggplot2::ggplot(neuralnet_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Neuralnet model: Histogram of residuals")

neuralnet_qq <- ggplot2::ggplot(neuralnet_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Neuralnet model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

neuralnet_summary <- grid.arrange(neuralnet_pred_vs_actual, neuralnet_resid_vs_actual, neuralnet_hist_residuals, neuralnet_qq, ncol = 2)

#### Partial Least Squares Regression visualizations ####
pls_df <- data.frame(
  actual = pls_actual_total,
  predicted = y_hat_pls_total ,
  residuals = pls_actual_total - y_hat_pls_total
)

pls_pred_vs_actual <- ggplot2::ggplot(pls_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Partial Least Squares model: Predicted vs actual", x = "Actual", y = "Predicted")

pls_resid_vs_actual <- ggplot2::ggplot(pls_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Partial Least Squares model: Residuals", x = "Actual", y = "Predicted")

pls_hist_residuals <- ggplot2::ggplot(pls_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Partial Least Squares model: Histogram of residuals")

pls_qq <- ggplot2::ggplot(pls_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Partial Least Squares model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

pls_summary <- grid.arrange(pls_pred_vs_actual, pls_resid_vs_actual, pls_hist_residuals, pls_qq, ncol = 2)

#### Principal Components Regression visualizations ####
pcr_df <- data.frame(
  actual = pcr_actual_total,
  predicted = y_hat_pcr_total ,
  residuals = pcr_actual_total - y_hat_pcr_total
)

pcr_pred_vs_actual <- ggplot2::ggplot(pcr_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Princial Components Regression (PCR) model: Predicted vs actual", x = "Actual", y = "Predicted")

pcr_resid_vs_actual <- ggplot2::ggplot(pcr_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Princial Components Regression (PCR) model: Residuals", x = "Actual", y = "Predicted")

pcr_hist_residuals <- ggplot2::ggplot(pcr_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Princial Components Regression (PCR) model: Histogram of residuals")

pcr_qq <- ggplot2::ggplot(pcr_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Princial Components Regression (PCR) model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

pcr_summary <- grid.arrange(pcr_pred_vs_actual, pcr_resid_vs_actual, pcr_hist_residuals, pcr_qq, ncol = 2)

#### Ridge data visualizations ####
Ridge_df <- data.frame(
  actual = ridge_actual_total,
  predicted = y_hat_ridge_total ,
  residuals = ridge_actual_total - y_hat_ridge_total
)

ridge_pred_vs_actual <- ggplot2::ggplot(Ridge_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ridge model: Predicted vs actual", x = "Actual", y = "Predicted")

ridge_resid_vs_actual <- ggplot2::ggplot(Ridge_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ridge model: Residuals", x = "Actual", y = "Predicted")

ridge_hist_residuals <- ggplot2::ggplot(Ridge_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ridge model: Histogram of residuals")

ridge_qq <- ggplot2::ggplot(Ridge_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ridge model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ridge_summary <- grid.arrange(ridge_pred_vs_actual, ridge_resid_vs_actual, ridge_hist_residuals, ridge_qq, ncol = 2)

#### Rpart visualizations ####
rpart_df <- data.frame(
  actual = rpart_actual_total,
  predicted = y_hat_rpart_total ,
  residuals = rpart_actual_total - y_hat_rpart_total
)

rpart_pred_vs_actual <- ggplot2::ggplot(rpart_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "RPart model: Predicted vs actual", x = "Actual", y = "Predicted")

rpart_resid_vs_actual <- ggplot2::ggplot(rpart_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "RPart model: Residuals", x = "Actual", y = "Predicted")

rpart_hist_residuals <- ggplot2::ggplot(rpart_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "RPart model: Histogram of residuals")

rpart_qq <- ggplot2::ggplot(rpart_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "RPart model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

rpart_summary <- grid.arrange(rpart_pred_vs_actual, rpart_resid_vs_actual, rpart_hist_residuals, rpart_qq, ncol = 2)

#### Support Vector Machines (SVM) visualizations ####
svm_df <- data.frame(
  actual = svm_actual_total,
  predicted = y_hat_svm_total ,
  residuals = svm_actual_total - y_hat_svm_total
)

svm_pred_vs_actual <- ggplot2::ggplot(svm_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Support Vector Machines model: Predicted vs actual", x = "Actual", y = "Predicted")

svm_resid_vs_actual <- ggplot2::ggplot(svm_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Support Vector Machines model: Residuals", x = "Actual", y = "Predicted")

svm_hist_residuals <- ggplot2::ggplot(svm_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Support Vector Machines model: Histogram of residuals")

svm_qq <- ggplot2::ggplot(svm_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Support Vector Machines model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

svm_summary <- grid.arrange(svm_pred_vs_actual, svm_resid_vs_actual, svm_hist_residuals, svm_qq, ncol = 2)


#### Tree visualizations ####
tree_df <- data.frame(
  actual = tree_actual_total,
  predicted = y_hat_tree_total ,
  residuals = tree_actual_total - y_hat_tree_total
)

tree_pred_vs_actual <- ggplot2::ggplot(tree_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Trees model: Predicted vs actual", x = "Actual", y = "Predicted")

tree_resid_vs_actual <- ggplot2::ggplot(tree_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Trees model: Residuals", x = "Actual", y = "Predicted")

tree_hist_residuals <- ggplot2::ggplot(tree_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Trees model: Histogram of residuals")

tree_qq <- ggplot2::ggplot(tree_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Trees model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

tree_summary <- grid.arrange(tree_pred_vs_actual, tree_resid_vs_actual, tree_hist_residuals, tree_qq, ncol = 2)

#### XGBoost visualizations ####
xgb_df <- data.frame(
  actual = xgb_actual_total,
  predicted = y_hat_xgb_total ,
  residuals = xgb_actual_total - y_hat_xgb_total
)

xgb_pred_vs_actual <- ggplot2::ggplot(xgb_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "XGBoost model: Predicted vs actual", x = "Actual", y = "Predicted")

xgb_resid_vs_actual <- ggplot2::ggplot(xgb_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "XGBoost model: Residuals", x = "Actual", y = "Predicted")

xgb_hist_residuals <- ggplot2::ggplot(xgb_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "XGBoost model: Histogram of residuals")

xgb_qq <- ggplot2::ggplot(xgb_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "XGBoost model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

xgb_summary <- grid.arrange(xgb_pred_vs_actual, xgb_resid_vs_actual, xgb_hist_residuals, xgb_qq, ncol = 2)

#### Ensemble Bagging Visualizations ####
ensemble_bagging_df <- data.frame(
  actual = ensemble_bagging_actual_total,
  predicted = ensemble_y_hat_bagging_total ,
  residuals = ensemble_bagging_actual_total - ensemble_y_hat_bagging_total
)

ensemble_bagging_pred_vs_actual <- ggplot2::ggplot(ensemble_bagging_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Bagging model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_bagging_resid_vs_actual <- ggplot2::ggplot(ensemble_bagging_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Bagging model: Residuals", x = "Actual", y = "Predicted")

ensemble_bagging_hist_residuals <- ggplot2::ggplot(ensemble_bagging_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Bagging model: Histogram of residuals")

ensemble_bagging_qq <- ggplot2::ggplot(ensemble_bagging_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Bagging: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_bagging_summary <- grid.arrange(ensemble_bagging_pred_vs_actual, ensemble_bagging_resid_vs_actual, ensemble_bagging_hist_residuals, ensemble_bagging_qq, ncol = 2)

#### Ensemble BayesGLM Visualizations ####
ensemble_bayesglm_df <- data.frame(
  actual = ensemble_bayesglm_actual_total,
  predicted = ensemble_y_hat_bayesglm_total ,
  residuals = ensemble_bayesglm_actual_total - ensemble_y_hat_bayesglm_total
)

ensemble_bayesglm_pred_vs_actual <- ggplot2::ggplot(ensemble_bayesglm_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble BayesGLM model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_bayesglm_resid_vs_actual <- ggplot2::ggplot(ensemble_bayesglm_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble BayesGLM model: Residuals", x = "Actual", y = "Predicted")

ensemble_bayesglm_hist_residuals <- ggplot2::ggplot(ensemble_bayesglm_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble BayesGLM model: Histogram of residuals")

ensemble_bayesglm_qq <- ggplot2::ggplot(ensemble_bayesglm_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble BayesGLM: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_bayesglm_summary <- grid.arrange(ensemble_bayesglm_pred_vs_actual, ensemble_bayesglm_resid_vs_actual, ensemble_bayesglm_hist_residuals, ensemble_bayesglm_qq, ncol = 2)

#### Ensemble BayesRNN Visualizations ####
ensemble_bayesrnn_df <- data.frame(
  actual = ensemble_bayesrnn_actual_total,
  predicted = ensemble_y_hat_bayesrnn_total ,
  residuals = ensemble_bayesrnn_actual_total - ensemble_y_hat_bayesrnn_total
)

ensemble_bayesrnn_pred_vs_actual <- ggplot2::ggplot(ensemble_bayesrnn_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble BayesRNN model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_bayesrnn_resid_vs_actual <- ggplot2::ggplot(ensemble_bayesrnn_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble BayesRNN model: Residuals", x = "Actual", y = "Predicted")

ensemble_bayesrnn_hist_residuals <- ggplot2::ggplot(ensemble_bayesrnn_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble BayesRNN model: Histogram of residuals")

ensemble_bayesrnn_qq <- ggplot2::ggplot(ensemble_bayesrnn_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble BayesRNN: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_bayesrnn_summary <- grid.arrange(ensemble_bayesrnn_pred_vs_actual, ensemble_bayesrnn_resid_vs_actual, ensemble_bayesrnn_hist_residuals, ensemble_bayesrnn_qq, ncol = 2)

### Ensemble Cubist Visualizations ####
ensemble_cubist_df <- data.frame(
  actual = ensemble_cubist_actual_total,
  predicted = ensemble_y_hat_cubist_total ,
  residuals = ensemble_cubist_actual_total - ensemble_y_hat_cubist_total
)

ensemble_cubist_pred_vs_actual <- ggplot2::ggplot(ensemble_cubist_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Cubist model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_cubist_resid_vs_actual <- ggplot2::ggplot(ensemble_cubist_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Cubist model: Residuals", x = "Actual", y = "Predicted")

ensemble_cubist_hist_residuals <- ggplot2::ggplot(ensemble_cubist_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Cubist model: Histogram of residuals")

ensemble_cubist_qq <- ggplot2::ggplot(ensemble_cubist_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Cubist model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_cubist_summary <- grid.arrange(ensemble_cubist_pred_vs_actual, ensemble_cubist_resid_vs_actual, ensemble_cubist_hist_residuals, ensemble_cubist_qq, ncol = 2)

### Ensemble Earth Visualizations ####
ensemble_earth_df <- data.frame(
  actual = ensemble_earth_actual_total,
  predicted = ensemble_y_hat_earth_total ,
  residuals = ensemble_earth_actual_total - ensemble_y_hat_earth_total
)

ensemble_earth_pred_vs_actual <- ggplot2::ggplot(ensemble_earth_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Earth model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_earth_resid_vs_actual <- ggplot2::ggplot(ensemble_earth_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Earth model: Residuals", x = "Actual", y = "Predicted")

ensemble_earth_hist_residuals <- ggplot2::ggplot(ensemble_earth_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Earth model: Histogram of residuals")

ensemble_earth_qq <- ggplot2::ggplot(ensemble_earth_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Earth model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_earth_summary <- grid.arrange(ensemble_earth_pred_vs_actual, ensemble_earth_resid_vs_actual, ensemble_earth_hist_residuals, ensemble_earth_qq, ncol = 2)

#### Ensemble Elastic data visualizations ####
ensemble_elastic_df <- data.frame(
  actual = ensemble_elastic_actual_total,
  predicted = ensemble_y_hat_elastic_total ,
  residuals = ensemble_elastic_actual_total - ensemble_y_hat_elastic_total
)

ensemble_elastic_pred_vs_actual <- ggplot2::ggplot(ensemble_elastic_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Elastic model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_elastic_resid_vs_actual <- ggplot2::ggplot(ensemble_elastic_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Elastic model: Residuals", x = "Actual", y = "Predicted")

ensemble_elastic_hist_residuals <- ggplot2::ggplot(ensemble_elastic_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Elastic model: Histogram of residuals")

ensemble_elastic_qq <- ggplot2::ggplot(ensemble_elastic_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Elastic model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_elastic_summary <- grid.arrange(ensemble_elastic_pred_vs_actual, ensemble_elastic_resid_vs_actual, ensemble_elastic_hist_residuals, ensemble_elastic_qq, ncol = 2)


#### Ensemble Gradient Boosted Visualizations ####
ensemble_gb_df <- data.frame(
  actual = ensemble_gb_actual_total,
  predicted = ensemble_y_hat_gb_total ,
  residuals = ensemble_gb_actual_total - ensemble_y_hat_gb_total
)

ensemble_gb_pred_vs_actual <- ggplot2::ggplot(ensemble_gb_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Gradient Boosted model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_gb_resid_vs_actual <- ggplot2::ggplot(ensemble_gb_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Gradient Boosted model: Residuals", x = "Actual", y = "Predicted")

ensemble_gb_hist_residuals <- ggplot2::ggplot(ensemble_gb_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Gradient Boosted model: Histogram of residuals")

ensemble_gb_qq <- ggplot2::ggplot(ensemble_gb_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Gradient Boosted model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_gb_summary <- grid.arrange(ensemble_gb_pred_vs_actual, ensemble_gb_resid_vs_actual, ensemble_gb_hist_residuals, ensemble_gb_qq, ncol = 2)


#### Ensemble Lasso data visualizations ####
ensemble_lasso_df <- data.frame(
  actual = ensemble_lasso_actual_total,
  predicted = ensemble_y_hat_lasso_total ,
  residuals = ensemble_lasso_actual_total - ensemble_y_hat_lasso_total
)

ensemble_lasso_pred_vs_actual <- ggplot2::ggplot(ensemble_lasso_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Lasso model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_lasso_resid_vs_actual <- ggplot2::ggplot(ensemble_lasso_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Lasso model: Residuals", x = "Actual", y = "Predicted")

ensemble_lasso_hist_residuals <- ggplot2::ggplot(ensemble_lasso_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Lasso model: Histogram of residuals")

ensemble_lasso_qq <- ggplot2::ggplot(ensemble_lasso_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Lasso model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_lasso_summary <- grid.arrange(ensemble_lasso_pred_vs_actual, ensemble_lasso_resid_vs_actual, ensemble_lasso_hist_residuals, ensemble_lasso_qq, ncol = 2)


#### Ensemble Linear Visualizations ####
ensemble_linear_df <- data.frame(
  actual = ensemble_linear_actual_total,
  predicted = ensemble_y_hat_linear_total ,
  residuals = ensemble_linear_actual_total - ensemble_y_hat_linear_total
)

ensemble_linear_pred_vs_actual <- ggplot2::ggplot(ensemble_linear_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Linear model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_linear_resid_vs_actual <- ggplot2::ggplot(ensemble_linear_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Linear model: Residuals", x = "Actual", y = "Predicted")

ensemble_linear_hist_residuals <- ggplot2::ggplot(ensemble_linear_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Linear model: Histogram of residuals")

ensemble_linear_qq <- ggplot2::ggplot(ensemble_linear_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Linear model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_linear_summary <- grid.arrange(ensemble_linear_pred_vs_actual, ensemble_linear_resid_vs_actual, ensemble_linear_hist_residuals, ensemble_linear_qq, ncol = 2)


#### Ensemble Neuralnet data visualizations ####
ensemble_neuralnet_df <- data.frame(
  actual = ensemble_neuralnet_actual_total,
  predicted = y_hat_ensemble_neuralnet_total ,
  residuals = ensemble_neuralnet_actual_total - y_hat_ensemble_neuralnet_total
)

ensemble_neuralnet_pred_vs_actual <- ggplot2::ggplot(ensemble_neuralnet_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Neuralnet model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_neuralnet_resid_vs_actual <- ggplot2::ggplot(ensemble_neuralnet_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Neuralnet model: Residuals", x = "Actual", y = "Predicted")

ensemble_neuralnet_hist_residuals <- ggplot2::ggplot(ensemble_neuralnet_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Neuralnet model: Histogram of residuals")

ensemble_neuralnet_qq <- ggplot2::ggplot(ensemble_neuralnet_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Neuralnet model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_neuralnet_summary <- grid.arrange(ensemble_neuralnet_pred_vs_actual, ensemble_neuralnet_resid_vs_actual, ensemble_neuralnet_hist_residuals, ensemble_neuralnet_qq, ncol = 2)

#### Ensemble Ridge data visualizations ####
ensemble_ridge_df <- data.frame(
  actual = ensemble_ridge_actual_total,
  predicted = ensemble_y_hat_ridge_total ,
  residuals = ensemble_ridge_actual_total - ensemble_y_hat_ridge_total
)

ensemble_ridge_pred_vs_actual <- ggplot2::ggplot(ensemble_ridge_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Ridge model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_ridge_resid_vs_actual <- ggplot2::ggplot(ensemble_ridge_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Ridge model: Residuals", x = "Actual", y = "Predicted")

ensemble_ridge_hist_residuals <- ggplot2::ggplot(ensemble_ridge_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Ridge model: Histogram of residuals")

ensemble_ridge_qq <- ggplot2::ggplot(ensemble_ridge_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Ridge model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_ridge_summary <- grid.arrange(ensemble_ridge_pred_vs_actual, ensemble_ridge_resid_vs_actual, ensemble_ridge_hist_residuals, ensemble_ridge_qq, ncol = 2)

#### Ensemble Rpart Visualizations ####
ensemble_rpart_df <- data.frame(
  actual = ensemble_rpart_actual_total,
  predicted = ensemble_y_hat_rpart_total ,
  residuals = ensemble_rpart_actual_total - ensemble_y_hat_rpart_total
)

ensemble_rpart_pred_vs_actual <- ggplot2::ggplot(ensemble_rpart_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble RPart model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_rpart_resid_vs_actual <- ggplot2::ggplot(ensemble_rpart_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble RPart model: Residuals", x = "Actual", y = "Predicted")

ensemble_rpart_hist_residuals <- ggplot2::ggplot(ensemble_rpart_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble RPart model: Histogram of residuals")

ensemble_rpart_qq <- ggplot2::ggplot(ensemble_rpart_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble RPart model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_rpart_summary <- grid.arrange(ensemble_rpart_pred_vs_actual, ensemble_rpart_resid_vs_actual, ensemble_rpart_hist_residuals, ensemble_rpart_qq, ncol = 2)

#### Ensemble Support Vector Machines Visualizations ####
ensemble_svm_df <- data.frame(
  actual = ensemble_svm_actual_total,
  predicted = ensemble_y_hat_svm_total ,
  residuals = ensemble_svm_actual_total - ensemble_y_hat_svm_total
)

ensemble_svm_pred_vs_actual <- ggplot2::ggplot(ensemble_svm_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Support Vector Machines model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_svm_resid_vs_actual <- ggplot2::ggplot(ensemble_svm_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Support Vector Machines model: Residuals", x = "Actual", y = "Predicted")

ensemble_svm_hist_residuals <- ggplot2::ggplot(ensemble_svm_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Support Vector Machines model: Histogram of residuals")

ensemble_svm_qq <- ggplot2::ggplot(ensemble_svm_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Support Vector Machines model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_svm_summary <- grid.arrange(ensemble_svm_pred_vs_actual, ensemble_svm_resid_vs_actual, ensemble_svm_hist_residuals, ensemble_svm_qq, ncol = 2)

#### Ensemble Tree Visualizations ####
ensemble_tree_df <- data.frame(
  actual = ensemble_tree_actual_total,
  predicted = ensemble_y_hat_tree_total ,
  residuals = ensemble_tree_actual_total - ensemble_y_hat_tree_total
)

ensemble_tree_pred_vs_actual <- ggplot2::ggplot(ensemble_tree_df, mapping = aes(x = actual, y = predicted)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(intercept = 0, slope = 1, color = "red") +
  ggplot2::labs(title = "Ensemble Trees model: Predicted vs actual", x = "Actual", y = "Predicted")

ensemble_tree_resid_vs_actual <- ggplot2::ggplot(ensemble_tree_df, mapping = aes(x = actual, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Trees model: Residuals", x = "Actual", y = "Predicted")

ensemble_tree_hist_residuals <- ggplot2::ggplot(ensemble_tree_df, mapping = aes(x = residuals)) +
  ggplot2::geom_histogram(bins = round(nrow(df))) +
  ggplot2::geom_vline(xintercept = 0, color = "red") +
  ggplot2::labs(title = "Ensemble Trees model: Histogram of residuals")

ensemble_tree_qq <- ggplot2::ggplot(ensemble_tree_df, aes(sample = as.numeric(predicted))) + ggplot2::stat_qq() +
  ggplot2::labs(title = "Ensemble Trees model: Q-Q plot") +
  ggplot2::stat_qq_line(color = "red")

ensemble_tree_summary <- grid.arrange(ensemble_tree_pred_vs_actual, ensemble_tree_resid_vs_actual, ensemble_tree_hist_residuals, ensemble_tree_qq, ncol = 2)


#### Barcharts ####

accuracy_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Mean_holdout_RMSE), y = Mean_holdout_RMSE)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::coord_cartesian(ylim = c(0, 1.25*max(summary_results$Mean_holdout_RMSE))) +
  ggplot2::labs(x = "Model", y = "Model accuracy RMSE Mean", title = "Model accuracy by RMSE, lower is better, 1 std deviation error bars") +
  ggplot2::geom_text(aes(label = Mean_holdout_RMSE), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::geom_errorbar(aes(x=Model, ymin=Mean_holdout_RMSE-RMSE_Std_Dev, ymax = Mean_holdout_RMSE+RMSE_Std_Dev))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_barchart.eps", plot = accuracy_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_barchart.jpeg", plot = accuracy_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_barchart.pdf", plot = accuracy_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_barchart.png", plot = accuracy_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_barchart.svg", plot = accuracy_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_barchart.tiff", plot = accuracy_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

k_s_test_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, KS_Test_P_Value_mean, decreasing = TRUE), y = KS_Test_P_Value_mean)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "P-Value", title = "Kolmogorov-Smirnov test, p-value, 1 std deviation error bars, above your p-value is better") +
  ggplot2::geom_text(aes(label = KS_Test_P_Value_mean), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::coord_cartesian(ylim = c(0, 1.25)) +
  ggplot2::scale_y_continuous(breaks = c(0.05, 0.10)) +
  ggplot2::geom_hline(yintercept = c(0.05, 0.10), linetype='dashed', color=c('blue', 'blue')) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = KS_Test_P_Value_mean - KS_Test_P_Value_std_dev, ymax = KS_Test_P_Value_mean + KS_Test_P_Value_std_dev))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("k_s_test_barchart.eps", plot = k_s_test_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("k_s_test_barchart.jpeg", plot = k_s_test_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("k_s_test_barchart.pdf", plot = k_s_test_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("k_s_test_barchart.png", plot = k_s_test_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("k_s_test_barchart.svg",  plot = k_s_test_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("k_s_test_barchart.tiff", plot = k_s_test_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

p_value_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, t_test_p.value_mean), y = t_test_p.value_mean)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "t_test p value", title = "t-test p-value, 1 standard deviation error bars") +
  ggplot2::geom_text(aes(label = t_test_p.value_mean), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::scale_y_continuous(breaks = c(0.05, 0.10), limits = c(0, 1.5)) +
  ggplot2::geom_hline(yintercept = c(0.05, 0.10), linetype='dashed', color=c('blue', 'blue'))+
  ggplot2::geom_errorbar(aes(x = Model, ymin = t_test_p.value_mean - t_test_p_value_sd, ymax = t_test_p.value_mean + t_test_p_value_sd))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("t_test_p_value_barchart.eps", plot = p_value_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("t_test_p_value_barchart.jpeg", plot = p_value_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("t_test_p_value_barchart.pdf", plot = p_value_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("t_test_p_value_barchart.png", plot = p_value_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("t_test_p_value_barchart.svg", plot = p_value_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("t_test_p_value_barchart.tiff", plot = p_value_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

overfitting_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Overfitting_mean), y = Overfitting_mean)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Overfitting mean", title = "Overfitting, closer to 1 is better, 1 std deviation error bars") +
  ggplot2::geom_text(aes(label = Overfitting_mean), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::coord_cartesian(ylim = c(0, 1.25*max(summary_results$Overfitting_mean))) +
  ggplot2::geom_errorbar(aes(x=Model, ymin=Overfitting_mean-Overfitting_sd, ymax = Overfitting_mean + Overfitting_sd))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("overfitting_barchart.eps", plot = overfitting_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("overfitting_barchart.jpeg", plot = overfitting_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("overfitting_barchart.pdf", plot = overfitting_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("overfitting_barchart.png", plot = overfitting_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("overfitting_barchart.svg", plot = overfitting_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("overfitting_barchart.tiff", plot = overfitting_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

duration_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Duration), y = Duration)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Duration", title = "Duration, shorter is better, 1 std deviation error bars") +
  ggplot2::geom_text(aes(label = Duration), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Duration - Duration_sd, ymax = Duration + Duration_sd))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("duration_barchart.eps", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("duration_barchart.jpeg", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("duration_barchart.pdf", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("duration_barchart.png", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("duration_barchart.svg", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("duration_barchart.tiff",  plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

bias_barchart <- ggplot2::ggplot(summary_results, aes(x = reorder(Model, Bias), y = Bias)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::coord_cartesian(ylim = c(1.25*min(summary_results$Bias), 1.25*max(summary_results$Bias))) +
  ggplot2::labs(x = "Model", y = "Holdout bias", title = "Mean bias, closer to zero is better") +
  ggplot2::geom_text(aes(label = Bias), vjust = -0.5, hjust = -0.5, angle = 90)
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("bias_barchart.eps", plot = bias_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("bias_barchart.jpeg", plot = bias_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("bias_barchart.pdf", plot = bias_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("bias_barchart.png", plot = bias_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("bias_barchart.svg", plot = bias_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("bias_barchart.tiff", plot = bias_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Plot of predicted vs actual, residuals, histogram of residuals and Q-Q plot for all models ####

for (i in 2:4) {

  data_visualizations <- summary_results[i, 1]

  if (data_visualizations == "Bagging") {
    grid.arrange(bagging_pred_vs_actual, bagging_resid_vs_actual, bagging_hist_residuals, bagging_qq, ncol = 2)
    gridExtra::grid.arrange(bagging_pred_vs_actual)
    gridExtra::grid.arrange(bagging_resid_vs_actual)
    gridExtra::grid.arrange(bagging_hist_residuals)
    gridExtra::grid.arrange(bagging_qq)
    if(save_all_plots == "Y" && data_visualizations == "Bagging" && device == "eps"){
      ggplot2::ggsave("bagging_pred_vs_actual.eps", plot = bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_resid_vs_actual.eps", plot = bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_hist_residuals.eps", plot = bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_qq.eps", plot = bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_summary.eps", plot = bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Bagging" && device == "pdf"){
      ggplot2::ggsave("bagging_pred_vs_actual.pdf", plot = bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_resid_vs_actual.pdf", plot = bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_hist_residuals.pdf", plot = bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_qq.pdf", plot = bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_summary.pdf", plot = bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Bagging" && device == "jpeg"){
      ggplot2::ggsave("bagging_pred_vs_actual.jpeg", plot = bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_resid_vs_actual.jpeg", plot = bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_hist_residuals.jpeg", plot = bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_qq.jpeg", plot = bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_summary.jpeg", plot = bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations[1i] == "Bagging" && device == "tiff"){
      ggplot2::ggsave("bagging_pred_vs_actual.tiff", plot = bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_resid_vs_actual.tiff", plot = bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_hist_residuals.tiff", plot = bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_qq.tiff", plot = bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_summary.tiff", plot = bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Bagging" && device == "png"){
      ggplot2::ggsave("bagging_pred_vs_actual.png", plot = bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_resid_vs_actual.png", plot = bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_hist_residuals.png", plot = bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_qq.png", plot = bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_summary.png", plot = bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations[1] == "Bagging" && device == "svg"){
      ggplot2::ggsave("bagging_pred_vs_actual.svg", plot = bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_resid_vs_actual.svg", plot = bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_hist_residuals.svg", plot = bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_qq.svg", plot = bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bagging_summary.svg", plot = bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "BayesGLM") {
    grid.arrange(bayesglm_pred_vs_actual, bayesglm_resid_vs_actual, bayesglm_hist_residuals, bayesglm_qq, ncol = 2)
    gridExtra::grid.arrange(bayesglm_pred_vs_actual)
    gridExtra::grid.arrange(bayesglm_resid_vs_actual)
    gridExtra::grid.arrange(bayesglm_hist_residuals)
    gridExtra::grid.arrange(bayesglm_qq)
    if(save_all_plots == "Y" && data_visualizations == "BayesGLM" && device == "eps"){
      ggplot2::ggsave("Bayesglm_pred_vs_actual.eps", plot = bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_resid_vs_actual.eps", plot = bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_hist_residuals.eps", plot = bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_qq.eps", plot = bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_summary.eps", plot = Bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "BayesGLM" && device == "pdf"){
      ggplot2::ggsave("Bayesglm_pred_vs_actual.pdf", plot = bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_resid_vs_actual.pdf", plot = bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_hist_residuals.pdf", plot = bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_qq.pdf", plot = bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_summary.pdf", plot = Bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "BayesGLM" && device == "jpeg"){
      ggplot2::ggsave("Bayesglm_pred_vs_actual.jpeg", plot = bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_resid_vs_actual.jpeg", plot = bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_hist_residuals.jpeg", plot = bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_qq.jpeg", plot = bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_summary.jpeg", plot = Bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "BayesGLM" && device == "tiff"){
      ggplot2::ggsave("Bayesglm_pred_vs_actual.tiff", plot = bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_resid_vs_actual.tiff", plot = bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_hist_residuals.tiff", plot = bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_qq.tiff", plot = bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_summary.tiff", plot = Bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "BayesGLM" && device == "png"){
      ggplot2::ggsave("Bayesglm_pred_vs_actual.png", plot = bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_resid_vs_actual.png", plot = bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_hist_residuals.png", plot = bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_qq.png", plot = bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_summary.png", plot = Bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations[1] == "BayesGLM" && device == "svg"){
      ggplot2::ggsave("Bayesglm_pred_vs_actual.svg", plot = bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_resid_vs_actual.svg", plot = bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_hist_residuals.svg", plot = bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_qq.svg", plot = bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("Bayesglm_summary.svg", plot = Bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }}

  if (data_visualizations == "BayesRNN") {
    grid.arrange(bayesrnn_pred_vs_actual, bayesrnn_resid_vs_actual, bayesrnn_hist_residuals, bayesrnn_qq, ncol = 2)
    gridExtra::grid.arrange(bayesrnn_pred_vs_actual)
    gridExtra::grid.arrange(bayesrnn_resid_vs_actual)
    gridExtra::grid.arrange(bayesrnn_hist_residuals)
    gridExtra::grid.arrange(bayesrnn_qq)
    if(save_all_plots == "Y" && data_visualizations == "BayesRNN" && device == "eps"){
      ggplot2::ggsave("bayesrnn_pred_vs_actual.eps", plot = bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_resid_vs_actual.eps", plot =  bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_hist_residuals.eps", plot = bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_qq.eps", plot = bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_summary.eps", plot = bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "BayesRNN" && device == "pdf"){
      ggplot2::ggsave("bayesrnn_pred_vs_actual.pdf",  plot = bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_resid_vs_actual.pdf", plot =  bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_hist_residuals.pdf", plot = bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_qq.pdf", plot = bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_summary.pdf", plot = bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "BayesRNN" && device == "jpeg"){
      ggplot2::ggsave("bayesrnn_pred_vs_actual.jpeg", plot = bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_resid_vs_actual.jpeg",  plot =  bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_hist_residuals.jpeg", plot = bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_qq.jpeg", plot = bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_summary.jpeg", plot = bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "BayesRNN" && device == "tiff"){
      ggplot2::ggsave("bayesrnn_pred_vs_actual.tiff", plot = bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_resid_vs_actual.tiff", plot =  bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_hist_residuals.tiff", plot = bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_qq.tiff", plot = bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_summary.tiff", plot = bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "BayesRNN" && device == "png"){
      ggplot2::ggsave("bayesrnn_pred_vs_actual.png", plot = bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_resid_vs_actual.png",  plot =  bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_hist_residuals.png", plot = bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_qq.png", plot = bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_summary.png", plot = bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations[1] == "BayesRNN" && device == "svg"){
      ggplot2::ggsave("bayesrnn_pred_vs_actual.svg", plot = bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_resid_vs_actual.svg",  plot =  bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_hist_residuals.svg", plot = bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_qq.svg", plot = bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("bayesrnn_summary.svg", plot = bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Cubist") {
    grid.arrange(cubist_pred_vs_actual, cubist_resid_vs_actual, cubist_hist_residuals, cubist_qq, ncol = 2)
    gridExtra::grid.arrange(cubist_pred_vs_actual)
    gridExtra::grid.arrange(cubist_resid_vs_actual)
    gridExtra::grid.arrange(cubist_hist_residuals)
    gridExtra::grid.arrange(cubist_qq)
    if(save_all_plots == "Y" && data_visualizations == "Cubist" && device == "eps"){
      ggplot2::ggsave("cubist_pred_vs_actual.eps", plot = cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_resid_vs_actual.eps", plot =cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_hist_residuals.eps", plot = cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_qq.eps", plot = cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_summary.eps", plot = cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Cubist" && device == "pdf"){
      ggplot2::ggsave("cubist_pred_vs_actual.pdf", plot = cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_resid_vs_actual.pdf", plot =cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_hist_residuals.pdf", plot = cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_qq.pdf", plot = cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_summary.pdf", plot = cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Cubist" && device == "jpeg"){
      ggplot2::ggsave("cubist_pred_vs_actual.jpeg", plot = cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_resid_vs_actual.jpeg", plot =cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_hist_residuals.jpeg", plot = cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_qq.jpeg", plot = cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_summary.jpeg", plot = cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Cubist" && device == "tiff"){
      ggplot2::ggsave("cubist_pred_vs_actual.tiff", plot = cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_resid_vs_actual.tiff", plot =cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_hist_residuals.tiff", plot = cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_qq.tiff", plot = cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_summary.tiff", plot = cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Cubist" && device == "png"){
      ggplot2::ggsave("cubist_pred_vs_actual.png", plot = cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_resid_vs_actual.png", plot =cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_hist_residuals.png", plot = cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_qq.png", plot = cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_summary.png", plot = cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations[1] == "Cubist" && device == "svg"){
      ggplot2::ggsave("cubist_pred_vs_actual.svg", plot = cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_resid_vs_actual.svg", plot =cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_hist_residuals.svg", plot = cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_qq.svg", plot = cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("cubist_summary.svg", plot = cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Earth") {
    grid.arrange(earth_pred_vs_actual, earth_resid_vs_actual, earth_hist_residuals, earth_qq, ncol = 2)
    gridExtra::grid.arrange(earth_pred_vs_actual)
    gridExtra::grid.arrange(earth_resid_vs_actual)
    gridExtra::grid.arrange(earth_hist_residuals)
    gridExtra::grid.arrange(earth_qq)
    if(save_all_plots == "Y" && data_visualizations == "Earth" && device == "eps"){
      ggplot2::ggsave("earth_pred_vs_actual.eps", plot = earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_resid_vs_actual.eps", plot = earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_hist_residuals.eps", plot = earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_qq.eps", plot = earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_summary.eps", plot = earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Earth" && device == "pdf"){
      ggplot2::ggsave("earth_pred_vs_actual.pdf", plot = earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_resid_vs_actual.pdf", plot = earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_hist_residuals.pdf", plot = earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_qq.pdf", plot = earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_summary.pdf", plot = earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Earth" && device == "jpeg"){
      ggplot2::ggsave("earth_pred_vs_actual.jpeg", plot = earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_resid_vs_actual.jpeg", plot = earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_hist_residuals.jpeg", plot = earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_qq.jpeg", plot = earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_summary.jpeg", plot = earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Earth" && device == "tiff"){
      ggplot2::ggsave("earth_pred_vs_actual.tiff", plot = earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_resid_vs_actual.tiff", plot = earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_hist_residuals.tiff", plot = earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_qq.tiff", plot = earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_summary.tiff", plot = earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Earth" && device == "png"){
      ggplot2::ggsave("earth_pred_vs_actual.png", plot = earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_resid_vs_actual.png", plot = earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_hist_residuals.png", plot = earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_qq.png", plot = earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_summary.png", plot = earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations[1] == "Earth" && device == "svg"){
      ggplot2::ggsave("earth_pred_vs_actual.svg", plot = earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_resid_vs_actual.svg", plot = earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_hist_residuals.svg", plot = earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_qq.svg", plot = earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("earth_summary.svg", plot = earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Elastic") {
    grid.arrange(elastic_pred_vs_actual, elastic_resid_vs_actual, elastic_hist_residuals, elastic_qq, ncol = 2)
    gridExtra::grid.arrange(elastic_pred_vs_actual)
    gridExtra::grid.arrange(elastic_resid_vs_actual)
    gridExtra::grid.arrange(elastic_hist_residuals)
    gridExtra::grid.arrange(elastic_qq)
    if(save_all_plots == "Y" && data_visualizations == "Elastic" && device == "eps"){
      ggplot2::ggsave("elastic_pred_vs_actual.eps", plot = elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_resid_vs_actual.eps", plot = elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_hist_residuals.eps", plot =  elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_qq.eps", plot = elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_summary.eps", plot = elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Elastic" && device == "pdf"){
      ggplot2::ggsave("elastic_pred_vs_actual.pdf", plot = elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_resid_vs_actual.pdf", plot = elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_hist_residuals.pdf", plot =  elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_qq.pdf", plot = elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_summary.pdf", plot = elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Elastic" && device == "jpeg"){
      ggplot2::ggsave("elastic_pred_vs_actual.jpeg", plot = elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_resid_vs_actual.jpeg", plot = elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_hist_residuals.jpeg", plot =  elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_qq.jpeg", plot = elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_summary.jpeg", plot = elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Elastic" && device == "tiff"){
      ggplot2::ggsave("elastic_pred_vs_actual.tiff", plot = elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_resid_vs_actual.tiff", plot = elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_hist_residuals.tiff", plot =  elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_qq.tiff", plot = elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_summary.tiff", plot = elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Elastic" && device == "png"){
      ggplot2::ggsave("elastic_pred_vs_actual.png", plot = elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_resid_vs_actual.png", plot = elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_hist_residuals.png", plot =  elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_qq.png", plot = elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_summary.png", plot = elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Elastic" && device == "svg"){
      ggplot2::ggsave("elastic_pred_vs_actual.svg", plot = elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_resid_vs_actual.svg", plot = elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_hist_residuals.svg", plot =  elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_qq.svg", plot = elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("elastic_summary.svg", plot = elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "GAM") {
    grid.arrange(gam_pred_vs_actual, gam_resid_vs_actual, gam_hist_residuals, gam_qq, ncol = 2)
    gridExtra::grid.arrange(gam_pred_vs_actual)
    gridExtra::grid.arrange(gam_resid_vs_actual)
    gridExtra::grid.arrange(gam_hist_residuals)
    gridExtra::grid.arrange(gam_qq)
    if(save_all_plots == "Y" && data_visualizations == "GAM" && device == "eps"){
      ggplot2::ggsave("gam_pred_vs_actual.eps", plot = gam_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_resid_vs_actual.eps", plot = gam_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_hist_residuals.eps", plot = gam_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_qq.eps", plot = gam_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_summary.eps", plot = gam_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "GAM" && device == "pdf"){
      ggplot2::ggsave("gam_pred_vs_actual.pdf", plot = gam_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_resid_vs_actual.pdf", plot = gam_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_hist_residuals.pdf",  plot =  gam_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_qq.pdf", plot = gam_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_summary.pdf", plot = gam_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "GAM" && device == "jpeg"){
      ggplot2::ggsave("gam_pred_vs_actual.jpeg", plot = gam_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_resid_vs_actual.jpeg", plot = gam_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_hist_residuals.jpeg", plot =  gam_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_qq.jpeg", plot = gam_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_summary.jpeg", plot = gam_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "GAM" && device == "tiff"){
      ggplot2::ggsave("gam_pred_vs_actual.tiff", plot = gam_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_resid_vs_actual.tiff", plot = gam_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_hist_residuals.tiff", plot =  gam_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_qq.tiff", plot = gam_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_summary.tiff", plot = gam_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "GAM" && device == "png"){
      ggplot2::ggsave("gam_pred_vs_actual.png", plot = gam_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_resid_vs_actual.png", plot = gam_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_hist_residuals.png", plot =  gam_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_qq.png", plot = gam_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_summary.png", plot = gam_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "GAM" && device == "svg"){
      ggplot2::ggsave("gam_pred_vs_actual.svg", plot = gam_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_resid_vs_actual.svg", plot = gam_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_hist_residuals.svg", plot =  gam_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_qq.svg", plot = gam_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gam_summary.svg", plot = gam_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Gradient Boosted") {
    grid.arrange(gb_pred_vs_actual, gb_resid_vs_actual, gb_hist_residuals, gb_qq, ncol = 2)
    gridExtra::grid.arrange(gb_pred_vs_actual)
    gridExtra::grid.arrange(gb_resid_vs_actual)
    gridExtra::grid.arrange(gb_hist_residuals)
    gridExtra::grid.arrange(gb_qq)
    if(save_all_plots == "Y" && data_visualizations == "Gardient Boosted" && device == "eps"){
      ggplot2::ggsave("gb_pred_vs_actual.eps", plot = gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_resid_vs_actual.eps", plot = gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_hist_residuals.eps", plot = gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_qq.eps", plot = gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_summary.eps", plot = gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Gardient Boosted" && device == "pdf"){
      ggplot2::ggsave("gb_pred_vs_actual.pdf", plot = gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_resid_vs_actual.pdf", plot = gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_hist_residuals.pdf", plot = gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_qq.pdf", plot = gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_summary.pdf", plot = gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Gardient Boosted" && device == "jpeg"){
      ggplot2::ggsave("gb_pred_vs_actual.jpeg", plot = gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_resid_vs_actual.jpeg", plot = gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_hist_residuals.jpeg", plot = gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_qq.jpeg", plot = gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_summary.jpeg", plot = gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Gardient Boosted" && device == "tiff"){
      ggplot2::ggsave("gb_pred_vs_actual.tiff", plot = gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_resid_vs_actual.tiff", plot = gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_hist_residuals.tiff", plot = gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_qq.tiff", plot = gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_summary.tiff", plot = gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Gardient Boosted" && device == "png"){
      ggplot2::ggsave("gb_pred_vs_actual.png", plot = gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_resid_vs_actual.png", plot = gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_hist_residuals.png", plot = gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_qq.png", plot = gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_summary.png", plot = gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Gardient Boosted" && device == "svg"){
      ggplot2::ggsave("gb_pred_vs_actual.svg", plot = gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_resid_vs_actual.svg", plot = gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_hist_residuals.svg", plot = gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_qq.svg", plot = gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("gb_summary.svg", plot = gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Lasso") {
    grid.arrange(lasso_pred_vs_actual, lasso_resid_vs_actual, lasso_hist_residuals, lasso_qq, ncol = 2)
    gridExtra::grid.arrange(lasso_pred_vs_actual)
    gridExtra::grid.arrange(lasso_resid_vs_actual)
    gridExtra::grid.arrange(lasso_hist_residuals)
    gridExtra::grid.arrange(lasso_qq)
    if(save_all_plots == "Y" && data_visualizations == "Lasso" && device == "eps"){
      ggplot2::ggsave("lasso_pred_vs_actual.eps", plot = lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_resid_vs_actual.eps", plot = lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_hist_residuals.eps", plot = lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_qq.eps", plot = lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_summary.eps", plot = lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Lasso" && device == "pdf"){
      ggplot2::ggsave("lasso_pred_vs_actual.pdf", plot = lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_resid_vs_actual.pdf", plot = lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_hist_residuals.pdf", plot = lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_qq.pdf", plot = lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_summary.pdf", plot = lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Lasso" && device == "jpeg"){
      ggplot2::ggsave("lasso_pred_vs_actual.jpeg", plot = lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_resid_vs_actual.jpeg", plot = lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_hist_residuals.jpeg", plot = lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_qq.jpeg", plot = lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_summary.jpeg", plot = lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Lasso" && device == "tiff"){
      ggplot2::ggsave("lasso_pred_vs_actual.tiff", plot = lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_resid_vs_actual.tiff", plot = lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_hist_residuals.tiff", plot = lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_qq.tiff", plot = lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_summary.tiff", plot = lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Lasso" && device == "png"){
      ggplot2::ggsave("lasso_pred_vs_actual.png", plot = lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_resid_vs_actual.png", plot = lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_hist_residuals.png", plot = lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_qq.png", plot = lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_summary.png", plot = lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Lasso" && device == "svg"){
      ggplot2::ggsave("lasso_pred_vs_actual.svg", plot = lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_resid_vs_actual.svg", plot = lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_hist_residuals.svg", plot = lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_qq.svg", plot = lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("lasso_summary.svg", plot = lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Linear") {
    gridExtra::grid.arrange(linear_pred_vs_actual, linear_resid_vs_actual, linear_hist_residuals, linear_qq, ncol = 2)
    gridExtra::grid.arrange(linear_pred_vs_actual)
    gridExtra::grid.arrange(linear_resid_vs_actual)
    gridExtra::grid.arrange(linear_hist_residuals)
    gridExtra::grid.arrange(linear_qq)
    if(save_all_plots == "Y" && data_visualizations == "Linear" && device == "eps"){
      ggplot2::ggsave("linear_pred_vs_actual.eps", plot = linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_resid_vs_actual.eps", plot = linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_hist_residuals.eps", plot = linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_qq.eps", plot = linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_summary.eps", plot = linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Linear" && device == "pdf"){
      ggplot2::ggsave("linear_pred_vs_actual.pdf", plot = linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_resid_vs_actual.pdf", plot = linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_hist_residuals.pdf", plot = linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_qq.pdf", plot = linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_summary.pdf", plot = linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Linear" && device == "jpeg"){
      ggplot2::ggsave("linear_pred_vs_actual.jpeg", plot = linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_resid_vs_actual.jpeg", plot = linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_hist_residuals.jpeg", plot = linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_qq.jpeg", plot = linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_summary.jpeg", plot = linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Linear" && device == "tiff"){
      ggplot2::ggsave("linear_pred_vs_actual.tiff", plot = linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_resid_vs_actual.tiff", plot = linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_hist_residuals.tiff", plot = linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_qq.tiff", plot = linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_summary.tiff", plot = linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Linear" && device == "png"){
      ggplot2::ggsave("linear_pred_vs_actual.png", plot = linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_resid_vs_actual.png", plot = linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_hist_residuals.png", plot = linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_qq.png", plot = linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_summary.png", plot = linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Linear" && device == "svg"){
      ggplot2::ggsave("linear_pred_vs_actual.svg", plot = linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_resid_vs_actual.svg", plot = linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_hist_residuals.svg", plot = linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_qq.svg", plot = linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("linear_summary.svg", plot = linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Neuralnet") {
    grid.arrange(neuralnet_pred_vs_actual, neuralnet_resid_vs_actual, neuralnet_hist_residuals, neuralnet_qq, ncol = 2)
    gridExtra::grid.arrange(neuralnet_pred_vs_actual)
    gridExtra::grid.arrange(neuralnet_resid_vs_actual)
    gridExtra::grid.arrange(neuralnet_hist_residuals)
    gridExtra::grid.arrange(neuralnet_qq)
    if(save_all_plots == "Y" && data_visualizations == "Neuralnet" && device == "eps"){
      ggplot2::ggsave("neuralnet_pred_vs_actual.eps", plot = neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_resid_vs_actual.eps", plot = neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_hist_residuals.eps", plot = neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_qq.eps", plot = neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_summary.eps", plot = neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Neuralnet" && device == "pdf"){
      ggplot2::ggsave("neuralnet_pred_vs_actual.pdf", plot = neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_resid_vs_actual.pdf", plot = neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_hist_residuals.pdf", plot = neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_qq.pdf", plot = neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_summary.pdf", plot = neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Neuralnet" && device == "jpeg"){
      ggplot2::ggsave("neuralnet_pred_vs_actual.jpeg", plot = neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_resid_vs_actual.jpeg", plot = neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_hist_residuals.jpeg", plot = neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_qq.jpeg", plot = neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_summary.jpeg", plot = neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Neuralnet" && device == "tiff"){
      ggplot2::ggsave("neuralnet_pred_vs_actual.tiff", plot = neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_resid_vs_actual.tiff", plot = neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_hist_residuals.tiff", plot = neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_qq.tiff", plot = neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_summary.tiff", plot = neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Neuralnet" && device == "png"){
      ggplot2::ggsave("neuralnet_pred_vs_actual.png", plot = neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_resid_vs_actual.png", plot = neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_hist_residuals.png", plot = neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_qq.png", plot = neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_summary.png", plot = neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Neuralnet" && device == "svg"){
      ggplot2::ggsave("neuralnet_pred_vs_actual.svg", plot = neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_resid_vs_actual.svg", plot = neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_hist_residuals.svg", plot = neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_qq.svg", plot = neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("neuralnet_summary.svg", plot = neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "PLS") {
    grid.arrange(pls_pred_vs_actual, pls_resid_vs_actual, pls_hist_residuals, pls_qq, ncol = 2)
    gridExtra::grid.arrange(pls_pred_vs_actual)
    gridExtra::grid.arrange(pls_resid_vs_actual)
    gridExtra::grid.arrange(pls_hist_residuals)
    gridExtra::grid.arrange(pls_qq)
    if(save_all_plots == "Y" && data_visualizations == "PLS" && device == "eps"){
      ggplot2::ggsave("pls_pred_vs_actual.eps", plot = pls_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_resid_vs_actual.eps", plot = pls_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_hist_residuals.eps", plot = pls_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_qq.eps", plot = pls_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_summary.eps", plot = pls_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "PLS" && device == "pdf"){
      ggplot2::ggsave("pls_pred_vs_actual.pdf", plot = pls_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_resid_vs_actual.pdf", plot = pls_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_hist_residuals.pdf", plot = pls_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_qq.pdf", plot = pls_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_summary.pdf", plot = pls_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "PLS" && device == "jpeg"){
      ggplot2::ggsave("pls_pred_vs_actual.jpeg", plot = pls_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_resid_vs_actual.jpeg", plot = pls_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_hist_residuals.jpeg", plot = pls_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_qq.jpeg", plot = pls_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_summary.jpeg", plot = pls_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "PLS" && device == "tiff"){
      ggplot2::ggsave("pls_pred_vs_actual.tiff", plot = pls_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_resid_vs_actual.tiff", plot = pls_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_hist_residuals.tiff", plot = pls_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_qq.tiff", plot = pls_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_summary.tiff", plot = pls_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "PLS" && device == "png"){
      ggplot2::ggsave("pls_pred_vs_actual.png", plot = pls_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_resid_vs_actual.png", plot = pls_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_hist_residuals.png", plot = pls_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_qq.png", plot = pls_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_summary.png", plot = pls_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "PLS" && device == "svg"){
      ggplot2::ggsave("pls_pred_vs_actual.svg", plot = pls_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_resid_vs_actual.svg", plot = pls_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_hist_residuals.svg", plot = pls_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_qq.svg", plot = pls_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pls_summary.svg", plot = pls_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "PCR") {
    grid.arrange(pcr_pred_vs_actual, pcr_resid_vs_actual, pcr_hist_residuals, pcr_qq, ncol = 2)
    gridExtra::grid.arrange(pcr_pred_vs_actual)
    gridExtra::grid.arrange(pcr_resid_vs_actual)
    gridExtra::grid.arrange(pcr_hist_residuals)
    gridExtra::grid.arrange(pcr_qq)
    if(save_all_plots == "Y" && data_visualizations == "PCR" && device == "eps"){
      ggplot2::ggsave("pcr_pred_vs_actual.eps", plot = pcr_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_resid_vs_actual.eps", plot = pcr_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_hist_residuals.eps", plot = pcr_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_qq.eps", plot = pcr_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_summary.eps", plot = pcr_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "PCR" && device == "pdf"){
      ggplot2::ggsave("pcr_pred_vs_actual.pdf", plot = pcr_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_resid_vs_actual.pdf", plot = pcr_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_hist_residuals.pdf", plot = pcr_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_qq.pdf", plot = pcr_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_summary.pdf", plot = pcr_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "PCR" && device == "jpeg"){
      ggplot2::ggsave("pcr_pred_vs_actual.jpeg", plot = pcr_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_resid_vs_actual.jpeg", plot = pcr_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_hist_residuals.jpeg", plot = pcr_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_qq.jpeg", plot = pcr_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_summary.jpeg", plot = pcr_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "PCR" && device == "tiff"){
      ggplot2::ggsave("pcr_pred_vs_actual.tiff", plot = pcr_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_resid_vs_actual.tiff", plot = pcr_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_hist_residuals.tiff", plot = pcr_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_qq.tiff", plot = pcr_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_summary.tiff", plot = pcr_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "PCR" && device == "png"){
      ggplot2::ggsave("pcr_pred_vs_actual.png", plot = pcr_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_resid_vs_actual.png", plot = pcr_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_hist_residuals.png", plot = pcr_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_qq.png", plot = pcr_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_summary.png", plot = pcr_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)

    }
    if(save_all_plots == "Y" && data_visualizations == "PCR" && device == "svg"){
      ggplot2::ggsave("pcr_pred_vs_actual.svg", plot = pcr_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_resid_vs_actual.svg", plot = pcr_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_hist_residuals.svg", plot = pcr_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("pcr_qq.svg", plot = pcr_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ridge") {
    grid.arrange(ridge_pred_vs_actual, ridge_resid_vs_actual, ridge_hist_residuals, ridge_qq, ncol = 2)
    gridExtra::grid.arrange(ridge_pred_vs_actual)
    gridExtra::grid.arrange(ridge_resid_vs_actual)
    gridExtra::grid.arrange(ridge_hist_residuals)
    gridExtra::grid.arrange(ridge_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ridge" && device == "eps"){
      ggplot2::ggsave("ridge_pred_vs_actual.eps", plot = ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_resid_vs_actual.eps", plot = ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_hist_residuals.eps", plot = ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_qq.eps", plot = ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_summary.eps", plot = ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ridge" && device == "pdf"){
      ggplot2::ggsave("ridge_pred_vs_actual.pdf", plot = ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_resid_vs_actual.pdf", plot = ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_hist_residuals.pdf", plot = ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_qq.pdf", plot = ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_summary.pdf", plot = ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ridge" && device == "jpeg"){
      ggplot2::ggsave("ridge_pred_vs_actual.jpeg", plot = ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_resid_vs_actual.jpeg", plot = ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_hist_residuals.jpeg", plot = ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_qq.jpeg", plot = ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_summary.jpeg", plot = ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ridge" && device == "tiff"){
      ggplot2::ggsave("ridge_pred_vs_actual.tiff", plot = ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_resid_vs_actual.tiff", plot = ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_hist_residuals.tiff", plot = ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_qq.tiff", plot = ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_summary.tiff", plot = ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ridge" && device == "png"){
      ggplot2::ggsave("ridge_pred_vs_actual.png", plot = ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_resid_vs_actual.png", plot = ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_hist_residuals.png", plot = ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_qq.png", plot = ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_summary.png", plot = ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ridge" && device == "svg"){
      ggplot2::ggsave("ridge_pred_vs_actual.svg", plot = ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_resid_vs_actual.svg", plot = ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_hist_residuals.svg", plot = ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_qq.svg", plot = ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ridge_summary.svg", plot = ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Rpart") {
    grid.arrange(rpart_pred_vs_actual, rpart_resid_vs_actual, rpart_hist_residuals, rpart_qq, ncol = 2)
    gridExtra::grid.arrange(rpart_pred_vs_actual)
    gridExtra::grid.arrange(rpart_resid_vs_actual)
    gridExtra::grid.arrange(rpart_hist_residuals)
    gridExtra::grid.arrange(rpart_qq)
    if(save_all_plots == "Y" && data_visualizations == "Rpart" && device == "eps"){
      ggplot2::ggsave("rpart_pred_vs_actual.eps", plot = rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_resid_vs_actual.eps", plot = rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_hist_residuals.eps", plot = rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_qq.eps", plot = rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_summary.eps", plot = rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Rpart" && device == "pdf"){
      ggplot2::ggsave("rpart_pred_vs_actual.pdf", plot = rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_resid_vs_actual.pdf", plot = rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_hist_residuals.pdf", plot = rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_qq.pdf", plot = rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_summary.pdf", plot = rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Rpart" && device == "jpeg"){
      ggplot2::ggsave("rpart_pred_vs_actual.jpeg", plot = rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_resid_vs_actual.jpeg", plot = rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_hist_residuals.jpeg", plot = rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_qq.jpeg", plot = rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_summary.jpeg", plot = rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Rpart" && device == "tiff"){
      ggplot2::ggsave("rpart_pred_vs_actual.tiff", plot = rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_resid_vs_actual.tiff", plot = rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_hist_residuals.tiff", plot = rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_qq.tiff", plot = rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_summary.tiff", plot = rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Rpart" && device == "png"){
      ggplot2::ggsave("rpart_pred_vs_actual.png", plot = rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_resid_vs_actual.png", plot = rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_hist_residuals.png", plot = rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_qq.png", plot = rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_summary.png", plot = rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Rpart" && device == "svg"){
      ggplot2::ggsave("rpart_pred_vs_actual.svg", plot = rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_resid_vs_actual.svg", plot = rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_hist_residuals.svg", plot = rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_qq.svg", plot = rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("rpart_summary.svg", plot = rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "SVM") {
    grid.arrange(svm_pred_vs_actual, svm_resid_vs_actual, svm_hist_residuals, svm_qq, ncol = 2)
    gridExtra::grid.arrange(svm_pred_vs_actual)
    gridExtra::grid.arrange(svm_resid_vs_actual)
    gridExtra::grid.arrange(svm_hist_residuals)
    gridExtra::grid.arrange(svm_qq)
    if(save_all_plots == "Y" && data_visualizations == "SVM" && device == "eps"){
      ggplot2::ggsave("svm_pred_vs_actual.eps", plot = svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_resid_vs_actual.eps", plot = svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_hist_residuals.eps", plot = svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_qq.eps", plot = svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_summary.eps", plot = svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "SVM" && device == "pdf"){
      ggplot2::ggsave("svm_pred_vs_actual.pdf", plot = svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_resid_vs_actual.pdf", plot = svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_hist_residuals.pdf", plot = svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_qq.pdf", plot = svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_summary.pdf", plot = svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "SVM" && device == "jpeg"){
      ggplot2::ggsave("svm_pred_vs_actual.jpeg", plot = svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_resid_vs_actual.jpeg", plot = svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_hist_residuals.jpeg", plot = svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_qq.jpeg", plot = svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_summary.jpeg", plot = svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "SVM" && device == "tiff"){
      ggplot2::ggsave("svm_pred_vs_actual.tiff", plot = svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_resid_vs_actual.tiff", plot = svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_hist_residuals.tiff", plot = svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_qq.tiff", plot = svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_summary.tiff", plot = svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "SVM" && device == "png"){
      ggplot2::ggsave("svm_pred_vs_actual.png", plot = svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_resid_vs_actual.png", plot = svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_hist_residuals.png", plot = svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_qq.png", plot = svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_summary.png", plot = svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "SVM" && device == "svg"){
      ggplot2::ggsave("svm_pred_vs_actual.svg", plot = svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_resid_vs_actual.svg", plot = svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_hist_residuals.svg", plot = svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_qq.svg", plot = svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("svm_summary.svg", plot = svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Tree") {
    grid.arrange(tree_pred_vs_actual, tree_resid_vs_actual, tree_hist_residuals, tree_qq, ncol = 2)
    gridExtra::grid.arrange(tree_pred_vs_actual)
    gridExtra::grid.arrange(tree_resid_vs_actual)
    gridExtra::grid.arrange(tree_hist_residuals)
    gridExtra::grid.arrange(tree_qq)
    if(save_all_plots == "Y" && data_visualizations == "Tree" && device == "eps"){
      ggplot2::ggsave("tree_pred_vs_actual.eps", plot = tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_resid_vs_actual.eps", plot = tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_hist_residuals.eps", plot = tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_qq.eps", plot = tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_summary.eps", plot = tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Tree" && device == "pdf"){
      ggplot2::ggsave("tree_pred_vs_actual.pdf", plot = tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_resid_vs_actual.pdf", plot = tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_hist_residuals.pdf", plot = tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_qq.pdf", plot = tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_summary.pdf", plot = tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Tree" && device == "jpeg"){
      ggplot2::ggsave("tree_pred_vs_actual.jpeg", plot = tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_resid_vs_actual.jpeg", plot = tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_hist_residuals.jpeg", plot = tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_qq.jpeg", plot = tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_summary.jpeg", plot = tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Tree" && device == "tiff"){
      ggplot2::ggsave("tree_pred_vs_actual.tiff", plot = tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_resid_vs_actual.tiff", plot = tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_hist_residuals.tiff", plot = tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_qq.tiff", plot = tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_summary.tiff", plot = tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Tree" && device == "png"){
      ggplot2::ggsave("tree_pred_vs_actual.png", plot = tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_resid_vs_actual.png", plot = tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_hist_residuals.png", plot = tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_qq.png", plot = tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_summary.png", plot = tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Tree" && device == "svg"){
      ggplot2::ggsave("tree_pred_vs_actual.svg", plot = tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_resid_vs_actual.svg", plot = tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_hist_residuals.svg", plot = tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_qq.svg", plot = tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("tree_summary.svg", plot = tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "XGBoost") {
    grid.arrange(xgb_pred_vs_actual, xgb_resid_vs_actual, xgb_hist_residuals, xgb_qq, ncol = 2)
    gridExtra::grid.arrange(xgb_pred_vs_actual)
    gridExtra::grid.arrange(xgb_resid_vs_actual)
    gridExtra::grid.arrange(xgb_hist_residuals)
    gridExtra::grid.arrange(xgb_qq)
    if(save_all_plots == "Y" && data_visualizations == "XGBoost" && device == "eps"){
      ggplot2::ggsave("xgb_pred_vs_actual.eps", plot = xgb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_resid_vs_actual.eps", plot = xgb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_hist_residuals.eps", plot = xgb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_qq.eps", plot = xgb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_summary.eps", plot = xgb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "XGBoost" && device == "pdf"){
      ggplot2::ggsave("xgb_pred_vs_actual.pdf", plot = xgb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_resid_vs_actual.pdf", plot = xgb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_hist_residuals.pdf", plot = xgb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_qq.pdf",  plot = xgb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_summary.pdf", plot = xgb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "XGBoost" && device == "jpeg"){
      ggplot2::ggsave("xgb_pred_vs_actual.jpeg", plot = xgb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_resid_vs_actual.jpeg", plot = xgb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_hist_residuals.jpeg", plot = xgb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_qq.jpeg", plot = xgb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_summary.jpeg", plot = xgb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "XGBoost" && device == "tiff"){
      ggplot2::ggsave("xgb_pred_vs_actual.tiff", plot = xgb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_resid_vs_actual.tiff", plot = xgb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_hist_residuals.tiff", plot = xgb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_qq.tiff", plot = xgb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_summary.tiff", plot = xgb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "XGBoost" && device == "png"){
      ggplot2::ggsave("xgb_pred_vs_actual.png", plot = xgb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_resid_vs_actual.png", plot = xgb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_hist_residuals.png", plot = xgb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_qq.png", plot = xgb_qq,, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_summary.png", plot = xgb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "XGBoost" && device == "svg"){
      ggplot2::ggsave("xgb_pred_vs_actual.svg", plot = xgb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_resid_vs_actual.svg", plot = xgb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_hist_residuals.svg", plot = xgb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_qq.svg", plot = xgb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("xgb_summary.svg", plot = xgb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble Bagging") {
    gridExtra::grid.arrange(ensemble_bagging_pred_vs_actual, ensemble_bagging_resid_vs_actual, ensemble_bagging_hist_residuals, ensemble_bagging_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_bagging_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_bagging_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_bagging_hist_residuals)
    gridExtra::grid.arrange(ensemble_bagging_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Bagging" && device == "eps"){
      ggplot2::ggsave("ensemble_bagging_pred_vs_actual.eps", plot = ensemble_bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_resid_vs_actual.eps", plot = ensemble_bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_hist_residuals.eps", plot = ensemble_bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_qq.eps", plot = ensemble_bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_summary.eps", plot = ensemble_bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Bagging" && device == "pdf"){
      ggplot2::ggsave("ensemble_bagging_pred_vs_actual.pdf", plot = ensemble_bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_resid_vs_actual.pdf", plot = ensemble_bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_hist_residuals.pdf", plot = ensemble_bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_qq.pdf", plot = ensemble_bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_summary.pdf", plot = ensemble_bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Bagging" && device == "jpeg"){
      ggplot2::ggsave("ensemble_bagging_pred_vs_actual.jpeg", plot = ensemble_bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_resid_vs_actual.jpeg", plot = ensemble_bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_hist_residuals.jpeg", plot = ensemble_bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_qq.jpeg", plot = ensemble_bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_summary.jpeg", plot = ensemble_bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Bagging" && device == "tiff"){
      ggplot2::ggsave("ensemble_bagging_pred_vs_actual.tiff", plot = ensemble_bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_resid_vs_actual.tiff", plot = ensemble_bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_hist_residuals.tiff", plot = ensemble_bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_qq.tiff", plot = ensemble_bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_summary.tiff", plot = ensemble_bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Bagging" && device == "png"){
      ggplot2::ggsave("ensemble_bagging_pred_vs_actual.png", plot = ensemble_bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_resid_vs_actual.png", plot = ensemble_bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_hist_residuals.png", plot = ensemble_bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_qq.png", plot = ensemble_bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_summary.png", plot = ensemble_bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Bagging" && device == "svg"){
      ggplot2::ggsave("ensemble_bagging_pred_vs_actual.svg", plot = ensemble_bagging_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_resid_vs_actual.svg", plot = ensemble_bagging_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_hist_residuals.svg", plot = ensemble_bagging_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_qq.svg", plot = ensemble_bagging_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bagging_summary.svg", plot = ensemble_bagging_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble BayesGLM") {
    gridExtra::grid.arrange(ensemble_bayesglm_pred_vs_actual, ensemble_bayesglm_resid_vs_actual, ensemble_bayesglm_hist_residuals, ensemble_bayesglm_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_bayesglm_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_bayesglm_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_bayesglm_hist_residuals)
    gridExtra::grid.arrange(ensemble_bayesglm_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesGLM" && device == "eps"){
      ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.eps", plot = ensemble_bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.eps", plot = ensemble_bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_hist_residuals.eps", plot = ensemble_bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_qq.eps", plot = ensemble_bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_summary.eps", plot = ensemble_bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesGLM" && device == "pdf"){
      ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.pdf", plot = ensemble_bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.pdf", plot = ensemble_bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_hist_residuals.pdf", plot = ensemble_bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_qq.pdf", plot = ensemble_bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_summary.pdf", plot = ensemble_bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesGLM" && device == "jpeg"){
      ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.jpeg", plot = ensemble_bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.jpeg", plot = ensemble_bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_hist_residuals.jpeg", plot = ensemble_bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_qq.jpeg", plot = ensemble_bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_summary.jpeg", plot = ensemble_bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesGLM" && device == "tiff"){
      ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.tiff", plot = ensemble_bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.tiff", plot = ensemble_bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_hist_residuals.tiff", plot = ensemble_bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_qq.tiff", plot = ensemble_bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_summary.tiff", plot = ensemble_bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesGLM" && device == "png"){
      ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.png", plot = ensemble_bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.png", plot = ensemble_bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_hist_residuals.png", plot = ensemble_bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_qq.png", plot = ensemble_bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_summary.png", plot = ensemble_bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesGLM" && device == "svg"){
      ggplot2::ggsave("ensemble_bayesglm_pred_vs_actual.svg", plot = ensemble_bayesglm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_resid_vs_actual.svg", plot = ensemble_bayesglm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_hist_residuals.svg", plot = ensemble_bayesglm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_qq.svg", plot = ensemble_bayesglm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesglm_summary.svg", plot = ensemble_bayesglm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble BayesRNN") {
    gridExtra::grid.arrange(ensemble_bayesrnn_pred_vs_actual, ensemble_bayesrnn_resid_vs_actual, ensemble_bayesrnn_hist_residuals, ensemble_bayesrnn_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_bayesrnn_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_bayesrnn_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_bayesrnn_hist_residuals)
    gridExtra::grid.arrange(ensemble_bayesrnn_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesRNN" && device == "eps"){
      ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.eps", plot = ensemble_bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.eps", plot = ensemble_bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.eps", plot = ensemble_bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_qq.eps", plot = ensemble_bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_summary.eps", plot = ensemble_bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesRNN" && device == "pdf"){
      ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.pdf", plot = ensemble_bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.pdf", plot = ensemble_bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.pdf", plot = ensemble_bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_qq.pdf", plot = ensemble_bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_summary.pdf", plot = ensemble_bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesRNN" && device == "jpeg"){
      ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.jpeg", plot = ensemble_bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.jpeg",plot = ensemble_bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.jpeg", plot = ensemble_bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_qq.jpeg", plot = ensemble_bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_summary.jpeg", plot = ensemble_bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesRNN" && device == "tiff"){
      ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.tiff", plot = ensemble_bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.tiff",plot = ensemble_bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.tiff", plot = ensemble_bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_qq.tiff", plot = ensemble_bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_summary.tiff", plot = ensemble_bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesRNN" && device == "png"){
      ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.png", plot = ensemble_bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.png", plot = ensemble_bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.png", plot = ensemble_bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_qq.png", plot = ensemble_bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_summary.png", plot = ensemble_bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble BayesRNN" && device == "svg"){
      ggplot2::ggsave("ensemble_bayesrnn_pred_vs_actual.svg", plot = ensemble_bayesrnn_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_resid_vs_actual.svg", plot = ensemble_bayesrnn_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_hist_residuals.svg", plot = ensemble_bayesrnn_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_qq.svg", plot = ensemble_bayesrnn_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_bayesrnn_summary.svg", plot = ensemble_bayesrnn_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble Cubist") {
    gridExtra::grid.arrange(ensemble_cubist_pred_vs_actual, ensemble_cubist_resid_vs_actual, ensemble_cubist_hist_residuals, ensemble_cubist_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_cubist_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_cubist_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_cubist_hist_residuals)
    gridExtra::grid.arrange(ensemble_cubist_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Cubist" && device == "eps"){
      ggplot2::ggsave("ensemble_cubist_pred_vs_actual.eps", plot = ensemble_cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_resid_vs_actual.eps", plot = ensemble_cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_hist_residuals.eps", plot = ensemble_cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_qq.eps", plot = ensemble_cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_summary.eps", plot = ensemble_cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Cubist" && device == "pdf"){
      ggplot2::ggsave("ensemble_cubist_pred_vs_actual.pdf",  plot = ensemble_cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_resid_vs_actual.pdf", plot = ensemble_cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_hist_residuals.pdf", plot = ensemble_cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_qq.pdf", plot = ensemble_cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_summary.pdf", plot = ensemble_cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Cubist" && device == "jpeg"){
      ggplot2::ggsave("ensemble_cubist_pred_vs_actual.jpeg", plot = ensemble_cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_resid_vs_actual.jpeg", plot = ensemble_cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_hist_residuals.jpeg", plot = ensemble_cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_qq.jpeg", plot = ensemble_cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_summary.jpeg", plot = ensemble_cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Cubist" && device == "tiff"){
      ggplot2::ggsave("ensemble_cubist_pred_vs_actual.tiff", plot = ensemble_cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_resid_vs_actual.tiff", plot = ensemble_cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_hist_residuals.tiff", plot = ensemble_cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_qq.tiff", plot = ensemble_cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_summary.tiff", plot = ensemble_cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Cubist" && device == "png"){
      ggplot2::ggsave("ensemble_cubist_pred_vs_actual.png", plot = ensemble_cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_resid_vs_actual.png", plot = ensemble_cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_hist_residuals.png", plot = ensemble_cubist_hist_residuals,, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_qq.png", plot = ensemble_cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_summary.png", plot = ensemble_cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Cubist" && device == "svg"){
      ggplot2::ggsave("ensemble_cubist_pred_vs_actual.svg", plot = ensemble_cubist_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_resid_vs_actual.svg", plot = ensemble_cubist_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_hist_residuals.svg", plot = ensemble_cubist_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_qq.svg", plot = ensemble_cubist_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_cubist_summary.svg", plot = ensemble_cubist_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble Earth") {
    gridExtra::grid.arrange(ensemble_earth_pred_vs_actual, ensemble_earth_resid_vs_actual, ensemble_earth_hist_residuals, ensemble_earth_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_earth_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_earth_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_earth_hist_residuals)
    gridExtra::grid.arrange(ensemble_earth_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Earth" && device == "eps"){
      ggplot2::ggsave("ensemble_earth_pred_vs_actual.eps", plot = ensemble_earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_resid_vs_actual.eps", plot = ensemble_earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_hist_residuals.eps", plot = ensemble_earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_qq.eps", plot = ensemble_earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_summary.eps", plot = ensemble_earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Earth" && device == "pdf"){
      ggplot2::ggsave("ensemble_earth_pred_vs_actual.pdf", plot = ensemble_earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_resid_vs_actual.pdf", plot = ensemble_earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_hist_residuals.pdf", plot = ensemble_earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_qq.pdf", plot = ensemble_earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_summary.pdf", plot = ensemble_earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Earth" && device == "jpeg"){
      ggplot2::ggsave("ensemble_earth_pred_vs_actual.jpeg", plot = ensemble_earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_resid_vs_actual.jpeg", plot = ensemble_earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_hist_residuals.jpeg", plot = ensemble_earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_qq.jpeg", plot = ensemble_earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_summary.jpeg", plot = ensemble_earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Earth" && device == "tiff"){
      ggplot2::ggsave("ensemble_earth_pred_vs_actual.tiff", plot = ensemble_earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_resid_vs_actual.tiff", plot = ensemble_earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_hist_residuals.tiff", plot = ensemble_earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_qq.tiff", plot = ensemble_earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_summary.tiff", plot = ensemble_earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Earth" && device == "png"){
      ggplot2::ggsave("ensemble_earth_pred_vs_actual.png", plot = ensemble_earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_resid_vs_actual.png", plot = ensemble_earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_hist_residuals.png", plot = ensemble_earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_qq.png", plot = ensemble_earth_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_summary.png", plot = ensemble_earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Earth" && device == "svg"){
      ggplot2::ggsave("ensemble_earth_pred_vs_actual.svg", plot = ensemble_earth_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_resid_vs_actual.svg", plot = ensemble_earth_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_hist_residuals.svg", plot = ensemble_earth_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_qq.svg", plot = ensemble_earth_qq,width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_earth_summary.svg", plot = ensemble_earth_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble_Elastic") {
    grid.arrange(ensemble_elastic_pred_vs_actual, ensemble_elastic_resid_vs_actual, ensemble_elastic_hist_residuals, ensemble_elastic_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_elastic_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_elastic_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_elastic_hist_residuals)
    gridExtra::grid.arrange(ensemble_elastic_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Elastic" && device == "eps"){
      ggplot2::ggsave("ensemble_elastic_pred_vs_actual.eps", plot = ensemble_elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_resid_vs_actual.eps", plot = ensemble_elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_hist_residuals.eps", plot = ensemble_elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_qq.eps", plot = ensemble_elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_summary.eps", plot = ensemble_elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Elastic" && device == "pdf"){
      ggplot2::ggsave("ensemble_elastic_pred_vs_actual.pdf", plot = ensemble_elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_resid_vs_actual.pdf", plot = ensemble_elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_hist_residuals.pdf", plot = ensemble_elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_qq.pdf", plot = ensemble_elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_summary.pdf", plot = ensemble_elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Elastic" && device == "jpeg"){
      ggplot2::ggsave("ensemble_elastic_pred_vs_actual.jpeg", plot = ensemble_elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_resid_vs_actual.jpeg", plot = ensemble_elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_hist_residuals.jpeg", plot = ensemble_elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_qq.jpeg", plot = ensemble_elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_summary.jpeg", plot = ensemble_elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Elastic" && device == "tiff"){
      ggplot2::ggsave("ensemble_elastic_pred_vs_actual.tiff", plot = ensemble_elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_resid_vs_actual.tiff", plot = ensemble_elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_hist_residuals.tiff", plot = ensemble_elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_qq.tiff", plot = ensemble_elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_summary.tiff", plot = ensemble_elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Elastic" && device == "png"){
      ggplot2::ggsave("ensemble_elastic_pred_vs_actual.png", plot = ensemble_elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_resid_vs_actual.png", plot = ensemble_elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_hist_residuals.png", plot = ensemble_elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_qq.png", plot = ensemble_elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_summary.png", plot = ensemble_elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Elastic" && device == "svg"){
      ggplot2::ggsave("ensemble_elastic_pred_vs_actual.svg", plot = ensemble_elastic_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_resid_vs_actual.svg", plot = ensemble_elastic_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_hist_residuals.svg", plot = ensemble_elastic_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_qq.svg", plot = ensemble_elastic_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_elastic_summary.svg", plot = ensemble_elastic_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble Gradient Boosted") {
    gridExtra::grid.arrange(ensemble_gb_pred_vs_actual, ensemble_gb_resid_vs_actual, ensemble_gb_hist_residuals, ensemble_gb_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_gb_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_gb_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_gb_hist_residuals)
    gridExtra::grid.arrange(ensemble_gb_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Gradient Boosted" && device == "eps"){
      ggplot2::ggsave("ensemble_gb_pred_vs_actual.eps", plot = ensemble_gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_resid_vs_actual.eps", plot = ensemble_gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_hist_residuals.eps", plot = ensemble_gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_qq.eps", plot = ensemble_gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_summary.eps", plot = ensemble_gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Gradient Boosted" && device == "pdf"){
      ggplot2::ggsave("ensemble_gb_pred_vs_actual.pdf", plot = ensemble_gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_resid_vs_actual.pdf", plot = ensemble_gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_hist_residuals.pdf", plot = ensemble_gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_qq.pdf", plot = ensemble_gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_summary.pdf", plot = ensemble_gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Gradient Boosted" && device == "jpeg"){
      ggplot2::ggsave("ensemble_gb_pred_vs_actual.jpeg", plot = ensemble_gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_resid_vs_actual.jpeg", plot = ensemble_gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_hist_residuals.jpeg", plot = ensemble_gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_qq.jpeg", plot = ensemble_gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_summary.jpeg", plot = ensemble_gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Gradient Boosted" && device == "tiff"){
      ggplot2::ggsave("ensemble_gb_pred_vs_actual.tiff", plot = ensemble_gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_resid_vs_actual.tiff", plot = ensemble_gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_hist_residuals.tiff", plot = ensemble_gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_qq.tiff", plot = ensemble_gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_summary.tiff", plot = ensemble_gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Gradient Boosted" && device == "png"){
      ggplot2::ggsave("ensemble_gb_pred_vs_actual.png", plot = ensemble_gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_resid_vs_actual.png", plot = ensemble_gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_hist_residuals.png", plot = ensemble_gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_qq.png", plot = ensemble_gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_summary.png", plot = ensemble_gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Gradient Boosted" && device == "svg"){
      ggplot2::ggsave("ensemble_gb_pred_vs_actual.svg", plot = ensemble_gb_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_resid_vs_actual.svg", plot = ensemble_gb_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_hist_residuals.svg", plot = ensemble_gb_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_qq.svg", plot = ensemble_gb_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_gb_summary.svg", plot = ensemble_gb_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble_Lasso") {
    grid.arrange(ensemble_lasso_pred_vs_actual, ensemble_lasso_resid_vs_actual, ensemble_lasso_hist_residuals, ensemble_lasso_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_lasso_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_lasso_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_lasso_hist_residuals)
    gridExtra::grid.arrange(ensemble_lasso_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Lasso" && device == "eps"){
      ggplot2::ggsave("ensemble_lasso_pred_vs_actual.eps", plot = ensemble_lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_resid_vs_actual.eps", plot = ensemble_lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_hist_residuals.eps", plot = ensemble_lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_qq.eps", plot = ensemble_lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_summary.eps", plot = ensemble_lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Lasso" && device == "pdf"){
      ggplot2::ggsave("ensemble_lasso_pred_vs_actual.pdf", plot = ensemble_lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_resid_vs_actual.pdf", plot = ensemble_lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_hist_residuals.pdf", plot = ensemble_lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_qq.pdf", plot = ensemble_lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_summary.pdf", plot = ensemble_lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Lasso" && device == "jpeg"){
      ggplot2::ggsave("ensemble_lasso_pred_vs_actual.jpeg", plot = ensemble_lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_resid_vs_actual.jpeg", plot = ensemble_lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_hist_residuals.jpeg", plot = ensemble_lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_qq.jpeg", plot = ensemble_lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_summary.jpeg", plot = ensemble_lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Lasso" && device == "tiff"){
      ggplot2::ggsave("ensemble_lasso_pred_vs_actual.tiff", plot = ensemble_lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_resid_vs_actual.tiff", plot = ensemble_lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_hist_residuals.tiff", plot = ensemble_lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_qq.tiff", plot = ensemble_lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_summary.tiff", plot = ensemble_lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Lasso" && device == "png"){
      ggplot2::ggsave("ensemble_lasso_pred_vs_actual.png", plot = ensemble_lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_resid_vs_actual.png", plot = ensemble_lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_hist_residuals.png", plot = ensemble_lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_qq.png", plot = ensemble_lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_summary.png", plot = ensemble_lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Lasso" && device == "svg"){
      ggplot2::ggsave("ensemble_lasso_pred_vs_actual.svg", plot = ensemble_lasso_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_resid_vs_actual.svg", plot = ensemble_lasso_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_hist_residuals.svg", plot = ensemble_lasso_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_qq.svg", plot = ensemble_lasso_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_lasso_summary.svg", plot = ensemble_lasso_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble Linear") {
    gridExtra::grid.arrange(ensemble_linear_pred_vs_actual, ensemble_linear_resid_vs_actual, ensemble_linear_hist_residuals, ensemble_linear_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_linear_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_linear_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_linear_hist_residuals)
    gridExtra::grid.arrange(ensemble_linear_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Linear" && device == "eps"){
      ggplot2::ggsave("ensemble_linear_pred_vs_actual.eps", plot = ensemble_linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_resid_vs_actual.eps", plot = ensemble_linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_hist_residuals.eps", plot = ensemble_linear_hist_residuals,  width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_qq.eps", plot = ensemble_linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_summary.eps", plot = ensemble_linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Linear" && device == "pdf"){
      ggplot2::ggsave("ensemble_linear_pred_vs_actual.pdf", plot = ensemble_linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_resid_vs_actual.pdf", plot = ensemble_linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_hist_residuals.pdf", plot = ensemble_linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_qq.pdf", plot = ensemble_linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_summary.pdf", plot = ensemble_linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Linear" && device == "jpeg"){
      ggplot2::ggsave("ensemble_linear_pred_vs_actual.jpeg", plot = ensemble_linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_resid_vs_actual.jpeg", plot = ensemble_linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_hist_residuals.jpeg", plot = ensemble_linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_qq.jpeg", plot = ensemble_linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_summary.jpeg", plot = ensemble_linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Linear" && device == "tiff"){
      ggplot2::ggsave("ensemble_linear_pred_vs_actual.tiff", plot = ensemble_linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_resid_vs_actual.tiff", plot = ensemble_linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_hist_residuals.tiff", plot = ensemble_linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_qq.tiff", plot = ensemble_linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_summary.tiff", plot = ensemble_linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Linear" && device == "png"){
      ggplot2::ggsave("ensemble_linear_pred_vs_actual.png", plot = ensemble_linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_resid_vs_actual.png", plot = ensemble_linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_hist_residuals.png", plot = ensemble_linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_qq.png", plot = ensemble_linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_summary.png", plot = ensemble_linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Linear" && device == "svg"){
      ggplot2::ggsave("ensemble_linear_pred_vs_actual.svg", plot = ensemble_linear_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_resid_vs_actual.svg", plot = ensemble_linear_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_hist_residuals.svg", plot = ensemble_linear_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_qq.svg", plot = ensemble_linear_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_linear_summary.svg", plot = ensemble_linear_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble Neuralnet") {
    grid.arrange(ensemble_neuralnet_pred_vs_actual, ensemble_neuralnet_resid_vs_actual, ensemble_neuralnet_hist_residuals, ensemble_neuralnet_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_neuralnet_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_neuralnet_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_neuralnet_hist_residuals)
    gridExtra::grid.arrange(ensemble_neuralnet_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Neuralnet" && device == "eps"){
      ggplot2::ggsave("ensemble_neuralnet_pred_vs_actual.eps", plot = ensemble_neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_resid_vs_actual.eps", plot = ensemble_neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_hist_residuals.eps", plot = ensemble_neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_qq.eps", plot = ensemble_neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_summary.eps", plot = ensemble_neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Neuralnet" && device == "pdf"){
      ggplot2::ggsave("ensemble_neuralnet_pred_vs_actual.pdf", plot = ensemble_neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_resid_vs_actual.pdf", plot = ensemble_neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_hist_residuals.pdf", plot = ensemble_neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_qq.pdf", plot = ensemble_neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_summary.pdf", plot = ensemble_neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Neuralnet" && device == "jpeg"){
      ggplot2::ggsave("ensemble_neuralnet_pred_vs_actual.jpeg", plot = ensemble_neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_resid_vs_actual.jpeg", plot = ensemble_neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_hist_residuals.jpeg", plot = ensemble_neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_qq.jpeg", plot = ensemble_neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_summary.jpeg", plot = ensemble_neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Neuralnet" && device == "tiff"){
      ggplot2::ggsave("ensemble_neuralnet_pred_vs_actual.tiff", plot = ensemble_neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_resid_vs_actual.tiff", plot = ensemble_neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_hist_residuals.tiff", plot = ensemble_neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_qq.tiff", plot = ensemble_neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_summary.tiff", plot = ensemble_neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Neuralnet" && device == "png"){
      ggplot2::ggsave("ensemble_neuralnet_pred_vs_actual.png", plot = ensemble_neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_resid_vs_actual.png", plot = ensemble_neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_hist_residuals.png", plot = ensemble_neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_qq.png", plot = ensemble_neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_summary.png", plot = ensemble_neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Neuralnet" && device == "svg"){
      ggplot2::ggsave("ensemble_neuralnet_pred_vs_actual.svg", plot = ensemble_neuralnet_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_resid_vs_actual.svg", plot = ensemble_neuralnet_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_hist_residuals.svg", plot = ensemble_neuralnet_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_qq.svg", plot = ensemble_neuralnet_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_neuralnet_summary.svg", plot = ensemble_neuralnet_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble_Ridge") {
    grid.arrange(ensemble_ridge_pred_vs_actual, ensemble_ridge_resid_vs_actual, ensemble_ridge_hist_residuals, ensemble_ridge_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_ridge_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_ridge_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_ridge_hist_residuals)
    gridExtra::grid.arrange(ensemble_ridge_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Ridge" && device == "eps"){
      ggplot2::ggsave("ensemble_ridge_pred_vs_actual.eps", plot = ensemble_ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_resid_vs_actual.eps", plot = ensemble_ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_hist_residuals.eps", plot = ensemble_ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_qq.eps", plot = ensemble_ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_summary.eps", plot = ensemble_ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Ridge" && device == "pdf"){
      ggplot2::ggsave("ensemble_ridge_pred_vs_actual.pdf", plot = ensemble_ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_resid_vs_actual.pdf", plot = ensemble_ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_hist_residuals.pdf", plot = ensemble_ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_qq.pdf", plot = ensemble_ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_summary.pdf", plot = ensemble_ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Ridge" && device == "jpeg"){
      ggplot2::ggsave("ensemble_ridge_pred_vs_actual.jpeg", plot = ensemble_ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_resid_vs_actual.jpeg", plot = ensemble_ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_hist_residuals.jpeg", plot = ensemble_ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_qq.jpeg", plot = ensemble_ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_summary.jpeg", plot = ensemble_ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Ridge" && device == "tiff"){
      ggplot2::ggsave("ensemble_ridge_pred_vs_actual.tiff", plot = ensemble_ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_resid_vs_actual.tiff", plot = ensemble_ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_hist_residuals.tiff", plot = ensemble_ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_qq.tiff", plot = ensemble_ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_summary.tiff", plot = ensemble_ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Ridge" && device == "png"){
      ggplot2::ggsave("ensemble_ridge_pred_vs_actual.png", plot = ensemble_ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_resid_vs_actual.png", plot = ensemble_ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_hist_residuals.png", plot = ensemble_ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_qq.png", plot = ensemble_ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_summary.png", plot = ensemble_ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble_Ridge" && device == "svg"){
      ggplot2::ggsave("ensemble_ridge_pred_vs_actual.svg", plot = ensemble_ridge_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_resid_vs_actual.svg", plot = ensemble_ridge_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_hist_residuals.svg", plot = ensemble_ridge_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_qq.svg", plot = ensemble_ridge_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_ridge_summary.svg", plot = ensemble_ridge_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble Rpart") {
    gridExtra::grid.arrange(ensemble_rpart_pred_vs_actual, ensemble_rpart_resid_vs_actual, ensemble_rpart_hist_residuals, ensemble_rpart_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_rpart_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_rpart_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_rpart_hist_residuals)
    gridExtra::grid.arrange(ensemble_rpart_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Rpart" && device == "eps"){
      ggplot2::ggsave("ensemble_rpart_pred_vs_actual.eps", plot = ensemble_rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_resid_vs_actual.eps", plot = ensemble_rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_hist_residuals.eps", plot = ensemble_rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_qq.eps", plot = ensemble_rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_summary.eps", plot = ensemble_rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Rpart" && device == "pdf"){
      ggplot2::ggsave("ensemble_rpart_pred_vs_actual.pdf", plot = ensemble_rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_resid_vs_actual.pdf", plot = ensemble_rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_hist_residuals.pdf", plot = ensemble_rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_qq.pdf", plot = ensemble_rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_summary.pdf", plot = ensemble_rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Rpart" && device == "jpeg"){
      ggplot2::ggsave("ensemble_rpart_pred_vs_actual.jpeg", plot = ensemble_rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_resid_vs_actual.jpeg", plot = ensemble_rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_hist_residuals.jpeg", plot = ensemble_rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_qq.jpeg", plot = ensemble_rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_summary.jpeg", plot = ensemble_rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Rpart" && device == "tiff"){
      ggplot2::ggsave("ensemble_rpart_pred_vs_actual.tiff", plot = ensemble_rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_resid_vs_actual.tiff", plot = ensemble_rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_hist_residuals.tiff", plot = ensemble_rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_qq.tiff", plot = ensemble_rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_summary.tiff", plot = ensemble_rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Rpart" && device == "png"){
      ggplot2::ggsave("ensemble_rpart_pred_vs_actual.png", plot = ensemble_rpart_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_resid_vs_actual.png", plot = ensemble_rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_hist_residuals.png", plot = ensemble_rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_qq.png", plot = ensemble_rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_summary.png", plot = ensemble_rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Rpart" && device == "svg"){
      ggplot2::ggsave("ensemble_rpart_pred_vs_actual.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_resid_vs_actual.svg", plot = ensemble_rpart_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_hist_residuals.svg", plot = ensemble_rpart_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_qq.svg", plot = ensemble_rpart_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_rpart_summary.svg", plot = ensemble_rpart_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble Support Vector Machines") {
    gridExtra::grid.arrange(ensemble_svm_pred_vs_actual, ensemble_svm_resid_vs_actual, ensemble_svm_hist_residuals, ensemble_svm_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_svm_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_svm_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_svm_hist_residuals)
    gridExtra::grid.arrange(ensemble_svm_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Support Vector Machines" && device == "eps"){
      ggplot2::ggsave("ensemble_svm_pred_vs_actual.eps", plot = ensemble_svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_resid_vs_actual.eps", plot = ensemble_svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_hist_residuals.eps", plot = ensemble_svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_qq.eps", plot = ensemble_svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_summary.eps", plot = ensemble_svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Support Vector Machines" && device == "pdf"){
      ggplot2::ggsave("ensemble_svm_pred_vs_actual.pdf", plot = ensemble_svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_resid_vs_actual.pdf", plot = ensemble_svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_hist_residuals.pdf", plot = ensemble_svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_qq.pdf", plot = ensemble_svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_summary.pdf", plot = ensemble_svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Support Vector Machines" && device == "jpeg"){
      ggplot2::ggsave("ensemble_svm_pred_vs_actual.jpeg", plot = ensemble_svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_resid_vs_actual.jpeg", plot = ensemble_svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_hist_residuals.jpeg", plot = ensemble_svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_qq.jpeg", plot = ensemble_svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_summary.jpeg", plot = ensemble_svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Support Vector Machines" && device == "tiff"){
      ggplot2::ggsave("ensemble_svm_pred_vs_actual.tiff", plot = ensemble_svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_resid_vs_actual.tiff", plot = ensemble_svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_hist_residuals.tiff", plot = ensemble_svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_qq.tiff", plot = ensemble_svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_summary.tiff", plot = ensemble_svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Support Vector Machines" && device == "png"){
      ggplot2::ggsave("ensemble_svm_pred_vs_actual.png", plot = ensemble_svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_resid_vs_actual.png", plot = ensemble_svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_hist_residuals.png", plot = ensemble_svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_qq.png", plot = ensemble_svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_summary.png", plot = ensemble_svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Support Vector Machines" && device == "svg"){
      ggplot2::ggsave("ensemble_svm_pred_vs_actual.svg", plot = ensemble_svm_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_resid_vs_actual.svg", plot = ensemble_svm_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_hist_residuals.svg", plot = ensemble_svm_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_qq.svg", plot = ensemble_svm_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_svm_summary.svg", plot = ensemble_svm_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }

  if (data_visualizations == "Ensemble Trees") {
    gridExtra::grid.arrange(ensemble_tree_pred_vs_actual, ensemble_tree_resid_vs_actual, ensemble_tree_hist_residuals, ensemble_tree_qq, ncol = 2)
    gridExtra::grid.arrange(ensemble_tree_pred_vs_actual)
    gridExtra::grid.arrange(ensemble_tree_resid_vs_actual)
    gridExtra::grid.arrange(ensemble_tree_hist_residuals)
    gridExtra::grid.arrange(ensemble_tree_qq)
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Trees" && device == "eps"){
      ggplot2::ggsave("ensemble_tree_pred_vs_actual.eps", plot = ensemble_tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_resid_vs_actual.eps", plot = ensemble_tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_hist_residuals.eps", plot = ensemble_tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_qq.eps", plot = ensemble_tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_summary.eps", plot = ensemble_tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Trees" && device == "pdf"){
      ggplot2::ggsave("ensemble_tree_pred_vs_actual.pdf", plot = ensemble_tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_resid_vs_actual.pdf", plot = ensemble_tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_hist_residuals.pdf", plot = ensemble_tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_qq.pdf", plot = ensemble_tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_summary.pdf", plot = ensemble_tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Trees" && device == "jpeg"){
      ggplot2::ggsave("ensemble_tree_pred_vs_actual.jpeg", plot = ensemble_tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_resid_vs_actual.jpeg", plot = ensemble_tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_hist_residuals.jpeg", plot = ensemble_tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_qq.jpeg", plot = ensemble_tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_summary.jpeg", plot = ensemble_tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Trees" && device == "tiff"){
      ggplot2::ggsave("ensemble_tree_pred_vs_actual.tiff", plot = ensemble_tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_resid_vs_actual.tiff", plot = ensemble_tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_hist_residuals.tiff", plot = ensemble_tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_qq.tiff", plot = ensemble_tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_summary.tiff", plot = ensemble_tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Trees" && device == "png"){
      ggplot2::ggsave("ensemble_tree_pred_vs_actual.png", plot = ensemble_tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_resid_vs_actual.png", plot = ensemble_tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_hist_residuals.png", plot = ensemble_tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_qq.png", plot = ensemble_tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_summary.png", plot = ensemble_tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
    if(save_all_plots == "Y" && data_visualizations == "Ensemble Trees" && device == "svg"){
      ggplot2::ggsave("ensemble_tree_pred_vs_actual.svg", plot = ensemble_tree_pred_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_resid_vs_actual.svg", plot = ensemble_tree_resid_vs_actual, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_hist_residuals.svg", plot = ensemble_tree_hist_residuals, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_qq.svg", plot = ensemble_tree_qq, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
      ggplot2::ggsave("ensemble_tree_summary.svg", plot = ensemble_tree_summary, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
    }
  }
}


#### Accuracy data frame and plots ####
accuracy_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" = c(
      c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
      c(rep("BayesRNN", numresamples)),
      c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
      c(rep("Gradient Boosted", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
      c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
      c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
      c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
      c(rep("Ensemble Bagging", numresamples)),
      c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
      c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
      c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble Lasso", numresamples)),
      c(rep("Ensemble Linear", numresamples)), c(rep("Ensemble Neuralnet", numresamples)), c(rep("Ensemble Ridge", numresamples)),
      c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
      c(rep("Ensemble Trees", numresamples))
    ),
    "data" = c(
      bagging_holdout_RMSE, bayesglm_holdout_RMSE,
      bayesrnn_holdout_RMSE,
      cubist_holdout_RMSE, earth_holdout_RMSE, elastic_holdout_RMSE_df$elastic_holdout_RMSE[2:nrow(elastic_holdout_RMSE_df)], gam_holdout_RMSE,
      gb_holdout_RMSE, lasso_holdout_RMSE_df$lasso_holdout_RMSE[2:nrow(lasso_holdout_RMSE_df)], linear_holdout_RMSE,
      neuralnet_holdout_RMSE, pcr_holdout_RMSE, pls_holdout_RMSE,
      ridge_holdout_RMSE_df$ridge_holdout_RMSE[2:nrow(ridge_holdout_RMSE_df)], rpart_holdout_RMSE,
      svm_holdout_RMSE, tree_holdout_RMSE, xgb_holdout_RMSE,
      ensemble_bagging_holdout_RMSE,
      ensemble_bayesglm_holdout_RMSE, ensemble_bayesrnn_holdout_RMSE,
      ensemble_cubist_holdout_RMSE, ensemble_earth_holdout_RMSE, ensemble_elastic_holdout_RMSE,
      ensemble_gb_holdout_RMSE, ensemble_lasso_holdout_RMSE,
      ensemble_linear_holdout_RMSE, ensemble_neuralnet_holdout_RMSE, ensemble_ridge_holdout_RMSE,
      ensemble_rpart_holdout_RMSE, ensemble_svm_holdout_RMSE,
      ensemble_tree_holdout_RMSE
    ),
    "mean" = rep(c(
      bagging_holdout_RMSE_mean, bayesglm_holdout_RMSE_mean,
      bayesrnn_holdout_RMSE_mean,
      cubist_holdout_RMSE_mean, earth_holdout_RMSE_mean, elastic_holdout_RMSE_mean, gam_holdout_RMSE_mean,
      gb_holdout_RMSE_mean, lasso_holdout_RMSE_mean, linear_holdout_RMSE_mean,
      neuralnet_holdout_RMSE_mean, pcr_holdout_RMSE_mean, pls_holdout_RMSE_mean,
      ridge_holdout_RMSE_mean, rpart_holdout_RMSE_mean,
      svm_holdout_RMSE_mean, tree_holdout_RMSE_mean, xgb_holdout_RMSE_mean,
      ensemble_bagging_holdout_RMSE_mean,
      ensemble_bayesglm_holdout_RMSE_mean, ensemble_bayesrnn_holdout_RMSE_mean,
      ensemble_cubist_holdout_RMSE_mean, ensemble_earth_holdout_RMSE_mean, ensemble_elastic_holdout_RMSE_mean,
      ensemble_gb_holdout_RMSE_mean, ensemble_lasso_holdout_RMSE_mean, ensemble_linear_holdout_RMSE_mean,
      ensemble_neuralnet_holdout_RMSE_mean, ensemble_ridge_holdout_RMSE_mean,
      ensemble_rpart_holdout_RMSE_mean, ensemble_svm_holdout_RMSE_mean,
      ensemble_tree_holdout_RMSE_mean
    ), each = numresamples)
  )

accuracy_plot_fixed_scales <- ggplot2::ggplot(data = accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("Accuracy data (RMSE), fixed scales\nRoot Mean Squared Error by model, lower is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "Root Mean Squared Error (RMSE), lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_plot_fixed_scales.eps", plot = accuracy_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_plot_fixed_scales.jpeg", plot = accuracy_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_plot_fixed_scales.pdf", plot = accuracy_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_plot_fixed_scales.png", plot = accuracy_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_plot_fixed_scales.svg", plot = accuracy_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_plot_fixed_scales.tiff", plot = accuracy_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
accuracy_plot_free_scales <- ggplot2::ggplot(data = accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("Accuracy data (RMSE), free scales\nRoot Mean Squared Error by model, lower is better. \nThe black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "Root Mean Squared Error (RMSE), lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_plot_free_scales.eps", plot = accuracy_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_plot_free_scales.jpeg", plot = accuracy_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_plot_free_scales.pdf",plot = accuracy_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_plot_free_scales.png", plot = accuracy_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_plot_free_scales.svg", plot = accuracy_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_plot_free_scales.tiff", plot = accuracy_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### Total data (train vs holdout) data frame and plots ####
total_data <-
  data.frame(
    "count" = 1:numresamples,
    "model" =
      c(
        c(rep("Bagging", numresamples)), c(rep("BayesGLM", numresamples)),
        c(rep("BayesRNN", numresamples)),
        c(rep("Cubist", numresamples)), c(rep("Earth", numresamples)), c(rep("Elastic", numresamples)), c(rep("Generalized Additive Models (GAM)", numresamples)),
        c(rep("Gradient Boosted", numresamples)), c(rep("Lasso", numresamples)), c(rep("Linear", numresamples)),
        c(rep("Neuralnet", numresamples)), c(rep("Principal Components Regression", numresamples)), c(rep("Partial Least Squares", numresamples)),
        c(rep("Ridge", numresamples)), c(rep("RPart", numresamples)),
        c(rep("Support Vector Machines", numresamples)), c(rep("Trees", numresamples)), c(rep("XGBoost", numresamples)),
        c(rep("Ensemble Bagging", numresamples)),
        c(rep("Ensemble BayesGLM", numresamples)), c(rep("Ensemble BayesRNN", numresamples)),
        c(rep("Ensemble Cubist", numresamples)), c(rep("Ensemble Earth", numresamples)), c(rep("Ensemble Elastic", numresamples)),
        c(rep("Ensemble Gradient Boosted", numresamples)), c(rep("Ensemble Lasso", numresamples)),
        c(rep("Ensemble Linear", numresamples)), c(rep("Ensemble Neuralnet", numresamples)), c(rep("Ensemble Ridge", numresamples)),
        c(rep("Ensemble RPart", numresamples)), c(rep("Ensemble Support Vector Machines", numresamples)),
        c(rep("Ensemble Trees", numresamples))
      ),
    "train" = c(
      bagging_train_RMSE, bayesglm_train_RMSE,
      bayesrnn_train_RMSE,
      cubist_train_RMSE, earth_train_RMSE, elastic_train_RMSE_df$elastic_train_RMSE[2:nrow(elastic_train_RMSE_df)], gam_train_RMSE,
      gb_train_RMSE, lasso_train_RMSE_df$lasso_train_RMSE[2:nrow(lasso_train_RMSE_df)], linear_train_RMSE,
      neuralnet_train_RMSE, pcr_train_RMSE, pls_train_RMSE,
      ridge_train_RMSE_df$ridge_train_RMSE[2:nrow(ridge_train_RMSE_df)], rpart_train_RMSE,
      svm_train_RMSE, tree_train_RMSE, xgb_train_RMSE,
      ensemble_bagging_train_RMSE,
      ensemble_bayesglm_train_RMSE, ensemble_bayesrnn_train_RMSE,
      ensemble_cubist_train_RMSE, ensemble_earth_train_RMSE,
      ensemble_elastic_train_RMSE_df$ensemble_elastic_train_RMSE[2:nrow(ensemble_elastic_train_RMSE_df)],
      ensemble_gb_train_RMSE,
      ensemble_lasso_train_RMSE_df$ensemble_lasso_train_RMSE[2:nrow(ensemble_lasso_train_RMSE_df)],
      ensemble_linear_train_RMSE, ensemble_neuralnet_train_RMSE,
      ensemble_ridge_train_RMSE_df$ensemble_ridge_train_RMSE[2:nrow(ensemble_ridge_train_RMSE_df)],
      ensemble_rpart_train_RMSE, ensemble_svm_train_RMSE,
      ensemble_tree_train_RMSE
    ),

    "holdout" = c(
      bagging_holdout_RMSE, bayesglm_holdout_RMSE,
      bayesrnn_holdout_RMSE,
      cubist_holdout_RMSE, earth_holdout_RMSE, elastic_holdout_RMSE_df$elastic_holdout_RMSE[2:nrow(elastic_holdout_RMSE_df)], gam_holdout_RMSE,
      gb_holdout_RMSE, lasso_holdout_RMSE_df$lasso_holdout_RMSE[2:nrow(lasso_holdout_RMSE_df)], linear_holdout_RMSE,
      neuralnet_holdout_RMSE, pcr_holdout_RMSE, pls_holdout_RMSE,
      ridge_holdout_RMSE_df$ridge_holdout_RMSE[2:nrow(ridge_holdout_RMSE_df)], rpart_holdout_RMSE,
      svm_holdout_RMSE, tree_holdout_RMSE, xgb_holdout_RMSE,
      ensemble_bagging_holdout_RMSE,
      ensemble_bayesglm_holdout_RMSE, ensemble_bayesrnn_holdout_RMSE,
      ensemble_cubist_holdout_RMSE, ensemble_earth_holdout_RMSE, ensemble_elastic_holdout_RMSE,
      ensemble_gb_holdout_RMSE, ensemble_lasso_holdout_RMSE,
      ensemble_linear_holdout_RMSE, ensemble_neuralnet_holdout_RMSE, ensemble_ridge_holdout_RMSE,
      ensemble_rpart_holdout_RMSE, ensemble_svm_holdout_RMSE,
      ensemble_tree_holdout_RMSE
    )
  )

total_plot_free_scales <- ggplot2::ggplot(data = total_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = train, color = "train")) +
  ggplot2::geom_point(mapping = aes(x = count, y = train)) +
  ggplot2::geom_line(mapping = aes(x = count, y = holdout, color = "holdout")) +
  ggplot2::geom_point(mapping = aes(x = count, y = holdout)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "optimal")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("Overfitting (Train vs holdout) results by resample and model. Free scales \nRoot Mean Squared Error by model, lower is better. \nThe black horizontal line is 0.") +
  ggplot2::labs(y = "Root Mean Squared Error (RMSE), lower is better \nthe black line is 0.\n") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("holdout", "train"),
    values = c(
      "train" = "blue", "holdout" = "red")
  )
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("total_plot_free_scales.eps", plot = total_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("total_plot_free_scales.jpeg", plot = total_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("total_plot_free_scales.pdf", plot = total_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("total_plot_free_scales.png", plot = total_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("total_plot_free_scales.svg", plot = total_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("total_plot_free_scales.tiff", plot = total_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

total_plot_fixed_scales <- ggplot2::ggplot(data = total_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = train, color = "train")) +
  ggplot2::geom_point(mapping = aes(x = count, y = train)) +
  ggplot2::geom_line(mapping = aes(x = count, y = holdout, color = "holdout")) +
  ggplot2::geom_point(mapping = aes(x = count, y = holdout)) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "optimal")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("Overfitting (Train vs holdout) results by resample and model. Fixed scales \nRoot Mean Squared Error by model, lower is better. \nThe black horizontal line is 0.") +
  ggplot2::labs(y = "Root Mean Squared Error (RMSE), lower is better \nthe black line is 0.\n") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("holdout", "train"),
    values = c(
      "train" = "blue", "holdout" = "red")
  )
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("total_plot_fixed_scales.eps", plot = total_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("total_plot_fixed_scales.jpeg", plot = total_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("total_plot_fixed_scales.pdf", plot = total_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("total_plot_fixed_scales.png", plot = total_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("total_plot_fixed_scales.svg", plot = total_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("total_plot_fixed_scales.tiff", plot = total_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### Variable importance plot ####
lm_vip <- lm(y ~ ., data = df)
vip_df <- vip::vi(lm_vip)
vip_df$Percentage <- round(vip_df$Importance / sum(vip_df$Importance), 4)
vip_df$Total_Percentage <- cumsum(vip_df$Percentage)
vip_df <- vip_df %>% dplyr::arrange(dplyr::desc(Percentage))
variable_importance <- reactable::reactable(as.data.frame(vip_df),
                                            searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                            striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "variable_importance")
)

variable_importance <- htmlwidgets::prependContent(variable_importance, htmltools::h2(class = "title", "Variable importance report"))

variable_importance_barchart <- ggplot2::ggplot(data = vip_df, mapping = aes(x = stats::reorder(Variable, -Percentage), y = Percentage)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(mapping = aes(label = paste0(100*Percentage, "%"), y = 1.03 * Percentage)) +
  ggplot2::ggtitle("Variable Importance (based on a linear model applied to the full data set)") +
  ggplot2::xlab(label = "Features") +
  ggplot2::scale_y_continuous(labels = scales::label_percent())
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("variable_importance_barchart.eps", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("variable_importance_barchart.jpeg", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("variable_importance_barchart.pdf", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("variable_importance_barchart.png", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("variable_importance_barchart.svg", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("variable_importance_barchart.tiff", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### Start making predictions on new data here ####

if (predict_on_new_data == "Y") {
  new_bagging <- predict(object = bagging_train_fit, newdata = new_data)
  new_bayesglm <- predict(object = bayesglm_train_fit, newdata = new_data)
  new_bayesrnn <- predict(object = bayesrnn_train_fit, newdata = new_data)
  new_cubist <- predict(object = cubist_train_fit, newdata = new_data)
  new_earth <- as.numeric(predict(object = earth_train_fit, newdata = new_data))
  new_elastic <- rowMeans(predict(object = best_elastic_model, newdata = new_data, newx = as.matrix(new_data[, 1:ncol(new_data) - 1])))
  new_gam <- predict(object = gam_train_fit, newdata = new_data)
  new_gb <- predict(object = gb_train_fit, newdata = new_data)
  new_lasso <- rowMeans(predict(object = best_lasso_model, newdata = new_data, newx = as.matrix(new_data[, 1:ncol(new_data) - 1])))
  new_linear <- predict(object = linear_train_fit$best.model, newdata = new_data)
  new_neuralnet <- predict(object = neuralnet_train_fit, newdata = new_data)
  new_pls <- predict(object = pls_train_fit, newdata = new_data)[, , 1]
  new_pcr <- predict(object = pcr_train_fit, newdata = new_data)[, , 1]
  new_ridge <- rowMeans(predict(object = best_ridge_model, newdata = new_data, newx = as.matrix(new_data[, 1:ncol(new_data) - 1])))
  new_rpart <- predict(object = rpart_train_fit, newdata = new_data)
  new_svm <- predict(object = svm_train_fit$best.model, k = svm_train_fit$best_model$k, newdata = new_data)
  new_tree <- predict(object = tree_train_fit, newdata = new_data)

  # XGBoost
  # split into training and testing set
  new_train <- train
  new_test <- new_data

  # define predictor and response variables in training set
  new_train_x <- data.matrix(new_train[, -ncol(new_train)])
  new_train_y <- new_test[, ncol(new_test)]

  # define predictor and response variables in testing set
  new_test_x <- data.matrix(new_test[, -ncol(new_test)])
  new_test_y <- new_test[, ncol(new_test)]

  new_xgb_test <- xgboost::xgb.DMatrix(data = test_x, label = test_y)

  new_watchlist_test <- list(train = xgb_train, test = xgb_test)

  new_xgb_model <- xgboost::xgb.train(data = new_xgb_test, evals = new_watchlist_test, nrounds = 70)

  new_XGBoost <- predict(object = new_xgb_model, newdata = new_test_x)

  new_ensemble <- data.frame(
    "Bagging" = new_bagging / bagging_holdout_RMSE_mean,
    "BayesGLM" = new_bayesglm / bayesglm_holdout_RMSE_mean,
    "BayesRNN" = new_bayesrnn / bayesrnn_holdout_RMSE_mean,
    "Cubist" = new_cubist / cubist_holdout_RMSE_mean,
    "Earth" = new_earth / earth_holdout_RMSE_mean,
    "Elastic" = new_elastic / elastic_holdout_RMSE_mean,
    "GAM" = new_gam / gam_holdout_RMSE_mean,
    "GBM" = new_gb / gb_holdout_RMSE_mean,
    "Lasso" = new_lasso / lasso_holdout_RMSE_mean,
    "Linear" = new_linear / linear_holdout_RMSE_mean,
    "Neuralnet" = new_pcr / pcr_holdout_RMSE_mean,
    "PCR" = new_pcr / pcr_holdout_RMSE_mean,
    "PLS" = new_pls / pls_holdout_RMSE_mean,
    "Ridge" = new_ridge / ridge_holdout_RMSE_mean,
    "Rpart" = new_rpart / rpart_holdout_RMSE_mean,
    "SVM" = new_svm / svm_holdout_RMSE_mean,
    "Tree" = new_tree / tree_holdout_RMSE_mean,
    "XGBoost" = new_XGBoost / xgb_holdout_RMSE_mean
  )

  new_ensemble$y_ensemble <- new_data$y

  thing1 <- colnames(new_ensemble)

  new_ensemble <- dplyr::select(new_ensemble, thing1)

  new_ensemble_bagging <- predict(object = ensemble_bagging_train_fit, newdata = new_ensemble)
  new_ensemble_bayesglm <- predict(object = ensemble_bayesglm_train_fit, newdata = new_ensemble)
  new_ensemble_bayesrnn <- predict(object = ensemble_bayesrnn_train_fit, newdata = new_ensemble)
  new_ensemble_cart <- predict(object = ensemble_rpart_train_fit, newdata = new_ensemble)
  new_ensemble_cubist <- predict(object = ensemble_cubist_train_fit, newdata = new_ensemble)
  new_ensemble_earth <- predict(object = ensemble_earth_train_fit, newdata = new_ensemble)
  new_ensemble_elastic <- rowMeans(predict(object = ensemble_elastic_model, newx = data.matrix(new_ensemble %>% dplyr::select(-y_ensemble))))
  new_ensemble_gb <- predict(object = ensemble_gb_train_fit, newdata = new_ensemble)
  new_ensemble_lasso <- rowMeans(predict(object = ensemble_lasso_model, newx = data.matrix(new_ensemble %>% dplyr::select(-y_ensemble))))
  new_ensemble_linear <- predict(object = ensemble_linear_train_fit$best.model, newdata = new_ensemble)
  new_ensemble_neuralnet <- predict(object = ensemble_neuralnet_train_fit, newdata = new_ensemble)
  new_ensemble_rpart <- predict(object = ensemble_rpart_train_fit, newdata = new_ensemble)
  new_ensemble_ridge <- rowMeans(predict(object = ensemble_ridge_model, newx = data.matrix(new_ensemble %>% dplyr::select(-y_ensemble))))
  new_ensemble_svm <- predict(object = ensemble_svm_train_fit$best.model, newdata = new_ensemble)
  new_ensemble_tree <- predict(object = ensemble_tree_train_fit, newdata = new_ensemble)

  new_data_results <-
    data.frame(
      "True_Value" = new_ensemble$y_ensemble,
      "Bagging" = round(new_bagging, 4),
      "BayesGLM" = round(new_bayesglm, 4),
      "BayesRNN" = round(new_bayesrnn, 4),
      "Cubist" = round(new_cubist, 4),
      "Earth" = round(new_earth, 4),
      "Elastic" = round(new_elastic, 4),
      "GAM" = round(new_gam, 4),
      "GBM" = round(new_gb, 4),
      "Lasso" = round(new_lasso, 4),
      "Linear" = round(new_linear, 4),
      "Neuralnet" = round(new_neuralnet, 4),
      "PLS" = round(new_pls, 4),
      "PCR" = round(new_pcr, 4),
      "Ridge" = round(new_ridge, 4),
      "Rpart" = round(new_rpart, 4),
      "SVM" = round(new_svm, 4),
      "Tree" = round(new_tree, 4),
      "XGBoost" = round(new_XGBoost, 4),
      "Ensemble_Bagging" = round(new_ensemble_bagging, 4),
      "Ensemble_BayesGLM" = round(new_ensemble_bayesglm, 4),
      "Ensemble_BayesRNN" = round(new_ensemble_bayesrnn, 4),
      "Ensemble_Cart" = round(new_ensemble_cart, 4),
      "Ensemble_Cubist" = round(new_ensemble_cubist, 4),
      "Ensemble_Earth" = as.numeric(round(new_ensemble_earth, 4)),
      "Ensemble_Elastic" = round(new_ensemble_elastic, 4),
      "Ensemble_Gardient_Boosted" = round(new_ensemble_gb, 4),
      "Ensemble_Lasso" = round(new_ensemble_lasso, 4),
      "Ensemble_Linear" = round(new_ensemble_linear, 4),
      "Ensemble_Neuralnet" = round(new_ensemble_neuralnet, 4),
      "Ensemble_RPart" = round(new_ensemble_rpart, 4),
      "Ensemble_SVM" = round(new_ensemble_svm, 4),
      "Ensemble_Tree" = round(new_ensemble_tree, 4)
    )

  df1 <- t(new_data_results)

  predictions_of_new_data <- reactable::reactable(
    data = df1, searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
    striped = TRUE, highlight = TRUE, resizable = TRUE
  )

  htmltools::div(class = "table",
                 htmltools::div(class = "title", "predictions_of_new_data")
  )

  predictions_of_new_data <- htmlwidgets::prependContent(predictions_of_new_data, htmltools::h2(class = "title", "Predictions of new data"))


  if (save_all_trained_models == "Y") {
    tempdir1 <- tempdir

    fil <- tempfile("bagging_train_fit", fileext = ".RDS")
    bagging_train_fit <- saveRDS(bagging_train_fit, fil)

    fil <- tempfile("bayesglm_train_fit", fileext = ".RDS")
    saveRDS(bayesglm_train_fit, fil)

    fil <- tempfile("bayesrnn_train_fit", fileext = ".RDS")
    saveRDS(bayesrnn_train_fit, fil)

    fil <- tempfile("cubist_train_fit", fileext = ".RDS")
    saveRDS(cubist_train_fit, fil)

    fil <- tempfile("earth_train_fit", fileext = ".RDS")
    saveRDS(earth_train_fit, fil)

    fil <- tempfile("best_elastic_model", fileext = ".RDS")
    saveRDS(best_elastic_model, fil)

    fil <- tempfile("gam_train_fit", fileext = ".RDS")
    saveRDS(gam_train_fit, fil)

    fil <- tempfile("gb_train_fit", fileext = ".RDS")
    saveRDS(gb_train_fit, fil)

    fil <- tempfile("best_lasso_model", fileext = ".RDS")
    saveRDS(best_lasso_model, fil)

    fil <- tempfile("linear_train_fit", fileext = ".RDS")
    saveRDS(linear_train_fit, fil)

    fil <- tempfile("neuralnet_train_fit", fileext = ".RDS")
    saveRDS(neuralnet_train_fit, fil)

    fil <- tempfile("pls_train_fit", fileext = ".RDS")
    saveRDS(pls_train_fit, fil)

    fil <- tempfile("pcr_train_fit", fileext = ".RDS")
    saveRDS(pcr_train_fit, fil)

    fil <- tempfile("best_ridge_model", fileext = ".RDS")
    saveRDS(best_ridge_model, fil)

    fil <- tempfile("rpart_train_fit", fileext = ".RDS")
    saveRDS(rpart_train_fit, fil)

    fil <- tempfile("svm_train_fit", fileext = ".RDS")
    saveRDS(svm_train_fit, fil)

    fil <- tempfile("tree_train_fit", fileext = ".RDS")
    saveRDS(tree_train_fit, fil)

    fil <- tempfile("xgb_model", fileext = ".RDS")
    saveRDS(xgb_model, fil)

    fil <- tempfile("ensemble", fileext = ".RDS")
    saveRDS(ensemble, fil)

    fil <- tempfile("ensemble_bagging_train_fit", fileext = ".RDS")
    saveRDS(ensemble_bagging_train_fit, fil)

    fil <- tempfile("ensemble_bayesglm_train_fit", fileext = ".RDS")
    saveRDS(ensemble_bayesglm_train_fit, fil)

    fil <- tempfile("ensemble_bayesrnn_train_fit", fileext = ".RDS")
    saveRDS(ensemble_bayesrnn_train_fit, fil)

    fil <- tempfile("ensemble_cubist_train_fit", fileext = ".RDS")
    saveRDS(ensemble_cubist_train_fit, fil)

    fil <- tempfile("ensemble_earth_train_fit", fileext = ".RDS")
    saveRDS(ensemble_earth_train_fit, fil)

    fil <- tempfile("ensemble_best_elastic_model", fileext = ".RDS")
    saveRDS(ensemble_best_elastic_model, fil)

    fil <- tempfile("ensemble_gb_train_fit", fileext = ".RDS")
    saveRDS(ensemble_gb_train_fit, fil)

    fil <- tempfile("ensemble_best_lasso_model", fileext = ".RDS")
    saveRDS(ensemble_best_lasso_model, fil)

    fil <- tempfile("ensemble_linear_train_fit", fileext = ".RDS")
    saveRDS(ensemble_linear_train_fit, fil)

    fil <- tempfile("ensemble_neuralnet_train_fit", fileext = ".RDS")
    saveRDS(ensemble_neuralnet_train_fit, fil)

    fil <- tempfile("ensemble_bagging_fit", fileext = ".RDS")
    saveRDS(ensemble_bagging_train_fit, fil)

    fil <- tempfile("ensemble_best_ridge_model", fileext = ".RDS")
    saveRDS(ensemble_best_ridge_model, fil)

    fil <- tempfile("ensemble_rpart_train_fit", fileext = ".RDS")
    saveRDS(ensemble_rpart_train_fit, fil)

    fil <- tempfile("ensemble_svm_train_fit", fileext = ".RDS")
    saveRDS(ensemble_svm_train_fit, fil)

    fil <- tempfile("ensemble_tree_train_fit", fileext = ".RDS")
    saveRDS(ensemble_tree_train_fit, fil)

    fil <- tempfile("head_of_data_frame", fileext = ".RDS")
    saveRDS(head(df), fil)

    fil <- tempfile("correlation_of_the_data", fileext = ".RDS")
    saveRDS(M1, fil)

    fil <- tempfile("variance_inflation_factor", fileext = ".RDS")
    saveRDS(VIF, fil)

    fil <- tempfile("head_of_ensemble", fileext = ".RDS")
    saveRDS(head(ensemble), fil)

    fil <- tempfile("summary_report", fileext = ".RDS")
    saveRDS(summary_results, fil)

    fil <- tempfile("correlation_of_the_ensemble", fileext = ".RDS")
    saveRDS(cor(ensemble), fil)

    fil <- tempfile("data_summary", fileext = ".RDS")
    saveRDS(data_summary, fil)

  }

  message('The trained models are temporariliy saved in this directory: tempdir1. This directory is automatically deleted at the end of the R session.
            You may save the trained models before you end this session if you chose to do so.')



  return(list(
    "head_of_data" = head_df, "boxplots" = boxplots, "variable_importance_barchart" = variable_importance_barchart, "variable_importance_table" = variable_importance,
    "Cooks_distance" = cooks_distance_plot, "histograms" = histograms, "predictor_vs_target" = predictor_vs_target, "predictor_vs_target" = predictor_vs_target, "data_correlation" = data_correlation,
    "Correlation_as_numbers" = corrplot_number, "Correlation_as_circles" = corrplot_circle, "Corrplot_full" = corrplot_full,'VIF' = VIF_report,
    "accuracy_barchart" = accuracy_barchart, "accuracy_plot_fixed_scales" = accuracy_plot_fixed_scales, "accuracy_free_scales" = accuracy_plot_free_scales, "bias_barchart" = bias_barchart, "bias_plot" = bias_plot, "duration_barchart" = duration_barchart,
    "head_of_ensemble" = head_ensemble, "overfitting_barchart" = overfitting_barchart, "overfitting_histograms" = overfitting_histograms,
    "overfitting_plot_fixed_scales" = overfitting_plot_fixed_scales, "overfitting_plot_free_scales" = overfitting_plot_free_scales,
    "train_vs_holdout" = total_plot_fixed_scales, "train_vs_holdout_free_scales" = total_plot_free_scales, "stratified_resampling_report" = stratified_sampling_report,
    "Kolmogorov-Smirnov test p-score" = k_s_test_barchart, "p-value_barchart" = p_value_barchart, "predictions_on_new_data" = predictions_of_new_data,
    "final_results_table" = final_results,  "ensemble_correlation" = ensemble_correlation,
    "data_summary" = data_summary, "outlier_data" = outlier_list,
    "colnum" = colnum, "numresamples" = numresamples, "save_all_trained_modesl" = save_all_trained_models, "how_to_handle_strings" = how_to_handle_strings,
    "data_reduction_method" = data_reduction_method,  "scale_data" = scale_all_predictors_in_data,
    "train_amount" = train_amount, "test_amount" = test_amount, "validation_amount" = validation_amount
  )
  )
}


#### Separators start here ####
olddata <- old_data %>% dplyr::relocate(all_of(colnum), .after = last_col())
olddata <- olddata %>% dplyr::arrange(dplyr::desc(olddata[, ncol(olddata)]))
separator <- round(nrow(olddata)*0.05,0)
high_5_percent <- head(olddata, n = separator)
high_5_percent$group <- as.factor(c("Highest_five_percent"))
low_5_percent <- tail(olddata, n = separator)
low_5_percent$group <- as.factor(c("Lowest_five_percent"))
summary <- rbind(high_5_percent, low_5_percent)
summary <- summary %>% mutate_if(is.numeric, round, digits = 0)
summary_list <- reactable::reactable(summary,
                                     searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                     striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "summary_list")
)

summary_list <- htmlwidgets::prependContent(summary_list, htmltools::h2(class = "title", "Highest 5% and lowest 5% report"))

plot_list <- lapply(1:(ncol(summary)-1), \(i) {
  df1 <- stats::aggregate(
    summary[, ncol(summary) - 1],
    by = list(summary[, i], summary$group), FUN = sum
  )
  max_y <- max(df1$x)

  ggplot2::ggplot(df1, ggplot2::aes(y = factor(Group.1), x = x)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      y = NULL,
      title = paste0(
        colnames(summary)[ncol(summary)-1], " by ", colnames(summary)[i]
      )
    ) +
    ggplot2::geom_label(
      aes(
        label = scales::comma(x),
        hjust = ifelse(x > .5 * max_y, 1, 0),
        color = I(ifelse(x > .5 * max_y, "white", "black"))
      ),
      size = 6,
      fill = NA, border.color = NA
    ) +
    ggplot2::facet_grid(~Group.2)
})

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("plot_list.eps", plot = plot_list, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("plot_list.jpeg", plot = plot_list, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("plot_list.pdf", plot = plot_list, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("plot_list.png", plot = plot_list, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("plot_list.svg", plot = plot_list, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("plot_list.tiff", plot = plot_list, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### Save all trained models starts here ####
if (save_all_trained_models == "Y") {
  tempdir1 <- tempdir()

  fil <- tempfile("bagging_train_fit", fileext = ".RDS")
  bagging_train_fit <- saveRDS(bagging_train_fit, fil)

  fil <- tempfile("bayesglm_train_fit", fileext = ".RDS")
  saveRDS(bayesglm_train_fit, fil)

  fil <- tempfile("bayesrnn_train_fit", fileext = ".RDS")
  saveRDS(bayesrnn_train_fit, fil)

  fil <- tempfile("cubist_train_fit", fileext = ".RDS")
  saveRDS(cubist_train_fit, fil)

  fil <- tempfile("earth_train_fit", fileext = ".RDS")
  saveRDS(earth_train_fit, fil)

  fil <- tempfile("best_elastic_model", fileext = ".RDS")
  saveRDS(best_elastic_model, fil)

  fil <- tempfile("gam_train_fit", fileext = ".RDS")
  saveRDS(gam_train_fit, fil)

  fil <- tempfile("gb_train_fit", fileext = ".RDS")
  saveRDS(gb_train_fit, fil)

  fil <- tempfile("best_lasso_model", fileext = ".RDS")
  saveRDS(best_lasso_model, fil)

  fil <- tempfile("linear_train_fit", fileext = ".RDS")
  saveRDS(linear_train_fit, fil)

  fil <- tempfile("neuralnet_train_fit", fileext = ".RDS")
  saveRDS(neuralnet_train_fit, fil)

  fil <- tempfile("pls_train_fit", fileext = ".RDS")
  saveRDS(pls_train_fit, fil)

  fil <- tempfile("pcr_train_fit", fileext = ".RDS")
  saveRDS(pcr_train_fit, fil)

  fil <- tempfile("best_ridge_model", fileext = ".RDS")
  saveRDS(best_ridge_model, fil)

  fil <- tempfile("rpart_train_fit", fileext = ".RDS")
  saveRDS(rpart_train_fit, fil)

  fil <- tempfile("svm_train_fit", fileext = ".RDS")
  saveRDS(svm_train_fit, fil)

  fil <- tempfile("tree_train_fit", fileext = ".RDS")
  saveRDS(tree_train_fit, fil)

  fil <- tempfile("xgb_model", fileext = ".RDS")
  saveRDS(xgb_model, fil)

  fil <- tempfile("ensemble", fileext = ".RDS")
  saveRDS(ensemble, fil)

  fil <- tempfile("ensemble_bagging_train_fit", fileext = ".RDS")
  saveRDS(ensemble_bagging_train_fit, fil)

  fil <- tempfile("ensemble_bayesglm_train_fit", fileext = ".RDS")
  saveRDS(ensemble_bayesglm_train_fit, fil)

  fil <- tempfile("ensemble_bayesrnn_train_fit", fileext = ".RDS")
  saveRDS(ensemble_bayesrnn_train_fit, fil)

  fil <- tempfile("ensemble_cubist_train_fit", fileext = ".RDS")
  saveRDS(ensemble_cubist_train_fit, fil)

  fil <- tempfile("ensemble_earth_train_fit", fileext = ".RDS")
  saveRDS(ensemble_earth_train_fit, fil)

  fil <- tempfile("ensemble_best_elastic_model", fileext = ".RDS")
  saveRDS(ensemble_best_elastic_model, fil)

  fil <- tempfile("ensemble_gb_train_fit", fileext = ".RDS")
  saveRDS(ensemble_gb_train_fit, fil)

  fil <- tempfile("ensemble_best_lasso_model", fileext = ".RDS")
  saveRDS(ensemble_best_lasso_model, fil)

  fil <- tempfile("ensemble_linear_train_fit", fileext = ".RDS")
  saveRDS(ensemble_linear_train_fit, fil)

  fil <- tempfile("ensemble_neuralnet_train_fit", fileext = ".RDS")
  saveRDS(ensemble_neuralnet_train_fit, fil)

  fil <- tempfile("ensemble_bagging_fit", fileext = ".RDS")
  saveRDS(ensemble_bagging_train_fit, fil)

  fil <- tempfile("ensemble_best_ridge_model", fileext = ".RDS")
  saveRDS(ensemble_best_ridge_model, fil)

  fil <- tempfile("ensemble_rpart_train_fit", fileext = ".RDS")
  saveRDS(ensemble_rpart_train_fit, fil)

  fil <- tempfile("ensemble_svm_train_fit", fileext = ".RDS")
  saveRDS(ensemble_svm_train_fit, fil)

  fil <- tempfile("ensemble_tree_train_fit", fileext = ".RDS")
  saveRDS(ensemble_tree_train_fit, fil)

  fil <- tempfile("head_of_data_frame", fileext = ".RDS")
  saveRDS(head(df), fil)

  fil <- tempfile("correlation_of_the_data", fileext = ".RDS")
  saveRDS(M1, fil)

  fil <- tempfile("variance_inflation_factor", fileext = ".RDS")
  saveRDS(VIF, fil)

  fil <- tempfile("head_of_ensemble", fileext = ".RDS")
  saveRDS(head(ensemble), fil)

  fil <- tempfile("summary_report", fileext = ".RDS")
  saveRDS(summary_results, fil)

  fil <- tempfile("correlation_of_the_ensemble", fileext = ".RDS")
  saveRDS(cor(ensemble), fil)

  fil <- tempfile("data_summary", fileext = ".RDS")
  saveRDS(data_summary, fil)

  fil <- tempfile("outliers_list", fileext = ".RDS")
  saveRDS(outlier_list, fil)

  fil <- tempfile("summary_list", fileext = ".RDS")
  saveRDS(summary_list, fil)

}

message('The trained models are temporariliy saved in this directory: tempdir1. This directory is automatically deleted at the end of the R session.
          You may save the trained models before you end this session if you chose to do so.')


return(list(
  "head_of_data" = head_df, "boxplots" = boxplots, "variable_importance_barchart" = variable_importance_barchart, "variable_importance_table" = variable_importance,
  "Cooks_distance" = cooks_distance_plot, "histograms" = histograms, "predictor_vs_target" = predictor_vs_target, "predictor_vs_target" = predictor_vs_target, "data_correlation" = data_correlation,
  "Correlation_as_numbers" = corrplot_number, "Correlation_as_circles" = corrplot_circle, "Corrplot_full" = corrplot_full,'VIF' = VIF_report,
  "accuracy_barchart" = accuracy_barchart, "accuracy_plot_fixed_scales" = accuracy_plot_fixed_scales, "accuracy_free_scales" = accuracy_plot_free_scales, "bias_barchart" = bias_barchart, "bias_plot" = bias_plot, "duration_barchart" = duration_barchart,
  "head_of_ensemble" = head_ensemble, "overfitting_barchart" = overfitting_barchart, "overfitting_histograms" = overfitting_histograms,
  "overfitting_plot_fixed_scales" = overfitting_plot_fixed_scales, "overfitting_plot_free_scales" = overfitting_plot_free_scales,
  "train_vs_holdout" = total_plot_fixed_scales, "train_vs_holdout_free_scales" = total_plot_free_scales, "stratified_resampling_report" = stratified_sampling_report,
  "Kolmogorov-Smirnov test p-score" = k_s_test_barchart, "p-value_barchart" = p_value_barchart,
  "final_results_table" = final_results,  "ensemble_correlation" = ensemble_correlation,
  "data_summary" = data_summary, "plot_list" = plot_list, "outlier_data" = outlier_list, "summary_list" = summary_list,
  "colnum" = colnum, "numresamples" = numresamples, "save_all_trained_modesl" = save_all_trained_models, "how_to_handle_strings" = how_to_handle_strings,
  "data_reduction_method" = data_reduction_method,  "scale_data" = scale_all_predictors_in_data,
  "train_amount" = train_amount, "test_amount" = test_amount, "validation_amount" = validation_amount
)
)
}
