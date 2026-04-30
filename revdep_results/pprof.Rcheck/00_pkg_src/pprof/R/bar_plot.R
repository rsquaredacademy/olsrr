#' Get a bar plot for flagging percentage overall and stratified by provider sizes
#'
#' Generate a bar plot for flagging percentage.
#'
#' @param flag_df a data frame from `test` function containing the flag of each provider.
#' @param group_num number of groups into which providers are divided based on their sample sizes. The default is 4.
#' @param bar_colors a vector of colors used to fill the bars representing the categories. The default is c("#66c2a5", "#fc8d62", "#8da0cb").
#' @param bar_width width of the bars in the bar chart. The default is 0.7.
#' @param label_color color of the text labels inside the bars. The default is "black".
#' @param label_size size of the text labels inside the bars. The default is 4.
#'
#' @details
#' This function generates a bar chart to visualize the percentage of flagging results based on provider sizes.
#' The input data frame `test_df` must be the output from package `pprof`'s `test` function.
#' Providers are grouped into a specified number of groups (`group_num`) based on their sample sizes, where
#' the number of providers are approximately equal across groups. An additional "overall" group is
#' included to show the flagging results across all providers.
#'
#' @return A ggplot object representing the bar chart of flagging results.
#'
#' @examples
#' data(ExampleDataLinear)
#' outcome <- ExampleDataLinear$Y
#' covar <- ExampleDataLinear$Z
#' ProvID <- ExampleDataLinear$ProvID
#' fit_linear <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
#' test_linear <- test(fit_linear)
#' bar_plot(test_linear)
#'
#' data(ExampleDataBinary)
#' fit_logis <- logis_fe(Y = ExampleDataBinary$Y,
#'                       Z = ExampleDataBinary$Z,
#'                       ProvID = ExampleDataBinary$ProvID, message = FALSE)
#' test_logis <- test(fit_logis)
#' bar_plot(test_logis)
#'
#' @seealso \code{\link{test.linear_fe}}, \code{\link{test.linear_re}}, \code{\link{test.logis_fe}}
#'
#' @importFrom ggplot2 ggplot geom_bar geom_text labs aes theme scale_y_continuous scale_fill_manual element_text element_line element_blank theme_minimal position_stack
#' @importFrom dplyr group_by summarise mutate n
#' @importFrom scales percent percent_format
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @importFrom rlang .data
#'
#' @export

bar_plot <- function(flag_df, group_num = 4,
                     bar_colors = c("#66c2a5", "#fc8d62", "#8da0cb"), bar_width = 0.7,
                     label_color = "black", label_size = 4) {
  if (missing(flag_df)) stop ("Argument 'flag_df' is required!",call.=F)
  if (!class(flag_df) %in% c("data.frame")) stop("Object flag_df should be a data frame!",call.=F)
  if (!"flag" %in% colnames(flag_df) || is.null(attr(flag_df, "provider size"))) {
    stop("Dataframe must contain a 'flag' column and an attribute 'provider size'.")
  }

  flag_df$category <- factor(flag_df$flag, levels = c(1, 0, -1), labels = c("higher", "as expected", "lower"))

  provider_size <- attr(flag_df, "provider size")
  flag_df$size <- cut(provider_size,
                      breaks = quantile(provider_size, probs = (0:group_num)/group_num, na.rm = TRUE),
                      include.lowest = TRUE,
                      labels = paste0("Q", 1:group_num))
  flag_df$size <- factor(flag_df$size, levels = c(paste0("Q", 1:group_num), "Overall"))

  flag_df_overall <- flag_df
  flag_df_overall$size <- "Overall"
  flag_df <- rbind(flag_df, flag_df_overall)

  # Calculate percentage of each flag in each group
  df_long <- flag_df %>%
    group_by(.data$size, .data$category) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(.data$size) %>%
    mutate(value = .data$count / sum(.data$count))

  # Plot the bar chart
  p <- ggplot(df_long, aes(x = .data$size, y = .data$value, fill = .data$category)) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    geom_text(aes(label = percent(.data$value, accuracy = 0.1)),
              position = position_stack(vjust = 0.5),
              color = label_color, size = label_size) +
    labs(x = "Provider Size",
         y = "Flagging Percentage",
         title = "Flagging Results Based on Provider Size",
         fill = "Category") +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    theme_minimal() +
    scale_fill_manual(values = bar_colors) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title.x = element_text(face = "bold", size = 14),
      axis.title.y = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = 'grey80'),
      panel.grid.minor = element_blank()
    )

  return(p)
}

# flag_df %>% filter(size == "Q1") %>% mutate(cat_low = category == "as expected") %>% pull(cat_low) %>% mean()
