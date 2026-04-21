#' Get funnel plot from a fitted `linear_fe` object for institutional comparisons
#'
#' Creates a funnel plot from a linear fixed effect model to compare provider performance.
#'
#' @param x a model fitted from \code{linear_fe}.
#' @param null a character string or a number specifying null hypotheses of fixed provider effects. The default is \code{"median"}.
#' @param target a numeric value representing the target outcome. The default value is 0.
#' @param alpha a number or a vector of significance levels. The default is 0.05.
#' @param labels a vector of labels for the plot.
#' @param point_colors a vector of colors representing different provider flags. The default is \code{c("#E69F00", "#56B4E9", "#009E73")}.
#' @param point_shapes a vector of shapes representing different provider flags. The default is \code{c(15, 17, 19)}.
#' @param point_size size of the points. The default is 2.
#' @param point_alpha transparency level of the points. The default is 0.8.
#' @param line_size size of all lines, including control limits and the target line. The default is 0.8.
#' @param target_line_type line type for the target line. The default is "longdash".
#' @param \dots additional arguments that can be passed to the function.
#'
#' @details
#' This function generates a funnel plot from a linear fixed effect model. Currently, it only supports the indirect standardized difference.
#' The parameter `alpha` is a vector used to calculate control limits at different significance levels.
#' The first value in the vector is used as the significance level for flagging each provider, utilizing the \code{\link{test.linear_fe}} function.
#'
#' @seealso \code{\link{linear_fe}}, \code{\link{linear_fe}}, \code{\link{linear_fe}}
#'
#' @return A ggplot object representing the funnel plot.
#'
#' @examples
#' data(ExampleDataLinear)
#' outcome <- ExampleDataLinear$Y
#' covar <- ExampleDataLinear$Z
#' ProvID <- ExampleDataLinear$ProvID
#' fit_fe <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
#' plot(fit_fe)
#'
#' @importFrom dplyr arrange cross_join mutate select filter
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @exportS3Method plot linear_fe

plot.linear_fe <- function(x, null = "median", target = 0, alpha = 0.05,
                           labels = c("lower", "expected", "higher"),
                           point_colors = c("#E69F00", "#56B4E9", "#009E73"),
                           point_shapes = c(15, 17, 19),
                           point_size = 2, point_alpha = 0.8,
                           line_size = 0.8,
                           target_line_type = "longdash", ...
) {
  if (missing(x)) stop ("Argument 'x' is required!", call.=F)
  if (!class(x) %in% c("linear_fe")) stop("Object 'x' is not of the classes 'linear_fe'!", call.=F)

  data <- x$data_include
  SM <- SM_output(x, null = null, stdz = "indirect")
  processed_data <- cbind(SM$indirect.difference, SM$OE$OE_indirect)
  colnames(processed_data) <- c("indicator", "Obs", "Exp")
  processed_data$precision <- sapply(split(data[, x$char_list$Y.char], data[, x$char_list$ProvID.char]), length)


  flagging <- test(x, level = 1-alpha[1], null = null)
  processed_data <- cbind(processed_data, flagging)
  plot_data <- processed_data %>%
    arrange(.data$precision) %>%
    cross_join(tibble(alpha = alpha)) %>%
    mutate(
      lower = target - qnorm(1 - alpha / 2) * sqrt(1 / .data$precision) * x$sigma,
      upper = target + qnorm(1 - alpha / 2) * sqrt(1 / .data$precision) * x$sigma
    ) %>%
    select(.data$precision, .data$indicator, .data$Exp, .data$flag, alpha, .data$lower, .data$upper) %>%
    mutate(
      alpha = factor(alpha),
    )

  plot <- ppfunnel_linear(plot_data,
                          target,
                          alpha,
                          labels,
                          point_colors,
                          point_shapes,
                          point_size,
                          point_alpha,
                          line_size,
                          target_line_type
  )

  return(plot)
}


#' @importFrom dplyr filter
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous geom_point scale_shape_manual scale_color_manual scale_linetype_manual geom_line geom_hline guides theme labs theme_classic guide_legend
ppfunnel_linear <- function(plot_data,
                            target,
                            alpha,
                            labels,
                            point_colors,
                            point_shapes,
                            point_size,
                            point_alpha,
                            line_size,
                            target_line_type,
                            xlab = "Precision",
                            ylab = "Outcome",
                            legend_justification = "center",
                            legend_position = "right",
                            point_legend_title = "Flagging",
                            linetype_legend_title = "Ctrl Limit",
                            legend_title_size = 14,
                            legend_size = 14,
                            legend_box = "vertical",
                            axis_title_size = 14,
                            axis_text_size = 14,
                            plot_title = "Funnel Plot",
                            plot_title_size = 18
) {

  # Check if plot_data is a data frame
  if (!is.data.frame(plot_data)) {
    stop("plot_data must be a data frame")
  }

  data <- plot_data %>% filter(alpha == alpha[1])

  # Ensure that data$flag is a factor
  data$flag <- factor(data$flag, levels = c(-1, 0, 1))

  # Create labels for the legend
  labs_color <- paste0(labels, " (", table(data$flag), ")")

  # Add dummy rows for missing levels with NA values
  missing_levels <- setdiff(c(-1, 0, 1), unique(data$flag))
  if (length(missing_levels) > 0) {
    dummy_data <- data.frame(flag = factor(missing_levels, levels = c(-1, 0, 1)),
                             precision = NA,
                             indicator = NA)
    data <- bind_rows(data, dummy_data)
  }

  num_levels <- length(levels(data$flag))

  # # Check if the length of shapes and color_palette is the same as the number of levels
  # if (length(color_palette) != num_levels) {
  #   stop("The length of color_palette must be the same as the number of levels in data$flag")
  # }
  #
  # # Check if the length of labels is the same as the number of levels
  # if (length(labels) != num_levels) {
  #   stop("The length of labels must be the same as the number of levels in data$flag")
  # }


  # Assign each level of data$flag to a color from the palette
  color_mapping <- setNames(point_colors, levels(data$flag))
  # Assign each level of data$flag to a shape
  shapes_mapping <- setNames(point_shapes, levels(data$flag))

  # Create a named vector of lables for the legend
  labels <- setNames(labels, levels(data$flag))

  xmax <- max(plot_data$precision)
  xmin <- min(plot_data$precision)
  ymax <- max(max(plot_data$upper), max(plot_data$indicator))
  ymin <- min(min(plot_data$lower), min(plot_data$indicator))

  labs_linetype <- paste0((1 - alpha) * 100, "%")

  values_linetype <- c('solid', 'dashed', 'dotted', 'dotdash', 'longdash', 'twodash')[1:length(alpha)]

  values_linetype <- values_linetype[order(alpha)]
  labs_linetype <- labs_linetype[order(alpha)]

  plot <-
    ggplot() +
    scale_x_continuous(limits = c(xmin, xmax),
                       expand = c(1, 1)/50) +
    scale_y_continuous(breaks = round(seq(0, ymax, by=1), 1),
                       limits = c(ymin, ymax),
                       expand = c(1, 1)/50) +
    geom_point(data = data, aes(x = .data$precision, y = .data$indicator, shape = .data$flag, color = .data$flag), size = point_size, alpha = point_alpha) +
    scale_shape_manual(
      name = bquote(.(point_legend_title) ~ "(" * alpha == .(alpha[1]) * ")"),
      labels = labs_color,
      values = shapes_mapping
    ) +
    scale_color_manual(
      name = bquote(.(point_legend_title) ~ "(" * alpha == .(alpha[1]) * ")"),
      labels = labs_color,
      values = color_mapping
    ) +
    geom_line(data = plot_data, aes(x = .data$precision, y = .data$lower, group = alpha, linetype = alpha), linewidth = line_size) +
    geom_line(data = plot_data, aes(x = .data$precision, y = .data$upper, group = alpha, linetype = alpha), linewidth = line_size) +
    scale_linetype_manual(
      name =  linetype_legend_title,
      values = values_linetype,
      labels = labs_linetype
    ) +
    guides(shape = guide_legend(order = 1), color = guide_legend(order = 1), linetype = guide_legend(reverse = TRUE, order = 2)) +
    geom_hline(yintercept = target, linewidth = line_size, linetype = target_line_type) +
    theme_classic() +
    theme(
      legend.justification = legend_justification,
      legend.position = legend_position,
      legend.box = legend_box,
      legend.title = element_text(size = legend_title_size),
      legend.text = element_text(size = legend_size),
      axis.title = element_text(size = axis_title_size, margin = margin(t = 0, r = 0, b = 0, l = 0)),
      axis.text = element_text(size = axis_text_size),
      plot.title = element_text(hjust = 0.5, size = plot_title_size),
      text = element_text(size = 13),
      legend.background = element_rect(fill = "transparent", colour = NULL, linewidth = 0, linetype = "solid"),
    ) +
    labs(
      x = xlab,
      y = ylab,
      title = plot_title
    )


  return(plot)
}

