#' Get funnel plot from a fitted `logis_fe` object for institutional comparisons
#'
#' Creates a funnel plot from a logistic fixed effect model to compare provider performance.
#'
#' @param x a model fitted from \code{logis_fe}.
#' @param null a character string or a number specifying null hypotheses of fixed provider effects. The default is \code{"median"}.
#' @param test a character string specifying the type of testing methods to be conducted. The default is "score".
#' @param target a numeric value representing the target outcome. The default value is 1.
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
#' This function generates a funnel plot from a logistic fixed-effect model. Currently, it only supports the indirect standardized ratio.
#' The parameter `alpha` is a vector used to calculate control limits at different significance levels.
#' The first value in the vector is used as the significance level for flagging each provider, utilizing the \code{\link{test.logis_fe}} function.
#'
#' @seealso \code{\link{logis_fe}}, \code{\link{SM_output.linear_re}}, \code{\link{test.logis_fe}}
#'
#' @return A ggplot object representing the funnel plot.
#'
#' @examples
#' data(ExampleDataBinary)
#' outcome <- ExampleDataBinary$Y
#' covar <- ExampleDataBinary$Z
#' ProvID <- ExampleDataBinary$ProvID
#' fit_fe <- logis_fe(Y = outcome, Z = covar, ProvID = ProvID)
#' plot(fit_fe)
#'
#' @importFrom dplyr filter arrange cross_join mutate select
#' @importFrom magrittr %>%
#' @importFrom poibin ppoibin dpoibin
#' @importFrom stats plogis
#' @importFrom tibble tibble
#' @importFrom rlang .data
#'
#' @references
#' Wu, W., Kuriakose, J. P., Weng, W., Burney, R. E., & He, K. (2023). Test-specific funnel plots for healthcare provider profiling leveraging
#' individual- and summary-level information. \emph{Health Services and Outcomes Research Methodology}, \strong{23(1)}, 45-58.
#' \cr
#'
#' @exportS3Method plot logis_fe

plot.logis_fe <- function(x, null = "median", test = "score", target = 1, alpha = 0.05,
                          labels = c("lower", "expected", "higher"),
                          point_colors = c("#E69F00", "#56B4E9", "#009E73"),
                          point_shapes = c(15, 17, 19),
                          point_size = 2, point_alpha = 0.8,
                          line_size = 0.8,
                          target_line_type = "longdash", ...
) {
  if (missing(x)) stop ("Argument 'x' is required!", call.=F)
  if (!class(x) %in% c("logis_fe")) stop("Object 'x' is not of the classes 'logis_fe'!", call.=F)
  if (!(test %in% c("exact", "score"))) stop("Argument 'test' NOT as required!", call.=F)

  # Indicator
  SM <- SM_output(x, null = null, stdz = "indirect", measure = "ratio")
  processed_data <- cbind(SM$indirect.ratio, SM$OE$OE_indirect)
  colnames(processed_data) <- c("indicator", "Obs", "Exp", "Var")
  processed_data$precision <- processed_data$Exp^2/processed_data$Var

  data <- x$data_include
  Z_beta <- x$linear_pred
  prov <- data[ ,x$char_list$ProvID.char]
  gamma <- x$coefficient$gamma
  gamma.null <- ifelse(null=="median", median(gamma),
                       ifelse(class(null)=="numeric", null[1],
                              stop("Argument 'null' NOT as required!", call.=F)))
  probs_all <- as.numeric(plogis(gamma.null + Z_beta)) # expected prob of events under null
  probs_list <- split(probs_all, prov)
  n.prov <- sapply(split(data[, x$char_list$Y.char], data[, x$char_list$ProvID.char]), length)

  if (test == "exact") {
    flagging <- test(x, level = 1-alpha[1], test = "exact.poisbinom", null = null)
    processed_data <- cbind(processed_data, flagging)

    cl_lower <- function(probs_list, alpha) {
      # lower CL for obs
      # o_lower <- qpoibin(alpha / 2, E/n)
      o_lower <- sapply(probs_list, .data$qpoibin, qq = alpha/2)
      # o_lower <- ifelse(ppoibin(o_lower - 1, E/n) + 0.5 * dpoibin(o_lower, E/n) >= alpha / 2, o_lower, o_lower + 1)
      o_lower <- sapply(1:length(probs_list), function(i){
        ifelse(ppoibin(o_lower[i] - 1, probs_list[[i]]) + 0.5 * dpoibin(o_lower[i], probs_list[[i]]) >= alpha / 2,
               o_lower[i], o_lower[i] + 1)})
      # lambda_lower <- (dpoibin(o_lower, E/n) + 2 * ppoibin(o_lower - 1, E/n) - alpha) / (dpoibin(o_lower, E/n) + dpoibin(o_lower - 1, E/n))
      lambda_lower <- sapply(1:length(probs_list), function(i){
        (dpoibin(o_lower[i], probs_list[[i]]) + 2 * ppoibin(o_lower[i] - 1, probs_list[[i]]) - alpha) /
          (dpoibin(o_lower[i], probs_list[[i]]) + dpoibin(o_lower[i] - 1, probs_list[[i]]))
      })
      lower <- pmax(o_lower - lambda_lower, 0)
      return(lower)
    }

    cl_upper <- function(probs_list, alpha) {
      # upper CL for obs
      o_upper <- sapply(probs_list, .data$qpoibin, qq = 1-alpha/2) # qpoibin(1 - alpha / 2, E)
      o_upper <- sapply(1:length(probs_list), function(i){
        ifelse(ppoibin(o_upper[i] - 1, probs_list[[i]]) + 0.5 * dpoibin(o_upper[i], probs_list[[i]]) >= 1-alpha / 2,
               o_upper[i], o_upper[i] + 1)})
      # ifelse(ppoibin(o_upper - 1, E) + 0.5 * dpoibin(o_upper, E) >= 1 - alpha / 2, o_upper, o_upper + 1)
      lambda_upper <- sapply(1:length(probs_list), function(i){
        (dpoibin(o_upper[i], probs_list[[i]]) + 2 * ppoibin(o_upper[i] - 1, probs_list[[i]]) - 2 + alpha) /
          (dpoibin(o_upper[i], probs_list[[i]]) + dpoibin(o_upper[i] - 1, probs_list[[i]]))
      })# (dpoibin(o_upper, E) + 2 * ppoibin(o_upper - 1, E) - 2 + alpha) / (dpoibin(o_upper - 1, E) + dpoibin(o_upper, E))
      upper <- o_upper - lambda_upper
      return(upper)
    }

    alpha_sort <- sort(alpha)

    cl <- lapply(alpha_sort, function(alpha){
      res <- cbind(cl_lower(probs_list,alpha),
            cl_upper(probs_list,alpha))
      colnames(res) <- c("lower", "upper")
      return(res)
    })
    CL_res <- NULL
    for (i in 1:length(alpha_sort)) {
      CL_res <- rbind(CL_res, cl[[i]])
    }

    plot_data <- processed_data %>%
      cross_join(tibble(alpha = alpha)) %>%
      arrange(alpha) %>%
      cbind(CL_res) %>%
      # mutate(
      #   lower = cl_lower(probs_list, alpha) / Exp,
      #   upper = cl_upper(probs_list, alpha) / Exp
      # ) %>%
      select(.data$precision, .data$indicator, .data$Exp, .data$flag, alpha, .data$lower, .data$upper) %>%
      mutate(
        alpha = factor(alpha),
        lower = pmax(.data$lower/.data$Exp, 0),
        upper = .data$upper/.data$Exp
      ) %>% arrange(.data$precision)
  }
  else if (test == "score") {
    flagging <- test(x, level = 1-alpha[1], test = "score", null = null)
    processed_data <- cbind(processed_data, flagging)
    plot_data <- processed_data %>%
      arrange(.data$precision) %>%
      cross_join(tibble(alpha = alpha)) %>%
      mutate(
        lower = target - qnorm(1 - alpha / 2) * sqrt(1 / .data$precision),
        upper = target + qnorm(1 - alpha / 2) * sqrt(1 / .data$precision)
      ) %>%
      select(.data$precision, .data$indicator, .data$Exp, .data$flag, alpha, .data$lower, .data$upper) %>%
      mutate(
        alpha = factor(alpha),
        lower = pmax(.data$lower, 0)
      )
  }


  plot <- ppfunnel_logis(plot_data,
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



#' @importFrom stats setNames
#' @importFrom dplyr filter bind_rows
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot scale_x_continuous scale_y_continuous geom_point aes scale_shape_manual scale_color_manual scale_linetype_manual geom_line geom_hline guides guide_legend theme labs theme_classic element_text element_rect
ppfunnel_logis <- function(plot_data,
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
                           legend_justification = c(1, 1),
                           legend_position = c(0.95, 0.95),
                           point_legend_title = "Flagging",
                           linetype_legend_title = "Ctrl Limit",
                           legend_title_size = 14,
                           legend_size = 14,
                           legend_box = "horizontal",
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
  ymax <- max(max(plot_data$upper), max(plot_data$indicator))

  labs_linetype <- paste0((1 - alpha) * 100, "%")

  values_linetype <- c('solid', 'dashed', 'dotted', 'dotdash', 'longdash', 'twodash')[1:length(alpha)]

  values_linetype <- values_linetype[order(alpha)]
  labs_linetype <- labs_linetype[order(alpha)]

  plot <-
    ggplot() +
    scale_x_continuous(limits = c(0, xmax),
                       expand = c(1, 1)/50) +
    scale_y_continuous(breaks = round(seq(0, ymax, by=1), 1),
                       limits = c(0, ymax),
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
