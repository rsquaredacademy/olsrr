#' Annual Spending by Wholesale Customers
#'
#' Annual spending in monetary units on product categories by wholesale customers.
#' The source does not specify a currency. The original category name Delicassen
#' is retained as `delicassen`.
#'
#' @format A data frame with 440 rows and 8 variables:
#' \describe{
#'   \item{channel}{Customer channel: hotel/restaurant/cafe or retail.}
#'   \item{region}{Customer region: Lisbon, Porto, or other.}
#'   \item{fresh, milk, grocery, frozen, detergents_paper, delicassen}{Annual spending in each product category.}
#' }
#' @source Cardoso, M. (2013), Wholesale customers, UCI Machine Learning
#'   Repository, <https://doi.org/10.24432/C5030X>;
#'   <https://archive.ics.uci.edu/dataset/292/wholesale+customers>.
#'   Licensed under Creative Commons Attribution 4.0 International
#'   (CC BY 4.0), <https://creativecommons.org/licenses/by/4.0/>.
"wholesale_customers"
