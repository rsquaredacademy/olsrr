#' Boston_housing data
#'
#' This is a modified version of the famous Boston housing data set. This data set includes rows 4:209 and 212:506. The data here is complete except for the data use to make New_Boston.
#' The data first appeared in a paper by David Harrison, Jr. and Daniel L. Rubenfeld, Hedonic housing Prices and the demand for clean air. This was published in March, 1978.
#' Journal of Environmental Economics and Management 5(1):81-102. The descriptions below are quoted from the original paper:
#' \describe{
#' \item{crim}{Crime rate by town. Original data in 1970 FBI data}
#' \item{zn}{Proportion of a town's residential land zoned for lots greater than 25,000 square feet}
#' \item{indus}{Proportional non-retail business per town}
#' \item{chas}{Captures the amenities of a riverside location and thus should be positive}
#' \item{nox}{Nitrogen oxygen concentrations in part per hundred million}
#' \item{rm}{Average number of rooms in owner units}
#' \item{age}{Proportion of owner units built prior to 1940}
#' \item{dis}{Weighted distances to five employment centers in the Boston region}
#' \item{rad}{Index of accessibility to radial highways}
#' \item{tax}{Full property value tax rate ($/$10,000)}
#' \item{ptratio}{Pupil-teacher ratio by town school district}
#' \item{black}{Black proportion of population}
#' \item{lstat}{Proportion of population that is lower status (proportion of adults without some high school education and proportion of male workers classified as laborers)}
#' \item{medv}{Median value of owner occupied homes, from the 1970 United States census}
#' }
#'
#' @source <https://www.law.berkeley.edu/files/Hedonic.PDF>
"Boston_housing"


#' NewBoston—These are only the five rows c(1:3, 210:211) from Boston Housing data set. This can be used as new data, and the Boston_housing data set as the original. The numeric function will return predictions on the new data.
#'
#' @description
#' This is the first five rows of the Boston housing data set, which have been removed from the Boston data set included here. It is otherwise identical to the Boston data set.
#'
#' \describe{
#' \item{crim}{Crime rate by town. Original data in 1970 FBI data}
#' \item{zn}{Proportion of a town's residential land zoned for lots greater than 25,000 square feet}
#' \item{indus}{Proportional non-retail business per town}
#' \item{chas}{Captures the amenities of a riverside location and thus should be positive}
#' \item{nox}{Nitrogen oxygen concentrations in part per hundred million}
#' \item{rm}{Average number of rooms in owner units}
#' \item{age}{Proportion of owner units built prior to 1940}
#' \item{dis}{Weighted distances to five employment centers in the Boston region}
#' \item{rad}{Index of accessibility to radial highways}
#' \item{tax}{Full property value tax rate ($/$10,000)}
#' \item{ptratio}{Pupil-teacher ratio by town school district}
#' \item{black}{Black proportion of population}
#' \item{lstat}{Proportion of population that is lower status (proportion of adults without some high school education and proportion of male workers classified as laborers)}
#' \item{medv}{Median value of owner occupied homes, from the 1970 United States census}
#' }
#'
#' @source <https://www.law.berkeley.edu/files/Hedonic.PDF>
"New_Boston"

#' Concrete - This is the strength of concrete daa set originally posted on UCI
#'
#' @description
#' Concrete is the most important material in civil engineering. The concrete compressive strength is a highly nonlinear function of age and ingredients.
#'
#' @format Concrete
#' A data frame with 1030 rows and 9 columns:
#' \describe{
#'    \item{Cement}{ quantitative -- kg in a m3 mixture -- Input Variable}
#'    \item{Blast_Furnace_Slag}{quantitative -- kg in a m3 mixture -- Input Variable}
#'    \item{Fly_Ash}{ quantitative  -- kg in a m3 mixture -- Input Variable}
#'    \item{Water}{quantitative  -- kg in a m3 mixture -- Input Variable}
#'    \item{Superplasticizer}{quantitative -- kg in a m3 mixture -- Input Variable}
#'    \item{Coarse_Aggregate}{quantitative -- kg in a m3 mixture -- Input Variable}
#'    \item{Fine_Aggregate}{quantitative  -- kg in a m3 mixture -- Input Variable}
#'    \item{Age}{Day (1~365) -- Input Variable}
#'    \item{Strength}{quantitative -- MPa -- Output Variable}
#' }
#' @source https://archive.ics.uci.edu/dataset/165/concrete+compressive+strength
"Concrete"

#' Insurance - The data is from UCI
#'
#' @description
#' This dataset contains detailed information about insurance customers, including their age, sex, body mass index (BMI), number of children, smoking status and region. Having access to such valuable insights allows analysts to get a better view into customer behaviour and the factors that contribute to their insurance charges.

#' @format Insurance
#' A data frame with 1338 rows and 7 columns
#' Credit to Bob Wakefield
#' \describe{
#'    \item{Age}{The age of the customer. (Integer)}
#'    \item{Children}{The number of children the customer has. (Integer)}
#'    \item{Smoker}{Whether or not the customer is a smoker. (Boolean)}
#'    \item{Region}{The region the customer lives in. (String)}
#'    \item{Charges}{The insurance charges for the customer. (Float)}
#' }
#' @source https://www.kaggle.com/datasets/thedevastator/prediction-of-insurance-charges-using-age-gender
"Insurance"
