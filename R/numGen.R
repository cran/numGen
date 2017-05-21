#' numGen: A package for generating number series items.
#'
#' The numGen package provides 14 item models for generating number series items.
#'
#'
#' @section Item model 1:
#' This number series generates simple linear sequences with a magnitude of up to 5000.\cr
#' \code{\link{imOne}}
#'
#' @section Item model 2:
#' This number series generate sequences consist of elements belonging to two homogeneous groups with equal number of elements.\cr
#' \code{\link{imTwo}}
#'
#' @section Item model 3:
#' This function allows you to select one of the four arithmetic operators following a sequence succession rule.\cr
#' \code{\link{imThree}}
#'
# #' @section Item model 4:
# #' The core feature of sequences is the numeric representation of the object count.  The sequences consist of homogeneous groups of letters followed by a number representing the amount of the group elements.\cr
# #' \code{\link{imFour}}
#'
#' @section Item model 4:
#' This create items that relates to comprehension of abstract object representation (Item model 5) and Identification of co-occurring relationships between elements (Item model 4).\cr
#' \code{\link{imFour}}
#'
#' @section Item model 5:
#' Generate items with two sequences combined into one number series.\cr
#' \code{\link{imFive}}
#'
#' @section Item model 6:
#' This model uses the addition and substraction (Arithmetic) operator, Linear pattern and Progressive coefficient to create the number series.\cr
#' \code{\link{imSix}}
#'
#' @section Item model 7:
#' This function creates number series that is a combination of Arithmetic, Linear and Complex coefficient. \cr
#' First logic of complex coefficient = i*x+y.\cr
#' Second logic of complex coefficient = (i+x)*y.\cr
#'  \code{\link{imSeven}}
#'
#' @section Item model 8:
#' This is based on the categorical / pattern recognition rule. Neighbouring pairs or triads of objects are related, includes arithmetic operations. \cr
#' \code{\link{imEight}}
#'
#' @section Item model 9:
#' This function creates Fibonacci sequences. The maximum number to be generated is 15 items.\cr
#' \code{\link{imNine}}
#'
#' @section Item model 10:
#' The number series is a combination of Arithmetic, linear sequence and progressive coefficient. \cr
#' First logic is combining sequences x y x y x y x y = one simple (cannot be controlled), one progressive. \cr
#' Second logic is combining sequences x y x y x y x y = two progressive. \cr
#'  \code{\link{imTen}}
#'
#' @section Item model 11:
#' Neighbouring objects + 2-sequence coefficient. \cr
#' This function creates number series that is a combination of Neighbouring objects + 2-sequence coefficient. \cr
#' Multiplication and Division is removed since the calculated value is too big. \cr
#'  \code{\link{imEleven}}
#'
#' @section Item model 12:
#' This function creates number series that is a irregular combination of sequences a b b a b b a ... \cr
#' Only the addition and substraction arithmetic operators are used to create the number series items. \cr
#'  \code{\link{imTwelve}}
#'
#' @section Item model 13:
#' Combination of sequences and ratios. \cr
#' \code{\link{imThirteen}}
#'
#' @references
#'
#'LeFevre, J. A., & Bisanz, J. (1986). A cognitive analysis of number-series problems: Sources of individual differences in performance. \emph{Memory & Cognition}, 14(4), 287-298.
#'
#'Holzman, T. G., Pellegrino, J. W., & Glaser, R. (1983). Cognitive variables in series completion. \emph{Journal of Educational Psychology}, 75(4), 603.
#'
#' Simon, H. A., & Kotovsky, K. (1963). Human acquisition of concepts for sequential patterns. \emph{Psychological Review}, 70(6), 534.
#' @docType package
#' @name numGen
NULL
