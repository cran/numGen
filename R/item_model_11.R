#' @export
#' @importFrom stats na.omit
#' @param items Generate a random mix of items.
#' @param fun1 The argument decides the arithmetic to be employed for Neighbouring objects. There are only two arithmetic: add, substr.
#' @param fun2 The argument decides the arithmetic to be employed for the two values between the grouped objects.  There are two arithmetic: add, substr.
#' @description  This uses item model 11 to create number series items - Identification of alternating coefficients of change.
#' @details This function creates number series that is a combination of Neighbouring objects and 2-sequence coefficient. Multiplication and Division are removed since the calculated value is too big. Example: A sequence whose coefficient of change alternates between (add 6) and (multiply by 2). 1 7 14 20 40 46 (92) (98).
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 11
#' @examples
#'
#' #Draws 5 items randomly.
#' imEleven(items=5, fun1 = "add", fun2= "add")
#'
#'

# Neighbouring objects + 2-sequence coefficient.

# 1	7	14 20	40 46 ? ?
# (92, 98)
imEleven <- function(items=1, fun1 = "add", fun2="add"){
  if(missing(items)){
    stop("Please include x number of items to generate")
  }
  stopifnot(fun1=="add" | fun1=="substr")
  stopifnot(fun2=="add" | fun2=="substr")

add <- function(x, y) {
  new <- x + y
  return(new)
}

sub <- function(x, y) {
  new <- x - y
  return(new)
}

# multi <- function(x, y) {
#   new <- x * y
#   return(new)
# }
#
# div <- function(x, y) {
#   new <- x / y
#   return(new)
# }

  fun_list <- list(add, sub)

  bank_42 <- matrix(ncol=8)
  colnames(bank_42) <- colnames(bank_42, do.NULL = FALSE, prefix = "Q")
  colnames(bank_42)[7:8] <- "A"

  combinations <- matrix(c(1,3,1,4,2,3,2,4,3,1,3,2,4,1,4,2), ncol = 2, byrow = TRUE)
  combinations
  for (i in 1:items) {
    start <- sample(1:99, 1)
    select <- combinations[sample(nrow(combinations), 1), ]

    if(fun1 == "add"){
      func1 <- fun_list[[1]]
    }
    if(fun1 == "substr"){
     func1 <- fun_list[[2]]
    }
    if(fun2 == "add"){
      func2 <- fun_list[[1]]
    }
    if(fun2 == "substr"){
      func2 <- fun_list[[2]]
    }
    # Arithmetic is employed based on what fun1 is selected
    if (select[1] < 3) {
      #starting value can range from 1 to 99
      coeff1 <- sample(1:99, 1)
    } else {
      # difference between 2nd and 3rd value range from 1:9
      coeff1 <- sample(2:9, 1)
    }

    # Arithmetic is employed based on what fun2 is selected
    if (select[2] < 3) {
      coeff2 <- sample(1:99, 1)
    } else {
      coeff2 <- sample(2:9, 1)
    }

    m1 <- start
    m2 <- func1(m1, coeff1)
    m3 <- func2(m2, coeff2)
    m4 <- func1(m3, coeff1)
    m5 <- func2(m4, coeff2)
    m6 <- func1(m5, coeff1)
    m7 <- func2(m6, coeff2)
    m8 <- func1(m7, coeff1)

    item <- c(m1, m2, m3, m4, m5, m6, m7, m8)
    item
    bank_42 <- na.omit(bank_42)
    bank_42 <- subset(bank_42, bank_42[ ,8] < 500)
    bank_42 <- subset(bank_42, bank_42[ ,8] > -300)

    if (all(item == floor(item))) {
      bank_42 <- rbind(bank_42, item)
    }

  }
  return(bank_42)
}




