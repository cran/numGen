#' @export
#' @importFrom stats na.omit
#' @param vOne The first value in the complex coefficient (x). Can be a sequence of values or a specific value.
#' @param vTwo The second value in the complex coefficient (y). Can be a sequence of values or a specific value.
#' @param items Generate a random mix of items.
#' @param seed To get the same random sampling of items
#' @param logic "one" or "two"
#' @param random If random=FALSE, the items will follow in sequential order.
#' @description This uses item model 7 to create number series items - Identification of complex coefficients of change
#' @details  This function creates number series that is a combination of Arithmetic, Linear and Complex coefficient. Ability to identify complex coefficients; the coefficient of change involves a combination of arithmetic operations (e.g. addition and multiplication) applied serially.\cr
#' There are two logic to calculate the number series.
#' First logic of complex coefficient = i*x+y.\cr
#' Second logic of complex coefficient = (i+x)*y. \cr.
#' Example: Each element in the sequence is derived from the preceding by adding two and multiplying the result by two. (2 8 20 44 92 (188)).
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 7
#' @examples
#'
#' #Draws 5 items randomly.
#' imSeven(vOne=1,vTwo=3,items=5,seed=2,logic="one",random=TRUE)
#'
#' # Calculates all combinations
#' # Items and seed arg is ignored.
#' imSeven(vOne=1:2,vTwo=1:3,items=5,seed=2,logic="one",random=FALSE)
#'
#'

# Arithmetic + Linear + Complex coefficient
# 2 5 11 23 ?
# complex coefficient = i*x+y
# complex coefficient = (i+x)*y
imSeven<- function(vOne=1, vTwo=3, items ,seed=1, logic = "one", random = FALSE){
  if(missing(items)){
    stop("Please include x number of items to generate")
  }

  stopifnot(logic=="one" || logic=="two")

if(logic == "one"){
generate_sequence <- function(i,x,y) {
  a <- NULL
  a[1] <- i
  b <- matrix(a[1], ncol=1)
  for (l in 2:9) {
    a[l] <- a[l-1]*x + y
    b <- cbind(b, a[l])
  }
  return(b)
}
}

if(logic == "two"){
generate_sequence <- function(i,x,y) {
  a <- NULL
  a[1] <- i
  b <- matrix(a[1], ncol=1)
  for (l in 2:9) {
    a[l] <- (a[l-1]+x) * y
    b <- cbind(b, a[l])
  }
  return(b)
  }
}


# generate i,x,y; i[1] = 1:10, x = 2:10, y = 1:99
# all items < 2000
bank_33 <- matrix(ncol=9)
colnames(bank_33) <- colnames(bank_33, do.NULL = FALSE, prefix = "Q")
colnames(bank_33)[9] <- "A"

# this decides how many items are to be generated
combinations <- expand.grid(i = c(1:items), y = c(vOne), z = c(vTwo))
if(random == TRUE){
set.seed(seed)
s.combinations<- combinations[sample(nrow(combinations), items,replace=FALSE), ]
    for (m in 1:nrow(s.combinations)) {
      item <- generate_sequence(s.combinations[m,1], s.combinations[m,2], s.combinations[m,3])
      bank_33 <- rbind(bank_33, item)
      bank_33 <- na.omit(bank_33)
      #bank_33 <- subset(bank_33, bank_33[,6] < 1999)
    }
}else{
  for (m in 1:nrow(combinations)) {
    item <- generate_sequence(combinations[m,1], combinations[m,2], combinations[m,3])
    bank_33 <- rbind(bank_33, item)
    bank_33 <- na.omit(bank_33)
    #bank_33 <- subset(bank_33, bank_33[,6] < 1999)
  }
}
rownames(bank_33) <- rep("item", times=nrow(bank_33))
return(bank_33)
}





