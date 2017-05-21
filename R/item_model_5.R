#' @export
#' @importFrom stats na.omit
#' @param arithOne Select the arithmetric operator of choice ("add","multi", "sub", "div").
#' @param arithTwo Select the arithmetric operator of choice ("add","multi", "sub", "div").
#' @param n Value you want use the arithmetic operator on.
#' @param items Generate a random mix of items.
#' @description This uses item model 5 to create number series items - Identification of co-occurring relationships between elements (with use of arithmetic skills)
#' @details Logic analogous to the Item Model 4, but at least one sub-sequence involves the basic arithmetic operations. Sequences combine items from Item Families 1 and 3. The arithmetic operations change but the differences in value remains the name. Example: Odd elements of the sequence increase by 2 and even elements of the sequence are multiplied by 2. (2 12 4 24 6 48 8 (96) (10))
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 5
#'
#' @examples
#'
#' imFive(arithOne="add",arithTwo="add",n=2,items=5)
#'
#'


imFive<- function(arithOne="add",arithTwo="substr",n=2,items=4){

  stopifnot(arithOne =="add" || arithOne =="multi" || arithOne =="substr"  || arithOne =="div")
  stopifnot(arithTwo =="add" || arithTwo =="multi" || arithTwo =="substr"  || arithTwo =="div")

  if(arithOne == "add"){
    bank_listA <- imThree(items,n,arith="add")
  }else if(arithOne == "substr"){
    bank_listA <- imThree(items,n,arith="substr")
  }else if(arithOne == "multi"){
    bank_listA <- imThree(items,n,arith="multi")
  }else {
    bank_listA <- imThree(items,n,arith="div")
  }


  if(arithTwo == "add"){
    bank_listB <- imThree(items,n,arith="add")
  }else if(arithTwo == "substr"){
    bank_listB <- imThree(items,n,arith="substr")
  }else if(arithTwo == "multi"){
    bank_listB <- imThree(items,n,arith="multi")
  }else {
    bank_listB <- imThree(items,n,arith="div")
  }



  #bank_list <- rbind(bank_21add, bank_21multi, bank_21sub, bank_21div)

  bank_31 <- matrix(ncol = 10)
  colnames(bank_31) <- colnames(bank_31, do.NULL = FALSE, prefix = "Q")
  colnames(bank_31)[9:10] <- "A"

  for (i in 1:items) {
    a <- sample(nrow(bank_listA), 1, replace = FALSE)
    b <- sample(nrow(bank_listB), 1, replace = FALSE)
    f <- bank_listA[a[1], ]
    g <- bank_listB[b[1], ]
    item <- c(f[1], g[1], f[2], g[2], f[3], g[3], f[4], g[4], f[5], g[5])
    bank_31 <- rbind(bank_31, item)
    bank_31 <- na.omit(bank_31)
  }
  return(bank_31)
}



