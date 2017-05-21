
#' @export
#' @param cat Number of categorical groups per question.
#' @param n The differences between the pair of objects
#' @param items The number of items you want to generate.
#' @param arith The arithmetic operator of your choice ("add","substr","multi","div").
#' @description This uses item model 8 to create number series items.
#' @details This is based on the categorical / pattern recognition rule. Neighbouring pairs or triads of objects are related, includes arithmetic operations.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 8
#' @examples
#'
#' imEight(cat=2,n=4,items=2, arith="add")
#'
#'

imEight <- function(cat ,n, items, arith){
if(cat==1) stop("Please select at least 2 items")

  #create columns
  bank_cat_6 <- matrix(ncol=cat*2)
  colnames(bank_cat_6) <- colnames(bank_cat_6, do.NULL = FALSE, prefix = "Q")
  colnames(bank_cat_6)[cat*2] <- "A"

  #create items
  for (i in 1:items) {
    item <- NULL
    objects <- sample(seq(1:99), cat, replace = FALSE) #random sample
    #generate number of category
  if(arith=="substr"){
    for(i in 1:length(objects)){
      item <-   c(item,objects[i],objects[i]- n)
    }
  }
    if(arith=="add"){
    for(i in 1:length(objects)){
      item <-   c(item,objects[i],objects[i] + n)
    }
}
    if(arith=="multi"){
  for(i in 1:length(objects)){
    item <-   c(item,objects[i],objects[i] * n)
  }
    }
    if(arith=="div"){
    for(i in 1:length(objects)){
    item <-   c(item,objects[i],objects[i] / n)
    }
  }
    bank_cat_6 <- rbind(bank_cat_6, item)
    bank_cat_6 <- na.omit(bank_cat_6)
  }
   return(bank_cat_6)
}


