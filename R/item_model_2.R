#' @export
#' @importFrom stats na.omit
#' @param cat Length of categorical groups per question.
#' @param items The number of items you want to generate.
#' @param random To randomise the position of the numeric values.
#' @description This uses item model 2 to create number series items - Understanding of object categorisation.
#' @details Sequences consist of elements belonging to two homogeneous groups with equal number of elements. Missing element belongs to the group with fewer elements present in the sequence. For example, 1 1 1 5 5 (5).
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 2
#' @examples
#'
#' imTwo(cat=2,items=4,random=FALSE)
#'
#'

# categorization (1	1	1	5	5	?)
imTwo <- function(cat=2, items=4 ,random=FALSE ){
  if(cat==1)
  stop("Please choose a 'cat' value greater than 1")

  #create columns
  bank_cat_6 <- matrix(ncol=cat^2)
  colnames(bank_cat_6) <- colnames(bank_cat_6, do.NULL = FALSE, prefix = "Q")
  colnames(bank_cat_6)[cat^2] <- "A"

    #create items
    for (i in 1:items) {
      item <- NULL
      objects <- sample(seq(1:99), cat, replace = FALSE) #random sample
      #generate number of category
        for(i in 1:length(objects)){
          item <-   c(item,rep(objects[i],cat))
         }
      bank_cat_6 <- rbind(bank_cat_6, item)
      bank_cat_6 <- na.omit(bank_cat_6)
    }

    if(random==TRUE){
      bank_noorder_6 <- matrix(ncol=cat^2)
        for (i in 1:nrow(bank_cat_6)) {
          item <- bank_cat_6[i, sample(1:cat^2, cat^2, replace = FALSE)]
          bank_noorder_6 <- rbind(bank_noorder_6, item)
          bank_noorder_6 <- na.omit(bank_noorder_6)
        }
      colnames(bank_noorder_6) <- colnames(bank_cat_6, do.NULL = FALSE, prefix = "Q")
      colnames(bank_noorder_6)[cat^2] <- "A"
      return(bank_noorder_6)
    }else{
      return(bank_cat_6)
      }

}






