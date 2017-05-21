# ' @export
# ' @import "stats"
# ' @param x Generate the length of the items
# ' @param items The number of items to generate.
# ' @description This creates number series items by Counting object categorisation.
# ' @details Sequences consist of elements belonging to one letter group with x number of elements. For example, single object: x x x x ?. The answer will be 4, where 4 = the total count of x.
# ' @author Aiden Loe and Filip Simonfy
# ' @title Item Model
# ' @examples \dontrun{
# '
# ' nmCountA(3,26)
# '
# ' }

# Is not the same as model 1 in the paper
# This should be a separate one.
# count (number representation)
# 1.1 single object: x x x x ?

# create vector containing all alphabet characters
# transform into a matrix column

# create bank of items
# each item contains x characters

nmCountA <- function(x,items) {
  if(items>26){
    stop("Number of items must less than 27.")
  }
  alphabet <- LETTERS[seq(1:items)]
  column <- matrix(alphabet, nrow = items, ncol = 1)
  bank_11 <- matrix(nrow = items, ncol = (x+1))
  bank_11[ ,c(1:x)] <- column
  colnames(bank_11) <- colnames(bank_11, do.NULL = FALSE, prefix = "Q")
  rownames(bank_11) <- rownames(bank_11, do.NULL = FALSE, prefix = "item")

  bank_11[ ,(x+1)] <- x
  colnames(bank_11)[x+1] <- "A"

  return(bank_11)
}


