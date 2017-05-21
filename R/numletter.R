# # ' @export
# # ' @importFrom stats na.omit
# # ' @param numLetter Number of Letters you want.
# # ' @param value Adding the value and the number of letters return the total count of the numeric value.
# # ' @param items Number of items to generate.
# # ' @param random If random=FALSE, the items will follow in sequential order.
# # ' @description This uses item model 4 to create number series items - Comprehension of object representation
# # ' @details The core feature of sequences is the numeric representation of the object count.  The sequences consist of homogeneous groups of letters followed by a number representing the amount of the group elements. For example (a 1 b b b 3 c c (2))
# # ' @author Aiden Loe and Filip Simonfy
# # ' @title Item Model 4
# # ' @examples \dontrun{
# # '
# # ' nmFour(numLetter= 2 ,value=3, items=10, random=FALSE)
# # '
# # ' }
#

# number representation + relationship between objects
# with order a 1 b b b 3 c c
nmFour <- function(numLetter=2,value=3, items=3, random=FALSE){


  bank_rep <- matrix(ncol = value+numLetter*2)
  bank_rep
  colnames(bank_rep) <- colnames(bank_rep, do.NULL = FALSE, prefix = "Q")
  colnames(bank_rep)[value+numLetter*2] <- "A"
for(i in 1:items) {
  a <- sample(LETTERS, numLetter, replace = FALSE)
  b <- c(a, unlist(sample(a, value, replace = TRUE)))
  table <- as.data.frame(table(b))
  table[ ,1] <- as.character(table[ ,1])
  element <- NULL
  for(j in 1:numLetter){
  element <- c(element, c((rep(table[j,1], c(table[j,2]))), table[j,2]))
  }
  if(random==TRUE){
    element <- sample(element, length(element), replace = FALSE)
  }
  bank_rep <- rbind(bank_rep, element)
  bank_rep <- na.omit(bank_rep)
}
  return(bank_rep)
}








