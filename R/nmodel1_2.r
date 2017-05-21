# ' @export
# ' @param length Generate the length of the items.
# ' @param items The number of items to generate.
# ' @param random The same alphabets grouped together is random = FALSE.
# ' @author Aiden Loe and Filip Simonfy
# ' @title Item Model
# ' @description This creates number series items by Counting object categorisation.
# ' @details Sequences consist of elements belonging to two letter groups with equal or unequal number of elements. For example, two objects: y y x x x ?? The answer is (2,3), where 2 = total count of y, and 3 = total count of x.
# '
# ' @examples \dontrun{
# '
# ' nmCountB(3,6, random=FALSE)
# ' }


nmCountB <- function(length=3,items=6, random=FALSE) {
 # alphabet <- LETTERS[seq(1:26)]
  bank_11b <- matrix(ncol = (length+2))
  colnames(bank_11b) <- colnames(bank_11b, do.NULL = FALSE, prefix = "Q")

  for (i in 1:items) {
    a <- sample(LETTERS, 2, replace = FALSE)

    # might sample all the same letters. We want to have at least sample 1 each letter,     if not reject the sampling.
    b <- sample(a, length, replace=TRUE)

    # always draw at least one
    if(sort(unique(b %in% b[duplicated(b)]))[1] == TRUE){
      b <- sample(a, length, replace=TRUE);
    }

    if(random==FALSE){
    b <- b[order(b)]
    }

    table <- as.data.frame(table(b))
    answer <- c(table[1:2,2])

colnames(bank_11b)[c(length+1, length+2)] <- "A"
    item <- c(b, answer)
    item

    bank_11b <- rbind(bank_11b, item)
    bank_11b <- na.omit(bank_11b)
    bank_11b
  }

  return(bank_11b)
}



# bank of items of 6, 7, 8 characters

# bank_11b_6 <- model_11b(6)
# bank_11b_7 <- model_11b(7)
# bank_11b_8 <- model_11b(8)




