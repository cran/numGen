#' @export
#' @importFrom stats na.omit
#' @param items Number of items to generate.
#' @param seed Setting the seed returns the same items on the local computer.
#' @description This uses item model 1 to create number series items - Elementary understanding of sequence succession.
#' @details Simple linear sequences which do not require use of advanced arithmetic operations, such as ordered multiples of 1, 10, or 100. Example: A sequence of ordered multiples of 10. (10 20 30 40 (50)).
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 1
#' @examples
#'
#' imOne(items=5, seed=5)
#'
#'

# same letter sequence bank = bank_1
imOne<- function(items=5,seed=1){

    if(missing(items)){
    stop("Please include x number of items to generate")
    }

  if(items> 138){
    stop("The maximum number of items is 138")
  }


# simple linear 'by 10' = bank_4
bank_1 <- matrix(ncol=5)
sequence_10 <- seq(50)*1
for (i in 1:(length(sequence_10) - 4)) {
  item <- c(sequence_10[i], sequence_10[i+1], sequence_10[i+2], sequence_10[i+3], sequence_10[i+4])
  bank_1 <- rbind(bank_1, item)
}
bank_1
# simple linear 'by 10' = bank_4
bank_2 <- matrix(ncol=5)
sequence_10 <- seq(50)*10
 for (i in 1:(length(sequence_10) - 4)) {
  item <- c(sequence_10[i], sequence_10[i+1], sequence_10[i+2], sequence_10[i+3], sequence_10[i+4])
  bank_2 <- rbind(bank_2, item)
 }
bank_3 <- matrix(ncol=5)
sequence_10 <- seq(50)*100
for (i in 1:(length(sequence_10) - 4)) {
  item <- c(sequence_10[i], sequence_10[i+1], sequence_10[i+2], sequence_10[i+3], sequence_10[i+4])
  bank_3 <- rbind(bank_3, item)
}

bank_all <- rbind(bank_1,bank_2, bank_3)
bank_all <- na.omit(bank_all)

set.seed(seed)
sample_bank_all <- bank_all[sample(nrow(bank_all), items, replace=FALSE), ]
return(sample_bank_all)
}


