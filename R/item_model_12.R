#' @export
#' @importFrom stats na.omit
#' @param items Generate a random mix of items.
#' @description This uses item model 12 to create number series items - Identification of unevenly ordered sub-sequences
#' @details This function creates number series that is a irregular combination of sequences a b b a b b a ... Only the addition and substraction arithmetic operators are used to create the number series items.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 12
#' @examples
#'
#' #Draws 10 items randomly.
#'imTwelve(10)
#'
#'

## Irregular combination of sequences a b b a b b a ...
# + - x / 2
# same/different operator

# RUN MODEL 2_1 FIRST + linear (below)
imTwelve <- function(items){
  if(missing(items)){
    stop("Please include x number of items to generate")
  }
bank_lin <- imAdd(9,95)
bank_lin <- as.matrix(bank_lin)

#add
add<- NULL
for(i in 1:50){
add[[i]] <- imThree(items=19,n=i,arith="add")
}
add <- do.call("rbind", add)

#substract
sub<- NULL
for(i in 1:25){
  sub[[i]] <- imThree(items=19,n=i,arith="substr")
}
sub <- do.call("rbind", sub)

#Also, we can limit the values of add and str by changing the value 99 to something smaller in the code.
# This was commented out because the numbers in the series become too big.
#multi
# multi<- NULL
# for(i in 2:99){
#   multi[[i]] <- nmThree(items=100,n=i,arithmetic="multi")
# }
# multi <- do.call("rbind", multi)
# multi <- subset(multi, multi[,6] < 1999)

# #division
# div<- NULL
# for(i in 2:5){
#   div[[i]] <- nmThree(items=10,n=i,arithmetic="div")
# }

#div <- do.call("rbind", div)
#div <- subset(div, div[,1] < 1999)

#combind all together
#bank_list <- rbind(bank_lin, add, multi, sub, div)
bank_list <- rbind(bank_lin, add, sub)
bank_list <- na.omit(bank_list)

# generate 100 random items
  if(items == 1){stop('Insert value greater than 1')}
bank_43 <- matrix(ncol=10)
colnames(bank_43) <- colnames(bank_43, do.NULL = FALSE, prefix = "Q")
colnames(bank_43)[9:10] <- "A"

for (i in 1:items) {
  random <- bank_list[sample(nrow(bank_list), 2, replace = FALSE), ]
  g <- random[1, ]
  f <- random[2, ]
  item <- c(f[1], g[1], g[2], f[2], g[3], g[4], f[3], g[5], g[6], f[4])
  bank_43 <- rbind(bank_43, item)
  bank_43 <- na.omit(bank_43)
}
return(bank_43)
}









