#' @export
#' @importFrom stats na.omit
#' @param items Generate a random mix of items.
#' @description This uses item model 13 to create number series items - Combined identification of unevenly ordered sub-sequences and non-successive relationships between elements.
#' @details This function creates number series creates a combination of sequences and ratios. TLogic analogous to the Item Model 13, but the second sequence belongs to the Item Model 9. As a result, pairs of elements following certain rule are embedded into a progressive sequence. Example: Sequence with coefficient of (+ 1) is interposed with pairs of elements which differ by 3. 1 5 8 2 209 212 3 41 (44) (4). Only the addition and substraction arimethic operators are used to generate the number series items.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 13
#' @examples
#'
#' #Draws 10 items randomly.
#' imThirteen(10)
#'
#'

# a : b = c : d = e : f
# a + b = x, c + d = x ?

# RUN MODEL 2_1 FIRST
# RUN MODEL 3_4 a, b, & d FIRST
# + linear (below)

imThirteen<- function(items){
  if(missing(items)){
    stop("Please include x number of items to generate")
  }

seed <- sample(1:1000, 1)

#Model 2_1
model2_1 <- function(value){
bank_lin <- imAdd(9,95)
bank_lin <- as.matrix(bank_lin)

#add
add<- NULL
for(i in 1:50){
  add[[i]] <- imThree(items=19,n=i,arith="add")
}
add <- do.call("rbind", add)

#substruct
sub<- NULL
for(i in 1:25){
  sub[[i]] <- imThree(items=19,n=i,arith="substr")
}
sub <- do.call("rbind", sub)

#multi
# multi<- NULL
# for(i in 2:50){
#   multi[[i]] <- imThree(items=19,n=i,arith="multi")
# }
# multi <- do.call("rbind", multi)
# multi <- subset(multi, multi[,6] < 500)

#division
# div<- NULL
# for(i in 2:50){
#   div[[i]] <- imThree(items=100,n=i,arith="div")
# }
# div <- do.call("rbind", div)
# div <- subset(div, div[,1] < 500)

#combind all together
#bank_list <- rbind(bank_lin, add, multi, sub, div)
bank_list <- rbind(bank_lin, add, sub)
bank_seq <- na.omit(bank_list)
return(bank_seq)
}

bank_seq <- model2_1(100) # always 100

#Model 3_4
bank_34 <- nmRatios(10, combo="one",seed)
bank_34b <- nmRatios(10, combo="two",seed)
#bank_34d<- nmRatios(10, combo="four",seed)

bank_ratio <- rbind(bank_34, bank_34b)

#combind together
bank_44 <- matrix(ncol=10)
colnames(bank_44) <- colnames(bank_44, do.NULL = FALSE, prefix = "Q")
colnames(bank_44)[9:10] <- "A"

for (i in 1:items) {
  random_seq <- bank_seq[sample(1:nrow(bank_seq), 1), ]
  random_rat <- bank_ratio[sample(1:nrow(bank_ratio), 1), ]
  item <- c(random_seq[1], random_rat[1], random_rat[2], random_seq[2], random_rat[3], random_rat[4], random_seq[3], random_rat[5], random_rat[6], random_seq[4])

  bank_44 <- rbind(bank_44, item)
  bank_44 <- na.omit(bank_44)
}
return(bank_44)
}

