#' @export
#' @importFrom stats na.omit
#' @param items Number of items to generate.
#' @description This uses item model 6 to create number series items - Identification of progressively evolving coefficients of change.
#' @details Non-linear progressive sequences which require a higher level of abstraction; the coefficient of change between two neighbouring elements is not invariable and its elements form a sequence. The coefficient sequences correspond to items from Item Families 1 and 3. Example: The coefficient of change between each pair of neighbouring elements in the sequence increases by 1. (2 4 7 11 16 (22))
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 6
#' @examples
#'
#' imSix(items=3)
#'

#Arithmetic + Linear + Progressive coefficient (i.e. coefficient = sequence)
# Define starting point - e.g. each number 1:100
# define coefficient sequence - first element and progression
# element = first element + sum of the sequence at the position of the element
imSix <- function(items){
bank_lin <- matrix(ncol=6)
colnames(bank_lin) <- colnames(bank_lin, do.NULL = FALSE, prefix = "Q")
colnames(bank_lin)[6] <- "A"

for (i in 1:95) {
  item <- c(i, i+1, i+2, i+3, i+4, i+6)
  bank_lin <- rbind(bank_lin, item)
  bank_lin <- na.omit(bank_lin)
}

#Model 2_1
model2_1 <- function(value){
  bank_lin <- imAdd(9,95)
  bank_lin <- as.matrix(bank_lin)


  #add
  add<- NULL
  for(i in 1:50){
    add[[i]] <- imThree(items=10,n=i,arith="add")
  }
  add <- do.call("rbind", add)

  #substruct
  sub<- NULL
  for(i in 1:25){
    sub[[i]] <- imThree(items=10,n=i,arith="substr")
  }
  sub <- do.call("rbind", sub)


#multi
#   multi<- NULL
#   for(i in 2:50){
#     multi[[i]] <- nmThree(items=10,n=i,arith="multi")
#   }
#   multi <- do.call("rbind", multi)
#   multi <- subset(multi, multi[,6] < 200)

  #division
#   div<- NULL
#   for(i in 2:50){
#     div[[i]] <- nmThree(items=10,n=i,arith="div")
#   }
#   div <- do.call("rbind", div)
#   div <- subset(div, div[,1] < 500)


#combind all together
  bank_list <- rbind(bank_lin, add, sub)
  bank_seq <- na.omit(bank_list)
  return(bank_seq)
  #return(add)
}

bank_list <- model2_1(100) # always 100
bank_list


bank_32 <- matrix(ncol=6)
colnames(bank_32) <- colnames(bank_32, do.NULL = FALSE, prefix = "Q")
colnames(bank_32)[6] <- "A"

# all combinations would be too lenghty >> random selection
# generate random 1000 items
for (i in 1:items) {
  # random item = coefficient sequence
  a <- sample(nrow(bank_list), 1)
  b <- bank_list[a,]
  b
  # d = progressive coefficient addition
  d <- c(0, b[1], sum(b[1:2]), sum(b[1:3]), sum(b[1:4]), sum(b[1:5]))

  # sequence with a random starting point
  item <- c(sample(1:30, 1) + d)

  bank_32 <- rbind(bank_32, item)
 # bank_32 <- subset(bank_32, bank_32[,6]<200)
  bank_32 <- na.omit(bank_32)
  nrow(bank_32)
  }

 return(bank_32)
}




