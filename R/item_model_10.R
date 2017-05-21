#' @export
#' @importFrom stats na.omit
#' @param items Generate a random mix of items.
#' @param logic The combination of sequences follow two logic ("one" or "two").
#' @param n The value that the arithmetic operator uses to calculate the next value
#' @param arith The arithmetic operator of your choice ("add","substr","multi","div").
#' @description This uses item model 10 to create number series items - Combined identification of parallel sub-sequences and progressively evolving coefficients of change.
#' @details The number series items are a combination of Arithmetic, linear sequence and progressive coefficient. \cr
#' First logic is combining sequences x y x y x y x y = one simple (cannot be controlled), one progressive . \cr
#' Second logic is combining sequences x y x y x y x y = two progressive. The minimum number of items that will be generated is 2. \cr
#' Logic analogous to the Item Model 5, but at least one sub-sequence involves a progressively evolving coefficient. Sub-sequences involve items from Item Families 1, 3, and 7. Example: The coefficient of change between odd elements in the sequence increases by 1. The coefficient of change between even elements increases by -1. (2 8 4 7 7 5 11 2 16 (-2) (22)).
#'
#' When using the first logic, n corresponds to the change in the progressive pattern. However, the simple pattern is fixed and hence drawn randomly.
#' @author Aiden Loe and Filip Simonfy
#' @title Item Model 10
#' @examples
#'
#' #Draws 10 items randomly.
#' imTen(10,logic="one", n=2,arith="add")
#'
#'

#####
# Arithmetic + Linear + Progressive coefficient
# (i.e. coefficient = sequence)

# 2 4 7 11 16 ?

# define starting point - e.g. each number 1:100
# define coefficient sequence - first element and progression
# element = first element + sum of the sequence at the position of the element


# RUN MODEL 2_1 FIRST + linear (below)
imTen <- function(items, logic="one" , n=2,arith = "add" ){
  if(missing(items)){
    stop("Please include x number of items to generate")
  }
  if(items==1) stop("Please put a value greater than 1")

# create matrix
bank_lin <- matrix(ncol=9)
colnames(bank_lin) <- colnames(bank_lin, do.NULL = FALSE, prefix = "Q")
colnames(bank_lin)[9] <- "A"

# Decides on the arithmetic value of the progressive matrix
if(arith == "add"){
  bank_seq <- imThree(items=items,n,arith="add")
}
if(arith == "substr"){
  bank_seq <- imThree(items=items,n,arith="substr")
}
if(arith == "multi"){
  bank_seq <- imThree(items=items,n,arith="multi")
}
if(arith=="div"){
  bank_seq <- imThree(items=items,n,arith="div")
}

bank_list <- rbind(bank_lin, bank_seq)
bank_list <- na.omit(bank_list)

# create progressive coefficient sequence
bank_prog <- NULL
for (k in 1:20) {
  for(l in 1:nrow(bank_list)) {
    b <- bank_list[l,]
    d <- c(0, b[1], sum(b[1:2]), sum(b[1:3]), sum(b[1:4]), sum(b[1:5]),sum(b[1:6]),sum(b[1:7]),sum(b[1:8]))
    e <- c(k) + d
    bank_prog <- rbind(bank_prog, e)
    #removes row where values at col 9 is greater than 100
    #bank_prog <- subset(bank_prog, bank_prog[,9] < 100)
  }
}

# bank of progressive coefficient sequences
#head(bank_prog, 50)

#bank of simple sequences from 1:10 (50 items per simple sequence)
bank_arith<- NULL

for(i in 1:10){
  bank_arith  <- rbind(bank_arith,imSeven(vOne=1,vTwo=i,items=ceiling(items/10),seed=NULL,logic="one",random=FALSE))
}


# combining sequences x y x y x y x y = one simple, one progressive
# creating combinations of whole banks is too lengthy >> randomize 100 items
if(logic=="one"){
  bank_41 <- matrix(ncol=10)
  colnames(bank_41) <- colnames(bank_41, do.NULL = FALSE, prefix = "Q")
  colnames(bank_41)[9:10] <- "A"
  for (i in 1:items) {
    # did not set seed
    f <- bank_arith[sample(nrow(bank_arith), 1), ]
    g <- bank_prog[sample(nrow(bank_prog), 1), ]
    item <- c(f[1], g[1], f[2], g[2], f[3], g[3], f[4], g[4], f[5], g[5])
    bank_41 <- rbind(bank_41, item)
    bank_41 <- na.omit(bank_41)
    bank_41 <- subset(bank_41, bank_41[ ,1] < 2000)
    bank_41 <- subset(bank_41, bank_41[ ,9] < 2000)
    bank_41 <- subset(bank_41, bank_41[ ,10] < 2000)
  }
  return(bank_41)
}

# combining sequences x y x y x y x y = two progressive
# creating combinations of whole banks is too lengthy >> randomize 100 items

if(logic=="two"){
  bank_41b <- matrix(ncol=10)
  colnames(bank_41b) <- colnames(bank_41b, do.NULL = FALSE, prefix = "Q")
  colnames(bank_41b)[9:10] <- "A"
for (i in 1:items) {
  a <- sample(nrow(bank_prog), 2, replace = FALSE)
  f <- bank_prog[a[1],]
  g <- bank_prog[a[2],]
  item <- c(f[1], g[1], f[2], g[2], f[3], g[3], f[4], g[4], f[5], g[5])
  bank_41b <- rbind(bank_41b, item)
  bank_41b <- na.omit(bank_41b)

  bank_41b <- subset(bank_41b, bank_41b[ ,1] < 2000)
  bank_41b <- subset(bank_41b, bank_41b[ ,9] < 2000)
  bank_41b <- subset(bank_41b, bank_41b[ ,10] < 2000)
  }
}
 return(bank_41b)
}






