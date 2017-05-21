# ' @export
# ' @param items Generate a random mix of items.
# ' @param combo There are four different type of combination.
# ' @param seed To get the same random sampling of items
# ' @details Relationships between objects + arithmetic (ratios).
# ' @description  This function creates number series that is a combination between relationships of objects and arithmetic (ratios). The maximum number to be generated is 80. There are four different types of ?na. combination.
# ' @author Aiden Loe and Filip Simonfy
# ' @title Ratios
# ' @examples \dontrun{
# '
# ' # Combo 1. a : b ; c : d ; e : f
# ' # Combo 2. a*x = b; c*x = d; e*x = f
# ' # Combo 3. a : b : c = d : e : f = g : h : i
# ' # Combo 4. a+b = x, c+d = x, e+f = x
# '
# ' # Draws 5 items
# ' nmRatios(items = 5,combo ="one", 1)
# '
# ' }

nmRatios <- function(items,combo = "one", seed){
  if(missing(items)){
    stop("Please include x number of items to generate")
  }
if(combo == "one" || combo == "two" || combo == "three" || combo == "four" ){
  if (items >80){
  stop("Please select item value less than 80.")
  }

  lin_sequence <- seq(1, 150, 1)

if(combo == "one"){
    generate_ratio <- function() {
      set.seed=(seed)
      select <- sample(lin_sequence, 4, replace = FALSE)
      pair1 <- select[1:2]
      pair2 <- c(select[3], select[3] + select[2] - select[1])
      pair3 <- c(select[4], select[4] + select[2] - select[1])

      ratio_sequence <- c(pair1, pair2, pair3)
      return(ratio_sequence)
    }


      bank_34 <- matrix(ncol=6)
      colnames(bank_34) <- colnames(bank_34, do.NULL = FALSE, prefix = "Q")
      colnames(bank_34)[6] <- "A"

      for (i in 1:items) {
        item <- generate_ratio()
        bank_34 <- rbind(bank_34, item)
        bank_34 <- na.omit(bank_34)
      }
  }else if(combo == "two"){

        generate_ratio_b <- function(x) {
        set.seed=(seed)
        select <- sample(lin_sequence, 3, replace = FALSE)
        ratio_sequence_b <- c(select[1], select[1]*x, select[2], select[2]*x, select[3], select[3]*x)
        return(ratio_sequence_b)
        }

      # generate 80 sequences
      bank_34 <- matrix(ncol=6)
      colnames(bank_34) <- colnames(bank_34, do.NULL = FALSE, prefix = "Q")
      colnames(bank_34)[6] <- "A"

      for (i in 1:10) {
        for (x in 2:9) {
          item <- generate_ratio_b(x)
          bank_34 <- rbind(bank_34, item)
          bank_34 <- na.omit(bank_34)
        }
      }
  bank_34 <-bank_34[sample(nrow(bank_34),items,replace=FALSE),]

   }else if(combo== "three"){

      generate_ratio_c <- function() {
        set.seed=(seed)
        select <- sample(lin_sequence, 5, replace = FALSE)
        triad1 <- select[1:3]
        triad2 <- c()
        triad2[1] <- c(select[4])
        triad2[2] <- c(triad2[1] + triad1[2] - triad1[1])
        triad2[3] <- c(triad2[2] + triad1[3] - triad1[2])

        triad3 <- c()
        triad3[1] <- c(select[5])
        triad3[2] <- c(triad3[1] + triad1[2] - triad1[1])
        triad3[3] <- c(triad3[2] + triad1[3] - triad1[2])

        ratio_sequence_c <- c(triad1, triad2, triad3)
        return(ratio_sequence_c)
      }

      # header
      bank_34 <- matrix(ncol=9)
      colnames(bank_34) <- colnames(bank_34, do.NULL = FALSE, prefix = "Q")
      colnames(bank_34)[8:9] <- "A"

      for (i in 1:items) {
        item <- generate_ratio_c()
        bank_34 <- rbind(bank_34, item)
        bank_34 <- na.omit(bank_34)
      }


  }else{


      generate_ratio_d <- function(x) {
        select <- sample(lin_sequence, 3, replace = FALSE)
        pair1 <- c(select[1], x - select[1])
        pair2 <- c(select[2], x - select[2])
        pair3 <- c(select[3], x - select[3])

        ratio_sequence <- c(pair1, pair2, pair3)
        return(ratio_sequence)
      }

      # header
      bank_34 <- matrix(ncol=6)
      colnames(bank_34) <- colnames(bank_34, do.NULL = FALSE, prefix = "Q")
      colnames(bank_34)[6] <- "A"

      for (i in 1:items) {
        set.seed=(seed)
        x <- sample(2:200, 1)
        item <- generate_ratio_d(x)
        bank_34 <- rbind(bank_34, item)
        bank_34 <- na.omit(bank_34)
      }

  }
return(bank_34)
  } else{
      stop("Please select between model one to four.")
    }
}








