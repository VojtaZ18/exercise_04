rm(list=ls())

ReturnCoins <- function(M){
  min_coins <- 0
  for (coin in c(50, 20, 10, 5, 2, 1)){
    min_coins <- min_coins + (M%/%coin)
    M <- M %% coin
  }
  return(min_coins)
}

print(ReturnCoins(176))

UniversalReturnCoins <- function(M, coins){
  num_of_coins <- rep(0, length(coins))
  for (i in (1:length(coins))){
    if(M==0){
      break
    }
    num_of_coins[i] <- M %/% coins[i]
    M <- M %% coins[i]
  }
  return(num_of_coins)
}

print(UniversalReturnCoins(170, c(50,20,10,5,2,1)))

Chocolate <- function(M, r, c){
  if(r==nrow(M)){
    return(M[r, c])
  }
  else{
    bars <- M[r, c]
    down <- Chocolate(M, r+1, c)
    diagonal <- Chocolate(M, r+1, c+1)
    return(max(down, diagonal)+bars)
  }
}

matrix <- matrix(c(3,1,5,1,0,4,3,2,0,0,0,6,0,0,0,7), nrow = 4)
print(matrix)
Chocolate(matrix, 1, 1)

HanoiTowers <- function(n, fromPeg, toPeg){
  if(n==1){
    print(paste0("Move disc from peg ", fromPeg, " to peg ", toPeg))
    return()
  }
  unusedPeg <- 6 - fromPeg - toPeg
  HanoiTowers(n-1, fromPeg, unusedPeg)
  print(paste0("Move disc from peg ", fromPeg, " to peg ", toPeg))
  HanoiTowers(n-1, unusedPeg, toPeg)
  return()
}

HanoiTowers(5, 1, 3)
