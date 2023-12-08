#' Title: Coin population.
#'
#' @return The number of coins tossed represented by a string of Heads and Tails.
#'
#' @export
#'
#' @note
#' Multiple coin tosses is a situation where the population is less than the sample.
#' That means 2 coins (considered the population) yield 4 samples; counter intuitive right.
#'
#' @examples
#' coins_population <- coinsPop()

coinsPop <- function(){

  tosses = readline(prompt = "Enter your number of tosses: ") #Also x = scan(); d=scan(what=double()) or  character(), what = "")
  n = as.integer(tosses)
  sides <- 'HT'
  #sides <- c('H','T')
  coins <- vector()

  for(i in 0:(n-1) ){
    if(n == 0)
      coins <- c(coins, NULL)
    else
      coins <- c(coins,sides)
  }
  return(as.vector(coins))
}


#' Title: Coins Sampling.
#'
#' @import gtools
#'
#' @param coins The coins population which is a no parameter subordinate function.
#' @param output A data frame with numeric or character outcomes
#'
#' @return A data frame showing the coins samples generated.
#'
#' @export
#'
#' @note
#' Each row represents a sample.
#'
#' @examples
#' coin_perms <- tossCoins(coins_pop())

tossCoins <- function(coins, output = c('char','num')){

  if(length(coins) == 0){
    return(print("No coins tossed"))
  }

  else if(length(coins) == 1){
    n <- length(coins)
    for(i in 1:n){
      return(strsplit(coins[i], "")[[1]])
    }
  }

  else if(length(coins) > 1){
    toss_outcomes <- c('H','T')
    n <- length(coins)
    perm_matrix <- gtools::permutations(length(toss_outcomes),n, toss_outcomes, repeats.allowed = T)
  }

  if( is.null(output) || 'char' %in% output){

    perm_matrix = as.data.frame(perm_matrix)
    return(perm_matrix)

  } else {

    perm_matrix = as.data.frame(perm_matrix)
    perm_matrix[perm_matrix == 'H'] <- 1
    perm_matrix[perm_matrix == 'T'] <- 0
    return(perm_matrix)
  }

}

