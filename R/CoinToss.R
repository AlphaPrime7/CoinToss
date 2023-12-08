#' Title: Coin population.
#'
#' @return The number of coins tossed represented by a string of Heads and Tails.
#'
#' @param num_coins The number of coins tossed. If NULL then the program will prompt the user.
#'
#' @export
#'
#' @note
#' Multiple coin tosses is a situation where the population is less than the sample.
#' That means 2 coins (considered the population) yield 4 samples; counter intuitive right.
#'
#' @examples
#' coins_population <- coinsPop(5)

coinsPop <- function(num_coins = NULL){

  if(is.null(num_coins)){
    tosses = readline(prompt = "Enter your number of tosses: ") #Also x = scan(); d=scan(what=double()) or  character(), what = "")
    n = as.integer(tosses)
  } else {
    n = num_coins
  }

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
#' coin_perms <- tossCoins(coinsPop(3), 'list')

tossCoins <- function(coins, output = c('char','num','list')){

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

  } else if( 'num' %in% output){

    perm_matrix = as.data.frame(perm_matrix)
    perm_matrix[perm_matrix == 'H'] <- 1
    perm_matrix[perm_matrix == 'T'] <- 0
    return(perm_matrix)

  }  else if( 'list' %in% output){

    initial_list <- vector(mode = 'list', length = nrow(perm_matrix) )
    resulting_vec <- vector(mode = 'list', length = nrow(perm_matrix) )

    perm_matrix = as.data.frame(perm_matrix)

    for (j in 1:nrow(perm_matrix)) {
      for(l in 1: length(resulting_vec)){
        segment = perm_matrix[j,]
        segment = as.character(segment)
        initial_list[[l]] = append(initial_list[[l]], segment)
      }
    }
    initial_list = initial_list[[1]]

    val = n
    sn = c(1:val)
    for(l in 1: length(resulting_vec)){
      segment = initial_list[sn]
      incby = n
      sn = sn + val
      resulting_vec[[l]] = append(resulting_vec[[l]], segment)
    }

    return(resulting_vec)

  }

}

#' Title: Probability calculator (All Heads and Tails for Now).
#'
#' @param num_coins The number of coins tossed.
#' @param probability The probability you intend to calculate.
#' For now 'all-heads' and 'all_tails'.
#'
#' @return A numeric value for the calculated probability.
#'
#' @export
#'
#' @examples
#' all_heads_p = calc_toss_probs(3)

calc_toss_probs <- function(num_coins = NULL, probability = c('all_heads', 'all_tails') ){

  coins_tossed = coinsPop(num_coins)
  coins_sample = tossCoins(coins_tossed,'list')

  #it seems I will manually have to define all possible outcomes, NOT SURE??

  #outcomes
  all_heads = rep('H', length(coins_tossed))
  all_heads = ifelse(all_heads == 'H', 1, 0)

  all_tails = rep('T', length(coins_tossed))
  all_tails = ifelse(all_tails == 'H', 1, 0)

  #calculate probabilities
  #1. probability-all heads
  if (is.null(probability)){
    warning('choose the probability you wish to calculate')
  }

  if ( !is.null(probability) && 'all_heads' %in% probability || 'all_tails' %in% probability){

    all_heads_list = c()
    for(i in coins_sample){
      i = ifelse(i == 'H', 1, 0)
      if (sum(i) == sum(all_heads)){
        all_heads_list = c(all_heads_list, i)
      }
    }
    sum_all_heads = length(all_heads_list)/length(coins_tossed)
    prob_head = sum_all_heads/length(coins_sample)

  }
  return(prob_head)

}

