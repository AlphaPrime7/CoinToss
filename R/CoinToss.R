coins_pop <- function(){
  
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

coins_population <- coins_pop()
print(coins_population)

toss_coins <- function(coins){
  library(gtools)
  
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
  as.data.frame(perm_matrix) 
  
}

#call function
coin_perms <- toss_coins(coins_pop()) #[r,c] ; col = [,i] row = [i,]

