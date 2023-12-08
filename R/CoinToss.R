#Author: Tingwei Adeck
#Dep: gtools
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

#Author: Tingwei Adeck
#Dep: gtools
toss_coins <- function(coins, output = c('char','num')){
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

#call function
coin_perms <- toss_coins(coins_pop())

