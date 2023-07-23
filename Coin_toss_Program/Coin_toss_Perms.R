#Theme: permutations in R. Looking at the different combinations of spinning coins while working on statistics 
#essentials then one arrives at the conclusion that understanding permutations is helpful in 
#having a deeper understanding of outcomes. Also some FUN!!

#Fun Fact: JARHEAD LIKE ME CAN FIGURE IT OUT, THEN YOU CAN

#write a function for the coin toss
#sides <- 'HT'; coins <- c(rep(sides,2))-another way to write function below

#simplest version of coin toss permutations

coins_pop <- function(){
  # DATE WRITTEN: 08 July 2023         LAST REVISED:  22 July 2023
  # AUTHOR:  Tingwei Adeck (awesome.tingwei@outlook.com)
  #
  # DESCRIPTION:
  #             Generates a vector representing the number of coins tossed.
  #
  # EXAMPLE:
  #	# coins_population <- coins_pop()
  # print(coins_population)
  
  tosses = readline(prompt = "Enter your number of tosses: ") #Also x = scan(); d=scan(what=double()) or  character(), what = "")
  n = as.integer(tosses)
  sides <- 'HT'
  coins <- vector()
  no_coins <- vector()
  
  for(i in 0:(n-1)){
    if(n == 0)
      no_coins <- c(no_coins, NULL)
    else
      coins <- c(coins,sides)
  }
  return(as.vector(coins))
}

#call function
coins_population <- coins_pop()
print(coins_population)

toss_coins <- function(coins){ #coins is a vector (scenario 1 is 1 coin toss)
  # DATE WRITTEN: 07 July 2023         LAST REVISED:  22 July 2023
  # AUTHOR:  Tingwei Adeck (awesome.tingwei@outlook.com)
  #
  # DESCRIPTION:
  #             Takes coins as input, derived from sister function coins_pop()
  #             Working on it returning a list of permutations for H & T
  #
  # EXAMPLE:
  #	# coin_sample1 <- toss_coins(coins_pop())
  # print(coin_sample1)
  # emphasis on syntax learning
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
    toss_outcomes <- c('H','T') #character vector of binary outcomes for coin toss
    n <- length(coins)
    perm_matrix <- gtools::permutations(2,n, toss_outcomes, repeats.allowed = T)
  }
  perm_matrix
      
}

#call function
coin_perms <- toss_coins(coins_pop()) #[r,c] ; col = [,i] row = [i,]


for(i in seq_len(nrow(coin_perms))){
  print(coin_perms[i,])
  for(j in coin_perms[i,]){
      for(k in j){
        if(k == "T" && k != k)
          print(k[1])
      }
  }
}


