n = 100000 #number of simulations
n_questions <- 4 #number of questions per bloc
num_known <- 0:n_questions #number of possible known answers
num_guess <- 0:n_questions #number of possible unknown answers

results <- data.frame(matrix(nrow = n_questions+1, ncol = n_questions+1))
names(results) <- num_known
rownames(results) <- num_guess

for(i in num_known){ # go through all known-unknown combinations
  for(y in num_guess){
    if(i + y <= n_questions){ # only calculate combinations where known + unknown <= questions
    
      #draw n*n_questions True/False answers, each with .5 probability
      df <- data.frame(matrix(data = sample(c(T,F), n*n_questions, replace = T), ncol = n_questions, nrow = n))
      
      #set all known answers to True
      if(i >= 1){df[,1:i] <- T}
      
      
      if(i + y == 0){ #deal with 0-0 case
        results[i+1,y+1] <- 0
      }else{ #calculate points (2 per True answer, -2 per False, min 0)
        df$results <- apply(data.frame(df[, 1:(i+y)]), 1, function(x) max((2*sum(x)-2*sum(!x)),0))
        results[i+1,y+1] <- sum(df$results)/n #take the average of all results
      }
    }
  }
}
