library(ggplot2)

setwd("/Users/apple/Desktop/Applied Stochastic Process/Final Project")

reference=readLines("Holmes.txt")
reference=toupper(reference)

# Transition matrix
trans.mat=matrix(0,27,27)
rownames(trans.mat)=colnames(trans.mat)=c(toupper(letters),"")

# A function that convert letters to numbers
letters2numbers = function(l){
  if( l %in% toupper(letters) )
    return(which( l == toupper(letters) ))
  else
    return(27)
}

# Calculate the transition matrix
i1 = 27
for(i in 1:length(reference)){
  cat('Line:',i,'\n')
  if(nchar(reference[i]) != 0){
    letters.in.line = strsplit(reference[i],'')[[1]]
    
    for(j in 1:length(letters.in.line)){
      i2 = letters2numbers(letters.in.line[j])
      if( i1!=27 | i2!=27 )
        trans.mat[i1,i2] = trans.mat[i1,i2] + 1
      i1 = i2
    }
  }
}

# Add 1 to each entry and normalize
trans.mat = trans.mat + 1
trans.mat = trans.mat / ( apply(trans.mat, 1, sum) %*% t(rep(1,27)) )


# MCMC
# sub_rule: current -> original
# A function that finds the original letter under certain substitution rule
sub = function(l, substitution.rule){
  retval = c(toupper(letters), ' ')
  if( l %in% toupper(letters) )
    return(retval[substitution.rule[which( l == toupper(letters) )]])
  else
    return(retval[substitution.rule[27]])
}

# Calculate the log likelihood of a string under certain substitution rule and transition matrix
log.likelihood = function(string, substitution.rule, trans.mat){
  split = strsplit(string, '')[[1]]
  indices = sapply(split, letters2numbers)
  indices = c(indices, 27)
  
  retval = 0
  last_idx = 27
  for(i in 1:length(indices)){
    cur_idx = substitution.rule[indices[i]]
    if( cur_idx != 27 | last_idx != 27 ){
      retval = retval + log( trans.mat[last_idx, cur_idx] )
      last_idx = cur_idx
    }
  }
  return(retval)
}

encode = function(string){
  indices = sapply(strsplit(string, '')[[1]], letters2numbers)
  sub_rule = c(sample(26,26), 27)
  key = 1:27
  key[sub_rule] = 1:27
  return( list( 'key' = key,
                'string'=paste(c(toupper(letters),' ')[sub_rule[indices]], collapse='')
  )
  )
}

decode = function(string, sub_rule){
  indices = sapply(strsplit(string, '')[[1]], letters2numbers)
  return( paste(c(toupper(letters),' ')[sub_rule[indices]], collapse='') )
}

mcmc = function(string, trans.mat, n, a){
  sub_rule = c(sample(26,26), 27) # Initial guess
  best_rule = sub_rule
  max.log.likelihood = log.likelihood(string, best_rule, trans.mat)
  for(i in 1:n){
    # Propose new substitution rule
    if( runif(1) < a ){ # Independent moves
      proposed_sub_rule = c(sample(26,26), 27)
    }else{ # Local move
      sub_idx = sample(26,2)
      proposed_sub_rule = sub_rule
      proposed_sub_rule[sub_idx[1]] = sub_rule[sub_idx[2]]
      proposed_sub_rule[sub_idx[2]] = sub_rule[sub_idx[1]]
    }
    # Calculate probability
    l1 = log.likelihood(string, sub_rule, trans.mat)
    l2 = log.likelihood(string, proposed_sub_rule, trans.mat)
    # If we get a better solution than best_rules
    if( l2 > max.log.likelihood ){
      max.log.likelihood = l2
      best_rule = proposed_sub_rule
    }
    # Acceptance / Rejection
    if( runif(1) < exp(l2-l1) ){
      sub_rule = proposed_sub_rule
      # cat('Iteration',i,'Accepted\n')
    }else{
      # cat('Iteration',i,'Rejected\n')
    }
    if( (i %% 1000) == 0 ){
      cat('Iteration:',i,'\n')
      cat(' ',decode(string, best_rule),'\n')
    }
  }
  
  split = strsplit(string, '')[[1]]
  indices = sapply(split, letters2numbers)
  return( paste(c(toupper(letters), ' ')[sub_rule[indices]], collapse='') )
  
}

# Plotting the transition matrix
Prob = as.vector(trans.mat)
ggplot(data = expand.grid('Letter 1'=c(letters,' '), 'Letter 2'=c(letters,' ')), aes(x = `Letter 1`, y = `Letter 2`)) + geom_tile(aes(fill = Prob)) 



sample = 'THAT IS THE QUESTION'
sample = 'IT IS AN OLD MAXIM OF MINE THAT WHEN YOU HAVE EXCLUDED THE IMPOSSIBLE WHATEVER REMAINS HOWEVER IMPROBABLE MUST BE THE TRUTH'
s = encode(sample)
# Print the encoded text
print(s)
# MCMC
mcmc(s$string, trans.mat, 50000, 0.025)








