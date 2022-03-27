####### Building the generalized terror and rescue operation system #######

# Define the parameter(After building a model these values will be used for parameter when using function)
lam = 0.2763 # the rate of terror group arrival per day
mu = 0.2829 # the rate of rescue group's operation successful per day(no hostages killed)
eps = 0.1718 # the rate of terror groups start terror per day
t <- 6 # the number of terror groups
r <- 4 # the number of rescue groups

line <- function(lam, mu, eps, t, r) {
  
  # Design the transition rate matrix
  k <- t+1 # size of square matrix(k X k)
  mat <- matrix(0, nrow = k, ncol = k)
  mat[1,2] <- lam
  
  # Allocate the rescue team according to No. of hostage takings 
  if (r != 1) {
    for (i in 1:(r-1)) {
      mat[1+i, i] <- (i*mu+i*eps)
      mat[1+i, i+2] <- lam
    }    
  }
  
  # If No. of hostage taking is larger than rescue team capacity, we allocate max rescue team capacity 
  if (r < t) {
    for (i in r:(k-1)) {
      mat[1+i, i] <- (r*mu+i*eps)
      if ((i+2) <= k) {
        mat[1+i, i+2] <- lam  
      } else
        break
    }
  }
  mat[k,k-1] <- r*mu+t*eps
  
  # 
  for (i in 1:k) {
    mat[i,i] <- -sum(mat[i,])
  }
  
  maxRate = lam+t*eps+r*mu
  new_mat = mat / maxRate
  for (i in 1:k) {
    new_mat[i,i] = 0
    new_mat[i,i] = 1 - sum(new_mat[i,])
  }   
  
  new_a = matrix(0, nrow=1, ncol=k)
  new_a[1] = 1
  a = matrix(0, nrow=1, ncol=k)

  while (sum((a - new_a)^2) != 0) {
    a = new_a
    b = new_a %*% new_mat
    new_a = b
    }  
  steady_prob = new_a
  
  terror <- matrix(0:t, nrow=1, ncol=t+1)
  ATG <- terror %*% t(steady_prob)
  rescue <- matrix(0, nrow=1, ncol=t+1)
  for (i in 1:(t+1)) {
    rescue[i] <- i-1
    if (r < t) {
      for (j in (r+1):(t+1))
        rescue[j] <- r
    } else {
      rescue <- rescue
    }
  }
  ASF <- rescue %*% t(steady_prob)
  result <- list("Prob_Matrix" = mat, "Steady_prob" = steady_prob,
                 "ATG" = ATG, "ASF" = ASF, "EKR" = ATG*eps, "ESR" = ASF*mu,
                 "W" = ATG/lam,
                 "Success_prob" = (ASF*mu) / (lam+ATG*eps+ASF*mu),
                 "Fail_prob" = (ATG*eps) / (lam+ATG*eps+ASF*mu),
                 "NewEntering_prob" = (lam) / (lam+ATG*eps+ASF*mu)
  )
  return(result)
}

line(lam=0.2763, mu=0.2829, eps=0.1718, t=10, r=10)



