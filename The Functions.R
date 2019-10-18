#code to simulate data in chapter 4
gen.data.prior <- function(n, p, alpha_T=1.5, beta_T =1, alpha_O=1,
                           beta_O=1, mu_0=2, sigma_0=1, mu_1=3, sigma_1=1, 
                           r1 = c(0,0.1,0.2,0.3,0.4), r2 = c(1,0.9,0.8,0.7,0.6),
                           g, iter = 50, seed){
  set.seed(seed)
  pre_T <- rgamma(p, alpha_T, beta_T) #precision of T
  pre_O <- rgamma(p, alpha_O, beta_O) #precision of O
  mu_T <- rnorm(p, mu_0, sigma_0)
  mu_O <- rnorm(p, mu_1, sigma_1)
  f1 <- runif(n, r1[1], r2[1])
  f2 <- runif(n, r1[2], r2[2])
  f3 <- runif(n, r1[3], r2[3])
  f4 <- runif(n, r1[4], r2[4])
  f5 <- runif(n, r1[5], r2[5])
  priors <- unname(cbind(mu_T, mu_O, pre_T, pre_O))
  T.rep <- replicate(iter, rlnorm(n, meanlog = priors[g,1], 
                                  sdlog = sqrt(1/priors[g,3])))
  O.rep <- replicate(iter, rlnorm(n, meanlog = priors[g,2], 
                                  sdlog = sqrt(1/priors[g,4])))
  Y.rep1 <- f1*T.rep + (1-f1)*O.rep
  Y.rep2 <- f2*T.rep + (1-f2)*O.rep
  Y.rep3 <- f3*T.rep + (1-f3)*O.rep
  Y.rep4 <- f4*T.rep + (1-f4)*O.rep
  Y.rep5 <- f5*T.rep + (1-f5)*O.rep
  return(list(Y.rep1, Y.rep2, Y.rep3, Y.rep4, Y.rep5, f1,f2,f3,f4,f5, priors))
}

#Function to compute the MLE of Fenton-Wilkinson
loop.func2 <- function(iter, data, a, b, init){
  Y.rep <- data[[a]]
  f <- data[[b]]
  param <- matrix(0, nrow = iter, ncol = 4)
  colnames(param) <- c("muT", "muO", "sT", "sO")
  for(j in 1:iter){
    HW_dens.loop <- function(i,j = j, mu_T, mu_O, s_T, s_O){
      Y <- Y.rep[,j]
      Yi <- Y[i]
      lfi <- log(f[i])
      lfio <- log(1-f[i])
      mu_Tstar <- lfi + mu_T
      mu_Ostar <- lfio + mu_O
      sigma2_Y <- log((exp(2*mu_Tstar + s_T^2)*(exp(s_T^2)-1) + 
                         exp(2*mu_Ostar + s_O^2)*(exp(s_O^2)-1))/
                        ((exp(mu_Tstar + s_T^2/2) + exp(mu_Ostar + s_O^2/2))^2) 
                      + 1)
      mu_Y <- log(exp(mu_Tstar + s_T^2/2) + exp(mu_Ostar + s_O^2/2))
      - sigma2_Y/2
      return(dlnorm(Yi, mu_Y, sqrt(sigma2_Y)))
    }
    loglike2.loop <- function(pars){
      mu_T <- pars[1]
      mu_O <- pars[2]
      s_T <- pars[3]
      s_O <- pars[4]
      return(-sum(log(sapply(1:nrow(Y.rep), HW_dens.loop, j=j, mu_T = mu_T,
                             mu_O = mu_O, s_T = s_T, s_O = s_O))))
    }
    param[j,] <- optim(par=init, fn = loglike2.loop, method = "Nelder-Mead")$par
  }
  return(param)
}

#Function to compute the MLE of exact density
loop.func <- function(data,k, a, b, init){
  Y.rep <- data[[a]]
  f <- data[[b]]
  param <- matrix(0, nrow = k, ncol = 4)
  colnames(param) = c("muT", "muO", "sT", "sO")
  for(j in 1:k){
    Ydens.loop <- function(i,j = j, mu_T, mu_O, s_T, s_O){
      Y <- Y.rep[,j]
      yi <- Y[i]
      lfi <- log(f[i])
      lfio <- log(1-f[i])
      g <- function(t) dlnorm(t,lfi+mu_T, s_T)*dlnorm(yi-t,lfio 
                                                      + mu_O, s_O)
      return(integrate(g, lower = 0, upper = yi, rel.tol = 0.05)$value)
    }
    loglik.loop <- function(pars){
      mu_T <- pars[1]
      mu_O <- pars[2]
      s_T <- pars[3]
      s_O <- pars[4]
      return(-sum(log(sapply(1:nrow(Y.rep), Ydens.loop, j=j, mu_T = mu_T,
                             mu_O = mu_O, s_T = s_T, s_O = s_O))))
    }
    param[j,] <- optim(par = init, loglik.loop, method = "Nelder-Mead")$par
  }
  return(param)
}

#Function to compute the MAP with Fenton-Wilkinson
loop.func3 <- function(iter, data,a,b, init, prikn){
  Y.rep <- data[[a]]
  f <- data[[b]]
  param <- matrix(0, nrow = iter, ncol = 4)
  colnames(param) <- c("muT", "muO", "sT", "sO")
  for(j in 1:iter){ 
    FW_dens.prior.loop <- function(i,j = j,m.T, m.O, s.T, s.O){
      Yij <- Y.rep[i,j]
      lfi <- log(f[i])
      lfio <- log(1-f[i])
      mu_Tstar <- lfi + m.T
      mu_Ostar <- lfio + m.O
      sigma2_Y <- log((exp(2*mu_Tstar + s.T^2)*(exp(s.T^2) - 1) + 
                         exp(2*mu_Ostar +  s.O^2)*(exp(s.O^2)-1))/
                        ((exp(mu_Tstar + s.T^2/2) + exp(mu_Ostar + 
                                                          s.O^2/2)^2)) + 1)
      mu_Y <- log(exp(mu_Tstar + s.T^2/2) + exp(mu_Ostar + s.O^2/2)) 
      - sigma2_Y/2
      return(dlnorm(Yij, mu_Y, sigma2_Y))
    }
    pen.loglike.FW.loop <- function(pars){
      m.T <- pars[1]
      m.O <- pars[2]
      s.T <- pars[3]
      s.O <- pars[4]
      penalty <- log(dnorm(m.T, mean = 2 , sd = prikn[1])) +
        log(dnorm(m.O,  mean = 3, sd = prikn[2])) +
        log(dgamma(1/(s.T^2), shape = prikn[3], rate = prikn[4])) +
        log(dgamma(1/(s.O^2), shape = prikn[5], rate = prikn[6]))
      lik <- sapply(1:nrow(Y.rep), function(s) FW_dens.prior.loop(i = s, 
                                                                  j = j, m.T, m.O, s.T, s.O))
      return(-(sum(log(lik))) - penalty)
    }
    param[j,] <- optim(par = init, pen.loglike.FW.loop, method =
                         "Nelder-Mead")$par
  }
  return(param)
}

#Function to compute MAP estimates with exact density
loop.func4 <- function(iter, data,a,b, init){
  Y.rep <- data[[a]]
  f <- data[[b]]
  param <- matrix(0, nrow = iter, ncol = 4)
  colnames(param) <- c("muT", "muO", "sT", "sO")
  for(j in 1:iter){
    Ydens.loop <- function(i,j = j, mu_T, mu_O, s_T, s_O){
      Y <- Y.rep[,j]
      yi <- Y[i]
      lfi <- log(f[i])
      lfio <- log(1-f[i])
      g <- function(t) dlnorm(t,lfi+mu_T, s_T)*dlnorm(yi-t,lfio + 
                                                        mu_O, s_O)
      return(integrate(g, lower = 0, upper = yi, rel.tol = 0.05)$value)
    }
    loglik.loop <- function(pars){
      mu_T <- pars[1]
      mu_O <- pars[2]
      s_T <- pars[3]
      s_O <- pars[4]
      penalty <- log(dnorm(mu_T, mean = 2 , sd = 1)) + 
        log(dnorm(mu_O,  mean = 3, sd = 1)) + 
        log(dgamma(1/(s_T^2), shape = 1.5, rate = 1)) +
        log(dgamma(1/(s_O^2), shape = 1, rate = 1))
      lik <- sapply(1:nrow(Y.rep), function(s) Ydens.loop(i = s, j = j, 
                                                          mu_T, mu_O, s_T, s_O))
      return(-(sum(log(lik))) - penalty)
    }
    param[j,] <- optim(par = init, fn = loglik.loop, 
                       method = "Nelder-Mead")$par  
  }
  return(param)
}

#Function to generate data for correlation testing
gen.data.prior.corr <- function(n, p, alpha_T=1.5, beta_T =1, alpha_O=1,
                                beta_O=1, mu_0=2, sigma_0=1, mu_1=3, sigma_1=1,
                                r1, r2, seed){
  set.seed(seed)
  pre_T <- rgamma(p, alpha_T, beta_T) #precision of T
  pre_O <- rgamma(p, alpha_O, beta_O) #precision of O
  mu_T <- rnorm(p, mu_0, sigma_0)
  mu_O <- rnorm(p, mu_1, sigma_1)
  f1 <- runif(n, r1, r2)
  priors <- unname(cbind(mu_T, mu_O, pre_T, pre_O))
  T <- matrix(0, nrow = n, ncol = p)
  O <- matrix(0, nrow = n, ncol = p)
  Y <- matrix(0, nrow = n, ncol = p)
  for(j in 1:p){
    T[,j] <- rlnorm(n, meanlog = mu_T[j], sdlog = sqrt(1/pre_T[j]))
    O[,j] <- rlnorm(n, meanlog = mu_O[j], sdlog = sqrt(1/pre_O[j]))
    Y[,j] <- f1*T[,j] + (1-f1)*O[,j]
  }
  return(list(T, O, Y, f1, priors))
}

#Function to generate data for estimation of f_i in chapter 5
gen.data.f2 <- function(seed, n, p, r1, r2, mu, alpha){
  set.seed(seed)
  f <- runif(n, r1, r2)
  diff <- runif(p, 0, alpha)
  Tu <- matrix(0, nrow = n, ncol = p)
  O <- matrix(0, nrow = n, ncol = p)
  Y <- matrix(0, nrow = n, ncol = p)
  for(i in 1:p){
    Tu[,i] <- rlnorm(n, meanlog = mu + diff[i], sdlog = 1)
    O[,i] <- rlnorm(n, meanlog = mu - diff[i], sdlog = 1)
    Y[,i]<- f*Tu[,i] + (1-f)*O[,i]
  }
  return(list(Y, Tu, O, f))
}

#Function to estimate the f_i's
est.f.part2 <- function(data, a, est, n){
  Y.rep <- data[[a]]
  param <- est
  est.f <- numeric(n)
  for(i in 1:n){
    FW_dens <- function(i, j, f){
      Yij <- Y.rep[i,j]
      lfi <- log(f)
      lfio <- log(1-f)
      mu_Tstar <- lfi + param[j,1]
      mu_Ostar <- lfio + param[j,2]
      sigma2_Y <- log((exp(2*mu_Tstar + param[j,3]^2)*
                         (exp(param[j,3]^2)-1) + exp(2*mu_Ostar +
                                                       param[j,4]^2)*(exp(param[j,4]^2)-1))/
                        ((exp(mu_Tstar + param[j,3]^2/2) + 
                            exp(mu_Ostar + param[j,4]^2/2))^2) + 1)
      mu_Y <- log(exp(mu_Tstar + param[j,3]^2/2) + 
                    exp(mu_Ostar + param[j,4]^2/2)) - sigma2_Y/2
      return(dlnorm(Yij, mu_Y, sqrt(sigma2_Y)))
    }
    loglike.f <- function(pars){
      f <- pars[1]
      return(-sum(log(sapply(1:nrow(param), function(s) 
        FW_dens(i = i, j = s, f = f)))))
    }
    est.f[i] <- optim(0.5, loglike.f, method = "Brent", 
                      lower = 0, upper = 1)$par
  }
  return(est.f)
}