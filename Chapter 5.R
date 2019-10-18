library(sm)

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

set.seed(1234)
rgamma(1,1,1)
rgamma(1,1,1)
f2data <- gen.data.f2(seed = 1234, n = 250, p = 5, r1 = 0.3, r2 = 0.9, mu = 1)

#####
#p=5
fdata.5 <- gen.data.f2(seed = 1234, n = 250, p = 5, r1 = 0.3, r2 = 0.9, mu = 1, alpha = 1)
only5.FW <- est.f.part1(data = fdata.5, a = 1, b = 4, p = 5, init =c(2,2,2,2))
only5.NI <- est.f.part1.NI(data = fdata.5, a = 1, b = 4, p = 5, init =c(2,2,2,2))
fs.5.FW <- est.f.part2(data = fdata.5, a = 1, est = only5.FW, n = 250)
fs.5.NI <- est.f.part2(data = fdata.5, a = 1, est = only5.NI, n = 250)
diff1.FW <- abs(fs.5.FW - fdata.5[[4]])
diff1.NI <- abs(fs.5.NI - fdata.5[[4]])
sort(diff1, index.return = TRUE)


gene.expr1<- c(fdata.5[[2]][,1], fdata.5[[3]][,1])
gene.expr2<- c(fdata.5[[2]][,2], fdata.5[[3]][,2])
gene.expr3<- c(fdata.5[[2]][,3], fdata.5[[3]][,3])
gene.expr4<- c(fdata.5[[2]][,4], fdata.5[[3]][,4])
gene.expr5<- c(fdata.5[[2]][,5], fdata.5[[3]][,5])
dens <- c(rep("T", 250), rep("O", 250))
g1.dens <- as.data.frame(cbind(gene.expr1, dens))
g2.dens <- as.data.frame(cbind(gene.expr2, dens))
g3.dens <- as.data.frame(cbind(gene.expr3, dens))
g4.dens <- as.data.frame(cbind(gene.expr4, dens))
g5.dens <- as.data.frame(cbind(gene.expr5, dens))
g1.dens$gene.expr1 <- as.numeric(levels(g1.dens$gene.expr1))[g1.dens$gene.expr1]
g2.dens$gene.expr2 <- as.numeric(levels(g2.dens$gene.expr2))[g2.dens$gene.expr2]
g3.dens$gene.expr3 <- as.numeric(levels(g3.dens$gene.expr3))[g3.dens$gene.expr3]
g4.dens$gene.expr4 <- as.numeric(levels(g4.dens$gene.expr4))[g4.dens$gene.expr4]
g5.dens$gene.expr5 <- as.numeric(levels(g5.dens$gene.expr5))[g5.dens$gene.expr5]
diff1.FW <- as.data.frame(diff1.FW)
library(ggplot2)
gene1 <- ggplot(g1.dens, aes(x = gene.expr1, color = dens)) +
  geom_density() + scale_x_continuous(name = "expression value") +
  ggtitle("Expression profiles (Gene 1)") + theme(plot.title = element_text(size=9))
gene2 <- ggplot(g2.dens, aes(x = gene.expr2, color = dens)) +
  geom_density() + scale_x_continuous(name = "expression value") +
  ggtitle("Expression profiles (Gene 2)") + theme(plot.title = element_text(size=9))
gene3 <- ggplot(g3.dens, aes(x = gene.expr3, color = dens)) +
  geom_density() + scale_x_continuous(name = "expression value") +
  ggtitle("Expression profiles (Gene 3)") + theme(plot.title = element_text(size=9))
gene4 <- ggplot(g4.dens, aes(x = gene.expr4, color = dens)) +
  geom_density() + scale_x_continuous(name = "expression value") +
  ggtitle("Expression profiles (Gene 4)") + theme(plot.title = element_text(size=9))
gene5 <- ggplot(g5.dens, aes(x = gene.expr5, color = dens)) +
  geom_density() + scale_x_continuous(name = "expression value") +
  ggtitle("Expression profiles (Gene 5)") + theme(plot.title = element_text(size=9))

histoerror <- ggplot(diff1.FW, aes(x = diff1.FW)) + geom_histogram(color = "black", fill = "white") +
  scale_x_continuous(name = "error") + ggtitle("Histogram of errors") +
  theme(plot.title = element_text(size=9))

gene1.cum <- ggplot(g1.dens, aes(x = gene.expr1, color = dens)) + 
  stat_ecdf(geom = "step") + scale_x_continuous(name = "expression value") +
  ggtitle("ECDFs (Gene 1)") + theme(plot.title = element_text(size=9))
gene2.cum <- ggplot(g2.dens, aes(x = gene.expr1, color = dens)) + 
  stat_ecdf(geom = "step") + scale_x_continuous(name = "expression value") +
  ggtitle("ECDFs (Gene 2)") + theme(plot.title = element_text(size=9))
gene3.cum <- ggplot(g3.dens, aes(x = gene.expr1, color = dens)) + 
  stat_ecdf(geom = "step") + scale_x_continuous(name = "expression value") +
  ggtitle("ECDFs (Gene 3)") + theme(plot.title = element_text(size=9))
gene4.cum <- ggplot(g4.dens, aes(x = gene.expr1, color = dens)) + 
  stat_ecdf(geom = "step") + scale_x_continuous(name = "expression value") +
  ggtitle("ECDFs (Gene 4)") + theme(plot.title = element_text(size=9))
gene5.cum <- ggplot(g5.dens, aes(x = gene.expr1, color = dens)) + 
  stat_ecdf(geom = "step") + scale_x_continuous(name = "expression value") +
  ggtitle("ECDFs (Gene 5)") + theme(plot.title = element_text(size=9))
combi1 <- ggarrange(gene1, gene2, gene3, gene4, gene5, histoerror, nrow = 2, ncol = 3, common.legend = TRUE)
combi2 <- ggarrange(gene1.cum, gene2.cum, gene3.cum, gene4.cum, gene5.cum, nrow = 2, ncol = 3, common.legend = TRUE)

colnames(g1.dens) <- c("T_dist", "O_dist")
colnames(g2.dens) <- c("T_dist", "O_dist")
colnames(g3.dens) <- c("T_dist", "O_dist")
colnames(g4.dens) <- c("T_dist", "O_dist")
colnames(g5.dens) <- c("T_dist", "O_dist")

par(mfrow=c(2,3))
plot(density(fdata.10[[3]][,1]), main = "Plot of gene 1 profiles")
points(density(fdata.10[[2]][,1]), type = 'l', col = 'red')

plot(density(fdata.10[[3]][,2]), main = "Plot of gene 2 profiles")
points(density(fdata.10[[2]][,2]), type = 'l', col = 'red')

plot(density(fdata.10[[3]][,3]), main = "Plot of gene 3 profiles")
points(density(fdata.10[[2]][,3]), type = 'l', col = 'red')

plot(density(fdata.10[[3]][,4]), main = "Plot of gene 4 profiles")
points(density(fdata.10[[2]][,4]), type = 'l', col = 'red')

plot(density(fdata.10[[3]][,5]), main = "Plot of gene 5 profiles")
points(density(fdata.10[[2]][,5]), type = 'l', col = 'red')

hist(diff1, breaks = 50, main = "Histogram of errors of f_i")

par(mfrow=c(2,3))
plot(ecdf(f2data[[3]][,1]), main = "ECD of gene 1")
lines(ecdf(f2data[[2]][,1]), col = "red")

plot(ecdf(f2data[[3]][,2]), main = "ECD of gene 2")
lines(ecdf(f2data[[2]][,2]), col = "red")

plot(ecdf(f2data[[3]][,3]), main = "ECD of gene 3")
lines(ecdf(f2data[[2]][,3]), col = "red")

plot(ecdf(f2data[[3]][,4]), main = "ECD of gene 4")
lines(ecdf(f2data[[2]][,4]), col = "red")

plot(ecdf(f2data[[3]][,5]), main = "ECD of gene 5")
lines(ecdf(f2data[[2]][,5]), col = "red")
ks.test(fdata.5[[3]][,1], fdata.5[[2]][,1])
ks.test(fdata.5[[3]][,2], fdata.5[[2]][,2])
ks.test(fdata.5[[3]][,3], fdata.5[[2]][,3])
ks.test(fdata.5[[3]][,4], fdata.5[[2]][,4])
ks.test(fdata.5[[3]][,5], fdata.5[[2]][,5])
#p=25
fdata.25 <- gen.data.f2(seed = 1234, n = 250, p = 25, r1 = 0.3, r2 = 0.9, mu = 1)
only25.FW <- est.f.part1(data = fdata.25, a = 1, b = 4, p = 25, init = c(2,2,2,2))
only25.NI <- est.f.part1.NI(data = fdata.25, a = 1, b = 4, p = 25, init =c(2,2,2,2))
only25.penFW <- est.f.part1.penFW(data = fdata.25, a = 1, b = 4, p = 25, init = c(2,2,2,2))
fs.25.FW <- est.f.part2(data = fdata.25, a = 1, est = only25.FW, n = 250)
fs.25.NI <- est.f.part2(data = fdata.25, a = 1, est = only25.NI, n = 250)
fs.25.penFW <- est.f.part2(data = fdata.25, a = 1, est = only25.penFW, n = 250)
diff2.FW <- abs(fs.25.FW - fdata.20[[4]])
diff2.NI <- abs(fs.25.NI - fdata.20[[4]])
diff2.penFW <- abs(fs.25.penFW - fdata.20[[4]])
sort(diff2, index.return = TRUE)

dist25 <- numeric(25)
sig <- numeric(25)
for(i in 1:25){
  dist25[i] <- ks.test(fdata.25[[2]][,i], fdata.25[[3]][,i])$statistic
  sig[i] <- ifelse(dist25[i] > 0.3, "sig", "not sig")
}

#p=50
fdata.50 <- gen.data.f2(seed = 1234, n = 250, p = 50, r1 = 0.3, r2 = 0.9, mu = 1)
only50 <- est.f.part1(data = fdata.50, a = 1, b=4, p = 50, init =c(2,2,2,2))
only50.NI <- est.f.part1.NI(data = fdata.50, a = 1, b = 4, p = 50, init=c(2,2,1,1))
only50.penFW <- est.f.part1.penFW(data = fdata.50, a = 1, b = 4, p = 50, init =c(2,2,2,2))
fs.50.FW <- est.f.part2(data = fdata.50, a = 1, est = only50, n = 250)
fs.50.NI <- est.f.part2(data = fdata.50, a = 1, est = only50.NI, n = 250)
fs.50.penFW <- est.f.part2(data = fdata.50, a = 1, est = only50.penFW, n = 250)
diff3.FW <- abs(fs.50.FW - fdata.50[[4]])
diff3.NI <- abs(fs.50.NI - fdata.50[[4]])
diff3.penFW <- abs(fs.50.penFW - fdata.50[[4]])
sort(diff3, index.return = TRUE)

dist50 <- numeric(50)
sig2 <- numeric(50)
for(i in 1:50){
  dist50[i] <- ks.test(fdata.50[[2]][,i], fdata.50[[3]][,i])$statistic
  sig2[i] <- ifelse(dist50[i] > 0.3, "sig", "not sig")
}

a <- which(sig2 == "sig")

hist(diff3, breaks = 50)
#p = 75
fdata.75 <- gen.data.f2(seed = 1234, n = 250, p = 75, r1 = 0.3, r2 = 0.9, mu = 1)
only75.FW <- est.f.part1(data = fdata.75, a = 1, b = 4, p = 75, init =c(2,2,2,2))
only75.NI <- est.f.part1.NI(data = fdata.75, a = 1, b = 4, p = 75, init =c(2,2,1,1))
only75.penFW <- est.f.part1.penFW(data = fdata.75, a = 1, b = 4, p = 75, init =c(2,2,2,2))
fs.75.FW <- est.f.part2(data = fdata.75, a = 1, est = only75.FW, n = 250)
fs.75.NI <- est.f.part2(data = fdata.75, a = 1, est = only75.NI, n = 250)
fs.75.penFW <- est.f.part2(data = fdata.75, a = 1, est = only75.penFW, n = 250)
diff4.FW <- abs(fs.75.FW - fdata.75[[4]])
diff4.NI <- abs(fs.75.NI - fdata.75[[4]])
diff4.penFW <- abs(fs.75.penFW - fdata.75[[4]])
sort(diff4, index.return = TRUE)

dist75 <- numeric(75)
sig3 <- numeric(75)
for(i in 1:75){
  dist75[i] <- ks.test(fdata.75[[2]][,i], fdata.75[[3]][,i])$statistic
  sig3[i] <- ifelse(dist75[i] > 0.3, "sig", "not sig")
}
#p = 100
fdata.100 <- gen.data.f2(seed = 1234, n = 250, p = 100, r1 = 0.3, r2 = 0.9, mu = 1)
only100.FW <- est.f.part1(data = fdata.100, a = 1, b = 4, p = 100, init =c(2,2,2,2))
only100.NI <- est.f.part1.NI(data = fdata.100, a = 1, b = 4, p = 100, init =c(2,2,1,1))
only100.penFW <- est.f.part1.penFW(data = fdata.100, a = 1, b = 4, p = 100, init =c(2,2,2,2))
fs.100.FW <- est.f.part2(data = fdata.100, a = 1, est = only100.FW, n = 250)
fs.100.NI <- est.f.part2(data = fdata.100, a = 1, est = only100.NI, n = 250)
fs.100.penFW <- est.f.part2(data = fdata.100, a = 1, est = only100.penFW, n = 250)
diff5.FW <- abs(fs.100.FW - fdata.100[[4]])
diff5.NI <- abs(fs.100.NI - fdata.100[[4]]) 
diff5.penFW <- abs(fs.100.penFW - fdata.100[[4]])
dist100 <- numeric(100)
sig4 <- numeric(100)
for(i in 1:100){
  dist100[i] <- ks.test(fdata.100[[2]][,i], fdata.100[[3]][,i])$statistic
  sig4[i] <- ifelse(dist100[i] >0.3, "sig", "not sig")
}
mean(diff4)
#####
as <- est.f.part1(data = f2data, a = 1, b = 4, p = 50)
as2 <- est.f.part1.NI(data = f2data, a = 1, b = 4, p = 5)

f.as <- est.f.part2(data = f2data, a = 1, est = as2, n = 200)
f.as2 <- est.f.part2(data = f2data, a = 1, est = as2, n = 100)

f2.as <- est.f.part2.NI(data = f2data, a=1, est=as, n=100)
diff <- abs(f.as2 - f2data[[4]])
sort(diff, index.return = TRUE)
lar1T <- density(f2data[[2]][65,])$y
lar1O <- density(f2data[[3]][65,])$y
distance(rbind(lar1O, lar1T), method = "euclidean")

smal1T <- density(f2data[[2]][7,])$y
smal1O <- density(f2data[[3]][7,])$y
distance(rbind(smal1T, smal1O), method = "euclidean")
##largest diff is 65, 13 71, and smallest diff is 7, 16, 63

par(mfrow =c(1,2))
plot(density(f2data[[2]][188, ]))
points(density(f2data[[3]][188,]), type = "l", col = "red")
plot(density(f2data[[2]][120,]))
points(density(f2data[[3]][120,]), type = "l", col = "red")
plot(density(f2data[[2]][71,]))
points(density(f2data[[3]][71,]), type = "l", col = "red")
plot(density(f2data[[2]][63,]))
points(density(f2data[[3]][63,]), type = "l", col = "red")
plot(density(f2data[[2]][16, ]))
points(density(f2data[[3]][16,]), type = "l", col = "red")
plot(density(f2data[[2]][7, ]))
points(density(f2data[[3]][7,]), type = "l", col = "red")
diff2 <- abs(f.as2 - f2data[[4]])

diff
est.f.part2 <- function(data, a, est, n){
  Y.rep <- data[[a]]
  param <- est
  est.f <- numeric(n)
  for(i in 1:n){
    FW_dens <- function(i, j, f){
      Yij <- Y.rep[i,j]
      lfi <- log(f)
      lfio <- log(1-f)
      mu_Tstar <- lfi + param[j, 1]
      mu_Ostar <- lfio + param[j,2]
      sigma2_Y <- log((exp(2*mu_Tstar + param[j,3]^2)*(exp(param[j,3]^2) - 1) + 
                         exp(2*mu_Ostar +  param[j,4]^2))*(exp(param[j,4]^2)-1))/(((exp(mu_Tstar + param[j,3]^2/2)) + 
                                                                                     exp(mu_Ostar + param[j,4]^2/2))^2) + 1)
      mu_Y <- log(exp(mu_Tstar + 1/(2*param[j,3])) + exp(mu_Ostar + 1/(2*param[j,4]))) - sigma2_Y/2
      return(dlnorm(Yij, mu_Y, sqrt(sigma2_Y)))
    }
    loglike.f <- function(pars){
      f <- pars[1]
      return(-sum(log(sapply(1:nrow(param), function(s) FW_dens(i = i, j = s, f = f)))))
    }
    est.f[i] <- optim(0.5, loglike.f, method = "Brent", lower = 0, upper = 1)$par
  }
  return(est.f)
}

fs1 <- est.f.part2(data = f.data, a = 1, est = e1, n = 250)

###
param[122,] <- data[[3]][122,]
param[139,] <- data[[3]][139,]

f.opt <- numeric(500)
f.opt2 <- numeric(500)

for(k in 1:500){
  FW_dens.prior.loop <- function(i = k, j, f){
    Yij <- Y.rep1[i,j]
    lfi <- log(f)
    lfio <- log(1-f)
    mu_Tstar <- lfi + param[j, 1]
    mu_Ostar <- lfio + param[j,2]
    sigma2_Y <- log((exp(2*mu_Tstar + (1/param[j,3]))*(exp(1/param[j,3]) - 1) + exp(2*mu_Ostar +  (1/param[j,4]))*(exp(1/param[j,4])-1))/((exp(mu_Tstar + 1/(2*param[j,3])) + exp(mu_Ostar + 1/(2*param[j,4])))^2) + 1)
    mu_Y <- log(exp(mu_Tstar + 1/(2*param[j,3])) + exp(mu_Ostar + 1/(2*param[j,4]))) - sigma2_Y/2
    return(dlnorm(Yij, mu_Y, sqrt(sigma2_Y)))
  }
  loglike.f.loop <- function(pars){
    f <- pars[1]
    return(-sum(log(sapply(1:nrow(param), function(j) FW_dens.prior.loop(i = k, j = j, f = f)))))
  }
  f.opt[k] <- optim(0.5, loglike.f.loop, method = "Brent", lower = 0, upper = 1)$par
}

for(k in 1:500){
  FW_dens.prior.loop <- function(i = k, j, f){
    Yij <- Y.rep2[i,j]
    lfi <- log(f)
    lfio <- log(1-f)
    mu_Tstar <- lfi + param2[j, 1]
    mu_Ostar <- lfio + param2[j,2]
    sigma2_Y <- log((exp(2*mu_Tstar + (1/param2[j,3]))*(exp(1/param2[j,3]) - 1) + exp(2*mu_Ostar +  (1/param2[j,4]))*(exp(1/param2[j,4])-1))/((exp(mu_Tstar + 1/(2*param2[j,3])) + exp(mu_Ostar + 1/(2*param2[j,4])))^2) + 1)
    mu_Y <- log(exp(mu_Tstar + 1/(2*param2[j,3])) + exp(mu_Ostar + 1/(2*param2[j,4]))) - sigma2_Y/2
    return(dlnorm(Yij, mu_Y, sqrt(sigma2_Y)))
  }
  loglike.f.loop <- function(pars){
    f <- pars[1]
    return(-sum(log(sapply(1:nrow(param2), function(j) FW_dens.prior.loop(i = k, j = j, f = f)))))
  }
  f.opt2[k] <- optim(0.5, loglike.f.loop, method = "Brent", lower = 0, upper = 1)$par
}

f.opt.pen <- numeric(500)

for(k in 1:500){
  FW_dens.prior.loop <- function(i = k, j, f){
    Yij <- Y.rep1[i,j]
    lfi <- log(f)
    lfio <- log(1-f)
    mu_Tstar <- lfi + param[j, 1]
    mu_Ostar <- lfio + param[j,2]
    sigma2_Y <- log((exp(2*mu_Tstar + (1/param[j,3]))*(exp(1/param[j,3]) - 1) + exp(2*mu_Ostar +  (1/param[j,4]))*(exp(1/param[j,4])-1))/((exp(mu_Tstar + 1/(2*param[j,3])) + exp(mu_Ostar + 1/(2*param[j,4])))^2) + 1)
    mu_Y <- log(exp(mu_Tstar + 1/(2*param[j,3])) + exp(mu_Ostar + 1/(2*param[j,4]))) - sigma2_Y/2
    return(dlnorm(Yij, mu_Y, sqrt(sigma2_Y)))
  }
  loglike.f.loop <- function(pars){
    f <- pars[1]
    penalty <- log(dunif(f, 0.3, 0.9))
    lik <- sapply(1:nrow(param), function(j) FW_dens.prior.loop(i = k, j = j, f = f))
    return(-(sum(log(lik))) - penalty)
  }
  f.opt.pen[k] <- optim(0.5, loglike.f.loop, method = "Brent", lower = 0, upper = 1)$par
}


g.name <- c("gene1", "gene2", "gene3", "gene4", "gene5", "gene6", "gene7", "gene8", 
"gene9","gene10", "gene11", "gene12","gene13", "gene14", "gene15","gene16", "gene17", 
"gene18", "gene19", "gene20", "gene21","gene22", "gene23", "gene24", "gene25")

diff <- f.opt - f.25
f.25 <- f[1:25]
f.data <- as.data.frame(cbind(g.name, f.opt, diff))
f.data$g.name <- as.factor(f.data$g.name)
f.data$diff <- as.numeric(f.data$diff)
f.data$type <- as.factor(ifelse(f.data$diff > 0, "Over", "Under"))

gen <- function(n, p){
  f <- runif(n, 0.3, 0.9)
  T <- replicate(p, rlnorm(n, meanlog = 1, sdlog = 1.5))
  O <- replicate(p, rlnorm(n, meanlog = 3, sdlog = 0.4))
  Y <- matrix(0, nrow = n, ncol = p)
  for(i in 1:p){
    Y[,i] <- f*T[,i] + (1-f)*O[,i]
  }
  return(list(Y,T,O,f))
}

gf.20 <- gen(n = 250, p = 20)
gf.100 <- gen(n = 250, p = 100)
gf.250 <- gen(n = 250, p = 250)

est.20 <- est.f.part1(data = gf.20, a = 1, b = 4, p = 20)
est.100 <- est.f.part1(data = gf.100, a = 1, b = 4, p = 100)
est.250 <- est.f.part1(data = gf.250, a = 1, b = 4, p = 250)

f20 <- est.f.part2(data = gf.20, a = 1, est = est.20, n = 250)
f100 <- est.f.part2(data = gf.100, a = 1, est = est.100, n = 250)
f250 <- est.f.part2(data = gf.250, a = 1, est = est.250, n =250)

dif1 <- abs(f20-gf.20[[4]])
dif2 <- abs(f100-gf.100[[4]])
dif3 <- abs(f250-gf.250[[4]])

plot(density(gf.20[[2]][1:200,]), xlim = c(0,200))
points(density(gf.20[[3]][1:100,]), type = "l", col = "red")

plot(density(gf.250[[2]][1:200,]), xlim = c(0,200))
points(density(gf.250[[3]][1:200,]), type = "l", col = "red")

####
dataframe <- as.data.frame(cbind(diff2.FW, diff3.FW, diff4.FW, diff5.FW))
hist1 <- ggplot(dataframe, aes(x = diff2.FW)) + 
  geom_histogram(color = "black", fill = "red") + 
  ggtitle("nr of genes = 25") + scale_x_continuous(name = "error", limits = c(0, 0.7))
hist2 <- ggplot(dataframe, aes(x = diff3.FW)) +
  geom_histogram(color = "black", fill = "red") +
  ggtitle("nr of genes = 50") + scale_x_continuous(name = "error", limits = c(0, 0.7))
hist3 <- ggplot(dataframe, aes(x = diff4.FW)) + 
  geom_histogram(color = "black", fill = "red") + 
  ggtitle("nr of genes = 75") + scale_x_continuous(name = "error", limits = c(0, 0.7))
hist4 <- ggplot(dataframe, aes(x = diff5.FW)) +
  geom_histogram(color = "black", fill = "red") + 
  ggtitle("nr of genes = 100") + scale_x_continuous(name = "error", limits = c(0, 0.7))

histo <- ggarrange(hist1, hist2, hist3, hist4, nrow = 2, ncol = 2)

type1 <- c(rep("FW", 250), rep("exact", 250), rep("penFW", 250))
data25 <- cbind(type1, c(diff2.FW, diff2.NI, diff2.penFW))

nrofg <- c(rep(25,250), rep(50,250), rep(75,250), rep(100,250))
FW <- c(diff2.FW, diff3.FW, diff4.FW, diff5.FW)
NI <- c(diff2.NI, diff3.NI, diff4.NI, diff5.NI)
penFW <- c(diff2.penFW, diff3.penFW, diff4.penFW, diff5.penFW)
data.FW <- as.data.frame(cbind(FW, nrofg))
data.NI <- as.data.frame(cbind(NI, nrofg))
data.penFW <- as.data.frame(cbind(penFW, nrofg))
SE.FW <- summarySE(data = data.FW, measurevar = "FW", groupvars = "nrofg")
SE.penFW <- summarySE(data = data.penFW, measurevar = "penFW", groupvars = "nrofg")
SE.NI <- summarySE(data = data.NI, measurevar = "NI", groupvars = "nrofg")

hist1.all <- ggplot(data25, aes(x = V2, fill = type1, color = type1)) +
  geom_histogram(position = "identity")

aaa <- ggplot(SE.FW, aes(x = nrofg, y = FW)) +
  geom_errorbar(aes(ymin = FW - se, ymax = FW + se), width = 0.1) +
  geom_line(color = "black") + geom_point() +
  scale_x_discrete(name = "number of genes") + scale_y_continuous(name = "avg error")

bbb <- ggplot(SE.NI, aes(x = nrofg, y = NI)) +
  geom_errorbar(aes(ymin = NI - se, ymax = NI + se), width = 0.1) +
  geom_line(color = "black") + geom_point() + 
  scale_x_discrete(name = "number of genes") + scale_y_continuous(name = "avg error")

ccc<- ggplot(SE.penFW, aes(x = nrofg, y = penFW)) +
  geom_errorbar(aes(ymin = penFW - se, ymax = penFW + se), width = 0.1) +
  geom_line(color = "black") + geom_point() + 
  scale_x_discrete(name = "number of genes") + scale_y_continuous(name = "avg error")

abc <- ggarrange(aaa, bbb,ccc, nrow = 1, ncol = 3)

dataframe.2 <- asas.data.frame(cbind(diff2.FW, diff3.FW, diff4.FW, diff5.FW))

fdata.43 <- gen.data.f2(seed = 1234, n = 250, p = 43, r1 = 0.3, 
                        r2 = 0.9, mu = 1, alpha = 0.01)
fdata.68 <- gen.data.f2(seed = 1234, n = 250, p = 68, r1 = 0.3, 
                        r2 = 0.9, mu = 1, alpha = 0.01)
fdata.118 <- gen.data.f2(seed = 1234, n = 250, p = 118, r1 = 0.3, 
                         r2 = 0.9, mu = 1, alpha = 0.01)
fdata.168 <- gen.data.f2(seed = 1234, n = 250, p = 168, r1 = 0.3, 
            r2 = 0.9, mu = 1, alpha = 0.01)
data75.c <- list(cbind(fdata.43[[1]], fdata.50[[1]][,a]), fdata.43[[4]])
data100.c <- list(cbind(fdata.68[[1]], fdata.50[[1]][,a]), fdata.68[[4]])
data150.c <- list(cbind(fdata.118[[1]], fdata.50[[1]][,a]), fdata.118[[4]])
data200.c <- list(cbind(fdata.168[[1]], fdata.50[[1]][,a]), fdata.168[[4]])
FW75.c <- est.f.part1(data = data75.c, a = 1, b = 2 , p = 75, init = c(2,2,2,2))
FW100.c <- est.f.part1(data = data100.c, a = 1, b = 2, p = 100, init = c(2,2,2,2))
FW150.c <- est.f.part1(data = data150.c, a = 1, b = 2, p = 150, init = c(2,2,2,2))
FW200.c <- est.f.part1(data = data200.c, a = 1, b = 2, p = 200, init = c(2,2,2,2))
f75.c <- est.f.part2(data = data75.c, a = 1 , est = FW75.c, n = 250)
f100.c <- est.f.part2(data = data100.c, a = 1, est = FW100.c, n = 250)
f150.c <- est.f.part2(data = data150.c, a = 1, est = FW150.c, n = 250)
f200.c <- est.f.part2(data = data200.c, a = 1, est = FW200.c, n = 250)
diff75.c <- abs(f75.c - data75.c[[2]])
diff100.c <- abs(f100.c - data100.c[[2]])
diff150.c <- abs(f150.c - data150.c[[2]])
diff200.c <- abs(f200.c - data200.c[[2]])

nrofgen <- c(rep(100, 250), rep(150, 250), rep(200, 250))
e <- c(diff100.c, diff150.c, diff200.c)
new <- as.data.frame(cbind(e, nrofgen))

SE <- summarySE(data = new, measurevar = "e", groupvars = "nrofgen")
dataframe2 <- as.data.frame(cbind(diff100.c, diff150.c, diff200.c))

c1 <- ggplot(dataframe2, aes(x = diff100.c)) + 
  geom_histogram(color = "black", fill = "white") +
  ggtitle("nr of genes = 100") + scale_x_continuous(name = "error")
c2 <- ggplot(dataframe2, aes(x = diff150.c)) + 
  geom_histogram(color = "black", fill = "white") +
  ggtitle("nr of genes = 150") + scale_x_continuous(name = "error")
c3 <- ggplot(dataframe2, aes(x = diff100.c)) + 
  geom_histogram(color = "black", fill = "white") +
  ggtitle("nr of genes = 200") + scale_x_continuous(name = "error")

asd <- ggplot(SE, aes(x = nrofgen, y = e)) +
  geom_errorbar(aes(ymin = e - se, ymax = e + se), width = 0.1) +
  geom_line() + geom_point() +
  scale_x_discrete(name = "number of genes") + scale_y_continuous(name = "avg error")

combination <- ggarrange(c1, c2, c3, asd, nrow = 2, ncol = 2)
