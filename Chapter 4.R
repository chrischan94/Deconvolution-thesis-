g4 <- gen.data.prior(n = 500, p = 1000, g = 4, iter = 50, seed = 1234)
g11 <- gen.data.prior(n = 500, p = 1000, g = 11, iter = 50, seed = 1234)
g66 <- gen.data.prior(n = 500, p = 1000, g = 66, iter = 50, seed = 1234)
g3 <- gen.data.prior(n = 500, p = 1000, g = 3, iter = 50, seed = 1234)

scen.1 <- read.csv("scenario1.r.csv", header = T)
scen.2 <- read.csv("scenario2.r.csv", header = T)
scen.3 <- read.csv("scenario3.r.csv", header = T)
scen.4 <- read.csv("scenario4.r.csv", header = T)
scen.1n <- read.csv("scenario1.n.csv", header = T)
scen.2n <- read.csv("scenario2.n.csv", header = T)
scen.3n <- read.csv("scenario3.n.csv", header = T)
scen.4n <- read.csv("scenario4.n.csv", header = T)

set.seed(1234)
a1 <- sample(1:500, 300)
a2 <- sample(1:500, 100)
g4.300 <- list(g4[[1]][a1,], g4[[6]][a1], g4[[7]])
g3.300 <- list(g3[[1]][a1,], g3[[6]][a1], g3[[7]])
g11.300 <- list(g11[[1]][a1,], g11[[6]][a1], g11[[7]])
g66.300 <- list(g66[[1]][a1,], g66[[6]][a1], g66[[7]])

g4.100 <- list(g4[[1]][a2,], g4[[6]][a2], g4[[7]])
g3.100 <- list(g3[[1]][a2,], g3[[6]][a2], g3[[7]])
g11.100 <- list(g11[[1]][a2,], g11[[6]][a2], g11[[7]])
g66.100 <- list(g66[[1]][a2,], g66[[6]][a2], g66[[7]])

penFW1_0.8 <- loop.func3(iter = 50, data = g4, a = 2, b = 7, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
penFW1_1.0 <- loop.func3(iter = 50, data = g4, a = 1, b = 6, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
penFW1_0.6 <- loop.func3(iter = 50, data = g4, a = 3, b = 8, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
penFW1_0.4 <- loop.func3(iter = 50, data = g4, a = 4, b = 9, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
penFW1_0.2 <- loop.func3(iter = 50, data = g4, a = 5, b = 10, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
FW1_0.8 <- loop.func2(iter = 50, data = g4, a = 2, b = 7, init =c(2,2,2,2))
FW1_1.0 <- loop.func2(iter = 50, data = g4, a = 1, b = 6, init = c(2,2,2,2))
FW1_0.6 <- loop.func2(iter = 50, data = g4, a = 3, b =8, init = c(2,2,2,2))
FW1_0.4 <- loop.func2(iter = 50, data = g4, a = 4, b =9, init = c(2,2,2,2))
FW1_0.2 <- loop.func2(iter = 50, data = g4, a = 5, b =10, init = c(2,2,2,2))
NI1_0.8 <- loop.func(data = g4, k = 50, a = 2, b = 7, init =c(2,2,2,2))
NI1_1.0 <- loop.func(data = g4, k = 50, a = 1, b = 6, init =c(2,2,2,2))
NI1_0.6 <- loop.func(data = g4, k = 50, a = 3, b = 8, init =c(2,2,2,2))
NI1_0.4 <- loop.func(data = g4, k = 50, a = 4, b = 9, init = c(2,2,2,2))
NI1_0.2 <- loop.func(data = g4, k = 50, a = 5, b = 10, init = c(2,2,2,2))
penNI1_0.8 <- loop.func4(iter = 50, data = g4, a = 2, b = 7, init =c(2,2,2,2))#
penNI1_1.0 <- loop.func4(iter = 50, data = g4, a = 1, b = 6, init =c(2,2,2,2))
penNI1_0.6 <- loop.func4(iter = 50, data = g4, a = 3, b = 8, init =c(2,2,2,2))
penNI1_0.4 <- loop.func4(iter = 50, data = g4, a = 4, b = 9, init =c(2,2,2,2))
penNI1_0.2 <- loop.func4(iter = 50, data = g4, a = 5, b = 10, init =c(2,2,2,2))

#penFW3_0.8 <- loop.func3(iter = 50, data = g3, a = 2, b = 5, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW3_1.0 <- loop.func3(iter = 50, data = g3, a = 1, b = 4, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW3_0.6 <- loop.func3(iter = 50, data = g3, a = 3, b = 6, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW3_0.4 <-  loop.func3(iter = 50, data = g3, a = 4, b = 9, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW3_0.2 <-  loop.func3(iter = 50, data = g3, a = 5, b = 10, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#FW3_0.8 <- loop.func2(iter = 50, data = g3, a = 2, b = 5, init = c(2,2,2,2))
#FW3_1.0 <- loop.func2(iter = 50, data = g3, a = 1, b = 4, init = c(2,2,2,2))
#FW3_0.6 <- loop.func2(iter = 50, data = g3, a = 3, b = 6, init = c(2,2,2,2))
#FW3_0.4 <-  loop.func2(iter = 50, data = g3, a = 4, b = 9, init = c(2,2,2,2))
#FW3_0.2 <-  loop.func2(iter = 50, data = g3, a = 5, b = 10, init = c(2,2,2,2))
#NI3_0.6 <- loop.func(data = g3, k = 50, a = 1, b = 4, init = c(2,2,2,2))
#NI3_0.8 <- loop.func(data = g3, k = 50, a = 2, b = 5, init =c(2,2,2,2))
#NI3_1.0 <- loop.func(data = g3, k = 50, a = 3, b = 6, init =c(2,2,2,2))
#NI3_0.4 <-  loop.func(data = g3, k = 50, a = 4, b = 9, init = c(2,2,2,2))
#NI3_0.2 <-  loop.func(data = g3, k = 50, a = 5, b = 10, init = c(2,2,2,2))
#penNI3_1.0 <- loop.func4(iter = 50, data = g3, a = 1, b = 6, init = c(2,2,2,2))
#penNI3_0.8 <- loop.func4(iter = 50, data = g3, a = 2, b = 7, init = c(2,2,2,2))
#penNI3_0.6 <- loop.func4(iter = 50, data = g3, a = 3, b = 8, init = c(2,2,2,2))
#penNI3_0.4 <- loop.func4(iter = 50, data = g3, a = 4, b = 9, init = c(2,2,2,2))
#penNI3_0.2 <- loop.func4(iter = 50, data = g3, a = 5, b = 10, init = c(2,2,2,2))

#penFW11_0.8 <- loop.func3(iter = 50, data = g11, a= 2, b = 5, init =c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW11_1.0 <- loop.func3(iter = 50, data = g11, a = 1, b = 4, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW11_0.6 <- loop.func3(iter = 50, data = g11, a = 3, b = 8, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW11_0.4 <- loop.func3(iter = 50, data = g11, a = 4, b = 9, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW11_0.2 <- loop.func3(iter = 50, data = g11, a = 5, b = 10, init = c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#FW11_0.8 <- loop.func2(iter = 50, data = g11, a = 2, b = 5, init =c(2,2,2,2))
#FW11_1.0 <- loop.func2(iter = 50, data = g11, a = 1, b = 4, init = c(2,2,2,2))
#FW11_0.6 <- loop.func2(iter = 50, data = g11, a = 3, b = 8, init =c(2,2,2,2))
#FW11_0.4<- loop.func2(iter = 50, data = g11, a = 4, b = 9, init =c(2,2,2,2))
#FW11_0.2 <- loop.func2(iter = 50, data = g11, a = 5, b = 10, init =c(2,2,2,2))
#NI11_0.8 <- loop.func(data = g11, k = 50, a = 2, b = 5, init =c(2,2,2,2))
#NI11_1.0 <- loop.func(data = g11, k = 50, a = 1, b = 4, init =c(2,2,2,2))
#NI11_0.6 <- loop.func(data = g11, k = 50, a = 3, b = 8, init = c(2,2,2,2))
#NI11_0.4 <- loop.func(data = g11, k = 50, a = 4, b = 9, init = c(2,2,2,2))
#NI11_0.2 <- loop.func(data = g11, k = 50, a = 5, b = 10, init = c(2,2,1,1))
#penNI11_1.0 <- loop.func4(iter = 50, data = g11, a = 1, b = 6, init = c(2,2,2,2))
#penNI11_0.8 <- loop.func4(iter = 50, data = g11, a = 2, b = 7, init = c(2,2,2,2))
#penNI11_0.6 <- loop.func4(iter = 50, data = g11, a = 3, b = 8, init = c(2,2,2,2))
#penNI11_0.4 <- loop.func4(iter = 50, data = g11, a = 4, b = 9, init = c(2,2,2,2))
#penNI11_0.2 <- loop.func4(iter = 50, data = g11, a = 5, b = 10, init = c(2,2,2,2))

#penFW66_0.8 <- loop.func3(iter = 50, data = g66, a= 2, b = 5, init =c(3,3,1,1), prikn = c(1,1,1.5,1,1,1))
#penFW66_1.0 <- loop.func3(iter = 50, data = g66, a= 1, b = 4, init =c(3,3,1,1), prikn = c(1,1,1.5,1,1,1))
#penFW66_0.6 <- loop.func3(iter = 50, data = g66, a = 3, b = 8, init =c(3,3,1,1), prikn = c(1,1,1.5,1,1,1))
#penFW66_0.4 <- loop.func3(iter = 50, data = g66, a = 4, b = 9, init =c(3,3,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW66_0.2 <- loop.func3(iter = 50, data = g66, a = 5, b = 10, init =c(3,3,1,1), prikn = c(1,1,1.5,1,1,1))
#FW66_0.8 <- loop.func2(iter = 50, data = g66, a = 2, b = 5, init =c(3,3,2,2))
#FW66_1.0 <- loop.func2(iter = 50, data = g66, a = 1, b = 4, init = c(3,3,2,2))
#FW66_0.6 <- loop.func2(iter = 50, data = g66, a = 3, b = 8, init =c(3,3,2,2))
#FW66_0.4 <- loop.func2(iter = 50, data = g66, a = 4, b = 9, init =c(3,3,2,2))
#FW66_0.2 <- loop.func2(iter = 50, data = g66, a = 5, b = 10, init =c(3,3,2,2))
#NI66_0.8 <- loop.func(data = g66, k = 50, a = 2, b = 5, init =c(3,3,1,1))
#NI66_1.0 <- loop.func(data = g66, k = 50, a = 1, b = 4, init =c(3,3,1,1))
#NI66_0.6 <- loop.func(data = g66, k = 50, a = 3, b = 8, init = c(2,2,2,2))
#NI66_0.4 <- loop.func(data = g66, k = 50, a = 4, b = 9, init = c(2,2,2,2))
#NI66_0.2 <- loop.func(data = g66, k = 50, a = 5, b = 10, init = c(2,2,2,2))
#penNI66_1.0 <- loop.func4(iter = 50, data = g66, a = 1, b = 6, init = c(2,2,2,2))
#penNI66_0.8 <- loop.func4(iter = 50, data = g66, a = 2, b = 7, init = c(2,2,2,2))
#penNI66_0.6 <- loop.func4(iter = 50, data = g66, a = 3, b = 8, init = c(2,2,2,2))
#penNI66_0.4 <- loop.func4(iter = 50, data = g66, a = 4, b = 9, init = c(2,2,2,2))
#penNI66_0.2 <- loop.func4(iter = 50, data = g66, a = 5, b = 10, init = c(2,2,2,2))

penFW1_300 <- loop.func3(iter = 50, data = g4.300, a = 1, b = 2, init =c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
penFW1_100 <- loop.func3(iter = 50, data = g4.100, a = 1, b = 2, init =c(2,2,2,2),prikn = c(1,1,1.5,1,1,1))
FW1_300 <- loop.func2(iter = 50, data = g4.300, a = 1, b = 2, init =c(2,2,2,2))
FW1_100 <- loop.func2(iter = 50, data = g4.100, a = 1, b = 2, init =c(2,2,2,2))
NI1_300 <- loop.func(data = g4.300, k = 50, a = 1, b =2, init =c(2,2,2,2))
NI1_100 <- loop.func(data = g4.100, k = 50, a = 1, b =2, init =c(2,2,2,2))
penNI1_300 <- loop.func4(iter = 50, data = g4.300, a = 1, b = 2, init =c(2,2,2,2))
penNI1_100 <- loop.func4(iter = 50, data = g4.100, a = 1, b = 2, init =c(2,2,3,3))

#penFW3_300 <- loop.func3(iter = 50, data = g3.300, a = 1, b = 2, init =c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW3_100 <- loop.func3(iter = 50, data = g3.100, a = 1, b = 2, init =c(2,2,2,2),prikn = c(1,1,1.5,1,1,1))
#FW3_300 <- loop.func2(iter = 50, data = g3.300, a = 1, b = 2, init =c(2,2,2,2))
#FW3_100 <- loop.func2(iter = 50, data = g3.100, a = 1, b = 2, init =c(2,2,2,2))
#NI3_300 <- loop.func(data = g3.300, k = 50, a = 1, b =2, init =c(2,2,2,2))
NI3_100 <- loop.func(data = g3.100, k = 50, a = 1, b =2, init =c(2,2,1,1))
#penNI3_300 <- loop.func4(iter = 50, data = g3.300, a = 1, b = 2, init =c(2,2,2,2))
penNI3_100 <- loop.func4(iter = 50, data = g3.100, a = 1, b = 2, init =c(2,2,1,1))

#penFW11_300 <- loop.func3(iter = 50, data = g11.300, a = 1, b = 2, init =c(2,2,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW11_100 <- loop.func3(iter = 50, data = g11.100, a = 1, b = 2, init =c(2,2,2,2),prikn = c(1,1,1.5,1,1,1))
#FW11_300 <- loop.func2(iter = 50, data = g11.300, a = 1, b = 2, init =c(2,2,2,2))
#FW11_100 <- loop.func2(iter = 50, data = g11.100, a = 1, b = 2, init =c(2,2,2,2))
#NI11_300 <- loop.func(data = g11.300, k = 50, a = 1, b =2, init =c(2,2,2,2))
#NI11_100 <- loop.func(data = g11.100, k = 50, a = 1, b =2, init =c(2,2,2,2))
#penNI11_300 <- loop.func4(iter = 50, data = g11.300, a = 1, b = 2, init =c(2,2,2,2))
#penNI11_100 <- loop.func4(iter = 50, data = g11.100, a = 1, b = 2, init =c(2,2,2,2))

#penFW66_300 <- loop.func3(iter = 50, data = g66.300, a = 1, b = 2, init =c(3,3,2,2), prikn = c(1,1,1.5,1,1,1))
#penFW66_100 <- loop.func3(iter = 50, data = g66.100, a = 1, b = 2, init =c(3,3,2,2),prikn = c(1,1,1.5,1,1,1))
#FW66_300 <- loop.func2(iter = 50, data = g66.300, a = 1, b = 2, init =c(3,3,2,2))
#FW66_100 <- loop.func2(iter = 50, data = g66.100, a = 1, b = 2, init =c(3,3,2,2))
NI66_300 <- loop.func(data = g66.300, k = 50, a = 1, b =2, init =c(2,2,2,2))
NI66_100 <- loop.func(data = g66.100, k = 50, a = 1, b =2, init =c(2,2,2,2))
penNI66_300 <- loop.func4(iter = 50, data = g66.300, a = 1, b = 2, init =c(2,2,2,2))
#penNI66_100 <- loop.func4(iter = 50, data = g66.100, a = 1, b = 2, init =c(3,3,2,2))

penFW66_0.2 <- as.data.frame(penFW1_0.2); penFW3_0.2 <- as.data.frame(penFW3_0.2)
penFW66_0.4 <- as.data.frame(penFW1_0.4); penFW3_0.4 <- as.data.frame(penFW3_0.4)
penFW1_0.6 <- as.data.frame(penFW1_0.6); penFW3_0.6 <- as.data.frame(penFW3_0.6)
penFW1_0.8 <- as.data.frame(penFW1_0.8); penFW3_0.8 <- as.data.frame(penFW3_0.8)
penFW1_1.0<- as.data.frame(penFW1_1.0); penFW3_1.0 <- as.data.frame(penFW3_1.0)

penFW11_0.2 <- as.data.frame(penFW11_0.2); penFW66_0.2 <- as.data.frame(penFW66_0.2)
penFW11_0.4 <- as.data.frame(penFW11_0.4); penFW66_0.4 <- as.data.frame(penFW66_0.4)
penFW11_0.6 <- as.data.frame(penFW11_0.6); penFW66_0.6 <- as.data.frame(penFW66_0.6)
penFW11_0.8 <- as.data.frame(penFW11_0.8); penFW66_0.8 <- as.data.frame(penFW66_0.8)
penFW11_1.0<- as.data.frame(penFW11_1.0); penFW66_1.0 <- as.data.frame(penFW66_1.0)

penFW1_0.2$sT <- sqrt(1/penFW1_0.2$preT); penFW1_0.2$sO <- sqrt(1/penFW1_0.2$preO)
penFW1_0.4$sT <- sqrt(1/penFW1_0.4$preT); penFW1_0.4$sO <- sqrt(1/penFW1_0.4$preO)
penFW1_0.6$sT <- sqrt(1/penFW1_0.6$preT); penFW1_0.6$sO <- sqrt(1/penFW1_0.6$preO)
penFW1_0.8$sT <- sqrt(1/penFW1_0.8$preT); penFW1_0.8$sO <- sqrt(1/penFW1_0.8$preO)
penFW1_1.0$sT <- sqrt(1/penFW1_1.0$preT); penFW1_1.0$sO <- sqrt(1/penFW1_1.0$preO)

penFW3_0.2$sT <- sqrt(1/penFW3_0.2$preT); penFW3_0.2$sO <- sqrt(1/penFW3_0.2$preO)
penFW3_0.4$sT <- sqrt(1/penFW3_0.4$preT); penFW3_0.4$sO <- sqrt(1/penFW3_0.4$preO)
penFW3_0.6$sT <- sqrt(1/penFW3_0.6$preT); penFW3_0.6$sO <- sqrt(1/penFW3_0.6$preO)
penFW3_0.8$sT <- sqrt(1/penFW3_0.8$preT); penFW3_0.8$sO <- sqrt(1/penFW3_0.8$preO)
penFW3_1.0$sT <- sqrt(1/penFW3_1.0$preT); penFW3_1.0$sO <- sqrt(1/penFW3_1.0$preO)

penFW11_0.2$sT <- sqrt(1/penFW11_0.2$preT); penFW11_0.2$sO <- sqrt(1/penFW11_0.2$preO)
penFW11_0.4$sT <- sqrt(1/penFW11_0.4$preT); penFW11_0.4$sO <- sqrt(1/penFW11_0.4$preO)
penFW11_0.6$sT <- sqrt(1/penFW11_0.6$preT); penFW11_0.6$sO <- sqrt(1/penFW11_0.6$preO)
penFW11_0.8$sT <- sqrt(1/penFW11_0.8$preT); penFW11_0.8$sO <- sqrt(1/penFW11_0.8$preO)
penFW11_1.0$sT <- sqrt(1/penFW11_1.0$preT); penFW11_1.0$sO <- sqrt(1/penFW11_1.0$preO)

penFW66_0.2$sT <- sqrt(1/penFW66_0.2$preT); penFW66_0.2$sO <- sqrt(1/penFW66_0.2$preO)
penFW66_0.4$sT <- sqrt(1/penFW66_0.4$preT); penFW66_0.4$sO <- sqrt(1/penFW66_0.4$preO)
penFW66_0.6$sT <- sqrt(1/penFW66_0.6$preT); penFW66_0.6$sO <- sqrt(1/penFW66_0.6$preO)
penFW66_0.8$sT <- sqrt(1/penFW66_0.8$preT); penFW66_0.8$sO <- sqrt(1/penFW66_0.8$preO)
penFW66_1.0$sT <- sqrt(1/penFW66_1.0$preT); penFW66_1.0$sO <- sqrt(1/penFW66_1.0$preO)

penFW1_0.2 <- penFW1_0.2[,c(1,2,5,6)]
penFW1_0.4 <- penFW1_0.4[,c(1,2,5,6)]
penFW1_0.6 <- penFW1_0.6[,c(1,2,5,6)]
penFW1_0.8 <- penFW1_0.8[,c(1,2,5,6)]
penFW1_1.0 <- penFW1_1.0[,c(1,2,5,6)]

penFW3_0.2 <- penFW3_0.2[,c(1,2,5,6)]
penFW3_0.4 <- penFW3_0.4[,c(1,2,5,6)]
penFW3_0.6 <- penFW3_0.6[,c(1,2,5,6)]
penFW3_0.8 <- penFW3_0.8[,c(1,2,5,6)]
penFW3_1.0 <- penFW3_1.0[,c(1,2,5,6)]

penFW11_0.2 <- penFW11_0.2[,c(1,2,5,6)]
penFW11_0.4 <- penFW11_0.4[,c(1,2,5,6)]
penFW11_0.6 <- penFW11_0.6[,c(1,2,5,6)]
penFW11_0.8 <- penFW11_0.8[,c(1,2,5,6)]
penFW11_1.0 <- penFW11_1.0[,c(1,2,5,6)]

penFW66_0.2 <- penFW66_0.2[,c(1,2,5,6)]
penFW66_0.4 <- penFW66_0.4[,c(1,2,5,6)]
penFW66_0.6 <- penFW66_0.6[,c(1,2,5,6)]
penFW66_0.8 <- penFW66_0.8[,c(1,2,5,6)]
penFW66_1.0 <- penFW66_1.0[,c(1,2,5,6)]

penFW1_100 <- as.data.frame(penFW1_100); penFW1_300 <- as.data.frame(penFW1_300)
penFW3_100 <- as.data.frame(penFW3_100); penFW3_300 <- as.data.frame(penFW3_300)
penFW11_100 <- as.data.frame(penFW11_100); penFW11_300 <- as.data.frame(penFW11_300)
penFW66_100 <- as.data.frame(penFW66_100); penFW66_300 <- as.data.frame(penFW66_300)

penFW1_100$sT <- sqrt(1/penFW1_100$preT); penFW1_100$sO <- sqrt(1/penFW1_100$preO)
penFW1_300$sT <- sqrt(1/penFW1_300$preT); penFW1_300$sO <- sqrt(1/penFW1_300$preO)
penFW3_100$sT <- sqrt(1/penFW3_100$preT); penFW3_100$sO <- sqrt(1/penFW3_100$preO)
penFW3_300$sT <- sqrt(1/penFW3_300$preT); penFW3_300$sO <- sqrt(1/penFW3_300$preO)
penFW11_100$sT <- sqrt(1/penFW11_100$preT); penFW11_100$sO <- sqrt(1/penFW11_100$preO)
penFW11_300$sT <- sqrt(1/penFW11_300$preT); penFW11_300$sO <- sqrt(1/penFW11_300$preO)
penFW66_100$sT <- sqrt(1/penFW66_100$preT); penFW66_100$sO <- sqrt(1/penFW66_100$preO)
penFW66_300$sT <- sqrt(1/penFW66_300$preT); penFW66_300$sO <- sqrt(1/penFW66_300$preO)

penFW1_100 <- penFW1_100[,c(1,2,5,6)]; penFW1_300 <- penFW1_300[,c(1,2,5,6)]
penFW3_100 <- penFW3_100[,c(1,2,5,6)]; penFW3_300 <- penFW3_300[,c(1,2,5,6)]
penFW11_100 <- penFW11_100[,c(1,2,5,6)]; penFW11_300 <- penFW11_300[,c(1,2,5,6)]
penFW66_100 <- penFW66_100[,c(1,2,5,6)]; penFW66_300 <- penFW66_300[,c(1,2,5,6)]

scen.1 <- rbind(FW66_0.2, FW66_0.4, FW66_0.6, FW66_0.8, FW66_1.0, penFW66_0.2, penFW66_0.4,
                penFW66_0.6, penFW66_0.8, penFW66_1.0, NI66_0.2, NI66_0.4, NI66_0.6, NI66_0.8,
                NI66_1.0, penNI66_0.2, penNI66_0.4, penNI66_0.6, penNI66_0.8, penNI66_1.0)
scen.2 <- rbind(FW1_0.2, FW1_0.4, FW1_0.6, FW1_0.8, FW1_1.0, penFW1_0.2, penFW1_0.4,
                penFW1_0.6, penFW1_0.8, penFW1_1.0, NI1_0.2, NI1_0.4, NI1_0.6, NI1_0.8,
                NI1_1.0, penNI1_0.2, penNI1_0.4, penNI1_0.6, penNI1_0.8, penNI1_1.0)
scen.3 <- rbind(FW11_0.2, FW11_0.4, FW11_0.6, FW11_0.8, FW11_1.0, penFW11_0.2, penFW11_0.4,
                penFW11_0.6, penFW11_0.8, penFW11_1.0, NI11_0.2, NI11_0.4, NI11_0.6, NI11_0.8,
                NI11_1.0, penNI11_0.2, penNI11_0.4, penNI11_0.6, penNI11_0.8, penNI11_1.0)
scen.4 <- rbind(FW3_0.2, FW3_0.4, FW3_0.6, FW3_0.8, FW3_1.0, penFW3_0.2, penFW3_0.4,
                penFW3_0.6, penFW3_0.8, penFW3_1.0, NI3_0.2, NI3_0.4, NI3_0.6, NI3_0.8,
                NI3_1.0, penNI3_0.2, penNI3_0.4, penNI3_0.6, penNI3_0.8, penNI3_1.0)
scen.1n <- rbind(FW66_100, FW66_300, FW66_1.0, penFW66_100, penFW66_300, penFW66_1.0, NI66_100,
                 NI66_300, NI66_1.0, penNI66_100, penNI66_300, penNI66_1.0)
scen.2n <- rbind(FW1_100, FW1_300, FW1_1.0, penFW1_100, penFW1_300, penFW1_1.0, NI1_100,
                 NI1_300, NI1_1.0, penNI1_100, penNI1_300, penNI1_1.0)
scen.3n <- rbind(FW11_100, FW11_300, FW11_1.0, penFW11_100, penFW11_300, penFW11_1.0, NI11_100,
                 NI11_300, NI11_1.0, penNI11_100, penNI11_300, penNI11_1.0)
scen.4n <- rbind(FW3_100, FW3_300, FW3_1.0, penFW3_100, penFW3_300, penFW3_1.0, NI3_100,
                 NI3_300, NI3_1.0, penNI3_100, penNI3_300, penNI3_1.0)
range <- c(rep(0.2, 50), rep(0.4, 50), rep(0.6, 50), rep(0.8,50), rep(1.0, 50),
           rep(0.2, 50), rep(0.4, 50), rep(0.6, 50), rep(0.8,50), rep(1.0, 50),
           rep(0.2, 50), rep(0.4, 50), rep(0.6, 50), rep(0.8,50), rep(1.0, 50),
           rep(0.2, 50), rep(0.4, 50), rep(0.6, 50), rep(0.8,50), rep(1.0, 50))
nrofsamp <- c(rep(100, 50), rep(300, 50), rep(500, 50),rep(100, 50), rep(300, 50), 
              rep(500, 50),rep(100, 50), rep(300, 50), rep(500, 50),rep(100, 50), 
              rep(300, 50), rep(500, 50))
method <- c(rep("MLE-FW", 150), rep("MAP-FW", 150), rep("MLE-exact", 150),rep("MAP-exact", 150))
scen.1 <- cbind(scen.1, range, method)
scen.2 <- cbind(scen.2, range, method)
scen.3 <- cbind(scen.3, range, method)
scen.4 <- cbind(scen.4, range, method)

scen.1n <- cbind(scen.1n, nrofsamp, method)
scen.2n <- cbind(scen.2n, nrofsamp, method)
scen.3n <- cbind(scen.3n, nrofsamp, method)
scen.4n <- cbind(scen.4n, nrofsamp, method)
priors1 <- g66[[11]][66,]
priors2 <- g4[[11]][4,]
priors3 <- g11[[11]][11,]
priors4 <- g3[[11]][3,]
library(dplyr)

scen.1 <- scen.1 %>% 
  mutate(muT.err = abs(muT - priors1[1]), muO.err = abs(muO - priors1[2]),
         sT.err = abs(sT - sqrt(1/priors1[3])), sO.err = abs(sO - sqrt(1/priors1[4])),
         mu.err = (muT.err + muO.err)/2, sigma.err = (sT.err + sO.err)/2, 
         over.err = (mu.err + sigma.err)/2)
scen.2 <- scen.2 %>% 
  mutate(muT.err = abs(muT - priors2[1]), muO.err = abs(muO - priors2[2]),
         sT.err = abs(sT - sqrt(1/priors2[3])), sO.err = abs(sO - sqrt(1/priors2[4])),
         mu.err = (muT.err + muO.err)/2, sigma.err = (sT.err + sO.err)/2,
         over.err = (mu.err + sigma.err)/2)
scen.3 <- scen.3 %>% 
  mutate(muT.err = abs(muT - priors3[1]), muO.err = abs(muO - priors3[2]),
         sT.err = abs(sT - sqrt(1/priors3[3])), sO.err = abs(sO - sqrt(1/priors3[4])),
         mu.err = (muT.err + muO.err)/2, sigma.err = (sT.err + sO.err)/2,
         over.err = (mu.err + sigma.err)/2)
scen.4 <- scen.4 %>% 
  mutate(muT.err = abs(muT - priors4[1]), muO.err = abs(muO - priors4[2]),
         sT.err = abs(sT - sqrt(1/priors4[3])), sO.err = abs(sO - sqrt(1/priors4[4])),
         mu.err = (muT.err + muO.err)/2, sigma.err = (sT.err + sO.err)/2,
         over.err = (mu.err + sigma.err)/2)
scen.1n <- scen.1n %>% 
  mutate(muT.err = abs(muT - priors1[1]), muO.err = abs(muO - priors1[2]),
         sT.err = abs(sT - sqrt(1/priors1[3])), sO.err = abs(sO - sqrt(1/priors1[4])),
         mu.err = (muT.err + muO.err)/2, sigma.err = (sT.err + sO.err)/2, 
         over.err = (mu.err + sigma.err)/2)
scen.2n <- scen.2n %>% 
  mutate(muT.err = abs(muT - priors2[1]), muO.err = abs(muO - priors2[2]),
         sT.err = abs(sT - sqrt(1/priors2[3])), sO.err = abs(sO - sqrt(1/priors2[4])),
         mu.err = (muT.err + muO.err)/2, sigma.err = (sT.err + sO.err)/2,
         over.err = (mu.err + sigma.err)/2)
scen.3n <- scen.3n %>% 
  mutate(muT.err = abs(muT - priors3[1]), muO.err = abs(muO - priors3[2]),
         sT.err = abs(sT - sqrt(1/priors3[3])), sO.err = abs(sO - sqrt(1/priors3[4])),
         mu.err = (muT.err + muO.err)/2, sigma.err = (sT.err + sO.err)/2,
         over.err = (mu.err + sigma.err)/2)
scen.4n <- scen.4n %>% 
  mutate(muT.err = abs(muT - priors4[1]), muO.err = abs(muO - priors4[2]),
         sT.err = abs(sT - sqrt(1/priors4[3])), sO.err = abs(sO - sqrt(1/priors4[4])),
         mu.err = (muT.err + muO.err)/2, sigma.err = (sT.err + sO.err)/2,
         over.err = (mu.err + sigma.err)/2)
means.scen.1 <- scen.1 %>%
  group_by(method, range) %>%
  summarise(avg.mu.err = mean(mu.err), avg.std.err = mean(sigma.err), avg.err = mean(over.err))
means.scen.2 <- scen.2 %>%
  group_by(method, range) %>%
  summarise(avg.mu.err = mean(mu.err), avg.std.err = mean(sigma.err), avg.err = mean(over.err))
means.scen.3 <- scen.3 %>%
  group_by(method, range) %>%
  summarise(avg.mu.err = mean(mu.err), avg.std.err = mean(sigma.err), avg.err = mean(over.err))
means.scen.4 <- scen.4 %>%
  group_by(method, range) %>%
  summarise(avg.mu.err = mean(mu.err), avg.std.err = mean(sigma.err), avg.err = mean(over.err))
means.scen.1n <- scen.1n %>%
  group_by(method,nrofsamp) %>%
  summarise(avg.mu.err = mean(mu.err), avg.std.err = mean(sigma.err), avg.err = mean(over.err))
means.scen.2n <- scen.2n %>%
  group_by(method, nrofsamp) %>%
  summarise(avg.mu.err = mean(mu.err), avg.std.err = mean(sigma.err), avg.err = mean(over.err))
means.scen.3n <- scen.3n %>%
  group_by(method, nrofsamp) %>%
  summarise(avg.mu.err = mean(mu.err), avg.std.err = mean(sigma.err), avg.err = mean(over.err))
means.scen.4n <- scen.4n %>%
  group_by(method, nrofsamp) %>%
  summarise(avg.mu.err = mean(mu.err), avg.std.err = mean(sigma.err), avg.err = mean(over.err))
means.scen.1$range <- as.factor(means.scen.1$range)
means.scen.2$range <- as.factor(means.scen.2$range)
means.scen.3$range <- as.factor(means.scen.3$range)
means.scen.4$range <- as.factor(means.scen.4$range)
means.scen.1n$nrofsamp <- as.factor(means.scen.1n$nrofsamp)
means.scen.2n$nrofsamp <- as.factor(means.scen.2n$nrofsamp)
means.scen.3n$nrofsamp <- as.factor(means.scen.3n$nrofsamp)
means.scen.4n$nrofsamp <- as.factor(means.scen.4n$nrofsamp)
diff.scen1 <- means.scen.1 %>%
  group_by(range) %>%
  summarise(diff = max(avg.err) - min(avg.err))
diff.scen2 <- means.scen.2 %>%
  group_by(range) %>%
  summarise(diff = max(avg.err) - min(avg.err))
diff.scen3 <- means.scen.3 %>%
  group_by(range) %>%
  summarise(diff = max(avg.err) - min(avg.err))
diff.scen4 <- means.scen.4 %>%
  group_by(range) %>%
  summarise(diff = max(avg.err) - min(avg.err))

diff.scen1n <- means.scen.1n %>%
  group_by(nrofsamp) %>%
  summarise(diff = max(avg.err) - min(avg.err))
diff.scen2n <- means.scen.2n %>%
  group_by(nrofsamp) %>%
  summarise(diff = max(avg.err) - min(avg.err))
diff.scen3n <- means.scen.3n %>%
  group_by(nrofsamp) %>%
  summarise(diff = max(avg.err) - min(avg.err))
diff.scen4n <- means.scen.4n %>%
  group_by(nrofsamp) %>%
  summarise(diff = max(avg.err) - min(avg.err))

library(ggplot2)
s1.m <- ggplot(means.scen.1, aes(x = range, y = avg.mu.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("mu error (scenario 1)") + theme(plot.title = element_text(size=9))
s2.m <- ggplot(means.scen.2, aes(x = range, y = avg.mu.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("mu error (scenario 2)") + theme(plot.title = element_text(size=9))
s3.m <- ggplot(means.scen.3, aes(x = range, y = avg.mu.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("mu error (scenario 3)") + theme(plot.title = element_text(size=9))
s4.m <- ggplot(means.scen.4, aes(x = range, y = avg.mu.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("mu error (scenario 4)") + theme(plot.title = element_text(size=9))

s1.sig <- ggplot(means.scen.1, aes(x = range, y = avg.std.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("sigma error (scenario 1)") + theme(plot.title = element_text(size=9))
s2.sig <- ggplot(means.scen.2, aes(x = range, y = avg.std.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("sigma error (scenario 2)") + theme(plot.title = element_text(size=9))
s3.sig <- ggplot(means.scen.3, aes(x = range, y = avg.std.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("sigma error (scenario 3)") + theme(plot.title = element_text(size=9))
s4.sig <- ggplot(means.scen.4, aes(x = range, y = avg.std.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("sigma error (scenario 4)") + theme(plot.title = element_text(size=9))

s1.msig <- ggplot(means.scen.1, aes(x = range, y = avg.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) + ylim(0, 1.2) +
  ggtitle(" error (scenario 1)") + theme(plot.title = element_text(size=9))
s2.msig <- ggplot(means.scen.2, aes(x = range, y = avg.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) + ylim(0, 1.2) +
  ggtitle(" error (scenario 2)") + theme(plot.title = element_text(size=9))
s3.msig <- ggplot(means.scen.3, aes(x = range, y = avg.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) + ylim(0, 1.2) +
  ggtitle(" error (scenario 3)") + theme(plot.title = element_text(size=9))
s4.msig <- ggplot(means.scen.4, aes(x = range, y = avg.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) + ylim(0, 1.2) +
  ggtitle(" error (scenario 4)") + theme(plot.title = element_text(size=9))

s1n.msig <- ggplot(means.scen.1n, aes(x = nrofsamp, y = avg.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) + ylim(0, 0.4) +
  ggtitle(" error (scenario 1)") + theme(plot.title = element_text(size=9)) + scale_x_discrete(name = "nr of samples")
s2n.msig <- ggplot(means.scen.2n, aes(x = nrofsamp, y = avg.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) + ylim(0, 0.4) +
  ggtitle(" error (scenario 2)") + theme(plot.title = element_text(size=9))+ scale_x_discrete(name = "nr of samples")
s3n.msig <- ggplot(means.scen.3n, aes(x = nrofsamp, y = avg.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) + ylim(0, 0.4) +
  ggtitle(" error (scenario 3)") + theme(plot.title = element_text(size=9))+ scale_x_discrete(name = "nr of samples")
s4n.msig <- ggplot(means.scen.4n, aes(x = nrofsamp, y = avg.err, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) + ylim(0, 0.4) +
  ggtitle(" error (scenario 4)") + theme(plot.title = element_text(size=9))+ scale_x_discrete(name = "nr of samples")
library(ggpubr)
muandsig.r <- ggarrange(s1.m, s2.m, s3.m, s4.m, s1.sig, s2.sig, s3.sig, s4.sig, nrow = 4, ncol = 2, common.legend = TRUE)
err.r <- ggarrange(s1.msig, s2.msig, s3.msig, s4.msig, nrow = 2, ncol = 2, common.legend = TRUE)
err.nsam <- ggarrange(s1n.msig, s2n.msig, s3n.msig, s4n.msig, nrow = 2, ncol = 2, common.legend = TRUE)
write.csv(scen.1, file = "scenario1.r.csv", row.names = FALSE)
write.csv(scen.1n, file = "scenario1.n.csv", row.names = FALSE)
write.csv(scen.2, file = "scenario2.r.csv", row.names = FALSE)
write.csv(scen.2n, file = "scenario2.n.csv", row.names = FALSE)
write.csv(scen.3, file = "scenario3.r.csv", row.names = FALSE)
write.csv(scen.3n, file = "scenario3.n.csv", row.names = FALSE)
write.csv(scen.4, file = "scenario4.r.csv", row.names = FALSE)
write.csv(scen.4n, file = "scenario4.n.csv", row.names = FALSE)

library(Rmisc)
scen1.SE <- summarySE(data =scen.1, measurevar = "over.err", groupvars = c("method", "range"))
scen2.SE <- summarySE(data =scen.2, measurevar = "over.err", groupvars = c("method", "range"))
scen3.SE <- summarySE(data =scen.3, measurevar = "over.err", groupvars = c("method", "range"))
scen4.SE <- summarySE(data =scen.4, measurevar = "over.err", groupvars = c("method", "range"))
scen1n.SE <- summarySE(data =scen.1n, measurevar = "over.err", groupvars = c("method", "nrofsamp"))
scen2n.SE <- summarySE(data =scen.2n, measurevar = "over.err", groupvars = c("method", "nrofsamp"))
scen3n.SE <- summarySE(data =scen.3n, measurevar = "over.err", groupvars = c("method", "nrofsamp"))
scen4n.SE <- summarySE(data =scen.4n, measurevar = "over.err", groupvars = c("method", "nrofsamp"))

scen1.SE$range <- as.factor(scen1.SE$range)
scen2.SE$range <- as.factor(scen2.SE$range)
scen3.SE$range <- as.factor(scen3.SE$range)
scen4.SE$range <- as.factor(scen4.SE$range)
scen1n.SE$nrofsamp <- as.factor(scen1n.SE$nrofsamp)
scen2n.SE$nrofsamp <- as.factor(scen2n.SE$nrofsamp)
scen3n.SE$nrofsamp <- as.factor(scen3n.SE$nrofsamp)
scen4n.SE$nrofsamp <- as.factor(scen4n.SE$nrofsamp)


diff1 <- ggplot(diff.scen1, aes(x = range, y = diff)) + 
  geom_line( ) + geom_point(size = 1) + scale_x_continuous(name ="Interval") + 
  scale_y_continuous(name = "Difference") + ggtitle("Scenario 1")
diff2 <- ggplot(diff.scen2, aes(x = range, y = diff)) + 
  geom_line( ) + geom_point(size = 1) + scale_x_continuous(name ="Interval") + 
  scale_y_continuous(name = "Difference") + ggtitle("Scenario 2")
diff3 <- ggplot(diff.scen3, aes(x = range, y = diff)) + 
  geom_line( ) + geom_point(size = 1) + scale_x_continuous(name ="Interval") + 
  scale_y_continuous(name = "Difference") + ggtitle("Scenario 3")
diff4 <- ggplot(diff.scen4, aes(x = range, y = diff)) + 
  geom_line( ) + geom_point(size = 1) + scale_x_continuous(name ="Interval") + 
  scale_y_continuous(name = "Difference") + ggtitle("Scenario 4")
diff1n <- ggplot(diff.scen1n, aes(x = nrofsamp, y = diff)) + 
  geom_line( ) + geom_point(size = 1) + scale_x_continuous(name ="Samples") + 
  scale_y_continuous(name = "Difference") + ggtitle("Scenario 1")
diff2n <- ggplot(diff.scen2n, aes(x = nrofsamp, y = diff)) + 
  geom_line( ) + geom_point(size = 1) + scale_x_continuous(name ="Samples") + 
  scale_y_continuous(name = "Difference") + ggtitle("Scenario 2")
diff3n <- ggplot(diff.scen3n, aes(x = nrofsamp, y = diff)) + 
  geom_line( ) + geom_point(size = 1) + scale_x_continuous(name ="Samples") + 
  scale_y_continuous(name = "Difference") + ggtitle("Scenario 3")
diff4n <- ggplot(diff.scen4n, aes(x = nrofsamp, y = diff)) + 
  geom_line( ) + geom_point(size = 1) + scale_x_continuous(name ="Samples") + 
  scale_y_continuous(name = "Difference") + ggtitle("Scenario 4")

s1.spread <- ggplot(scen1.SE, aes(x = range, y = sd, fill = method)) +
  geom_bar(stat="identity", position=position_dodge())  +
  ggtitle(" sd of error (scenario 1)") + theme(plot.title = element_text(size=9))
s2.spread <- ggplot(scen2.SE, aes(x = range, y = sd, fill = method)) +
  geom_bar(stat="identity", position=position_dodge())  +
  ggtitle(" sd of error (scenario 2)") + theme(plot.title = element_text(size=9))
s3.spread <- ggplot(scen3.SE, aes(x = range, y = sd, fill = method)) +
  geom_bar(stat="identity", position=position_dodge())  + ylim(0, 0.8) +
  ggtitle(" sd of error (scenario 3)") + theme(plot.title = element_text(size=9))
s4.spread <- ggplot(scen4.SE, aes(x = range, y = sd, fill = method)) +
  geom_bar(stat="identity", position=position_dodge())  + ylim(0, 0.8) +
  ggtitle(" sd of error (scenario 4)") + theme(plot.title = element_text(size=9))

s1n.spread <- ggplot(scen1n.SE, aes(x = nrofsamp, y = sd, fill = method)) +
  geom_bar(stat="identity", position=position_dodge())  +
  ggtitle(" sd of error (scenario 1)") + theme(plot.title = element_text(size=9)) +
  scale_x_discrete(name = "samples")
s2n.spread <- ggplot(scen2n.SE, aes(x = nrofsamp, y = sd, fill = method)) +
  geom_bar(stat="identity", position=position_dodge())  + ylim(0, 0.2) +
  ggtitle(" sd of error (scenario 2)") + theme(plot.title = element_text(size=9)) +
  scale_x_discrete(name = "samples")
s3n.spread <- ggplot(scen3n.SE, aes(x = nrofsamp, y = sd, fill = method)) +
  geom_bar(stat="identity", position=position_dodge())   + ylim(0, 0.2) +
  ggtitle(" sd of error (scenario 3)") + theme(plot.title = element_text(size=9)) +
  scale_x_discrete(name = "samples")
s4n.spread <- ggplot(scen4n.SE, aes(x = nrofsamp, y = sd, fill = method)) +
  geom_bar(stat="identity", position=position_dodge()) + ylim(0, 0.2) +
  ggtitle(" sd of error (scenario 4)") + theme(plot.title = element_text(size=9)) +
  scale_x_discrete(name = "samples")
library(ggpubr)
Diffe <- ggarrange(diff1, diff2, diff3, diff4, nrow = 2, ncol = 2)
Diffe.n <- ggarrange(diff1n, diff2n, diff3n, diff4n, nrow = 2, ncol = 2)
stderr <- ggarrange(s1.spread, s2.spread, s3.spread, s4.spread, nrow = 2, ncol = 2, 
                    common.legend = TRUE)
stderr.n <- ggarrange(s1n.spread, s2n.spread, s3n.spread, s4n.spread, nrow = 2, ncol = 2, common.legend = TRUE)
gen.data.prior.corr <- function(n, p, alpha_T=1.5, beta_T =1, alpha_O=1, beta_O=1, mu_0=2, sigma_0=1, mu_1=3, sigma_1=1, r1, 
                                r2, seed){
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
data.corr1 <- gen.data.prior.corr(n = 500, p = 50, r1 = 0, r2 = 1, seed = 1234)
data.corr0.6 <- gen.data.prior.corr(n = 500, p = 50, r1 = 0.2, r2 = 0.8, seed = 1234)
data.corr0.2 <- gen.data.prior.corr(n = 500, p = 50, r1 = 0.4, r2 = 0.6, seed = 1234)
pri1.0 <- data.corr2[[3]]
FW1.corr <- loop.func2(iter = 50, data = data.corr2, a = 1, b = 2, init = c(3,3,2,2))
FW0.6.corr <- loop.func2(iter = 50, data = data.corr0.6, a = 1, b = 2, init = c(3,3,2,2))
FW0.2.corr <- loop.func2(iter = 50, data = data.corr0.2, a = 1, b = 2, init =c(3,3,2,2))

Y.rep <- g4.100[[1]]
f <- g4.100[[2]]
penNI1_100 <- matrix(0, nrow = 50, ncol = 4)
colnames(penNI1_100) <- c("muT", "muO", "sT", "sO")
for(j in 1:50){ 
  Ydens.loop <- function(i,j = j, mu_T, mu_O, s_T, s_O){
    Y <- Y.rep[,j]
    yi <- Y[i]
    lfi <- log(f[i])
    lfio <- log(1-f[i])
    g <- function(t) dlnorm(t,lfi+mu_T, s_T)*dlnorm(yi-t,lfio + mu_O, s_O)
    return(integrate(g, lower = 0, upper = yi, rel.tol = 0.5)$value)
  }
  loglik.loop <- function(pars){
    mu_T <- pars[1]
    mu_O <- pars[2]
    s_T <- pars[3]
    s_O <- pars[4]
    return(-sum(log(sapply(1:nrow(Y.rep), Ydens.loop, j=j, mu_T = mu_T, mu_O = mu_O, s_T = s_T, s_O = s_O))))
  }
  penNI1_100[j,] <- optim(par = c(2,2,2,2), loglik.loop, method = "Nelder-Mead")$par
}



FW0.2.est <- c(FW0.2.corr[,1], FW0.2.corr[,2], FW0.2.corr[,3], FW0.2.corr[,4])
FW0.2.true <- c(pri1.0[,1], pri1.0[,2], sqrt(1/pri1.0[,3]), sqrt(1/pri1.0[,4]))

FW0.6.est <- c(FW0.6.corr[,1], FW0.6.corr[,2], FW0.6.corr[,3], FW0.6.corr[,4])
FW1.est <- c(FW1.corr[,1], FW1.corr[,2], FW1.corr[,3], FW1.corr[,4])

penFW1.est <- c(penFW1.0[-35,1], penFW1.0[-35,2], sqrt(1/penFW1.0[-35,3]), sqrt(1/penFW1.0[-35,4]))
penFW.true <- c(pri1.0[-35,1], pri1.0[-35,2], sqrt(1/pri1.0[-35,3]), sqrt(1/pri1.0[-35,4]))

penFW0.6.est <- c(penFW0.6[-35,1], penFW0.6[-35,2], sqrt(1/penFW0.6[-35,3]), sqrt(1/penFW0.6[-35,4]))
penFW0.2.est <- c(penFW0.2[-35,1], penFW0.2[-35,2], sqrt(1/penFW0.2[-c(35,16),3]), sqrt(1/penFW0.2[-35,4]))
penFW.true.spec <- c(pri1.0[-35,1], pri1.0[-35,2], sqrt(1/pri1.0[-c(35,16), 3]), sqrt(1/pri1.0[-35, 4]))
NI1.est <- c(NI1.corr[-35,1], NI1.corr[-35,2], NI1.corr[-35,3], NI1.corr[-35,4])
NI0.6.est <- c(NI0.6.corr[-c(3,35), 1], NI0.6.corr[-c(3,35), 2], NI0.6.corr[-c(3,35), 3], NI0.6.corr[-c(3,35), 4])
NI0.2.est <- c(NI0.2.corr[-c(3,35), 1], NI0.2.corr[-c(3,35), 2], NI0.2.corr[-c(3,35), 3], NI0.2.corr[-c(3,35), 4])
NI.true <- c(pri1.0[-c(3,35),1], pri1.0[-c(3,35),2], sqrt(1/pri1.0[-c(3,35),3]), sqrt(1/pri1.0[-c(3,35),4]))

penNI1.est <- c(penNI1.corr[-c(3,35),1], penNI1.corr[-c(3,35),2], penNI1.corr[-c(3,35),3], 
                penNI1.corr[-c(3,35),4])
penNI0.6.est <- c(penNI0.6.corr[-c(3,35),1], penNI0.6.corr[-c(3,35),2], penNI0.6.corr[-c(3,35),3], 
                penNI0.6.corr[-c(3,35),4])
penNI0.2.est <- c(penNI0.2.corr[-c(3,35),1], penNI0.2.corr[-c(3,35),2], penNI0.2.corr[-c(3,35),3], 
                  penNI0.2.corr[-c(3,35),4])

FW.scatter <- as.data.frame(cbind(FW0.2.est, FW0.6.est, FW1.est, FW0.2.true))
NI.scatter <- as.data.frame(cbind(NI0.6.est, NI0.2.est, NI.true))
penFW.scatter <- as.data.frame(cbind(penFW1.est, penFW0.6.est, NI1.est, penFW.true))
penNI.scatter <- as.data.frame(cbind(penNI1.est, penNI0.6.est, penNI0.2.est, NI.true))
penFW.scatter.spec <- as.data.frame(cbind(penFW0.2.est, penFW.true.spec))

##Reconstructing the latent gene expressions
FW_muT0.2 <- FW.scatter[1:50, 1]; FW_muT0.6 <- FW.scatter[1:50, 2]; FW_muT1 <- FW.scatter[1:50, 3]
FW_muO0.2 <- FW.scatter[51:100, 1]; FW_muO0.6 <- FW.scatter[51:100, 2]; FW_muO1 <- FW.scatter[51:100, 3]
FW_sT0.2 <- FW.scatter[101:150, 1]; FW_sT0.6 <- FW.scatter[101:150, 2]; FW_sT1 <- FW.scatter[101:150, 3]
FW_sO0.2 <- FW.scatter[151:200, 1]; FW_sO0.6 <- FW.scatter[151:200, 2]; FW_sO1 <- FW.scatter[151:200, 3]

#left out row 35
penFW_muT0.6 <- penFW.scatter[1:49, 2]; penFW_muT1 <- penFW.scatter[1:49, 1]; NI_muT1 <- penFW.scatter[1:49, 3]
penFW_muO0.6 <- penFW.scatter[50:98, 2]; penFW_muO1 <- penFW.scatter[50:98, 1];NI_muO1 <- penFW.scatter[50:98, 3]
penFW_sT0.6 <- penFW.scatter[99:147, 2]; penFW_sT1 <- penFW.scatter[99:147, 1]; NI_sT1 <- penFW.scatter[99:147, 3]
penFW_sO0.6 <- penFW.scatter[148:196, 2]; penFW_sO1 <- penFW.scatter[148:196, 1]; NI_sO1 <- penFW.scatter[148:196, 3]

#left out row 3 and 35
NI_muT0.2 <- NI.scatter[1:48, 2]
NI_muO0.2 <- NI.scatter[49:96, 2]
NI_sT0.2 <- NI.scatter[97:144, 2]
NI_sO0.2 <- NI.scatter[145:192, 2]
NI_muT0.6 <- NI.scatter[1:48, 1]
NI_muO0.6 <- NI.scatter[49:96, 1]
NI_sT0.6 <- NI.scatter[97:144, 1]
NI_sO0.6 <- NI.scatter[145:192, 1]
penNI_muT0.2 <- penNI.scatter[1:48, 3]
penNI_muO0.2 <- penNI.scatter[49:96, 3]
penNI_sT0.2 <- penNI.scatter[97:144, 3]
penNI_sO0.2 <- penNI.scatter[145:192, 3]
penNI_muT0.6 <- penNI.scatter[1:48, 2]
penNI_muO0.6 <- penNI.scatter[49:96, 2]
penNI_sT0.6 <- penNI.scatter[97:144, 2]
penNI_sO0.6 <- penNI.scatter[145:192, 2]
penNI_muT1 <- penNI.scatter[1:48, 2]
penNI_muO1 <- penNI.scatter[49:96, 2]
penNI_sT1 <- penNI.scatter[97:144, 2]
penNI_sO1 <- penNI.scatter[145:192, 2]
penFW_muT0.2 <- penFW.scatter.spec[c(1:15, 17:49), 1]
penFW_muO0.2 <- penFW.scatter.spec[c(50:64, 66:98), 1]
penFW_sT0.2 <- penFW.scatter.spec[c(99:146), 1]
penFW_sO0.2 <- penFW.scatter.spec[c(147:161, 163:195), 1]
data49T <- data.corr0.2[[1]][,-35] 
data49O <- data.corr0.2[[2]][,-35]
data48T <- data.corr0.2[[1]][,-c(3,35)]
data48O <- data.corr0.2[[2]][,-c(3,35)]
data.specialT <- data.corr0.2[[1]][,-c(16,35)]
data.specialO <- data.corr0.2[[2]][,-c(16,35)]

FW0.2T <- numeric(50)
FW0.2O <- numeric(50)
FW0.6T <- numeric(50)
FW0.6O <- numeric(50)
FW1T<- numeric(50)
FW1O <- numeric(50)
penFW0.2T <- numeric(48)
penFW0.2O <- numeric(48)
penFW0.6T <- numeric(49)
penFW0.6O <-numeric(49)
penFW1T <- numeric(49)
penFW1O <- numeric(49)
NI0.2T <- numeric(48)
NI0.2O <- numeric(48)
NI0.6T <- numeric(48)
NI0.6O <- numeric(48)
NI1T<- numeric(49)
NI1O <- numeric(49)
penNI0.2T <- numeric(48)
penNI0.2O <- numeric(48)
penNI0.6T <- numeric(48)
penNI0.6O <-numeric(48)
penNI1T <- numeric(48)
penNI1O <- numeric(48)

for(i in 1:50){
  FW0.2T[i] <- unname(ks.test(x = data.corr0.2[[1]][,i], "plnorm", FW_muT0.2[i], FW_sT0.2[i])$statistic)
}

for(i in 1:50){
  FW0.2O[i] <- unname(ks.test(x = data.corr0.2[[2]][,i], "plnorm", FW_muO0.2[i], FW_sO0.2[i])$statistic)
}  
 
for(i in 1:50){
  FW0.6T[i] <- unname(ks.test(x = data.corr0.6[[1]][,i], "plnorm", FW_muT0.6[i], FW_sT0.6[i])$statistic)
}

for(i in 1:50){
  FW0.6O[i] <- unname(ks.test(x = data.corr0.6[[2]][,i], "plnorm", FW_muO0.6[i], FW_sO0.6[i])$statistic)
} 

for(i in 1:50){
  FW1T[i] <- unname(ks.test(x = data.corr1[[1]][,i], "plnorm", FW_muT1[i], FW_sT1[i])$statistic)
}

for(i in 1:50){
  FW1O[i] <- unname(ks.test(x = data.corr1[[2]][,i], "plnorm", FW_muO1[i], FW_sO1[i])$statistic)
} 

for(i in 1:49){
  NI1T[i] <- unname(ks.test(data49T[,i], "plnorm", NI_muT1[i], NI_sT1[i])$statistic)
}

for(i in 1:49){
  NI1O[i] <- unname(ks.test(data49O[,i], "plnorm", NI_muO1[i], NI_sO1[i])$statistic)
}

for(i in 1:49){
  penFW1T[i] <- unname(ks.test(data49T[,i], "plnorm", penFW_muT1[i], penFW_sT1[i])$statistic)
}

for(i in 1:49){
  penFW1O[i] <- unname(ks.test(data49O[,i], "plnorm", penFW_muO1[i], penFW_sO1[i])$statistic)
}

for(i in 1:49){
  penFW0.6T[i] <- unname(ks.test(data49T[,i], "plnorm", penFW_muT0.6[i], penFW_sT0.6[i])$statistic)
}

for(i in 1:49){
  penFW0.6O[i] <- unname(ks.test(data49O[,i], "plnorm", penFW_muO0.6[i], penFW_sO0.6[i])$statistic)
}
#
for(i in 1:48){
  penNI0.2T[i] <- unname(ks.test(data48T[,i], "plnorm", penNI_muT0.2[i], penNI_sT0.2[i])$statistic)
}

for(i in 1:48){
  penNI0.2O[i] <- unname(ks.test(data48O[,i], "plnorm", penNI_muO0.2[i], penNI_sO0.2[i])$statistic)
}

for(i in 1:48){
  penNI0.6T[i] <- unname(ks.test(data48T[,i], "plnorm", penNI_muT0.6[i], penNI_sT0.6[i])$statistic)
}

for(i in 1:48){
  penNI0.6O[i] <- unname(ks.test(data48O[,i], "plnorm", penNI_muO0.6[i], penNI_sO0.6[i])$statistic)
}

for(i in 1:48){
  penNI1T[i] <- unname(ks.test(data48T[,i], "plnorm", penNI_muT1[i], penNI_sT1[i])$statistic)
}

for(i in 1:48){
  penNI1O[i] <- unname(ks.test(data48O[,i], "plnorm", penNI_muO1[i], penNI_sO1[i])$statistic)
}

for(i in 1:48){
  NI0.2T[i] <- unname(ks.test(data48T[,i], "plnorm", NI_muT0.2[i], NI_sT0.2[i])$statistic)
}

for(i in 1:48){
  NI0.2O[i] <- unname(ks.test(data48O[,i], "plnorm", NI_muO0.2[i], NI_sO0.2[i])$statistic)
}

for(i in 1:48){
  NI0.6T[i] <- unname(ks.test(data48T[,i], "plnorm", NI_muT0.6[i], NI_sT0.6[i])$statistic)
}

for(i in 1:48){
  NI0.6O[i] <- unname(ks.test(data48O[,i], "plnorm", NI_muO0.6[i], NI_sO0.6[i])$statistic)
}

for(i in 1:48){
  penFW0.2T[i] <- unname(ks.test(data.specialT[,i], "plnorm", penFW_muT0.2[i], penFW_sT0.2[i])$statistic)
}

for(i in 1:48){
  penFW0.2O[i] <- unname(ks.test(data.specialO[,i], "plnorm", penFW_muO0.2[i], penFW_sO0.2[i])$statistic)
}

T_dist <- c(FW0.2T, FW0.6T, FW1T, penFW0.2T, penFW0.6T, penFW1T, NI0.2T, NI0.6T, NI1T, penNI0.2T, penNI0.6T, penNI1T)
O_dist <- c(FW0.2O, FW0.6O, FW1O, penFW0.2O, penFW0.6O, penFW1O, NI0.2O, NI0.6O, NI1O,
                penNI0.2O, penNI0.6O, penNI1O)
method <- c(rep("FW", 150), rep("penFW", 146), rep("NI", 145), rep("penNI", 144))
range <- c(rep(0.2, 50), rep(0.6,50), rep(1, 50), rep(0.2, 48), rep(0.6, 49), rep(1, 49), 
           rep(0.2, 48), rep(0.6, 48), rep(1, 49), rep(0.2, 48), rep(0.6, 48), rep(1, 48))
T_dist <- as.data.frame(cbind(T_dist, method, range))
O_dist <- as.data.frame(cbind(O_dist, method, range))

T_dist$T_dist <- as.numeric(levels(T_dist$T_dist))[T_dist$T_dist]
O_dist$O_dist <- as.numeric(levels(O_dist$O_dist))[O_dist$O_dist]
T_dist <- T_dist[complete.cases(T_dist),]
O_dist <- O_dist[complete.cases(O_dist),]
library(Rmisc)
library(dplyr)
library(ggplot2)
T_dist_summary <- summarySE(T_dist, measurevar = "T_dist", groupvars = c("method", "range"))
O_dist_summary <- summarySE(O_dist, measurevar = "O_dist", groupvars = c("method", "range"))

Tdist.plot <- ggplot(T_dist_summary, aes(x = range, y = T_dist, group = method, color = method)) +
  geom_point(position = position_dodge(0.25))+ geom_line(position = position_dodge(0.25)) +
  geom_errorbar(aes(ymin= T_dist - 0.5*sd, ymax= T_dist + 0.5*sd), width=.2, position=position_dodge(0.2)) +
  scale_y_continuous(name = "D_n", limits = c(0,0.5)) 

Odist.plot <- ggplot(O_dist_summary, aes(x = range, y = O_dist, group = method, color = method)) +
  geom_point(position = position_dodge(0.2))+ geom_line(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin= O_dist - 0.5*sd, ymax= O_dist + 0.5*sd), width=.2, position=position_dodge(0.2)) +
  scale_y_continuous(name = "D_n", limits = c(0,0.5)) 
library(ggpubr)
TandO <- ggarrange(Tdist.plot, Odist.plot, nrow = 1, ncol = 2, common.legend = TRUE)
write.csv(FW.scatter, file = "FW.scatter.csv", row.names = FALSE)
write.csv(NI.scatter, file = "NI.scatter.csv", row.names = FALSE)
write.csv(penFW.scatter, file = "penFW.scatter.csv", row.names = FALSE)
write.csv(penNI.scatter, file = "penNI.scatter.csv", row.names = FALSE)
write.csv(penFW.scatter.spec, file = "penFW.scatter.special.csv", row.names = FALSE)

FW.scatter <- read.csv("FW.scatter.csv", header = TRUE)
NI.scatter <- read.csv("NI.scatter.csv", header = TRUE)
penFW.scatter <- read.csv("penFW.scatter.csv", header = TRUE)
penNI.scatter <- read.csv("penNI.scatter.csv", header = TRUE)
penFW.scatter.spec <- read.csv("penFW.scatter.special.csv", header = TRUE)

FW.1.sp <- ggplot(FW.scatter, aes(x = FW0.2.true, y = FW1.est)) +
  geom_point(col = "green") + geom_smooth(method = "lm", se = FALSE, col = "black") + 
   scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")
FW0.2.sp <- ggplot(FW.scatter, aes(x = FW0.2.true, y = FW0.2.est))  +
  geom_point(col = "green") + geom_smooth(method = "lm", se = FALSE, col = "black") + 
  scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")
FW0.6.sp <- ggplot(FW.scatter, aes(x = FW0.2.true, y = FW0.6.est)) +
  geom_point(col = "green") + geom_smooth(method = "lm", se = FALSE, col = "black") +
 scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")

NI.1.sp <- ggplot(penFW.scatter, aes(x = penFW.true, y = NI1.est)) +
  geom_point(col = "red") + geom_smooth(method = "lm", se = FALSE, col = "black") + 
  scale_x_continuous(name = "true") + scale_y_continuous(name  = "est") 
NI0.6.sp <- ggplot(NI.scatter, aes(x = NI.true, y = NI0.6.est)) +
  geom_point(col = "red") + geom_smooth(method = "lm", se = FALSE, col= "black") +
  scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")
NI0.2.sp <- ggplot(NI.scatter, aes(x = NI.true, y = NI0.2.est)) +
  geom_point(col = "red") + geom_smooth(method = "lm", se = FALSE, col = "black")  +
  scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")

penNI.1.sp <- ggplot(penNI.scatter, aes(x = NI.true, y = penNI1.est)) +
  geom_point(col = "blue") + geom_smooth(method = "lm", se = FALSE, col = "black") +
  scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")
penNI0.2.sp <- ggplot(penNI.scatter, aes(x = NI.true, y = penNI0.2.est)) +
  geom_point(col = "blue") + geom_smooth(method = "lm", se = FALSE, col = "black") +
  scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")
penNI0.6.sp <- ggplot(penNI.scatter, aes(x = NI.true, y = penNI0.6.est)) +
  geom_point(col = "blue") + geom_smooth(method = "lm", se = FALSE, col = "black") +
  scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")

penFW.1.sp <- ggplot(penFW.scatter, aes(x = penFW.true, y = penFW1.est)) +
  geom_point(col = "yellow") + geom_smooth(method = "lm", se = FALSE, col = "black") +
  scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")
penFW0.6.sp <- ggplot(penFW.scatter, aes(x = penFW.true, y = penFW0.6.est)) +
  geom_point(col = "yellow") + geom_smooth(method = "lm", se = FALSE, col = "black") +
  scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")
penFW0.2.sp <- ggplot(penFW.scatter.spec, aes(x = penFW.true.spec, y = penFW0.2.est)) + 
  geom_point(col = "yellow") + geom_smooth(method = "lm", se = FALSE, col = "black") +
  scale_x_continuous(name = "true")+ scale_y_continuous(name  = "est")

scatt <- ggarrange(FW0.2.sp, FW0.6.sp, FW.1.sp, penFW0.2.sp, penFW0.6.sp, penFW.1.sp,
                   NI0.2.sp, NI0.6.sp, NI.1.sp, penNI0.2.sp, penNI0.6.sp, penNI.1.sp, nrow = 4, ncol = 3)
