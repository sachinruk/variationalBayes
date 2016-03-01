setwd('~/Documents/variationalBayes/')
set.seed(1)
source('sparseBayes.R')

N = 20
D = 100
sparsity = 0.2
X = matrix(runif(N*D),N)
w = rbinom(100,1,0.1)
w = w*runif(100)
y = X%*%w + rnorm(N)

w_inf = SBR(y,X,50)