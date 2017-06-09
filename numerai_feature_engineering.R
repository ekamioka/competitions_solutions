setwd('~/workspace/numerai/')

library(Rtsne)
library(caret)
library(MLmetrics)

train = read.csv('numerai_datasets/numerai_training_data.csv')
test = read.csv('numerai_datasets/numerai_tournament_data.csv')

### Generating the TSNE features 
t_id = test$t_id
test = test[,-1]

y = train[,ncol(train)]

x = rbind(train[,-ncol(train)], test)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))

#x = 1/(1+exp(-sqrt(x)))

set.seed(1012)
tsne <- Rtsne(as.matrix(x), dims = 2, check_duplicates = FALSE, pca = TRUE, perplexity=30, theta=0.5) #0.68926

tsne <- Rtsne(as.matrix(x), dims = 2, check_duplicates = FALSE, pca = TRUE, perplexity=60, theta=0.5) #0.68926

tsne <- Rtsne(as.matrix(x), dims = 3, check_duplicates = FALSE, pca = FALSE, perplexity=30, theta=0.5) #0.69174

tsne <- Rtsne(as.matrix(x), dims = 3, check_duplicates = FALSE, pca = FALSE, perplexity=60, theta=0.5) #0.69174


Rtsne(train, dims = 2, initial_dims = 50, perplexity = 30,
      theta = 0.5, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
      verbose = FALSE, is_distance = FALSE, Y_init = NULL)

#x = round(x, 3)
x = cbind(x, tsne$Y[,1])
x = cbind(x, tsne$Y[,2])
x = cbind(x, tsne$Y[,3])

trind = 1:length(y)
teind = (nrow(train)+1):nrow(x)

trainX = data.frame(x[trind,])
trainX$target = y 
testX = data.frame(x[teind,])

write.csv(trainX, 'trainX-tsne3D-perp30-theta0.5.csv', quote = F, row.names = F)
write.csv(testX, 'testX-tsne3D-perp30-theta0.5.csv', quote = F, row.names = F)

trainX = read.csv('trainX-tsne2D-perp30-theta0.5.csv')
testX = read.csv('testX-tsne2D-perp30-theta0.5.csv')

### Generating the binning features
bina <- function(x){
  return(as.numeric(ifelse(x > 0.5,1,0)))
}

trinca <- function(x){
  return(as.numeric(ifelse(x <= 0.33, 0, ifelse(x > 0.33 & x < 0.63 ,1, 2))))
}

quadra <- function(x){
  return(as.numeric(ifelse(x <= 0.25, 0, ifelse(x > 0.25 & x <= 0.50 ,1, ifelse(x > 0.50 & x <= 0.75 ,3, 4)))))
}

# binning train
train_bina = data.frame(sapply(train, bina))
train_bina$target = NULL
lbin = names(train_bina)
nnames = vector()
for(i in seq(1,length(lbin))){
  nnames[i] = paste0(lbin[i],'bina',collapse = '_')
}
train_bina = setNames(train_bina, nnames)

train_trinca = data.frame(sapply(train, trinca))
train_trinca$target = NULL
lbin = names(train_trinca)
nnames = vector()
for(i in seq(1,length(lbin))){
  nnames[i] = paste0(lbin[i],'trinca',collapse = '_')
}
train_trinca = setNames(train_trinca, nnames)

train_quadra = data.frame(sapply(train, quadra))
train_quadra$target = NULL
lbin = names(train_quadra)
nnames = vector()
for(i in seq(1,length(lbin))){
  nnames[i] = paste0(lbin[i],'quadra',collapse = '_')
}
train_quadra = setNames(train_quadra, nnames)

# binning test
test_bina = data.frame(sapply(test, bina))
test_bina$target = NULL
lbin = names(test_bina)
nnames = vector()
for(i in seq(1,length(lbin))){
  nnames[i] = paste0(lbin[i],'bina',collapse = '_')
}
test_bina = setNames(test_bina, nnames)

test_trinca = data.frame(sapply(test, trinca))
test_trinca$target = NULL
lbin = names(test_trinca)
nnames = vector()
for(i in seq(1,length(lbin))){
  nnames[i] = paste0(lbin[i],'trinca',collapse = '_')
}
test_trinca = setNames(test_trinca, nnames)

test_quadra = data.frame(sapply(test, quadra))
test_quadra$target = NULL
lbin = names(test_quadra)
nnames = vector()
for(i in seq(1,length(lbin))){
  nnames[i] = paste0(lbin[i],'quadra',collapse = '_')
}
test_quadra = setNames(test_quadra, nnames)

### Generating polynomials

p2 <- function(x){
  return(x ^ 2 )
}

p3 <- function(x){
  return(x ^ 3 )
}

p4 <- function(x){
  return(x ^ 4 )
}

p5 <- function(x){
  return(x ^ 5 )
}

p6 <- function(x){
  return(x ^ 6 )
}

p7 <- function(x){
  return(x ^ 7 )
}

test_poly = data.frame(sapply(test, p2))
test_poly$target = NULL
lbin = names(test_poly)
nnames = vector()
for(i in seq(1,length(lbin))){
  nnames[i] = paste0(lbin[i],'p2',collapse = '_')
}
test_poly = setNames(test_poly, nnames)


# joing
train_bins = cbind(train_bina, train_trinca, train_quadra)
test_bins = cbind(t_id, test_bina, test_trinca, test_quadra)

rm(train_bina, train_trinca, train_quadra, lbin, i, nnames, test_bina, test_trinca, test_quadra)
gc()

### Joing TSNE with BINS
trainX = cbind(trainX, train_bins)
testX = cbind(testX, test_bins)

label = trainX$target
trainX$target = NULL
trainX = cbind(trainX, label)

write.csv(trainX, 'trainx-ulti.csv', quote = F, row.names = F)
write.csv(testX, 'testx-ulti.csv', quote = F, row.names = F)
