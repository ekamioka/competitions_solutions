setwd('~/workspace/numerai/')

library(Rtsne)
library(caret)
library(MLmetrics)

## 80% of the sample size
smp_size <- floor(0.80 * nrow(trainX))

## set the seed to make your partition reproductible
set.seed(1337)
train_ind <- sample(seq_len(nrow(trainX)), size = smp_size)

training <- trainX[train_ind, ]
testing <- trainX[-train_ind, ]
testing_y = testing$target
testing$target = NULL

model <- glm(target ~., family=poisson(link = "log"), data=training) #15.91233

model <- glm(target ~ X2 + X3 + X6 + X14 + X4 + X8 + X22 + X23, family=poisson(link = "log"), data=training) #16.30496

model <- glm(target ~ X1 + X2 + X3 + X4 + X5 + X9 + X10 + X11 + X16 + X18 + X22 + X23, family=poisson(link = "log"), data=training) #

model <- glm(target ~ X2 + X3 + X6 + X14 + X4 + X8 + X22 + X23 + X1 + X9 + X11 + X12 + X15 + X19 + X21, family=poisson(link = "log"), data=training) #15.993

model <- glm(target ~ X22 + X23 + X24 + X1dpers60 + X2dpers60 + X1dpers30 + X2dpers30, family=poisson(link = "log"), data=training) #16.67252

anova(model, test="Chisq")
pred <- predict(model, newdata=testing, type='response')
pred <- as.numeric(ifelse(pred > 0.5,1,0))
LogLoss(testing_y, as.numeric(pred))


probability = predict(model, testX, type = 'response')
sub = data.frame(t_id, probability)
write.csv(sub, 'lr-tsne-2dpers30-all-local-16.65071-lb-.csv', quote = F, row.names = F)


# Make prediction
pred = predict(bst,testX)
pred = matrix(pred,9,length(pred)/9)
pred = t(pred)


tmpC = 1:240
tmpL = length(trind)
gtree = 200
for (z in tmpC) {
  print(z)
  tmpS1 = sample(trind,size=tmpL,replace=T)
  tmpS2 = setdiff(trind,tmpS1)
  
  tmpX2 = trainX[tmpS2,]
  tmpY2 = y[tmpS2]
  
  cst = randomForest(x=tmpX2, y=as.factor(tmpY2), replace=F, ntree=100, do.trace=T, mtry=7)
  
  tmpX1 = trainX[tmpS1,]
  tmpY1 = y[tmpS1]
  
  tmpX2 = predict(cst, tmpX1, type="prob")
  tmpX3 = predict(cst, testX, type="prob")
  
  bst = xgboost(param=param, data = cbind(tmpX1,tmpX2), label = tmpY1, column_subsample = 0.8, 
                nrounds=60, max.depth=11, eta=0.46, min_child_weight=10) 
  
  # Make prediction
  pred0 = predict(bst,cbind(testX,tmpX3))
  pred0 = matrix(pred0,9,length(pred0)/9)
  pred = pred + t(pred0)
}
pred = pred/(z+1)

pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))
write.csv(pred,file='/home/mikeskim/Desktop/kaggle/otto/data/ottoHomeBagG4.csv', quote=FALSE,row.names=FALSE)