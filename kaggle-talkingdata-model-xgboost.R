setwd('/home/dogz/workspace/kaggle/talking/')
options(stringsAsFactors=F,scipen=99)
rm(list=ls());gc()
library(data.table)

gender_age_train = data.table(read.csv('gender_age_train.csv'))
gender_age_test = data.table(read.csv('gender_age_test.csv'))
gender_age_test$gender <- gender_age_test$age <- gender_age_test$group <- NA
gender_age_binded = rbind(gender_age_train, gender_age_test)

phone_brand_device_model = data.table(read.csv('phone_brand_device_model.csv'))
phone_brand_device_model = phone_brand_device_model[!duplicated(phone_brand_device_model), ]

mg = merge(gender_age_binded, phone_brand_device_model, by="device_id", all.x=T)
mg$age = NULL
mg$gender = NULL
rm(gender_age_train, gender_age_test, gender_age_binded, phone_brand_device_model);gc()

events = read.csv('events.csv')
events$timestamp = strptime(events$timestamp, tz = '', format = '%Y-%m-%d %H:%M:%S')
events$day = format(events$timestamp, '%d')
events$month = format(events$timestamp, '%m')
events$year = format(events$timestamp, '%Y')
events$hour = format(events$timestamp, '%H')
events$minute = format(events$timestamp, '%M')
events$second = format(events$timestamp, '%S')
events$weekday = weekdays(events$timestamp)
events$timestamp = NULL
events = data.table(events)

app_events = data.table(read.csv('app_events.csv'))
app_events = app_events[!duplicated(app_events), ]

mge = merge(events, app_events, by="event_id")
rm(events, app_events);gc()
mge = mge[!duplicated(mge), ]

app_labels = data.table(read.csv('app_labels.csv'))
app_labels = app_labels[!duplicated(app_labels), ]

mgl = merge(mge, app_labels, by = 'app_id', allow.cartesian=TRUE)
rm(app_labels, mge);gc()

mf = merge(mg, mgl, by = 'device_id')
rm(mg, mgl);gc()
#mf$event_id = NULL

########## Modeling
mf$group[mf$group == 'F23-'] = 0
mf$group[mf$group == 'F24-26'] = 1
mf$group[mf$group == 'F27-28'] = 2
mf$group[mf$group == 'F29-32'] = 3
mf$group[mf$group == 'F33-42'] = 4
mf$group[mf$group == 'F43+'] = 5
mf$group[mf$group == 'M22-'] = 6
mf$group[mf$group == 'M23-26'] = 7
mf$group[mf$group == 'M27-28'] = 8
mf$group[mf$group == 'M29-31'] = 9
mf$group[mf$group == 'M32-38'] = 10
mf$group[mf$group == 'M39+'] = 11

df = mf
df$group = as.numeric(as.factor(df$group))
df$weekday = as.numeric(as.factor(df$weekday))
df$phone_brand = as.numeric(as.factor(df$phone_brand))
df$device_model = as.numeric(as.factor(df$device_model))
df$day = as.numeric(as.factor(df$day))
df$month = as.numeric(as.factor(df$month))
df$year = as.numeric(as.factor(df$year))
df$hour = as.numeric(as.factor(df$hour))
df$minute = as.numeric(as.factor(df$minute))
df$second = as.numeric(as.factor(df$second))



# dummy
d1 <- df[,list(device_id,phone_brand)]
d2 <- df[,list(device_id,device_model)]
d3 <- df[,list(device_id,longitude)]
d4 <- df[,list(device_id,latitude)]
d5 <- df[,list(device_id,day)]
d6 <- df[,list(device_id,month)]
d7 <- df[,list(device_id,year)]
d8 <- df[,list(device_id,hour)]
d9 <- df[,list(device_id,minute)]
d10 <- df[,list(device_id,second)]
d11 <- df[,list(device_id,weekday)]
d12 <- df[,list(device_id, app_id)]
d13 <- df[,list(device_id, event_id)]
d14 <- df[,list(device_id, is_installed)]
d15 <- df[,list(device_id, is_active)]
d16 <- df[,list(device_id, label_id)]
label = df$group
rm(df)

d1[,phone_brand:=paste0("phone_brand:",phone_brand)]
d2[,device_model:=paste0("device_model:",device_model)]
d3[,longitude:=paste0("longitude:",longitude)]
d4[,latitude:=paste0("latitude:",latitude)]
d5[,day:=paste0("day:",day)]
d6[,month:=paste0("month:",month)]
d7[,year:=paste0("year:",year)]
d8[,hour:=paste0("hour:",hour)]
d9[,minute:=paste0("minute:",minute)]
d10[,second:=paste0("second:",second)]
d11[,weekday:=paste0("weekday:",weekday)]
d12[,app_id:=paste0("app_id:",app_id)]
d13[,event_id:=paste0("event_id:",event_id)]
d14[,is_installed:=paste0("is_installed:",is_installed)]
d15[,is_active:=paste0("is_active:",is_active)]
d16[,label_id:=paste0("label_id:",label_id)]

names(d1) <- names(d2) <- names(d3) <- names(d4) <- names(d5) <- names(d6) <- names(d7) <- names(d8) <- names(d9) <- names(d10) <- names(d11) <- names(d12) <- c("device_id","feature_name")
names(d13) <- names(d14) <- names(d15) <- names(d16) <- c("device_id","feature_name")
dd <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16)
rm(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16);gc()

require(Matrix)
ii <- unique(dd$device_id)
jj <- unique(dd$feature_name)
id_i <- match(dd$device_id,ii)
id_j <- match(dd$feature_name,jj)
id_ij <- cbind(id_i,id_j)
ii = as.character(ii)
M <- Matrix(0, nrow=length(ii), ncol=length(jj), dimnames=list(ii,jj), sparse=T)
M[id_ij] <- 1
rm(ii,jj,id_i,id_j,id_ij,dd);gc()



x <- M[rownames(M) %in% mf$device_id,]
id <- mf$device_id[match(mf$device_id,rownames(x))]
y <- mf$group[match(mf$device_id,rownames(x))]
rm(M)


# level reduction
x_train <- x[!is.na(y),]
tmp_cnt_train <- colSums(x_train)
x <- x[, tmp_cnt_train > 0 & tmp_cnt_train < nrow(x_train)]
rm(x_train,tmp_cnt_train)

require(xgboost)
depth <- 6
shrk <- 0.01
ntree <- 100
(group_name <- na.omit(unique(y)))

set.seed(10)
library(caret)
rn = rownames(data.table(y))
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(y, p=0.20, list=FALSE)
# select 20% of the data for validation
idx_train <- as.numeric(rn[-validation_index])
idx_test <- as.numeric(rn[validation_index])
train_data <- x[idx_train,]
test_data <- x[idx_test,]

y_train <- df[-validation_index,'group', with=F]
# use the remaining 80% of data to training and testing the models

y_test <- df[validation_index,'group', with=F]
train$group = NULL
test$group = NULL

train_label <- match(y[idx_train],group_name)-1
test_label <- match(y[idx_test],group_name)-1

dtrain <- xgb.DMatrix(x,label=y,missing=NA)
dtest <- xgb.DMatrix(test_data,label=test_label,missing=NA)
param <- list(booster="gblinear",
              num_class=length(group_name),
              objective="multi:softprob",
              eval_metric="mlogloss",
              eta=shrk,
              max.depth=depth,
              lambda=0.75,
              lambda_bias=0.5,
              alpha=0.5,
              subsample=0.75,
              colsample_bytree=0.72,
              num_parallel_tree=8)
watchlist <- list(train=dtrain)
# set.seed(114)
# fit_cv <- xgb.cv(params=param,
#                  data=dtrain,
#                  nrounds=ntree*100000,
#                  watchlist=watchlist,
#                  nfold=5,
#                  early.stop.round=3,
#                  verbose=1)

ntree <- 145
set.seed(114)
fit_xgb <- xgb.train(params=param,
                     data=dtrain,
                     nrounds=ntree,
                     watchlist=watchlist,
                     verbose=1)

pred <- predict(fit_xgb,dtest)
pred_detail <- t(matrix(pred,nrow=length(group_name)))
res_submit <- cbind(id=id[idx_test],as.data.frame(pred_detail))
colnames(res_submit) <- c("device_id",group_name)
write.csv(res_submit,file="submit_v0_5_1.csv",row.names=F,quote=F)
