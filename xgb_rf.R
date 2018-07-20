library(data.table)
library(h2o)

# h2o RF
nthreads <- -1
ntrees <- 1000
max_depth <- 20
min_rows <- 4
sample_rate <- 0.7


# merging the xgb preds
train_xgb <- fread("train_xgb.csv")
train <- cbind(train_xgb,feature_train_nout)
y<-log1p(trainY_nout)
namesx<-names(train)
namesy<-names(y)
train<-cbind(train,y)
## h2o random forest
h2o.init(nthreads=nthreads)
h2o.init(nthreads = -1, max_mem_size = '2g', ip = "127.0.0.1", port = 50001)
trainHex<-as.h2o(train, destination_frame="train.hex")
rfHex<-h2o.randomForest(namesx, y=namesy, training_frame=trainHex,model_id="rf.hex", ntrees=ntrees, max_depth=max_depth, min_rows=min_rows, sample_rate=sample_rate)

#test

# merging the xgb preds
test_xgb <- fread("test_xgb7.csv")
test <- cbind(feature_test, test_xgb)


## scoring the test data
testHex <- as.h2o(test, destination_frame="test.hex")
predictions <- as.data.frame(h2o.predict(rfHex,testHex))


# submission
submission <- fread("sample_solution.csv")
submission$Expected <- expm1(predictions$predict) 
submission$Expected <- round(submission$Expected / 0.254) * 0.254

write.csv(submission, "test_rf.csv")