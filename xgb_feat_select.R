
library(xgboost)
#导入特征数据
train=read.csv('C:\\Users\\zc_zs\\multivariate\\feat1_train_noID.csv',nrows=10000)
test=read.csv('C:\\Users\\zc_zs\\multivariate\\feat1_test_noID.csv',nrows=50)
y=read.csv('C:\\Users\\zc_zs\\Documents\\Tencent Files\\923162296\\FileRecv\\feat1_y.csv',nrows=10000)
y<-y$obs

param0 <- list("objective"  = "reg:linear" 
               , "eval_metric" = "mae"
               , "eta" = 0.007
               , "subsample" = 0.7
               , "min_child_weight" = 10    
               , "max_depth" = 12#8
               #               , "nthreads" = 4
)
xgtrain1 = xgb.DMatrix(as.matrix(train), label = y, missing = NA)


model_xgb <- xgb.train(params = param0, data = xgtrain1 , nrounds =100,print_every_n=20)
names <- dimnames(data.matrix(train))[[2]]
importance_matrix <- xgb.importance(names, model = model_xgb)
xgb.plot.importance(importance_matrix[1:10,],)
importance<-data.frame(importance_matrix[,1],importance_matrix[,2])
write.csv(importance, "C:\\Users\\zc_zs\\multivariate\\importance.csv",row.names =F)

