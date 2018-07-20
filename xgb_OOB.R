library(data.table)
library(xgboost)
library(fastmatch)

`%fin%` <- function(x, lkup) {
  fmatch(x, lkup, nomatch = 0L) > 0L
}
MAE <- function(a,b)
{
  sum(abs(a-b))/length(a)
}
#sample_solution<-fread("sample_solution.csv")
feature_train <- mixed1_train
feature_test <- feature_test0
y <- mixed_y$obs

valid_vals <- 0.254 * 1:300
feature_train<-cbind(feature_train,y)
feature_train_out <- feature_train[!(round(y, 4) %fin% valid_vals),]
feature_train_nout <- feature_train[(round(y, 4) %fin% valid_vals),]
trainY_out <- feature_train_out$y
trainY_nout<- feature_train_nout$y
feature_train_out<-subset(feature_train_out,select=-c(y))
feature_train_nout<-subset(feature_train_nout,select=-c(y))

param0 <- list("objective"  = "reg:linear" 
               , "eval_metric" = "rmse"
               , "eta" = 0.007
               , "subsample" = 0.7
               , "min_child_weight" = 10    
               , "max_depth" = 12#8
               #               , "nthreads" = 4
)
xgtrain1 = xgb.DMatrix(as.matrix(feature_train_out), label = trainY_out, missing = NA)
#names(feature_train_out)<-names(feat_test)

set.seed(3)
model_xgb <- xgb.train(params = param0, data = xgtrain1 , nrounds = 1, print_every_n=20)
xgtrain2 = xgb.DMatrix(as.matrix(feature_train_nout), missing = NA)
feat_out = predict(model_xgb, xgtrain2)
cat( "MAE", ": ", MAE(feat_out, trainY_nout), "\n", sep = "")
train1<- as.data.frame(cbind(feature_train_nout,feat_out))

xgtrain3 = xgb.DMatrix(as.matrix(feature_test), missing = NA)
feat_out = predict(model_xgb, xgtrain3)

test1<- as.data.frame(cbind(feature_test,feat_out))
y<-trainY_nout

XGBoost <- function(train,y,X_test=data.frame(),cv=10,transform="none",objective="reg:linear",eta=0.1,max_depth=5,nrounds=50,gamma=0,min_child_weight=1,subsample=1,colsample_bytree=1,seed=123)
{
  # ??????????׼MAE
  #????׼??
  train$order <- seq(1, nrow(train))
  train$Expected <- as.numeric(y)
  train$Expected <- log(train$Expected+1)
  for (i in 1:ncol(train))
  {
    train[,i] <- as.numeric(train[,i])
  }
  for (i in 1:ncol(X_test))
  {
    X_test[,i] <- as.numeric(X_test[,i])
  }    
  
  
  train[is.na(train)] <- 0
  X_test[is.na(X_test)] <- 0
  X_test2=X_test
  
  set.seed(seed)
  
  train$randomCV <- floor(runif(nrow(train), 1, (cv+1)))
  
  # cross-validation
  cat(cv, "-fold Cross Validation\n", sep = "")
  for (i in 1:cv)
  {
    set.seed(seed+i)
    X_build <- subset(train, randomCV != i)
    X_val <- subset(train, randomCV == i)
    
    feature_names <- colnames(subset(X_build, select = -c(order, randomCV, Expected)))
    
    build <- as.matrix(subset(X_build, select = -c(order, randomCV, Expected)))
    val <- as.matrix(subset(X_val, select = -c(order, randomCV, Expected)))
    test <- as.matrix(X_test2)
    
    build_label <- as.matrix(subset(X_build, select = c('Expected')))
    
    # ģ?ͽ?��
    model_xgb <- xgboost(build,build_label,objective=objective,eta=eta,max_depth=max_depth,nrounds=nrounds,gamma=gamma,min_child_weight=min_child_weight,subsample=subsample,colsample_bytree=colsample_bytree,nthread=-1,verbose=0)
    
    
    
    # validation??????
    pred_xgb <- predict(model_xgb, val)
    pred_xgb <- exp(pred_xgb)-1
    
    
    X_val <- cbind(X_val, pred_xgb)
    X_val$Expected1=exp(X_val$Expected)-1
    # test??Ԥ??
    pred_xgb <- predict(model_xgb, test)
    pred_xgb <- exp(pred_xgb)-1
    
    
    cat("CV Fold-", i, " ", "MAE", ": ", MAE(X_val$Expected1, X_val$pred_xgb), "\n", sep = "")
    
    
    if (i == 1)
    {
      output <- X_val
      X_test <- cbind(X_test, pred_xgb)
      
    }
    
    if (i > 1)
    {
      output <- rbind(output, X_val)
      X_test$pred_xgb <- (X_test$pred_xgb * (i-1) + pred_xgb)/i
    }
    
    gc()
  } 
  
  output <- output[order(output$order),]
  output$Expected1=exp(output$Expected)-1
  cat("\nXGBoost ", cv, "-Fold CV ", "MAE", ": ", MAE(output$Expected1, output$pred_xgb), "\n", sep = "")
  
  output <- subset(output, select = c("order", "pred_xgb"))
  
  return(list("train"=output, "test"=X_test))  
}

model_xgb <- XGBoost(train1,y,test1,cv=10,objective="reg:linear",nrounds=100,eta=0.09,max_depth=12,subsample=0.8,min_child_weight=10,colsample_bytree=0.85)

train_xgb <- data.frame("pred_xgb" = model_xgb$train$pred_xgb)
test_xgb <- data.frame("pred_xgb" = model_xgb$test$pred_xgb)
test_xgb2<-test_xgb
#test_xgb2$pred_xgb<-0.94*test_xgb2$pred_xgb+0.06*sample_solution$Expected
test_xgb2$pred_xgb<-round(test_xgb$pred_xgb/ 0.254) * 0.254
test_xgb2 <- cbind(ID,test_xgb2)
names(test_xgb2) <- c('Id','Expected')
test_xgb2
write.csv(test_xgb2, "test_xgb7.csv", row.names = FALSE)

