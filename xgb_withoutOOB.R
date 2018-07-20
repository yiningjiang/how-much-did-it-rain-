


library(xgboost)
#
train <- mixed1_train
test <- feature_test
y <- mixed_y
y <- y$obs
train <- as.data.frame(train)
test <- as.data.frame(test)
# MAE
  MAE <- function(a,b)
  {
    sum(abs(a-b))/length(a)
  }
  
#XGBoost
XGBoost <- function(train,y,X_test=data.frame(),cv=10,transform="none",objective="reg:linear",eta=0.1,max_depth=12,nrounds=100,gamma=0,min_child_weight=10,subsample=1,colsample_bytree=0.85,seed=123)
{
  
  
  #
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
    X_build <- subset(train, randomCV != i)
    X_val <- subset(train, randomCV == i)
    
    feature_names <- colnames(subset(X_build, select = -c(order, randomCV, Expected)))
    
    build <- as.matrix(subset(X_build, select = -c(order, randomCV, Expected)))
    val <- as.matrix(subset(X_val, select = -c(order, randomCV, Expected)))
    test <- as.matrix(X_test2)
    
    build_label <- as.matrix(subset(X_build, select = c('Expected')))
    
    # 
    model_xgb <- xgboost(build,build_label,objective=objective,eta=eta,max_depth=max_depth,nrounds=nrounds,gamma=gamma,min_child_weight=min_child_weight,subsample=subsample,colsample_bytree=colsample_bytree,nthread=-1,verbose=0)
    
    
    
    # validation?
    pred_xgb <- predict(model_xgb, val)
    pred_xgb <- exp(pred_xgb)-1
    
    
    X_val <- cbind(X_val, pred_xgb)
    X_val$Expected1=exp(X_val$Expected)-1
    # test
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

model_xgb <- XGBoost(train,y,test,cv=5,objective="reg:linear",nrounds=100,eta=0.05,max_depth=12,subsample=0.8,min_child_weight=10,colsample_bytree=0.85)

test_xgb=data.frame("pred_xgb"=model_xgb$test$pred_xgb)
train_xgb=data.frame("pred_xgb"=model_xgb$train$pred_xgb)

#feature1
#5-fold Cross Validation
#CV Fold-1 MAE: 2.902647
#CV Fold-2 MAE: 2.899063
#CV Fold-3 MAE: 2.908984
#CV Fold-4 MAE: 2.887983
#CV Fold-5 MAE: 2.890899

#XGBoost 5-Fold CV MAE: 2.897917

#feature2
#5-fold Cross Validation
#CV Fold-1 MAE: 2.902617
#CV Fold-2 MAE: 2.899076
#CV Fold-3 MAE: 2.908982
#CV Fold-4 MAE: 2.887954
#CV Fold-5 MAE: 2.890943

#XGBoost 5-Fold CV MAE: 2.897917

id <- fread('feat1_test_withID.csv')
ID <- id$ID
test_xgb <- cbind(ID,test_xgb)
names(test_xgb) <- c('Id','Expected')
#write.csv(test_xgb, "test_xgb1.csv", row.names = FALSE)
#write.csv(test_xgb, "test_xgb2.csv", row.names = FALSE)
write.csv(test_xgb, "test_xgb5.csv", row.names = FALSE)

# 5-fold Cross Validation
# CV Fold-1 MAE: 2.286692
# CV Fold-2 MAE: 2.273906
# CV Fold-3 MAE: 2.293508
# CV Fold-4 MAE: 2.272868
# CV Fold-5 MAE: 2.273014

