#导入原始数据
train_raw=read.csv("C:\\Users\\zc_zs\\multivariate\\train.csv")
test_raw=read.csv("C:\\Users\\zc_zs\\multivariate\\test.csv")
#可视化缺失值
library("VIM")  
#缺失值的比例
aggr(train_raw,prop=TRUE,numbers=TRUE)
matrixplot(train_raw) 
train_new=read.csv("C:\\Users\\zc_zs\\multivariate\\train_new.csv")
train_new2=train_new[train_new["Expected"]<300*0.254,]
#除去上述缺失值外，剩余缺失值的比例也很大，
aggr(train_new2,prop=TRUE,numbers=TRUE)
train_new2["nan_sum"]<-apply(train_new2,1,function(x) sum(is.na(x)))
namec<-colnames(train_new2)[1:24]
#对缺失值个数<=12的记录进行随机森林填补
library(missForest)
subset(train_new2,train_new2['nan_sum']<=12,select=namec)<-missForest(subset(train_new2,train_new2['nan_sum']<=12,select=namec))
