#����ԭʼ����
train_raw=read.csv("C:\\Users\\zc_zs\\multivariate\\train.csv")
test_raw=read.csv("C:\\Users\\zc_zs\\multivariate\\test.csv")
#���ӻ�ȱʧֵ
library("VIM")  
#ȱʧֵ�ı���
aggr(train_raw,prop=TRUE,numbers=TRUE)
matrixplot(train_raw) 
train_new=read.csv("C:\\Users\\zc_zs\\multivariate\\train_new.csv")
train_new2=train_new[train_new["Expected"]<300*0.254,]
#��ȥ����ȱʧֵ�⣬ʣ��ȱʧֵ�ı���Ҳ�ܴ�
aggr(train_new2,prop=TRUE,numbers=TRUE)
train_new2["nan_sum"]<-apply(train_new2,1,function(x) sum(is.na(x)))
namec<-colnames(train_new2)[1:24]
#��ȱʧֵ����<=12�ļ�¼�������ɭ���
library(missForest)
subset(train_new2,train_new2['nan_sum']<=12,select=namec)<-missForest(subset(train_new2,train_new2['nan_sum']<=12,select=namec))