library(data.table)
library(fastmatch)
library(zoo)
library(xgboost)
library(dplyr)

##########FUNCTIONS###########
entropy <- function(s){#定义函数计算熵
  freq = prop.table(table(s))#then one gets x/sum(x)
  -sum(freq * log(freq, base = 2))#-sum(x*log2(x))
}

gini <- function(s){#定义函数计算GINI指数
  freq = prop.table(table(s))
  1 - sum(freq^2)
}

#Fast %in%
`%fin%` <- function(x, lkup) {
  fmatch(x, lkup, nomatch = 0L) > 0L
}

#Get the time differences between each measure
time_difference <- function(times, num_per_segment = 60) {
  n <- length(times)
  valid_time <- vector(mode="numeric", length = n)
  valid_time[1] <- times[1]
  valid_time[-1] <- diff(times, 1)#valid_time删去第一个元素后的剩余元素为时间差
  valid_time[n] <- valid_time[n] + num_per_segment - sum(valid_time)#最后一个时间差
  valid_time <- valid_time / num_per_segment
  valid_time
}

#Convert reflectivity (dbz) to mm/hr
marshall_palmer <- function(dbz) {
  ((10**(dbz/10))/200) ** 0.625
}

##Valid values based on 0.01in measurements
valid_vals <- 0.254 * 1:300

######Training Data############
tr_raw0 <- fread("D:/R temp/rain/train_new.csv")[,-1]
tr_raw <- tr_raw0
#partition the raw data into a non-outlier one and an outlier one
#tr_raw<- tr_raw[round(Expected, 4) %fin% valid_vals]
deleterow <- which(tr_raw$Expected>300*0.254)
tr_raw<- tr_raw[-deleterow,]
tr_raw_outlier <- tr_raw[deleterow,]
#保留每个Id的最后一条记录
tmp  <-  unique(tr_raw, by="Id", fromLast=T)
ID <- tmp$Id
tmp$Id <-  NULL; tmp$Expected <- NULL
tmp2 <-  as.data.frame(tmp)
names(tmp2)<- paste0(names(tmp2),"_last")


#保留每个Id的第一条记录
tmp = unique(tr_raw, by="Id", fromLast=F)
tmp$Id = NULL; tmp$Expected=NULL
names(tmp)<- paste0(names(tmp),"_first")
tmp2 = cbind(tmp2,as.data.frame(tmp))

tr_raw <- tr_raw%>%
          group_by(Id) %>% 
          mutate(dt=time_difference(minutes_past),mp=marshall_palmer(Ref))

#Collapse to one record per Id
tr <- tr_raw%>%
  group_by(Id) %>% 
  summarise( target = log1p(mean(Expected, na.rm = T)),
             records = length(Id),
             rdist = mean(radardist_km, na.rm=T)
             #naCounts = sum(is.na(Ref))
             )
train_y <- tr_raw%>%
  group_by(Id) %>% 
  summarise(obs = mean(Expected, na.rm = T))
train_y_outlier <- tr_raw%>%
  group_by(Id) %>% 
  summarise(obs = mean(Expected, na.rm = T))

tr2 <- tr_raw%>%
  group_by(Id)%>%
  summarise(#mp
            mp = sum(dt * mp, na.rm = T),
            sddtmp = sd(dt * mp, na.rm = T), 
            sdmp = sd(mp, na.rm=T),
            summp = sum(abs(mp), na.rm=T),
            meanmp = mean(mp, na.rm=T),
            diffsdmp = sd(diff(mp), na.rm=T),
            #Ref,Ref_composite, Ref_5x5_quantile
            meanref = mean(dt * Ref, na.rm = T),
            meanref1 = mean(dt * RefComposite, na.rm = T),
            sdref = sd(dt * Ref, na.rm = T),
            sdref1 = sd(dt * RefComposite, na.rm = T),
            medref1 = median(dt * Ref_5x5_10th, na.rm = T),#
            medref2 = median(dt * Ref_5x5_50th, na.rm = T),#
            medref3 = median(dt * Ref_5x5_90th, na.rm = T),#
            medref4 = median(dt * RefComposite_5x5_10th, na.rm = T),#  
            medref5 = median(dt * RefComposite_5x5_50th, na.rm = T),#
            medref6 = median(dt * RefComposite_5x5_90th, na.rm = T),#
            refmin =ifelse (sum(is.na(Ref_5x5_90th))==length(Id),0,Ref_5x5_10th[which.max(Ref_5x5_90th)]), 
            refmax =ifelse (sum(is.na(Ref_5x5_10th))==length(Id),0,Ref_5x5_90th[which.max(Ref_5x5_10th)]),
            #RhoHV,Zdr,Kdp,5x5_quantile,
            meanrho = mean(RhoHV, na.rm=T),
            sdrho = sd(RhoHV, na.rm=T),
            meanzdr = mean(Zdr, na.rm=T),
            sdzdr = sd(Zdr, na.rm=T),
            meankdp = mean(Kdp, na.rm=T),
            sdkdp = sd(Kdp, na.rm=T),
            medkdp = median(dt * Kdp_5x5_10th, na.rm = T),
            medzdr = median(dt * Zdr_5x5_10th, na.rm = T),
            #dt,熵与信息增益函数计算（关于dt与zdr）
            lenu1 = length(unique(dt)), 
            tmax = max(minutes_past, na.rm=T),
            entu = entropy(dt),
            entu2 = entropy(diff(dt))
            giniP = gini(dt),
            entz = entropy(Zdr),
            entk = entropy(RefComposite_5x5_10th),
            reft1 = ifelse (sum(is.na(Ref))==length(Id),0,minutes_past[which.max(Ref)]),
            cut2 = sum(diff(dt)==0, na.rm=T),
            cut3 = sum(diff(dt)>0, na.rm=T), 
            cut1 = sum(Ref > 40, na.rm=T),
            cut4 = sum(RefComposite_5x5_90th > 40, na.rm=T), 
            cut5 = sum(RefComposite_5x5_10th > 40, na.rm=T)
            # cor1 =  cor(Ref, Ref_5x5_50th, use="pairwise.complete.obs"),
            # cor2 = cor(RefComposite_5x5_10th,RefComposite_5x5_90th, use="pairwise.complete.obs"), 
            # cor3 = cor(Ref_5x5_90th, RefComposite_5x5_90th, use="pairwise.complete.obs"), 
            # cor4 = cor(is.na(Ref), is.na(RefComposite), use="pairwise.complete.obs"),
            # cor5 = cor(is.na(Kdp), is.na(Zdr), use="pairwise.complete.obs"), 
            # cor6 = cor(is.na(Ref), is.na(RhoHV), use="pairwise.complete.obs"),
            # cor7 = cor(is.na(RhoHV_5x5_10th), is.na(RhoHV_5x5_90th), use="pairwise.complete.obs")
            )
tr2<-as.data.frame(tr2)
tmp2 <-  cbind(tmp2, tr2)


print("training model...")
cs <- c("meanref", "meanref1", "mp", "rd", "records")
y<-tr$target #store the log(1+rainobs)
tr<-as.data.frame(tr)
tr<-tr[,cs]
tr <-  cbind(ID,tr,tmp2)
tr[is.na(tr)] <- 0


write.csv(tr, "feat1_train_withID.csv", row.names = FALSE)
write.csv(tr[,-1], "feat1_train_noID.csv", row.names = FALSE)
write.csv(train_y, "feat1_y.csv", row.names = FALSE)
write.csv(train_y_outlier, "feat1_y_outlier.csv", row.names = FALSE)



# rm(tr,tr_raw) #remove tr and tr_raw
# gc()


#########################test data processing 
print("Processing test data...")
# te_raw<-fread("test.csv")
# te_raw0 <- te_raw
te_raw <- te_raw0[1:1000,]

#保留每个Id的最后一条记录
tmp <-  unique(te_raw, by="Id", fromLast=T)
tmp$Id <-  NULL
tmp2 <-  as.data.frame(tmp)
names(tmp2)<- paste0(names(tmp2),"_last")


#保留每个Id的第一条记录
tmp = unique(te_raw, by="Id", fromLast=F)
ID <- tmp$Id
tmp$Id = NULL
names(tmp)<- paste0(names(tmp),"_first")
tmp2 = cbind(tmp2,as.data.frame(tmp))

te_raw <- te_raw%>%
          group_by(Id) %>% 
          mutate(dt=time_difference(minutes_past),mp = marshall_palmer(Ref))

te <- te_raw%>%
  group_by(Id) %>% 
  summarise(
    target = log1p(mean(Expected, na.rm = T)),
    records = length(Id),
    rdist = mean(radardist_km, na.rm=T)
    #naCounts = sum(is.na(Ref))
    )
te<-as.data.frame(te)


te2 <- te_raw%>%
  group_by(Id) %>% 
  summarise(
    #mp
    mp = sum(dt * mp, na.rm = T),
    sddtmp = sd(dt * mp, na.rm = T), 
    sdmp = sd(mp, na.rm=T),
    summp = sum(abs(mp), na.rm=T),
    meanmp = mean(mp, na.rm=T),
    diffsdmp = sd(diff(mp), na.rm=T),
    #Ref,Ref_composite, Ref_5x5_quantile
    meanref = mean(dt * Ref, na.rm = T),
    meanref1 = mean(dt * RefComposite, na.rm = T),
    sdref = sd(dt * Ref, na.rm = T),
    sdref1 = sd(dt * RefComposite, na.rm = T),
    medref1 = median(dt * Ref_5x5_10th, na.rm = T),#
    medref2 = median(dt * Ref_5x5_50th, na.rm = T),#
    medref3 = median(dt * Ref_5x5_90th, na.rm = T),#
    medref4 = median(dt * RefComposite_5x5_10th, na.rm = T),#  
    medref5 = median(dt * RefComposite_5x5_50th, na.rm = T),#
    medref6 = median(dt * RefComposite_5x5_90th, na.rm = T),#
    refmin =ifelse (sum(is.na(Ref_5x5_90th))==length(Id),0,Ref_5x5_10th[which.max(Ref_5x5_90th)]), 
    refmax =ifelse (sum(is.na(Ref_5x5_10th))==length(Id),0,Ref_5x5_90th[which.max(Ref_5x5_10th)]),
    #RhoHV,Zdr,Kdp,5x5_quantile,
    meanrho = mean(RhoHV, na.rm=T),
    sdrho = sd(RhoHV, na.rm=T),
    meanzdr = mean(Zdr, na.rm=T),
    sdzdr = sd(Zdr, na.rm=T),
    meankdp = mean(Kdp, na.rm=T),
    sdkdp = sd(Kdp, na.rm=T),
    medkdp = median(dt * Kdp_5x5_10th, na.rm = T),
    medzdr = median(dt * Zdr_5x5_10th, na.rm = T),
    #dt,熵与信息增益函数计算（关于dt与zdr）
    lenu1 = length(unique(dt)), 
    tmax = max(minutes_past, na.rm=T),
    entu = entropy(dt),
    entu2 = entropy(diff(dt)),
    giniP = gini(dt),
    entz = entropy(Zdr),
    entk = entropy(RefComposite_5x5_10th),
    reft1 = ifelse (sum(is.na(Ref))==length(Id),0,minutes_past[which.max(Ref)]),
    cut2 = sum(diff(dt)==0, na.rm=T),
    cut3 = sum(diff(dt)>0, na.rm=T), 
    cut1 = sum(Ref > 40, na.rm=T),
    cut4 = sum(RefComposite_5x5_90th > 40, na.rm=T), 
    cut5 = sum(RefComposite_5x5_10th > 40, na.rm=T)
    # cor1 =  cor(Ref, Ref_5x5_50th, use="pairwise.complete.obs"),
    # cor2 = cor(RefComposite_5x5_10th,RefComposite_5x5_90th, use="pairwise.complete.obs"), 
    # cor3 = cor(Ref_5x5_90th, RefComposite_5x5_90th, use="pairwise.complete.obs"), 
    # cor4 = cor(is.na(Ref), is.na(RefComposite), use="pairwise.complete.obs"),
    # cor5 = cor(is.na(Kdp), is.na(Zdr), use="pairwise.complete.obs"), 
    # cor6 = cor(is.na(Ref), is.na(RhoHV), use="pairwise.complete.obs"),
    # cor7 = cor(is.na(RhoHV_5x5_10th), is.na(RhoHV_5x5_90th), use="pairwise.complete.obs")
  )
te2<-as.data.frame(te2)
tmp2 <-  cbind(tmp2, te2)


te<-te[,cs]
te <-  cbind(ID,te,tmp2)
te[is.na(te)] <- 0

gc()
write.csv(te, "D:/R temp/rain/feat1_test_withID.csv", row.names = FALSE)
write.csv(te[,-1], "D:/R temp/rain/feat1_test_noID.csv", row.names = FALSE)




