import json
import lightgbm as lgb
import pandas as pd
from sklearn.metrics import mean_squared_error
import numpy as np
#import xgboost as xgb
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn import preprocessing
import time
%matplotlib inline

df_train = pd.read_csv('C:/Users/zc_zs/multivariate/feat1_train_noID.csv', nrows=10000)
df_test = pd.read_csv('C:/Users/zc_zs/multivariate/feat1_test_noID.csv')
y_train = pd.read_csv('C:/Users/zc_zs/Documents/Tencent Files/923162296/FileRecv/feat1_y.csv',nrows=10000)
flg_train = np.random.choice([False, True], len(df_train), p=[0.2, 0.8])
flg_valid = np.logical_not(flg_train)
dt_lgb   = lgb.Dataset(df_train[flg_train], y_train[flg_train]["obs"])
dv_lgb   = lgb.Dataset(df_train[flg_valid], y_train[flg_valid]["obs"], reference=dt_lgb)
test=df_test.values

params = {
    'task': 'train',
    'boosting_type': 'gbdt',
    'objective': 'regression',
    'metric': { 'mae'},
    'num_leaves': 2**12,
    'learning_rate': 0.05,
    'feature_fraction': 0.9,
    'bagging_fraction': 0.9,
    'bagging_freq': 5,
    'verbose': 0
}


# train
scores_lgb_1 = {}
time_s = time.time()
gbm = lgb.train(
    params, dt_lgb, num_boost_round=100,
    valid_sets=dv_lgb,
    early_stopping_rounds=5)

time_t = time.time()
print(time_t - time_s)

# predict
y_pred = gbm.predict(df_test)


feature_imp=pd.DataFrame({
        'column': gbm.feature_name(),
        'importance': gbm.feature_importance(),
    }).sort_values(by='importance',ascending=False)

plt.figure(figsize=(15,30))
ax=sns.barplot(y="column",x="importance",data=feature_imp)
plt.xlabel("the importance of feature")
plt.ylabel("feature")


