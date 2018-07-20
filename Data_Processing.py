import seaborn as sns
import csv
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
%matplotlib inline
sns.set()

from IPython.display import display, Latex, Markdown
from client.api.notebook import Notebook

train=pd.read_csv("train.csv")
test=pd.read_csv("test.csv")

# Remove ids with only NaNs in the "Ref" column 
train_raw_tmp = train[~np.isnan(train.Ref)]
raw_ids_tmp = train_raw_tmp["Id"].unique()
train_new = train[np.in1d(raw_ids_all, raw_ids_tmp)]

len(train)
len(train_new)

train_new.to_csv("train_new.csv")
train_new_group = train_new.groupby('Id')
df = pd.DataFrame(train_new_group['Expected'].mean()) 
df.describe()

#plot for expected value
sns.distplot(df["Expected"],color="b",label="expected value")
plt.ylabel("frequency")
plt.xlabel("the distribution of expected value")
plt.title("the distribution of expected value")

sns.distplot(df[df["Expected"]<300*0.254]["Expected"],color="b",label="expected value")
plt.ylabel("frequency")
plt.xlabel("the distribution of expected value")
plt.title("the distribution of expected value")

sns.distplot(np.log(df[df["Expected"]<300*0.254]["Expected"]),color="b",label="expected value")
plt.ylabel("frequency")
plt.xlabel("the distribution of expected value")
plt.title("the distribution of expected value")