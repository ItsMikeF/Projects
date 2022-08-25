import pandas as pd
import numpy as np

#generate random data
np.random.seed(0)
x = np.random.randn(100)
y = np.random.randn(100)

#write data to csv file
df = pd.DataFrame({'x':x, 'y':y})
df.to_csv('data.csv', index=False)

#read data from csv file
df = pd.read_csv('data.csv')
print(df)

#read data from csv file and plot it
import matplotlib.pyplot as plt
plt.scatter(df['x'], df['y'])

#lets predict the y value
df['y_pred'] = df['x'] + df['x']**2
print(df)

#cumulative sum of the squared errors
df['err'] = (df['y_pred'] - df['y'])**2
print(df)

#fuck it, lets do it again, this time with a for loop'
df['err'] = 0
for i in range(len(df)):
    df['err'][i] = (df['y_pred'][i] - df['y'][i])**2
print(df)

#lets do it again, this time with a list comprehension
df['err'] = [(df['y_pred'][i] - df['y'][i])**2 for i in range(len(df))]
print(df)

#lets do it again, this time with a list comprehension and a lambda function
df['err'] = [lambda x: (x['y_pred'][i] - x['y'][i])**2 for i in range(len(df))]
print(df)

