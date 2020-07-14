# -*- coding: utf-8 -*-

import numpy as np
from sklearn import datasets
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsClassifier

#讀取iris資料
iris = datasets.load_iris()
iris_X = iris.data
iris_y = iris.target

#將資料分成training data 以及 testing data
X_train, X_test, y_train, y_test = train_test_split(iris_X, iris_y, test_size=0.3)

#定義knn為Classifier並fit訓練資料
knn = KNeighborsClassifier()
knn.fit(X_train, y_train)
print knn

#將預測資料與測試資料輸出做比對
print(knn.predict(X_test))
print(y_test)

