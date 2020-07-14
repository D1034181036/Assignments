# -*- coding: utf-8 -*-

import numpy as np
import cv2
import time
from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import GaussianNB

#引入自定義functions
import sys;
sys.path.append("Script")
import functions

class Duck:

    def __init__(self, config):
        self.config = config

    def run(self):
        config = self.config
        #計時開始
        tStart = time.time()

        #輸入訓練資料集
        data,target = functions.Image_RGB(config['train_src'], 0)
        print("加入non_ducks資料完成")
        data2,target2 = functions.Image_RGB(config['train_src2'], 1)
        print("加入ducks資料完成")

        #將資料串在一起
        data += data2
        target += target2

        #轉換成numpy_array
        data = np.array(data)
        target = np.array(target)

        #將資料分成訓練資料及測試資料
        X_train, X_test, y_train, y_test = train_test_split(data, target, test_size=config['test_size'])

         #定義、訓練分類器gnb
        Gnb = GaussianNB()
        Gnb.fit(X_train, y_train)
        print("分類器訓練完成")

        #測試分類器gnb的準確度
        Accuracy = float((y_test == Gnb.predict(X_test)).sum())/float(y_test.shape[0])
        print("分類器準確度:" + str(Accuracy))

        #將要辨識的圖片輸入給分類器做處理
        if(config['replace_type']==0):
            img, count = functions.Classifier_Red(config['input_src'], Gnb)
        elif(config['replace_type']==1):
            img, count = functions.Classifier_Black(config['input_src'], Gnb)
        print("圖片處理完成")

        #輸出辨識的資訊
        print("共有 %d 點像素被分類為鴨子" % count)

        #圖片輸出
        cv2.imwrite(config['output_src'], img)
        print("圖片已輸出，路徑: %s" % config['output_src'])

        #計算執行時間
        tEnd = time.time()
        print("程式執行時間: %d 秒" % round(tEnd-tStart))