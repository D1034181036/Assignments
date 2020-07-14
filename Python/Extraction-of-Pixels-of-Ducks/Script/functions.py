# -*- coding: utf-8 -*-

import numpy as np
import cv2
from os import walk

#取得圖片每個像素的RGB陣列
def Image_RGB(input_src, input_class):
    data=[]
    target=[]
    files = []
    
    for (dirpath, dirnames, filenames) in walk(input_src):
        files.extend(filenames)
        break

    for x in files:
        src = input_src + x
        img = cv2.imread(src, cv2.IMREAD_COLOR)

        for i in range(img.shape[0]):
            for j in range(img.shape[1]):
                data.append(img[i,j])
                target.append(input_class)
    return(data, target)


#圖片辨識與處理(將1標示為紅點)
def Classifier_Red(input_src, classifier):
    count=0
    img = cv2.imread(input_src, cv2.IMREAD_COLOR)
    
    for i in range(img.shape[0]):
        data=[]
        
        for j in range(img.shape[1]) :
            data.append(img[i,j])

        data = np.array(data)
        predict = classifier.predict(data)
            
        for j in range(img.shape[1]) :
            if(predict[j]==1):
                img[i,j]=0,0,255
                count += 1
        del data
    return(img, count)


#圖片辨識與處理(將0標示為黑點)
def Classifier_Black(input_src, classifier):
    count=0
    img = cv2.imread(input_src, cv2.IMREAD_COLOR)
    cf = classifier
    
    for i in range(img.shape[0]):
        data=[]
        
        for j in range(img.shape[1]) :
            data.append(img[i,j])

        data = np.array(data)
        predict = cf.predict(data)
            
        for j in range(img.shape[1]) :
            if(predict[j]==0):
                img[i,j]=0,0,0
            else:
                count += 1
        del data
    return(img, count)
