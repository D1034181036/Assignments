# -*- coding: utf-8 -*-

import numpy as np
import cv2

#以RGB方式載入圖片
img = cv2.imread('images/full_ducks.jpg', cv2.IMREAD_COLOR)

# [100,100]的BGR值
px=img[100,100]
print(px)

# 修改[101,101]的BGR值
img[101,101]=[255,255,255]
print(img[101,101])

#取得圖片資訊
print(img.shape)
print(img.size)

#圖片輸出
cv2.imwrite('Output.jpg', img)

#圖片顯示
#cv2.imshow("Title",img)
#cv2.waitKey(0)
