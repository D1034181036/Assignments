## Extraction-of-Pixels-of-Ducks
- 此程式可將圖片中鴨子的像素辨識出來並標註於圖像上。  
- 實驗報告請見：Report.pdf

![](https://raw.githubusercontent.com/D1034181036/Extraction-of-Pixels-of-Ducks/master/Images/Input/demo.jpg)
![](https://raw.githubusercontent.com/D1034181036/Extraction-of-Pixels-of-Ducks/master/Images/Output/demo_black.jpg)
![](https://raw.githubusercontent.com/D1034181036/Extraction-of-Pixels-of-Ducks/master/Images/Output/demo_red.jpg)

## 使用說明
- 執行Main.py即可，詳細參數設定請參考下方程式說明。
* 可手動新增或刪除訓練資料，請將圖片變更自以下資料夾：
> * 鴨子預設路徑: /Images/Input/duck_sample/
> * 非鴨子預設路徑: /Images/Input/non_duck_sample/
> * 資料夾路徑可以在Main.py中做設定


## 環境需求

#### 建議執行環境
![](https://img.shields.io/badge/python-3.6-blue.svg) or ![](https://img.shields.io/badge/python-2.7-blue.svg)

#### 所需套件
```
numpy
opencv-python
scikit-learn
```

套件安裝指令如下：
```
pip install numpy
pip install opencv-python
pip install scikit-learn
```

## 程式說明

#### Main.py
負責參數設定與程式執行，詳細參數範例如下：
```
#設定非鴨子資料夾位置
config['train_src']='Images/Input/non_duck_sample/'

#設定鴨子資料夾位置
config['train_src2']='Images/Input/duck_sample/'

#測試資料大小
config['test_size']=0.1

#圖片處理方式(標紅點=0，標黑點=1)
config['replace_type']=1

#Input圖片路徑
config['input_src']='Images/Input/test_ducks.jpg'

#Output圖片路徑
config['output_src']='Images/Output/img_output.jpg'
```

#### Duck.py
主程式，負責分類器訓練、資料輸入輸出與處理。

#### functions.py
主程式會調用的函式庫，其中包含：
1. 取得圖片每個像素的RGB陣列並標註類別
2. 圖片辨識與處理後輸出