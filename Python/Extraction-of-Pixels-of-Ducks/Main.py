# -*- coding: utf-8 -*-
from Duck import *

#------Config------

config = {}

#設定非鴨子資料夾位置
config['train_src']='Images/Input/non_duck_sample/'

#設定鴨子資料夾位置
config['train_src2']='Images/Input/duck_sample/'

#測試資料大小
config['test_size']=0.1

#圖片處理方式(標紅點=0，標黑點=1)
config['replace_type']=1

#Input圖片路徑
#config['input_src']='Images/Input/full_ducks.jpg'
config['input_src']='Images/Input/test_ducks.jpg'

#Output圖片路徑
config['output_src']='Images/Output/img_output.jpg'


#------Config------

#執行程式
Duck(config).run()
