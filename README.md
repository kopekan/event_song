# event_song
KKBOX event song project 

## Project discribtion
利用KKTIX上近幾年演唱會售票資訊，嘗試建立模型預測每場演唱會的售票率。  
中期加入演唱會票開售前，在PTT、Dcard平台上關於此歌手的討論升量，以輔助預測。

## Data introduction
原始演唱會數量：472

## Project result
在嘗試許多迴歸及深度學習模型後，發現xgboost在所有的模型中始終保持最好的預測效能。比起MLP好上許多，可能是因為MLP在訓練時需要學習許多參數，但持有資料過少，因此很難將參數學習好。因此在這種情況下，不需要太多參數學習的迴歸方法反而可以表現得比較好。
