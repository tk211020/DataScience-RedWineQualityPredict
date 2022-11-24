# [group4] Red Wine Quality Predict

### Groups
* 賴玠忠, 109753101
* 林彥賓, 109753111
* 簡筑節, 109753146
* 鄭宇軒, 108753121

### Goal
Create a model to predict the quality of red wine

### Demo 
You should provide an example commend to reproduce your result
```R
Rscript final_project.R --fold 5 --train winequality-red.csv --report xgboost_performance_recall.csv --model xgboost.rds --evulate recall
```
* fold: Cross validation fold
* train: training data path
* report: Cross validation output path
* model: the finished training model for predict
* evulate: evulate method, it will use accurate by default

### any on-line visualization

[Red Wine Quality Prediction Shiny](https://tk211020.shinyapps.io/RedWineQualityPrediction/)


## Folder organization and its related information

### docs
* Your presentation, 1091_datascience_FP_group4.pptx, by **Jan. 12**
* Any related document for the final project
  * papers
  * software user guide

### data

* Source
  * Red Wine Quality
  * [Red Wine Quality Kaggle](https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009)
* Input format
  * One .csv file.
  * Attribute Information
    * `fixed acidity` - most acids involved with wine or fixed or nonvolatile (do not evaporate readily);
    * `volatile acidity` - the amount of acetic acid in wine, which at too high of levels can lead to an unpleasant, vinegar taste;  
    * `citric acid` - found in small quantities, citric acid can add 'freshness' and flavor to wines;  
    * `residual sugar` - the amount of sugar remaining after fermentation stops, it's rare to find wines with less than 1 gram/liter and wines with greater than 45 grams/liter are considered sweet;  
    * `chlorides` - the amount of salt in the wine;  
    * `free sulfur dioxide` - the free form of SO2 exists in equilibrium between molecular SO2 (as a dissolved gas) and bisulfite ion; it prevents microbial growth and the oxidation of wine;  
    * `total sulfur dioxide` - amount of free and bound forms of S02; in low concentrations, SO2 is mostly undetectable in wine, but at free SO2 concentrations over 50 ppm, SO2 becomes evident in the nose and taste of wine;  
    * `density` - the density of water is close to that of water depending on the percent alcohol and sugar content;  
    * `pH` - describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic); most wines are between 3-4 on the pH scale;  
    * `sulphates` - a wine additive which can contribute to sulfur dioxide gas (S02) levels, wich acts as an antimicrobial and antioxidant
    * `alcohol` - the percent alcohol content of the wine;
    The output feature is:  
    * `quality` - output variable (based on sensory data, score between 0 and 10);
* Preprocessing
  * There is no missing value in this dataset

### code

* Which method do you use?
  * randomForest
  * xgboost
  * CART
* What is a null model for comparison?
  * use liner scale to assign quality to each feature and get average
* How do your perform evaluation? ie. Cross-validation, or extra separated data
  * Cross-validation
  * accuracy
  * Recall

### results

* Which metric do you use 
  * Accuracy, Recall
  * Null model : 0.861159874, 0.559015191
  * Random forest: 0.955577978, 0.464642851
  * xgboost: 0.992494122, 0.700042208
  * CART: 0.974367163, 0.345487005
* Is your improvement significant?
  * xgboost performance is both better than null model
  * Random forest and CART only better at accuracy
* What is the challenge part of your project?
  1. 需要嘗試多種方法以達到最佳預測結果
  2. 特徵分析時繁瑣但有其重要性
  3. 此專案使用Ｒ語言撰寫是極大挑戰

## References
* Code/implementation which you include/reference (__You should indicate in your presentation if you use code for others. Otherwise, cheating will result in 0 score for final project.__)
  * https://www.kaggle.com/gpreda/red-wine-quality-simple-eda-and-prediction
  * Create a matrix of scatterplots (pairs() equivalent) in ggplot2, https://stackoverflow.com/questions/3735286/create-a-matrix-of-scatterplots-pairs-equivalent-in-ggplot2
  * Gally R package: Extension to ggplot2 for correlation matrix and survival plots - R software and data visualization, http://www.sthda.com/english/wiki/ggally-r-package-extension-to-ggplot2-for-correlation-matrix-and-survival-plots-r-software-and-data-visualization
  * Shiny 入門, https://bookdown.org/tpemartin/shiny_intro/shiny-part-i.html#ui-
  * Shiny - renderPlot, https://shiny.rstudio.com/reference/shiny/1.0.4/renderPlot.html
  * Kaggle機器學習競賽神器XGBoost介紹, https://medium.com/jameslearningnote/%E8%B3%87%E6%96%99%E5%88%86%E6%9E%90-%E6%A9%9F%E5%99%A8%E5%AD%B8%E7%BF%92-%E7%AC%AC5-2%E8%AC%9B-kaggle%E6%A9%9F%E5%99%A8%E5%AD%B8%E7%BF%92%E7%AB%B6%E8%B3%BD%E7%A5%9E%E5%99%A8xgboost%E4%BB%8B%E7%B4%B9-1c8f55cffcc
  * 多類別模型深入分析-Amazon Machine Learning, https://docs.aws.amazon.com/zh_tw/machine-learning/latest/dg/multiclass-model-insights.html
* Packages you use
  * shiny
  * DT
  * data.table
  * caret
  * GGally
  * ggbiplot
  * ggplot2
  * dplyr
  * tidyr
* Related publications


