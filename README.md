# zavrsni_projekt

Project on [Faculty of Electrical Engineering and Computing, University of Zagreb](http://www.fer.unizg.hr/en) for testing machine learning algorithms for predicting plants growth on different geographic parcels.

## Requirements
- [R](https://www.r-project.org/) (4.8.2. or higher)
- [RStudio](https://www.rstudio.com/) (optional)

## Installation

Download project ( or git pull ) and start R script spojiPoKoordinatama.R, it's possible that you'll need to install some packages for R, but compiler will tell you if it's missing.

## Usage

Data used in script is located in downloaded folder, so the only thing needed is to start the script.

##### Results on example of ambrosia
````R
  # MLR
  results_amb = 1 / (1 + exp(-1 * X_amb_test %*% theta_amb_initial))
  results_amb # this part of code prints results of multiple linear regression for ambrosia

````

````R
  # ANN
  seedsANN = nnet(y_amb~., X_amb, size=23, rang = 0.5, maxit = 100)

  predict(seedsANN, X_amb_test, type="raw") # prints ANN results in console for ambrosia

````

````R
  # SVM
  pred <- predict(svm.model, X_amb_test)

  pred # prints SVM results in console for ambrosia

````

````R
  # MAXENT
  mxent.model <- maxent(X_amb, y_amb)
  
  maxent.result <- predict.maxent(mxent.model, X_amb_test)
  
  maxent.result # prints MAXENT results in console for ambrosia

````

##### Comparing sppedup

````R

  sum(pred == results_y_amb) # this prints number of correct predictions for specific algorithm
  
  # pred => SVM given in example code
  # maxent.results => MAXENT, use maxent.results instead of pred
  # annPred = predict(seedsANN, X_amb_test, type="raw") and then use annPred instead of pred
  # results_amb => MLR, use results_amb instead of pred

````


