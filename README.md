# Customer Churn with Keras in R
Solving the Customer Churn Problem with Multilayer Perceptron Neural Networks

Dataset: IBM WATSON Telco Dataset
* 7,043 rows
* 21 columns

Problem to solve: identify customers who are at risk of leaving. “Churn” column is our target.

### PREREQUISITES/INSTALLATIONS
In R:
```pkgs <- c("keras", "tidyquant", "rsample", "recipes", "yardstick", "corrr", "ggplot2", "caret", "devtools")
install.packages(pkgs)```

To install lime, magick should be installed as follows
In terminal:
```export PGK_CONFIG_PATH=/opt/local/lib/pkgconfig```
In R:
```devtools::install_github("ropensci/magick",force=TRUE)
install.packages('lime')```

Install Keras if you have not installed before
```library(keras)
install_keras()```

# To check to see if keras was installed do the following:
```is_keras_available() # if the response was TRUE you're all good to go.```


