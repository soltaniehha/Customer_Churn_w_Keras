# Customer Churn with Keras in R
Solving the Customer Churn Problem with Multilayer Perceptron Neural Networks

**Problem to solve:** identify customers who are at risk of leaving

**Dataset:** IBM WATSON Telco Dataset
* 7,043 rows
* 20 varibales plus “Churn” column, which is our target variable

## PREREQUISITES/INSTALLATIONS
Install the following packages in R:

```{r}
pkgs <- c("keras", "tidyquant", "rsample", "recipes", "yardstick", "corrr", "ggplot2", "caret", "devtools")
install.packages(pkgs)
```

To install lime, magick should be installed as follows:

In terminal execute ```export PGK_CONFIG_PATH=/opt/local/lib/pkgconfig``` and in R:

```{r}
devtools::install_github("ropensci/magick",force=TRUE)
install.packages('lime')
```

Install Keras if you have not installed it before:

```{r}
library(keras)
install_keras()
```

For further instructions please see <https://keras.rstudio.com>. To check to see if keras was installed successfully:

```{r}
is_keras_available() # if the response was TRUE you're all good to go.
```


