# kedis (Keras-Disaggregation)

![GitHub R package version](https://img.shields.io/github/r-package/v/jackahall/kedis)
![GitHub issues](https://img.shields.io/github/issues/jackahall/kedis)
[![DOI](https://zenodo.org/badge/518570817.svg)](https://zenodo.org/badge/latestdoi/518570817)

A package allowing disaggregation regression to be fitted using Neural Networks from Keras.

## Installation
```{r}
remotes::install_github("jackahall/kedis")
````

## Overview
Import library
```{r}
library(kedis)
```

Prepare data. `shapes`, `covariates` and `population` can either be paths to .shp or .tif files, or `SpatVector` and `SpatRasters`.
```{r}
data <- prepare_data(shapes = "data/shapes.shp",
                     covariates = "data/covariates.tif",
                     population = "data/population.tif",
                     id_var = "id_var",
                     filter_var = "filter_var")
```

Build a kedis model. Log link because this data has a poisson distribution. Parameters are the same as for `keras::fit`. Layers are included as lists.

```{r}
model <- build_model(data,
                     layers_cov = list(layer_dense(units = 20, activation = "relu"),
                                       layer_dropout(rate = 0.2),
                                       layer_dense(units = 10, activation = "relu")),
                     layers_xy = list(layer_dense(units = 20, activation = "relu"),
                                       layer_dropout(rate = 0.2),
                                       layer_dense(units = 10, activation = "relu")),
                     link = "log",
                     optimizer = optimizer_adam(),
                     loss = loss_poisson())
```

Train the model on 1000 epochs, with early stopping.

```{r}
history <- train(model,
                 epochs = 1000,
                 callbacks = list(callback_early_stopping(monitor = "loss",
                                                          min_delta = 1e-10,
                                                          patience = 20)))
```

Plot history
```{r}
plot(history)
```

Predict disaggregated data
```{r}
predict(model)
```

Plot disaggregated data
```{r}
plot(model)
```

View scatter plots and metrics
```{r}
cor.plot(model)
mae(model)
rmse(model)
```

For predictions with new data, first format new data using `new_data_for_predict` where `model` is a `kd_model` object to predict with, `covariates` is a `SpatRaster` of covariates (layers must match that in the model) and `population` is a `SpatRaster` of population data
```{r}
newdata <- new_data_for_predict(model, covariates, population)
```

Predictions can then be made using the `predict` function:
```{r}
new_data_prediction <- predict(new_data)
```

This can be plotted:
```{r}
plot(new_data_prediction)
```

## Project Status
This project is still in early development. There is a working API, although this may change before the first major release.
