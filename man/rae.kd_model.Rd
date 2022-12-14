% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rae.R
\name{rae.kd_model}
\alias{rae.kd_model}
\title{Relative Absolute Error of kd_model}
\usage{
\method{rae}{kd_model}(
  actual,
  predicted = predict(actual$predict_model, actual$data$inputs, verbose = 0)[[4]]
)
}
\arguments{
\item{actual}{a fitted kd_model object}

\item{predicted}{the predicted rates, default is from kd_model}
}
\description{
Relative Absolute Error of kd_model
}
