% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmsle.R
\name{rmsle.kd_cv}
\alias{rmsle.kd_cv}
\title{Root Mean Squared Log Error of kd_cv}
\usage{
\method{rmsle}{kd_cv}(actual, predicted = join_cv_predictions(actual)$predicted)
}
\arguments{
\item{actual}{a fitted kd_cv object}

\item{predicted}{the predicted rates, default is from kd_cv}
}
\description{
Root Mean Squared Log Error of kd_cv
}
